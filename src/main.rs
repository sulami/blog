use std::{
    collections::HashMap,
    ffi::OsStr,
    hash::Hash,
    path::{Path, PathBuf},
    str::FromStr,
};

use async_recursion::async_recursion;
use clap::{Parser, Subcommand};
use color_eyre::{
    eyre::{eyre, WrapErr},
    Report, Result,
};
use once_cell::sync::{Lazy, OnceCell};
use pulldown_cmark::{CodeBlockKind, Event, Tag, TagEnd};
use serde::{Deserialize, Serialize};
use syntect::{highlighting::ThemeSet, html::highlighted_html_for_string, parsing::SyntaxSet};
use tera::{to_value, Function, Tera, Value};
use time::{format_description::well_known::Rfc3339, Date, OffsetDateTime};
use tokio::{
    fs::{copy, create_dir_all, read_dir, remove_dir_all, File},
    io::{AsyncReadExt, AsyncWriteExt},
    sync::Mutex,
};

mod config;
mod server;

static SITE: OnceCell<Mutex<Site>> = OnceCell::new();

#[derive(Debug, Parser)]
struct Cli {
    #[command(subcommand)]
    command: Command,

    #[clap(long, short, default_value = "site.toml")]
    config: PathBuf,

    #[clap(long, short, default_value = "input")]
    input: PathBuf,

    #[clap(long, short, default_value = "output")]
    output: PathBuf,
}

#[derive(Clone, Copy, Debug, Subcommand)]
enum Command {
    /// Renders the site and exits
    Render,
    /// Starts a development server
    Serve {
        /// The port to listen on
        #[clap(long, short, default_value = "8080")]
        port: u16,
    },
    /// Removes the output directory
    Clean,
}

#[tokio::main]
async fn main() -> Result<()> {
    color_eyre::install()?;
    let args = Cli::parse();
    let config = config::load_config(&args.config).await?;

    let site = Site::new(&args.input, &args.output, &config.site);
    SITE.set(Mutex::new(site)).unwrap();

    match args.command {
        Command::Render => {
            render_site().await.wrap_err("failed to render site")?;
        }
        Command::Serve { port } => {
            render_site().await.wrap_err("failed to render site")?;
            server::development_server(port, Path::new(&args.input), Path::new(&args.output))
                .await?;
        }
        Command::Clean => {
            remove_dir_all(&args.output)
                .await
                .wrap_err("failed to remove output directory")?;
        }
    }

    Ok(())
}

/// Renders the entire site once.
async fn render_site() -> Result<()> {
    let mut site = SITE.get().unwrap().lock().await;
    let input = site.input_path.clone();
    let output = site.output_path.clone();

    let start = time::Instant::now();

    create_dir_all(&output)
        .await
        .wrap_err("failed to create output directory")?;

    copy_css(&input, &output)
        .await
        .wrap_err("failed to copy css")?;
    copy_raw_files(&input, &output)
        .await
        .wrap_err("failed to copy static files")?;

    site.load_pages(&input.join("content"))
        .await
        .wrap_err("failed to load pages")?;

    let index_page = index_page(&site);
    site.insert_page(index_page);

    let feed_page = atom_feed(&site).wrap_err("failed to render feed")?;
    site.insert_page(feed_page);

    site.render_pages(&output)
        .await
        .wrap_err("failed to render site pages")?;

    let finish = time::Instant::now();
    println!(
        "Rendered site in {:.3} seconds",
        (finish - start).as_seconds_f32()
    );

    Ok(())
}

/// Creates the index page. Should be called after all regular pages have been loaded.
fn index_page(site: &Site) -> Page {
    let mut page = Page {
        title: "Welcome".into(),
        kind: PageKind::Custom {
            template: "index.html",
            destination: "index.html",
        },
        source: PageSource::new_virtual("index"),
        slug: String::new(),
        link: "/".into(),
        url: site.url.clone(),
        tags: vec![],
        timestamp: None,
        content: String::new(),
        extra_context: HashMap::default(),
    };

    let mut posts: Vec<&Page> = site
        .pages
        .values()
        .filter(|p| p.kind == PageKind::Post)
        .collect();
    posts.sort_unstable_by_key(|p| p.timestamp);
    posts = posts.into_iter().rev().take(5).collect();
    page.insert_context("posts", &posts);

    page
}

/// Renders the Atom feed.
fn atom_feed(site: &Site) -> Result<Page> {
    let link = "/atom.xml".into();
    let url = format!("{}{}", site.url, link);
    let mut page = Page {
        title: "Archive".into(),
        kind: PageKind::Custom {
            template: "feed.xml",
            destination: "atom.xml",
        },
        source: PageSource::new_virtual("feed"),
        slug: "feed".into(),
        link,
        url,
        tags: vec![],
        timestamp: Some(OffsetDateTime::now_utc()),
        content: String::new(),
        extra_context: HashMap::default(),
    };

    let mut posts: Vec<&Page> = site
        .pages
        .values()
        .filter(|p| p.kind == PageKind::Post)
        .collect();
    posts.sort_unstable_by_key(|p| p.timestamp);
    posts.reverse();
    posts = posts.into_iter().take(10).collect();
    page.insert_context("posts", &posts);

    Ok(page)
}

/// Copies all raw files to the output directory.
async fn copy_raw_files(input: &Path, output: &Path) -> Result<()> {
    let source_dir = input.join("raw");

    let mut source_files = read_dir(&source_dir)
        .await
        .wrap_err("failed to read source directory")?;
    while let Some(path) = source_files.next_entry().await? {
        if !path.file_type().await?.is_file() {
            continue;
        }
        let target = output.join(path.file_name());
        copy(path.path(), target).await?;
    }

    Ok(())
}

/// Copies all CSS files to stylesheet.css in the output directory.
async fn copy_css(input: &Path, output: &Path) -> Result<()> {
    let source_dir = input.join("css");

    let mut source_files = read_dir(&source_dir)
        .await
        .wrap_err("failed to read source directory")?;

    let target = output.join("stylesheet.css");
    let mut output_file = File::create(&target).await?;

    while let Some(path) = source_files.next_entry().await? {
        if !path.file_type().await?.is_file() {
            continue;
        }
        let Some(Some("css")) = path.path().extension().map(OsStr::to_str) else {
            continue;
        };
        let mut contents = vec![];
        File::open(path.path())
            .await
            .wrap_err("failed to open source file")?
            .read_to_end(&mut contents)
            .await
            .wrap_err("failed to read source file")?;
        output_file
            .write_all(&contents)
            .await
            .wrap_err("failed to write file contents")?;
    }

    Ok(())
}

/// The context for rendering a page.
#[derive(Debug, Serialize)]
struct Context<'a> {
    site: &'a Site,
    page: &'a Page,
}

/// Site metadata.
#[derive(Debug, Serialize)]
struct Site {
    title: String,
    description: String,
    author: String,
    email: String,
    url: String,
    code_theme: String,
    input_path: PathBuf,
    output_path: PathBuf,
    menu: Vec<MenuItem>,
    pages: HashMap<PageSource, Page>,
    #[serde(skip)]
    tera: Tera,
}

impl Site {
    /// Creates a new site.
    fn new(input: &Path, output: &Path, site_config: &config::Site) -> Self {
        let mut tera = Tera::new(&format!("{}/templates/**/*", input.display()))
            .expect("failed to load templates");
        tera.autoescape_on(vec![]);

        Self {
            title: site_config.title.clone(),
            description: site_config.description.clone(),
            author: site_config.author.clone(),
            email: site_config.email.clone(),
            url: site_config.url.clone(),
            code_theme: site_config.code_theme.clone(),
            input_path: input.to_path_buf(),
            output_path: output.to_path_buf(),
            menu: vec![
                MenuItem::new("Feed", PageSource::new_virtual("feed")),
                MenuItem::new("About", PageSource::new_file("input/content/about.md")),
            ],
            pages: HashMap::default(),
            tera,
        }
    }
    /// Inserts a page into the site.
    fn insert_page(&mut self, page: Page) {
        self.pages.insert(page.source.clone(), page);
    }

    /// Loads all pages from the given directory.
    #[async_recursion]
    async fn load_pages(&mut self, dir: &Path) -> Result<()> {
        let mut source_files = read_dir(dir).await?;
        while let Some(path) = source_files.next_entry().await? {
            if path.file_type().await?.is_dir() {
                self.load_pages(&path.path()).await?;
            }
            let Some(Some("md")) = path.path().extension().map(OsStr::to_str) else {
                continue;
            };
            self.pages.insert(
                PageSource::File(path.path().clone()),
                Page::new(path.path(), self)
                    .await
                    .wrap_err(format!("failed to load page {}", path.path().display()))?,
            );
        }

        Ok(())
    }

    /// Renders all pages and writes them to the output directory.
    async fn render_pages(&mut self, output: &Path) -> Result<()> {
        // NB Reload the url_for function with new pages.
        self.tera
            .register_function("url_for", make_url_for(self.pages.clone()));

        for page in self.pages.values() {
            let rendered = page
                .render(self)
                .await
                .wrap_err_with(|| format!("failed to render page {:?}", page.source))?;
            create_and_write(&output.join(page.output_path()), &rendered)
                .await
                .wrap_err_with(|| format!("failed to write page {:?}", page.output_path()))?;
        }

        Ok(())
    }
}

/// An item in the navigation menu.
#[derive(Debug, Serialize)]
struct MenuItem {
    title: String,
    link: PageSource,
}

impl MenuItem {
    fn new(title: &str, link: PageSource) -> Self {
        Self {
            title: title.into(),
            link,
        }
    }
}

/// A page on the site.
#[derive(Debug, Serialize, Clone)]
struct Page {
    kind: PageKind,
    source: PageSource,
    title: String,
    slug: String,
    link: String,
    url: String,
    tags: Vec<String>,
    #[serde(serialize_with = "serialize_optional_timestamp")]
    timestamp: Option<OffsetDateTime>,
    content: String,
    extra_context: HashMap<String, Value>,
}

fn serialize_optional_timestamp<S>(
    timestamp: &Option<OffsetDateTime>,
    serializer: S,
) -> Result<S::Ok, S::Error>
where
    S: serde::ser::Serializer,
{
    match timestamp {
        Some(ts) => ts
            .format(&Rfc3339)
            .unwrap()
            .to_string()
            .serialize(serializer),
        None => "".serialize(serializer),
    }
}

/// The type of a page.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
enum PageSource {
    File(PathBuf),
    Virtual(String),
}

impl Serialize for PageSource {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::ser::Serializer,
    {
        match self {
            Self::File(path) => format!("file:{}", path.display()).serialize(serializer),
            Self::Virtual(name) => format!("virtual:{name}").serialize(serializer),
        }
    }
}

impl PageSource {
    /// Creates a new virtual page source.
    fn new_virtual(name: &str) -> Self {
        Self::Virtual(name.into())
    }

    /// Creates a new file page source.
    fn new_file(name: impl Into<PathBuf>) -> Self {
        Self::File(name.into())
    }
}

/// The type of a page.
#[derive(Default, Debug, Deserialize, Serialize, PartialEq, Eq, Clone)]
enum PageKind {
    #[default]
    Post,
    Page,
    Custom {
        template: &'static str,
        destination: &'static str,
    },
}

impl FromStr for PageKind {
    type Err = Report;

    fn from_str(s: &str) -> Result<Self> {
        match s {
            "post" => Ok(Self::Post),
            "page" => Ok(Self::Page),
            _ => Err(eyre!("invalid page kind")),
        }
    }
}

// NB These are somewhat expensive to load, so we use a lazy static to only load them once.
static SS: Lazy<SyntaxSet> = Lazy::new(SyntaxSet::load_defaults_newlines);
static TS: Lazy<ThemeSet> = Lazy::new(ThemeSet::load_defaults);

impl Page {
    /// Creates a new page from the given source file.
    async fn new(source: PathBuf, site: &Site) -> Result<Self> {
        let mut fp = File::open(&source).await?;
        let mut file_contents = vec![];
        fp.read_to_end(&mut file_contents).await?;
        let file_string = String::from_utf8(file_contents)?;

        let (frontmatter_section, content_section) = file_string
            .split_once("---")
            .ok_or(eyre!("no frontmatter divider found"))?;

        let frontmatter: Frontmatter = frontmatter_section
            .parse()
            .wrap_err("failed to parse frontmatter")?;

        let link = match frontmatter.kind {
            PageKind::Post => format!("/posts/{}/", frontmatter.slug),
            PageKind::Page => format!("/{}/", frontmatter.slug),
            PageKind::Custom { destination, .. } => destination.into(),
        };
        let url = format!("{}{}", site.url, link);
        let content = render_markdown(content_section, site);

        Ok(Self {
            kind: frontmatter.kind,
            source: PageSource::File(source),
            title: frontmatter.title,
            slug: frontmatter.slug,
            link,
            url,
            tags: frontmatter.tags,
            timestamp: frontmatter.timestamp,
            content,
            extra_context: HashMap::default(),
        })
    }

    /// Inserts a key-value-pair into the extra context.
    fn insert_context<T>(&mut self, key: &str, val: &T)
    where
        T: Serialize + ?Sized,
    {
        self.extra_context
            .insert(key.into(), to_value(val).unwrap());
    }

    /// Returns the template to use for rendering the page.
    fn template(&self) -> String {
        match &self.kind {
            PageKind::Post => "post.html".into(),
            PageKind::Page => "page.html".into(),
            PageKind::Custom { template, .. } => template.to_string(),
        }
    }

    /// Returns the path where the page should be written to.
    fn output_path(&self) -> PathBuf {
        match &self.kind {
            PageKind::Post => PathBuf::from(format!("posts/{}/index.html", self.slug)),
            PageKind::Page => PathBuf::from(format!("{}/index.html", self.slug)),
            PageKind::Custom { destination, .. } => destination.into(),
        }
    }

    /// Renders the page using the given template and Tera instance.
    async fn render(&self, site: &Site) -> Result<String> {
        // println!("Rendering page {:?}", self.source);
        let ctx = Context { page: self, site };
        let rendered = site.tera.render(
            &self.template(),
            &tera::Context::from_serialize(ctx).wrap_err("failed to create context")?,
        )?;

        Ok(rendered)
    }
}

/// Creates a file and all required parent directories, then writes the given content to it.
async fn create_and_write(path: &Path, content: &str) -> Result<()> {
    create_dir_all(path.parent().unwrap())
        .await
        .wrap_err("failed to create parent directory")?;
    File::create(path)
        .await
        .wrap_err("failed to create file")?
        .write_all(content.as_bytes())
        .await
        .wrap_err("failed to write file contents")?;
    Ok(())
}

/// Converts a title into a slug.
fn slugify(title: &str) -> String {
    title.to_lowercase().replace(' ', "-")
}

/// Creates the url_for template filter.
fn make_url_for(pages: HashMap<PageSource, Page>) -> impl Function {
    Box::new(
        move |args: &HashMap<String, Value>| -> Result<Value, tera::Error> {
            let link: String = args
                .get("link")
                .expect("argument link not found")
                .as_str()
                .unwrap()
                .into();
            let (kind, name) = link
                .split_once(':')
                .ok_or(tera::Error::from("invalid link format"))?;
            let key = match kind {
                "file" => Ok(PageSource::File(name.into())),
                "virtual" => Ok(PageSource::Virtual(name.into())),
                _ => Err(tera::Error::from("invalid kind")),
            }?;
            let page = pages.get(&key).ok_or(tera::Error::from("page not found"))?;
            Ok(to_value(page.link.clone()).unwrap())
        },
    )
}

struct Frontmatter {
    title: String,
    slug: String,
    kind: PageKind,
    timestamp: Option<OffsetDateTime>,
    tags: Vec<String>,
}

impl FromStr for Frontmatter {
    type Err = Report;

    fn from_str(s: &str) -> Result<Self> {
        #[derive(Default, Deserialize)]
        enum DeserializedPageKind {
            #[default]
            #[serde(rename = "post")]
            Post,
            #[serde(rename = "page")]
            Page,
        }

        impl From<DeserializedPageKind> for PageKind {
            fn from(kind: DeserializedPageKind) -> Self {
                match kind {
                    DeserializedPageKind::Post => Self::Post,
                    DeserializedPageKind::Page => Self::Page,
                }
            }
        }

        #[derive(Deserialize)]
        struct DeserializedFrontmatter {
            title: String,
            slug: Option<String>,
            kind: Option<DeserializedPageKind>,
            timestamp: Option<Date>,
            tags: Option<Vec<String>>,
        }

        let deserialized: DeserializedFrontmatter = toml::from_str(s)?;
        let slug = deserialized
            .slug
            .unwrap_or_else(|| slugify(&deserialized.title));

        Ok(Self {
            title: deserialized.title,
            slug,
            kind: deserialized.kind.unwrap_or_default().into(),
            timestamp: deserialized.timestamp.map(|d| d.midnight().assume_utc()),
            tags: deserialized.tags.unwrap_or_default(),
        })
    }
}

fn render_markdown(source: &str, site: &Site) -> String {
    let mut rendered = String::new();
    let mut code_language: Option<String> = None;
    let ss = &SS;
    let theme_set = &TS;

    let parser = pulldown_cmark::Parser::new_ext(
        source,
        pulldown_cmark::Options::ENABLE_FOOTNOTES | pulldown_cmark::Options::ENABLE_TABLES,
    )
    .filter_map(|mut ev| match ev {
        Event::Start(Tag::CodeBlock(CodeBlockKind::Fenced(ref lang))) => {
            code_language = Some(lang.to_string());
            None
        }
        Event::End(TagEnd::CodeBlock) => {
            code_language = None;
            None
        }
        Event::Text(ref mut text) => {
            if let Some(lang) = &code_language {
                let syntax = ss
                    .find_syntax_by_token(lang)
                    .unwrap_or_else(|| ss.find_syntax_plain_text());
                let code = highlighted_html_for_string(
                    text,
                    ss,
                    syntax,
                    &theme_set.themes[&site.code_theme],
                )
                .expect("failed to highlight code");
                Some(Event::Html(code.into()))
            } else {
                Some(ev)
            }
        }
        // Event::FootnoteReference(ref label) => {
        //     ev = Event::Html(
        //         format!(
        //             r##"<input type="checkbox" id="fnr{label}" /><label for="fnr{label}"><sup></sup></label>"##,
        //         )
        //         .into(),
        //     );
        //     Some(ev)
        // }
        // Event::Start(Tag::FootnoteDefinition(ref label)) => {
        //     ev = Event::Html(
        //         format!(
        //             r##"<div class="footnote-definition" id="fnd{label}">"##,
        //         )
        //         .into(),
        //     );
        //     Some(ev)
        // }
        _ => Some(ev),
    });

    pulldown_cmark::html::push_html(&mut rendered, parser);

    rendered
}
