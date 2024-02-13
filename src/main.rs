use std::{
    collections::HashMap,
    ffi::OsStr,
    hash::Hash,
    path::{Path, PathBuf},
    str::FromStr,
};

use async_recursion::async_recursion;
use clap::{Parser, Subcommand};
use color_eyre::{eyre::WrapErr, Report, Result};
use notify::{recommended_watcher, Event, EventKind, RecursiveMode, Watcher};
use once_cell::sync::Lazy;
use serde::Serialize;
use tera::{to_value, Function, Tera, Value};
use tokio::{
    fs::{copy, create_dir_all, read_dir, remove_dir_all, File},
    io::{AsyncReadExt, AsyncWriteExt},
    signal, spawn,
    sync::Mutex,
};
use toml::Table;
use tower_http::services::ServeDir;

const SITE_TITLE: &str = "sulami's blog";
const SITE_DESCRIPTION: &str = "Weak Opinions, Strongly Held";
const SITE_AUTHOR: &str = "Robin Schroer";
const DEFAULT_PORT: u16 = 8080;
const INPUT_DIR: &str = "/Users/sulami/src/blog/input";
const OUTPUT_DIR: &str = "/Users/sulami/src/blog/output";

static TERA: Lazy<Mutex<Tera>> = Lazy::new(|| {
    let mut tera =
        Tera::new(&format!("{INPUT_DIR}/templates/**/*.html")).expect("failed to load templates");
    tera.autoescape_on(vec![]);
    Mutex::new(tera)
});

#[derive(Debug, Parser)]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Clone, Copy, Debug, Subcommand)]
enum Command {
    /// Renders the site and exits
    Render,
    /// Starts a development server
    Serve {
        /// The port to listen on
        port: Option<u16>,
    },
    /// Removes the output directory
    Clean,
}

#[tokio::main]
async fn main() -> Result<()> {
    color_eyre::install()?;
    let args = Cli::parse();

    match args.command {
        Command::Render => {
            render_site().await.wrap_err("failed to render site")?;
        }
        Command::Serve { port } => {
            render_site().await.wrap_err("failed to render site")?;
            let server = spawn(async move {
                if let Err(err) = serve_site(port.unwrap_or(DEFAULT_PORT)).await {
                    eprintln!("Error: {err:?}");
                }
            });

            let mut watcher = recommended_watcher(handle_notify_event)?;
            watcher.watch(Path::new(INPUT_DIR), RecursiveMode::Recursive)?;

            if let Err(err) = server.await {
                eprintln!("Error joining task: {err:?}");
            }
        }
        Command::Clean => {
            remove_dir_all(OUTPUT_DIR)
                .await
                .wrap_err("failed to remove output directory")?;
        }
    }

    Ok(())
}

/// Handles a notify event, i.e. a file on disk has changed.
///
/// Reloads Tera templates and re-renders all pages. In the future we could be smart about it and
/// only re-render files that have changed, but computers are fast.
#[tokio::main]
async fn handle_notify_event(res: notify::Result<Event>) {
    if let Ok(Event {
        kind: EventKind::Modify(_),
        ..
    }) = res
    {
        if let Err(err) = TERA.lock().await.full_reload() {
            eprintln!("Error: {err:?}");
        }
        if let Err(err) = render_site().await.wrap_err("failed to re-render site") {
            eprintln!("Error: {err:?}");
        }
    }
}

/// Serves the site output.
async fn serve_site(port: u16) -> Result<()> {
    let app = axum::Router::new().nest_service("/", ServeDir::new(OUTPUT_DIR));
    let listener = tokio::net::TcpListener::bind(format!("0.0.0.0:{port}"))
        .await
        .unwrap();
    println!("Listening on http://0.0.0.0:{port}");
    axum::serve(listener, app)
        .with_graceful_shutdown(shutdown_signal())
        .await
        .unwrap();
    Ok(())
}

/// Shutdown handler.
async fn shutdown_signal() {
    let ctrl_c = async {
        signal::ctrl_c()
            .await
            .expect("failed to install Ctrl+C handler");
    };

    #[cfg(unix)]
    let terminate = async {
        signal::unix::signal(signal::unix::SignalKind::terminate())
            .expect("failed to install signal handler")
            .recv()
            .await;
    };

    #[cfg(not(unix))]
    let terminate = std::future::pending::<()>();

    tokio::select! {
        _ = ctrl_c => {},
        _ = terminate => {},
    }
}

/// Renders the entire site once.
async fn render_site() -> Result<()> {
    let mut site = Site {
        title: SITE_TITLE.to_string(),
        description: SITE_DESCRIPTION.to_string(),
        author: SITE_AUTHOR.to_string(),
        menu: vec![
            MenuItem::new("Home", PageSource::new_virtual("index")),
            MenuItem::new("Archive", PageSource::new_virtual("archive")),
        ],
        pages: HashMap::default(),
    };

    create_dir_all(OUTPUT_DIR)
        .await
        .wrap_err("failed to create output directory")?;

    copy_css().await.wrap_err("failed to copy css")?;
    copy_raw_files()
        .await
        .wrap_err("failed to copy static files")?;

    site.load_pages(&PathBuf::from(INPUT_DIR).join("content"))
        .await
        .wrap_err("failed to load pages")?;

    let index_page = index_page(&site);
    site.insert_page(index_page);

    let archive_page = archive_page(&site);
    site.insert_page(archive_page);

    site.render_pages()
        .await
        .wrap_err("failed to render site")?;

    Ok(())
}

/// Creates the index page. Should be called after all regular pages have been loaded.
fn index_page(site: &Site) -> Page {
    let mut page = Page {
        title: "Welcome".to_string(),
        kind: PageKind::Custom {
            template: "index.html",
            destination: "index.html",
        },
        source: PageSource::new_virtual("index"),
        slug: String::new(),
        link: "/".to_string(),
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
    posts.sort_by_cached_key(|p| p.timestamp.clone());
    posts = posts.into_iter().rev().take(5).collect();
    page.insert_context("recent_posts", &posts);

    page
}

/// Renders the archive page.
fn archive_page(site: &Site) -> Page {
    let mut page = Page {
        title: "Archive".to_string(),
        kind: PageKind::Custom {
            template: "archive.html",
            destination: "posts/index.html",
        },
        source: PageSource::new_virtual("archive"),
        slug: "archive".to_string(),
        link: "/posts/".to_string(),
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
    posts.sort_by_cached_key(|p| p.timestamp.clone());
    posts.reverse();
    page.insert_context("posts", &posts);

    page
}

/// Copies all raw files to the output directory.
async fn copy_raw_files() -> Result<()> {
    let source_dir = PathBuf::from(INPUT_DIR).join("raw");
    let output_dir = PathBuf::from(OUTPUT_DIR);

    let mut source_files = read_dir(&source_dir)
        .await
        .wrap_err("failed to read source directory")?;
    while let Some(path) = source_files.next_entry().await? {
        if !path.file_type().await?.is_file() {
            continue;
        }
        let target = output_dir.join(path.file_name());
        copy(path.path(), target).await?;
    }

    Ok(())
}

/// Copies all CSS files to stylesheet.css in the output directory.
async fn copy_css() -> Result<()> {
    let source_dir = PathBuf::from(INPUT_DIR).join("css");
    let output_dir = PathBuf::from(OUTPUT_DIR);

    let mut source_files = read_dir(&source_dir)
        .await
        .wrap_err("failed to read source directory")?;

    let target = output_dir.join("stylesheet.css");
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
    menu: Vec<MenuItem>,
    pages: HashMap<PageSource, Page>,
}

impl Site {
    /// Inserts a page into the site.
    fn insert_page(&mut self, page: Page) {
        self.pages.insert(page.source.clone(), page);
    }

    /// Returns a page by its key.
    fn get_page(&self, key: &PageSource) -> Option<&Page> {
        self.pages.get(key)
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
                Page::new(path.path())
                    .await
                    .wrap_err(format!("failed to load page {}", path.path().display()))?,
            );
        }

        Ok(())
    }

    /// Renders all pages and writes them to the output directory.
    async fn render_pages(&self) -> Result<()> {
        let output_dir = PathBuf::from(OUTPUT_DIR);

        // NB Reload the url_for function with new pages.
        TERA.lock()
            .await
            .register_function("url_for", make_url_for(self.pages.clone()));

        for page in self.pages.values() {
            let rendered = page.render(self).await.wrap_err("failed to render page")?;
            create_and_write(&output_dir.join(page.output_path()), &rendered)
                .await
                .wrap_err("failed to write page")?;
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
            title: title.to_string(),
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
    tags: Vec<String>,
    timestamp: Option<String>,
    content: String,
    extra_context: HashMap<String, Value>,
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
        Self::Virtual(name.to_string())
    }
}

/// The type of a page.
#[derive(Default, Debug, Serialize, PartialEq, Eq, Clone)]
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
            _ => Err(Report::msg("invalid page kind")),
        }
    }
}

impl Page {
    /// Creates a new page from the given source file.
    async fn new(source: PathBuf) -> Result<Self> {
        let mut fp = File::open(&source).await?;
        let mut file_contents = vec![];
        fp.read_to_end(&mut file_contents).await?;
        let file_string = String::from_utf8(file_contents)?;

        let (metadata_section, content_section) = file_string
            .split_once("---")
            .ok_or(Report::msg("no metadata divider found"))?;
        let metadata = metadata_section.parse::<Table>()?;
        let title = metadata
            .get("title")
            .ok_or(Report::msg("no title found in metadata"))?
            .as_str()
            .ok_or(Report::msg("invalid type for title"))?
            .to_string();
        let slug = metadata
            .get("slug")
            .map(|slug| -> Result<String> {
                Ok(slug
                    .as_str()
                    .ok_or(Report::msg("invalid type for slug"))?
                    .to_string())
            })
            .transpose()?
            .unwrap_or(slugify(&title));
        let kind = metadata
            .get("kind")
            .map(|kind| -> Result<PageKind> {
                kind.as_str()
                    .ok_or(Report::msg("invalid type for kind"))?
                    .parse::<PageKind>()
            })
            .transpose()?
            .unwrap_or_default();
        let timestamp = metadata
            .get("timestamp")
            .map(|timestamp| -> Result<String> {
                Ok(timestamp
                    .as_str()
                    .ok_or(Report::msg("invalid type for timestamp"))?
                    .to_string())
            })
            .transpose()?;
        let tags = metadata
            .get("tags")
            .map(|tags| -> Result<Vec<String>> {
                tags.as_array()
                    .ok_or(Report::msg("invalid type for tags"))?
                    .iter()
                    .map(|tag| -> Result<String> {
                        Ok(tag
                            .as_str()
                            .ok_or(Report::msg("invalid type for tag"))?
                            .to_string())
                    })
                    .collect::<Result<Vec<String>>>()
            })
            .transpose()?
            .unwrap_or_default();

        let link = match kind {
            PageKind::Post => format!("/posts/{}/", slug),
            PageKind::Page => format!("/{}/", slug),
            PageKind::Custom { destination, .. } => destination.to_string(),
        };

        let mut content = String::new();
        let parser = pulldown_cmark::Parser::new_ext(
            content_section,
            pulldown_cmark::Options::ENABLE_FOOTNOTES,
        );
        pulldown_cmark::html::push_html(&mut content, parser);

        Ok(Self {
            kind,
            source: PageSource::File(source),
            title,
            slug,
            link,
            tags,
            timestamp,
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
            PageKind::Post => "post.html".to_string(),
            PageKind::Page => "page.html".to_string(),
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
        println!("Rendering page {:?}", self.source);
        let ctx = Context { page: self, site };
        let rendered = TERA.lock().await.render(
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
            let link = args
                .get("link")
                .expect("argument link not found")
                .as_str()
                .unwrap()
                .to_string();
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
