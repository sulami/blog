use crate::Site;
use eyre::{eyre, Report, Result, WrapErr};
use itertools::Itertools;
use jiff::{civil::Date, Zoned};
use minijinja::Value;
use regex::Regex;
use serde::{Deserialize, Serialize};
use std::{
    collections::HashMap, fs::File, hash::Hash, io::Read, path::PathBuf, str::FromStr,
    sync::LazyLock,
};
use tracing::{debug, instrument};

pub mod markdown;

/// Regex used to strip footnotes from rendered output.
static FOOTNOTE_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r#"(?s)<input type="checkbox".+?/>.+?<span class="footnote">.+?</span>"#)
        .expect("invalid footnote regex")
});

/// Regex used to `script` tags from rendered output.
static SCRIPT_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r#"(?s)<script.+?</script>"#).expect("invalid script regex"));

/// A page on the site.
#[derive(Clone, Debug, Serialize)]
pub struct Page {
    pub kind: PageKind,
    pub source: PageSource,
    title: String,
    slug: String,
    pub link: String,
    pub tags: Vec<String>,
    pub draft: bool,
    templated: bool,
    pub timestamp: Option<Date>,
    content: String,
    extra_context: HashMap<String, Value>,
}

impl Page {
    /// Creates a new page from the given source file.
    #[instrument(skip(site))]
    pub fn new(source: PathBuf) -> Result<Self> {
        let file_string = {
            debug!("Loading page file");
            let mut fp = File::open(&source)?;
            let mut file_contents = vec![];
            fp.read_to_end(&mut file_contents)?;
            String::from_utf8(file_contents)?
        };

        let (frontmatter_section, content_section) = file_string
            .split_once("---")
            .ok_or(eyre!("no frontmatter divider found"))?;

        let frontmatter: Frontmatter = frontmatter_section
            .parse()
            .wrap_err("failed to parse frontmatter")?;

        let link = match frontmatter.kind {
            PageKind::Post => format!("/posts/{}/", frontmatter.slug),
            PageKind::Page => format!("/{}/", frontmatter.slug),
            PageKind::Custom {
                ref destination, ..
            } => destination.into(),
        };

        Ok(Self {
            kind: frontmatter.kind,
            source: PageSource::File(source),
            title: frontmatter.title,
            slug: frontmatter.slug,
            link,
            tags: frontmatter.tags,
            draft: frontmatter.draft,
            templated: frontmatter.templated,
            timestamp: frontmatter.timestamp,
            content: content_section.to_string(),
            extra_context: HashMap::default(),
        })
    }

    /// Inserts a key-value-pair into the extra context.
    fn insert_context<T>(&mut self, key: &str, val: &T)
    where
        T: Serialize + ?Sized,
    {
        self.extra_context
            .insert(key.into(), Value::from_serialize(val));
    }

    /// Returns the template to use for rendering the page.
    fn template(&self) -> &str {
        match &self.kind {
            PageKind::Post => "post.html",
            PageKind::Page => "page.html",
            PageKind::Custom { template, .. } => template,
        }
    }

    /// Returns the path where the page should be written to.
    pub fn output_path(&self) -> PathBuf {
        match &self.kind {
            PageKind::Post => PathBuf::from(format!("posts/{}/index.html", self.slug)),
            PageKind::Page => PathBuf::from(format!("{}/index.html", self.slug)),
            PageKind::Custom { destination, .. } => destination.into(),
        }
    }

    /// Renders the page in the context of the given site.
    ///
    /// This is a multi-pass process:
    /// 1. If `self.templated` is true, render the content in-place as Jinja.
    /// 2. If `self.markdown` is true, render the content from Markdown to HTML.
    /// 3. If `self.template()` is Some, render to it with the rendered content.
    #[instrument(skip_all, fields(source = ?self.source, output = ?self.output_path()))]
    pub fn render(&self, site: &Site) -> Result<String> {
        debug!("Rendering page");

        let mut ctx = Context {
            page: self,
            site,
            rendered_content: None,
        };

        let templated_content = if self.templated {
            &site
                .jinja
                .render_str(&self.content, &ctx)
                .wrap_err("failed to render content Jinja")?
        } else {
            &self.content
        };

        let rendered_content = markdown::render(templated_content, site);
        ctx.rendered_content = Some(&rendered_content);
        let template = site
            .jinja
            .get_template(self.template())
            .wrap_err("template not found")?;
        let rendered = template.render(&ctx).wrap_err("failed to render page")?;

        Ok(rendered)
    }

    /// Creates the index page. Should be called after all regular pages have been loaded into
    /// `site`.
    pub fn index_page(site: &Site) -> Self {
        let mut page = Self {
            title: "Index".into(),
            kind: PageKind::Custom {
                template: "index.html",
                destination: "index.html".into(),
            },
            source: PageSource::new_virtual("index"),
            slug: "index".into(),
            link: "/".into(),
            tags: vec![],
            draft: false,
            templated: true,
            timestamp: None,
            content: String::new(),
            extra_context: HashMap::default(),
        };
        page.insert_context(
            "recent_posts",
            &site.posts().iter().take(5).collect::<Vec<_>>(),
        );
        page.insert_context(
            "best_posts",
            &site
                .posts()
                .iter()
                .filter(|p| p.tags.contains(&"best-of".into()))
                .take(5)
                .collect::<Vec<_>>(),
        );
        page
    }

    /// Creates the Atom feed. Should be called after all posts have been loaded into `site`.
    pub fn atom_feed(site: &Site) -> Self {
        let mut page = Self {
            title: "Feed".into(),
            kind: PageKind::Custom {
                template: "feed.xml",
                destination: "atom.xml".into(),
            },
            source: PageSource::new_virtual("feed"),
            slug: "feed".into(),
            link: "/atom.xml".into(),
            tags: vec![],
            draft: false,
            templated: true,
            timestamp: Some(Zoned::now().date()),
            content: String::new(),
            extra_context: HashMap::default(),
        };

        // Strip the footnotes from the content, the checkboxes render weirdly in feed readers, and
        // the footnotes don't fit in inline without CSS.
        let posts = site
            .posts()
            .into_iter()
            .take(10)
            .map(|mut post| {
                post.content = FOOTNOTE_RE.replace_all(&post.content, "").to_string();
                post.content = SCRIPT_RE.replace_all(&post.content, "").to_string();
                post
            })
            .collect::<Vec<_>>();
        page.insert_context("posts", &posts);

        page
    }

    /// Creates the sitemap. Should be called after all posts have been loaded into `site`.
    pub fn sitemap(site: &Site) -> Self {
        let mut page = Self {
            title: "Sitemap".into(),
            kind: PageKind::Custom {
                template: "sitemap.xml",
                destination: "sitemap.xml".into(),
            },
            source: PageSource::new_virtual("sitemap"),
            slug: "feed".into(),
            link: "/sitemap.xml".into(),
            tags: vec![],
            draft: false,
            templated: true,
            timestamp: Some(Zoned::now().date()),
            content: String::new(),
            extra_context: HashMap::default(),
        };
        page.insert_context(
            "pages",
            &site
                .pages
                .values()
                .sorted_unstable_by_key(|p| &p.slug)
                .collect::<Vec<_>>(),
        );
        page
    }

    /// Creates a page for the given tag.
    pub fn tag_page(site: &Site, tag: &str) -> Self {
        let mut page = Self {
            title: format!("Tag: {tag}"),
            kind: PageKind::Custom {
                template: "tag.html",
                destination: format!("tags/{}/index.html", tag),
            },
            source: PageSource::new_virtual(format!("tags/{}", tag)),
            slug: tag.into(),
            link: format!("/tags/{}/", tag),
            tags: vec![],
            draft: false,
            templated: true,
            timestamp: None,
            content: String::new(),
            extra_context: HashMap::default(),
        };

        let posts = site
            .posts()
            .into_iter()
            .filter(|p| p.tags.contains(&tag.to_string()))
            .collect::<Vec<_>>();
        page.insert_context("posts", &posts);

        page
    }
}

/// The context for rendering a page.
#[derive(Debug, Serialize)]
struct Context<'a> {
    site: &'a Site,
    page: &'a Page,
    /// Only present in the second render pass, when rendering into the template.
    rendered_content: Option<&'a str>,
}

/// The source of a page.
#[derive(Debug, PartialEq, Eq, Hash, Clone)]
pub enum PageSource {
    /// A markdown file with the content.
    File(PathBuf),
    /// A virtual page created in code.
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

impl FromStr for PageSource {
    type Err = Report;

    fn from_str(s: &str) -> std::result::Result<Self, Self::Err> {
        let (kind, name) = s
            .split_once(':')
            .ok_or(eyre!("invalid page source format"))?;
        match kind {
            "file" => Ok(Self::File(name.into())),
            "virtual" => Ok(Self::Virtual(name.into())),
            _ => Err(eyre!("invalid kind")),
        }
    }
}

impl PageSource {
    /// Creates a new virtual page source.
    pub fn new_virtual(name: impl Into<String>) -> Self {
        Self::Virtual(name.into())
    }
}

/// The kind of page.
#[derive(Debug, Deserialize, Serialize, PartialEq, Eq, Clone)]
pub enum PageKind {
    /// A blog post, located at /posts/.
    Post,
    /// A regular page, located at /.
    Page,
    /// A custom page, located at the given destination.
    Custom {
        template: &'static str,
        destination: String,
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

/// Converts a title into a slug.
fn slugify(title: &str) -> String {
    title.to_lowercase().replace(' ', "-")
}

/// The frontmatter of a page.
struct Frontmatter {
    title: String,
    slug: String,
    kind: PageKind,
    timestamp: Option<Date>,
    tags: Vec<String>,
    draft: bool,
    templated: bool,
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
            draft: Option<bool>,
            templated: Option<bool>,
        }

        let deserialized: DeserializedFrontmatter = toml::from_str(s)?;
        let slug = deserialized
            .slug
            .unwrap_or_else(|| slugify(&deserialized.title));

        Ok(Self {
            title: deserialized.title,
            slug,
            kind: deserialized.kind.unwrap_or_default().into(),
            timestamp: deserialized.timestamp,
            tags: deserialized.tags.unwrap_or_default(),
            draft: deserialized.draft.unwrap_or(false),
            templated: deserialized.templated.unwrap_or(false),
        })
    }
}
