use std::{collections::HashMap, hash::Hash, path::PathBuf, str::FromStr};

use color_eyre::{
    eyre::{eyre, WrapErr},
    Report, Result,
};
use once_cell::sync::Lazy;
use regex::Regex;
use serde::{Deserialize, Serialize};
use tera::{to_value, Value};
use time::{format_description::well_known::Rfc3339, Date, OffsetDateTime};
use tokio::{fs::File, io::AsyncReadExt};

use crate::Site;

pub mod markdown;

/// Regex used to strip footnotes from rendered output.
static FOOTNOTE_RE: Lazy<Regex> = Lazy::new(|| {
    Regex::new(r#"(?s)<input type="checkbox".+?/>.+?<span class="footnote">.+?</span>"#)
        .expect("invalid footnote regex")
});

/// A page on the site.
#[derive(Debug, Serialize, Clone)]
pub struct Page {
    pub kind: PageKind,
    pub source: PageSource,
    title: String,
    slug: String,
    pub link: String,
    url: String,
    pub tags: Vec<String>,
    pub draft: bool,
    #[serde(serialize_with = "serialize_optional_timestamp")]
    pub timestamp: Option<OffsetDateTime>,
    content: String,
    extra_context: HashMap<String, Value>,
}

impl Page {
    /// Creates a new page from the given source file.
    pub async fn new(source: PathBuf, site: &Site) -> Result<Self> {
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
            PageKind::Custom {
                ref destination, ..
            } => destination.into(),
        };
        let url = format!("{}{}", site.url, link);
        let content = markdown::render(content_section, site);

        Ok(Self {
            kind: frontmatter.kind,
            source: PageSource::File(source),
            title: frontmatter.title,
            slug: frontmatter.slug,
            link,
            url,
            tags: frontmatter.tags,
            draft: frontmatter.draft,
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
    pub fn output_path(&self) -> PathBuf {
        match &self.kind {
            PageKind::Post => PathBuf::from(format!("posts/{}/index.html", self.slug)),
            PageKind::Page => PathBuf::from(format!("{}/index.html", self.slug)),
            PageKind::Custom { destination, .. } => destination.into(),
        }
    }

    /// Renders the page in the context of the given site.
    pub async fn render(&self, site: &Site) -> Result<String> {
        // println!("Rendering page {:?}", self.source);
        let ctx = Context { page: self, site };
        let rendered = site.tera.render(
            &self.template(),
            &tera::Context::from_serialize(ctx).wrap_err("failed to create context")?,
        )?;

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
            slug: String::new(),
            link: "/".into(),
            url: site.url.clone(),
            tags: vec![],
            draft: false,
            timestamp: None,
            content: String::new(),
            extra_context: HashMap::default(),
        };
        page.insert_context("posts", &site.posts());
        page
    }

    /// Creates the Atom feed. Should be called after all posts have been loaded into `site`.
    pub fn atom_feed(site: &Site) -> Self {
        let link = "/atom.xml".into();
        let url = format!("{}{}", site.url, link);
        let mut page = Self {
            title: "Archive".into(),
            kind: PageKind::Custom {
                template: "feed.xml",
                destination: "atom.xml".into(),
            },
            source: PageSource::new_virtual("feed"),
            slug: "feed".into(),
            link,
            url,
            tags: vec![],
            draft: false,
            timestamp: Some(OffsetDateTime::now_utc()),
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
                post
            })
            .collect::<Vec<_>>();
        page.insert_context("posts", &posts);

        page
    }

    /// Creates the tags page. Should be called after all posts have been loaded into `site`.
    pub fn tags_page(site: &Site) -> Self {
        let link = "/tags/".into();
        let url = format!("{}{}", site.url, link);
        let mut page = Self {
            title: "Tags".into(),
            kind: PageKind::Custom {
                template: "tags.html",
                destination: "tags/index.html".into(),
            },
            source: PageSource::new_virtual("tags"),
            slug: "tags".into(),
            link,
            url,
            tags: vec![],
            draft: false,
            timestamp: None,
            content: String::new(),
            extra_context: HashMap::default(),
        };

        page.insert_context("tags", &site.tags());
        page
    }

    /// Creates a page for the given tag.
    pub fn tag_page(site: &Site, tag: &str) -> Self {
        let destination = format!("tags/{}/index.html", tag);
        let link = format!("/tags/{}/", tag);
        let url = format!("{}{}", site.url, link);
        let mut page = Self {
            title: format!("Tag: {tag}"),
            kind: PageKind::Custom {
                template: "tag.html",
                destination,
            },
            source: PageSource::new_virtual(&format!("tags/{}", tag)),
            slug: tag.into(),
            link,
            url,
            tags: vec![],
            draft: false,
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

impl PageSource {
    /// Creates a new virtual page source.
    pub fn new_virtual(name: &str) -> Self {
        Self::Virtual(name.into())
    }

    /// Creates a new file page source.
    pub fn new_file(name: impl Into<PathBuf>) -> Self {
        Self::File(name.into())
    }
}

/// The kind of a page.
#[derive(Default, Debug, Deserialize, Serialize, PartialEq, Eq, Clone)]
pub enum PageKind {
    /// A blog post, located at /posts/.
    #[default]
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
    timestamp: Option<OffsetDateTime>,
    tags: Vec<String>,
    draft: bool,
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
            draft: deserialized.draft.unwrap_or(false),
        })
    }
}

/// Serializes an optional timestamp, defaulting to an empty string.
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

/// Tera filter for converting a tag into a link to its tag page.
pub fn tag_link_filter(val: &Value, _args: &HashMap<String, Value>) -> tera::Result<Value> {
    let tag = val.as_str().unwrap();
    Ok(to_value(format!("/tags/{}/", tag)).unwrap())
}
