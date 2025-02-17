use crate::Site;
use eyre::{eyre, Report, Result, WrapErr};
use jiff::civil::Date;
use serde::{Deserialize, Serialize};
use std::{ffi::OsString, fmt::Debug, fs::File, io::Read, path::PathBuf, str::FromStr};
use tracing::{debug, instrument};

mod markdown;

/// A page on the site.
#[derive(Clone, Debug, PartialEq, Eq, Serialize)]
pub struct Page {
    pub kind: PageKind,
    pub source: PathBuf,
    title: String,
    slug: String,
    pub link: String,
    pub tags: Vec<String>,
    pub draft: bool,
    templated: bool,
    markdown: bool,
    pub timestamp: Option<Date>,
    pub content: String,
}

impl Page {
    /// Creates a new page from the given source file.
    ///
    /// This reads the source file into memory.
    #[instrument]
    pub fn new(source: impl Into<PathBuf> + Debug) -> Result<Self> {
        let source = source.into();
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
            PageKind::Page if frontmatter.slug == "/" => String::from("/"),
            PageKind::Page => format!("/{}/", frontmatter.slug),
            PageKind::Other => format!("/{}", frontmatter.slug),
        };
        let markdown = source.extension() == Some(&OsString::from("md"));

        Ok(Self {
            kind: frontmatter.kind,
            source,
            title: frontmatter.title,
            slug: frontmatter.slug,
            link,
            tags: frontmatter.tags,
            draft: frontmatter.draft,
            templated: frontmatter.templated,
            markdown,
            timestamp: frontmatter.timestamp,
            content: content_section.trim().to_string(),
        })
    }

    /// Returns the template to use for rendering the page.
    fn template(&self) -> Option<&str> {
        match &self.kind {
            PageKind::Post => Some("post.html"),
            PageKind::Page => Some("page.html"),
            PageKind::Other => None,
        }
    }

    /// Returns the path where the page should be written to.
    pub fn output_path(&self) -> PathBuf {
        match &self.kind {
            PageKind::Post => PathBuf::from(format!("posts/{}/index.html", self.slug)),
            PageKind::Page if self.slug == "/" => PathBuf::from("index.html"),
            PageKind::Page => PathBuf::from(format!("{}/index.html", self.slug)),
            PageKind::Other => PathBuf::from(&self.slug),
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

        let rendered_content = self
            .render_content(site)
            .wrap_err("failed to render page content")?;
        let ctx = Context {
            page: self,
            site,
            rendered_content: Some(&rendered_content),
        };

        if let Some(tmpl) = self.template() {
            let template = site
                .jinja
                .get_template(tmpl)
                .wrap_err("template not found")?;
            let rendered = template.render(&ctx).wrap_err("failed to render page")?;
            Ok(rendered)
        } else {
            Ok(rendered_content)
        }
    }

    /// Renders only the content of the page, without insertion into a template.
    ///
    /// This is mainly useful for feed generation.
    pub fn render_content(&self, site: &Site) -> Result<String> {
        let ctx = Context {
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

        let rendered_content = if self.markdown {
            markdown::render(templated_content, site)
        } else {
            templated_content.to_string()
        };

        Ok(rendered_content)
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

/// The kind of page.
#[derive(Debug, Deserialize, Serialize, PartialEq, Eq, Clone)]
pub enum PageKind {
    /// A blog post, located at /posts/.
    Post,
    /// A regular page, located at /.
    Page,
    /// Not an HTML page to be rendered with a template.
    Other,
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
            #[serde(rename = "other")]
            Other,
        }

        impl From<DeserializedPageKind> for PageKind {
            fn from(kind: DeserializedPageKind) -> Self {
                match kind {
                    DeserializedPageKind::Post => Self::Post,
                    DeserializedPageKind::Page => Self::Page,
                    DeserializedPageKind::Other => Self::Other,
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
