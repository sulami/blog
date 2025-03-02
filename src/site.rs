use crate::{
    config,
    fs::{collect_files, create_and_write, deep_copy_dir},
    page::{Page, PageKind},
    template::{load_filters, UrlFor},
};
use eyre::{OptionExt, Report, Result, WrapErr};
use itertools::Itertools;
use jiff::{tz::TimeZone, Zoned};
use minijinja::Value;
use rayon::prelude::*;
use regex::Regex;
use serde::Serialize;
use std::{
    cmp::Reverse,
    collections::HashMap,
    fs::{create_dir_all, read_to_string},
    path::{Path, PathBuf},
    sync::LazyLock,
    time::Instant,
};
use tracing::{debug, info, instrument};

/// Regex used to strip footnotes from rendered output.
static FOOTNOTE_RE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r#"(?s)<input type="checkbox".+?/>.+?<span class="footnote">.+?</span>"#)
        .expect("invalid footnote regex")
});

/// Regex used to `script` tags from rendered output.
static SCRIPT_RE: LazyLock<Regex> =
    LazyLock::new(|| Regex::new(r#"(?s)<script.+?</script>"#).expect("invalid script regex"));

/// Site metadata.
#[derive(Debug, Serialize)]
pub struct Site {
    title: String,
    author: String,
    email: String,
    pub url: String,
    pub code_theme: String,
    pub input_path: PathBuf,
    pub output_path: PathBuf,
    menu: Vec<MenuItem>,
    pub pages: HashMap<PathBuf, Page>,
    mode: Mode,
    source_sha: Option<String>,
    source_url: Option<String>,
    build_url: Option<String>,
    build_time: Zoned,
    #[serde(skip)]
    pub jinja: minijinja::Environment<'static>,
}

impl Site {
    /// Creates a new site.
    #[instrument(skip_all)]
    pub fn new(
        input: &Path,
        output: &Path,
        site_config: &config::Site,
        mode: Mode,
        source_sha: Option<impl Into<String>>,
        source_url: Option<impl Into<String>>,
        build_url: Option<impl Into<String>>,
    ) -> Result<Self> {
        let mut jinja = minijinja::Environment::new();
        load_filters(&mut jinja);

        Ok(Self {
            title: site_config.title.clone(),
            author: site_config.author.clone(),
            email: site_config.email.clone(),
            url: site_config.url.clone(),
            code_theme: site_config.code_theme.clone(),
            input_path: input.to_path_buf(),
            output_path: output.to_path_buf(),
            source_sha: source_sha.map(Into::into),
            source_url: source_url.map(Into::into),
            build_url: build_url.map(Into::into),
            build_time: Zoned::now().with_time_zone(TimeZone::UTC),
            menu: site_config
                .menu
                .iter()
                .map(std::convert::TryInto::try_into)
                .try_collect()
                .wrap_err("failed to convert menu items")?,
            mode,
            pages: HashMap::default(),
            jinja,
        })
    }

    /// Finds all pages sources in the given directory and its subdirectories, adding them to
    /// `acc`.
    #[instrument]
    fn find_page_sources(dir: &Path) -> Result<Vec<PathBuf>> {
        collect_files(dir).wrap_err("failed to collect page sources")
    }

    /// Returns the template directory for the site.
    fn template_dir(&self) -> PathBuf {
        self.input_path.join("templates")
    }

    /// Returns the content directory for the site.
    fn content_dir(&self) -> PathBuf {
        self.input_path.join("content")
    }

    /// Loads all pages in the given directory and its subdirectories.
    #[instrument(skip(self))]
    fn load_pages(&mut self) -> Result<()> {
        let sources =
            Self::find_page_sources(&self.content_dir()).wrap_err("failed to find page sources")?;
        self.pages = sources
            .into_iter()
            .par_bridge()
            .map(|source| {
                Ok((
                    source.clone(),
                    Page::new(&source)
                        .wrap_err(format!("failed to load page {}", source.display()))?,
                ))
            })
            .collect::<Result<_, Report>>()?;
        Ok(())
    }

    /// Loads all templates from the given directory and its subdirectories.
    ///
    /// Clears all pre-existing templates.
    #[instrument(skip(self))]
    pub fn load_templates(&mut self) -> Result<()> {
        self.jinja.clear_templates();

        let template_paths = collect_files(&self.template_dir())?;
        for template_path in template_paths {
            debug!(
                template = ?template_path.strip_prefix(self.template_dir())?,
                "Loading template"
            );
            self.jinja.add_template_owned(
                template_path
                    .strip_prefix(self.template_dir())?
                    .to_str()
                    .ok_or_eyre("non-UTF8 template path")?
                    .to_string(),
                read_to_string(&template_path)?,
            )?;
        }

        Ok(())
    }

    /// Renders the site.
    #[instrument(skip(self))]
    pub fn render(&mut self) -> Result<()> {
        let input = self.input_path.clone();
        let output = self.output_path.clone();

        let start = Instant::now();
        self.build_time = Zoned::now().with_time_zone(TimeZone::UTC);

        create_dir_all(&output).wrap_err("failed to create output directory")?;

        deep_copy_dir(&input.join("raw"), &output).wrap_err("failed to copy raw files")?;

        self.load_templates().wrap_err("failed to load templates")?;
        self.load_pages().wrap_err("failed to load pages")?;

        self.render_pages()
            .wrap_err("failed to render site pages")?;

        let finish = Instant::now();
        info!(
            page_count = self.pages.len(),
            "Rendered site in {:.3} seconds",
            (finish - start).as_secs_f32()
        );

        Ok(())
    }

    /// Renders all pages and writes them to the output directory.
    #[instrument(skip(self))]
    fn render_pages(&mut self) -> Result<()> {
        self.load_globals();

        self.pages
            .values()
            .par_bridge()
            .map(|page| {
                let rendered = page
                    .render(self)
                    .wrap_err_with(|| format!("failed to render page {:?}", page.source))?;
                create_and_write(&self.output_path.join(page.output_path()), &rendered)
                    .wrap_err_with(|| format!("failed to write page {:?}", page.output_path()))?;
                Ok::<(), Report>(())
            })
            .collect::<Result<()>>()?;

        Ok(())
    }

    /// Loads global values for templating, e.g. a list of all posts.
    fn load_globals(&mut self) {
        self.jinja
            .add_global("url_for", Value::from_object(UrlFor::new(&self.pages)));
        self.jinja.add_global(
            "pages",
            Value::from_serialize(
                self.pages
                    .values()
                    .sorted_unstable_by_key(|p| &p.link)
                    .collect_vec(),
            ),
        );
        self.jinja
            .add_global("posts", Value::from_serialize(self.posts()));
        self.jinja
            .add_global("tag_counts", Value::from_serialize(self.tag_counts()));
        self.jinja.add_global(
            "best_posts",
            Value::from_serialize(
                self.posts()
                    .iter()
                    .filter(|p| p.tags.contains(&"best-of".into()))
                    .take(5)
                    .collect::<Vec<_>>(),
            ),
        );
        self.jinja.add_global(
            "feed_posts",
            Value::from_serialize(
                self.posts()
                    .into_par_iter()
                    .take(10)
                    .map(|mut post| {
                        post.content = post.render_content(self).unwrap();
                        post.content = FOOTNOTE_RE.replace_all(&post.content, "").to_string();
                        post.content = SCRIPT_RE.replace_all(&post.content, "").to_string();
                        post
                    })
                    .collect::<Vec<_>>(),
            ),
        );
    }

    /// Returns all posts in the site, in reverse chronological order.
    pub fn posts(&self) -> Vec<Page> {
        self.pages
            .values()
            .filter(|p| p.kind == PageKind::Post)
            .filter(|p| match self.mode {
                Mode::Development => true,
                Mode::Release => !p.draft,
            })
            .cloned()
            .sorted_unstable_by_key(|p| Reverse(p.created_at))
            .collect()
    }

    /// Returns all tags in the site with their respective counts, deduplicated, in alphabetical
    /// order.
    fn tag_counts(&self) -> HashMap<String, usize> {
        self.posts()
            .iter()
            .flat_map(|p| p.tags.iter())
            .sorted_unstable()
            .dedup_with_count()
            .map(|(count, s)| (s.clone(), count))
            .collect()
    }
}

/// The mode the site is running in. Controls if drafts are rendered or not.
#[derive(Debug, Serialize, Copy, Clone)]
#[cfg_attr(not(feature = "server"), allow(dead_code))]
pub enum Mode {
    Release,
    Development,
}

/// An item in the navigation menu.
#[derive(Debug, Serialize)]
struct MenuItem {
    title: String,
    link: PathBuf,
}

impl TryFrom<&config::MenuItem> for MenuItem {
    type Error = Report;

    fn try_from(item: &config::MenuItem) -> Result<Self, Report> {
        Ok(Self {
            title: item.title.clone(),
            link: item
                .link
                .parse()
                .wrap_err("failed to parse menu item link")?,
        })
    }
}
