use std::{
    cmp::Reverse,
    collections::HashMap,
    ffi::OsStr,
    fs::{create_dir_all, read_dir},
    path::{Path, PathBuf},
    time::Instant,
};

use color_eyre::{eyre::WrapErr, Report, Result};
use itertools::Itertools;
use rayon::prelude::*;
use serde::Serialize;
use tera::{Function, Tera, to_value, Value};
use time::OffsetDateTime;

use crate::{
    config,
    fs::{create_and_write, deep_copy_dir},
    page::{Page, PageKind, PageSource, tag_link_filter},
};

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
    pub pages: HashMap<PageSource, Page>,
    mode: Mode,
    #[serde(serialize_with = "time::serde::rfc3339::serialize")]
    build_time: OffsetDateTime,
    #[serde(skip)]
    pub tera: Tera,
}

impl Site {
    /// Creates a new site.
    pub fn new(
        input: &Path,
        output: &Path,
        site_config: &config::Site,
        mode: Mode,
    ) -> Result<Self> {
        let mut tera = Tera::new(&format!("{}/templates/*", input.display()))
            .expect("failed to load templates");
        tera.autoescape_on(vec![]);
        tera.register_filter("tag_link", tag_link_filter);

        Ok(Self {
            title: site_config.title.clone(),
            author: site_config.author.clone(),
            email: site_config.email.clone(),
            url: site_config.url.clone(),
            code_theme: site_config.code_theme.clone(),
            input_path: input.to_path_buf(),
            output_path: output.to_path_buf(),
            build_time: OffsetDateTime::now_utc(),
            menu: site_config
                .menu
                .iter()
                .map(std::convert::TryInto::try_into)
                .try_collect()
                .wrap_err("failed to convert menu items")?,
            mode,
            pages: HashMap::default(),
            tera,
        })
    }
    /// Inserts a page into the site.
    fn insert_page(&mut self, page: Page) {
        self.pages.insert(page.source.clone(), page);
    }

    /// Finds all pages sources in the given directory and its subdirectories, adding them to
    /// `acc`.
    fn find_page_sources(dir: &Path, acc: &mut Vec<PageSource>) -> Result<()> {
        for path in read_dir(dir)? {
            let path = path?;
            if path.file_type()?.is_dir() {
                Self::find_page_sources(&path.path(), acc)?;
            }
            let Some(Some("md")) = path.path().extension().map(OsStr::to_str) else {
                continue;
            };
            acc.push(PageSource::File(path.path().clone()));
        }

        Ok(())
    }

    /// Loads all pages in the given directory and its subdirectories.
    fn load_pages(&mut self, dir: &Path) -> Result<()> {
        let mut sources = vec![];
        Self::find_page_sources(&dir.join("content"), &mut sources)
            .wrap_err("failed to find page sources")?;
        self.pages = sources
            .into_iter()
            .par_bridge()
            .map(|source| {
                let path = match source {
                    PageSource::File(ref path) => path,
                    _ => unreachable!(),
                };
                (
                    source.clone(),
                    Page::new(path.to_path_buf(), self)
                        .wrap_err(format!("failed to load page {}", path.display()))
                        .unwrap(),
                )
            })
            .collect();
        Ok(())
    }

    /// Renders the site.
    pub fn render(&mut self) -> Result<()> {
        let input = self.input_path.clone();
        let output = self.output_path.clone();

        let start = Instant::now();
        self.build_time = OffsetDateTime::now_utc();

        create_dir_all(&output).wrap_err("failed to create output directory")?;

        deep_copy_dir(&input.join("raw"), &output).wrap_err("failed to copy raw files")?;

        self.load_pages(&input).wrap_err("failed to load pages")?;

        // NB Ordering here is important. Sitemap after all regular content pages, Atom feed after
        // that so it's not included in the sitemap.
        self.insert_page(Page::index_page(self));
        self.insert_page(Page::posts_page(self));
        self.insert_page(Page::tags_page(self));
        self.tags()
            .iter()
            .for_each(|tag| self.insert_page(Page::tag_page(self, tag)));
        self.insert_page(Page::sitemap(self));
        self.insert_page(Page::atom_feed(self));

        self.render_pages(&output)
            .wrap_err("failed to render site pages")?;

        let finish = Instant::now();
        tracing::info!(
            "Rendered site in {:.3} seconds",
            (finish - start).as_secs_f32()
        );

        Ok(())
    }

    /// Renders all pages and writes them to the output directory.
    fn render_pages(&mut self, output: &Path) -> Result<()> {
        // NB Reload the url_for function with new pages.
        self.tera
            .register_function("url_for", make_url_for(self.pages.clone()));

        self.pages
            .values()
            .par_bridge()
            .map(|page| {
                let rendered = page
                    .render(self)
                    .wrap_err_with(|| format!("failed to render page {:?}", page.source))?;
                create_and_write(&output.join(page.output_path()), &rendered)
                    .wrap_err_with(|| format!("failed to write page {:?}", page.output_path()))?;
                Ok::<(), Report>(())
            })
            .collect::<Result<()>>()?;

        Ok(())
    }

    /// Returns all posts in the site, in reverse chronological order.
    pub fn posts(&self) -> Vec<Page> {
        let mut posts: Vec<Page> = self
            .pages
            .values()
            .filter(|p| p.kind == PageKind::Post)
            .filter(|p| match self.mode {
                Mode::Development => true,
                Mode::Release => !p.draft,
            })
            .cloned()
            .collect();
        posts.sort_unstable_by_key(|p| Reverse(p.timestamp));
        posts
    }

    /// Returns all tags in the site, deduplicated, in alphabetical order.
    pub fn tags(&self) -> Vec<String> {
        self.posts()
            .iter()
            .flat_map(|p| p.tags.iter())
            .unique()
            .sorted()
            .cloned()
            .collect()
    }
}

/// The mode the site is running in. Controls if drafts are rendered or not.
#[derive(Debug, Serialize, Copy, Clone)]
pub enum Mode {
    Release,
    Development,
}

/// An item in the navigation menu.
#[derive(Debug, Serialize)]
struct MenuItem {
    title: String,
    link: PageSource,
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

/// Creates the url_for template filter.
fn make_url_for(pages: HashMap<PageSource, Page>) -> impl Function {
    Box::new(
        move |args: &HashMap<String, Value>| -> Result<Value, tera::Error> {
            let link: String = args
                .get("link")
                .expect("argument link not found")
                .as_str()
                .expect("argument link is not a string")
                .into();
            let (kind, name) = link
                .split_once(':')
                .ok_or(tera::Error::from("invalid link format"))?;
            let key = match kind {
                "file" => Ok(PageSource::File(name.into())),
                "virtual" => Ok(PageSource::Virtual(name.into())),
                _ => Err(tera::Error::from("invalid kind")),
            }?;
            let page = pages
                .get(&key)
                .ok_or_else(|| tera::Error::from(format!("page '{:?}' not found", &key)))?;
            Ok(to_value(page.link.clone()).unwrap())
        },
    )
}
