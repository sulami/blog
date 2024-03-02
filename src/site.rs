use std::{
    cmp::Reverse,
    collections::HashMap,
    ffi::OsStr,
    path::{Path, PathBuf},
};

use async_recursion::async_recursion;
use color_eyre::{eyre::WrapErr, Result};
use itertools::Itertools;
use serde::Serialize;
use tera::{to_value, Function, Tera, Value};
use tokio::fs::{create_dir_all, read_dir};

use crate::{
    config,
    page::{tag_link_filter, Page, PageKind, PageSource},
};

/// Site metadata.
#[derive(Debug, Serialize)]
pub struct Site {
    title: String,
    description: String,
    author: String,
    email: String,
    pub url: String,
    pub code_theme: String,
    input_path: PathBuf,
    output_path: PathBuf,
    menu: Vec<MenuItem>,
    pages: HashMap<PageSource, Page>,
    mode: Mode,
    #[serde(skip)]
    pub tera: Tera,
}

impl Site {
    /// Creates a new site.
    pub fn new(input: &Path, output: &Path, site_config: &config::Site, mode: Mode) -> Self {
        let mut tera = Tera::new(&format!("{}/templates/**/*", input.display()))
            .expect("failed to load templates");
        tera.autoescape_on(vec![]);
        tera.register_filter("tag_link", tag_link_filter);

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
            mode,
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

    /// Renders the site.
    pub async fn render(&mut self) -> Result<()> {
        let input = self.input_path.clone();
        let output = self.output_path.clone();

        let start = time::Instant::now();

        create_dir_all(&output)
            .await
            .wrap_err("failed to create output directory")?;

        crate::fs::deep_copy_dir(&input.join("raw"), &output)
            .await
            .wrap_err("failed to copy raw files")?;

        self.load_pages(&input.join("content"))
            .await
            .wrap_err("failed to load pages")?;

        let index_page = Page::index_page(self);
        self.insert_page(index_page);

        let feed_page = Page::atom_feed(self);
        self.insert_page(feed_page);

        self.tags()
            .iter()
            .for_each(|tag| self.insert_page(Page::tag_page(self, tag)));

        let tags_page = Page::tags_page(self);
        self.insert_page(tags_page);

        self.render_pages(&output)
            .await
            .wrap_err("failed to render site pages")?;

        let finish = time::Instant::now();
        println!(
            "Rendered site in {:.3} seconds",
            (finish - start).as_seconds_f32()
        );

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
            crate::fs::create_and_write(&output.join(page.output_path()), &rendered)
                .await
                .wrap_err_with(|| format!("failed to write page {:?}", page.output_path()))?;
        }

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

impl MenuItem {
    /// Creates a new menu item.
    fn new(title: &str, link: PageSource) -> Self {
        Self {
            title: title.into(),
            link,
        }
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
