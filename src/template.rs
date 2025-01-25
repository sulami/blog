//! Template engine support

use crate::page::{Page, PageSource};
use minijinja::{Error, ErrorKind, State, Value};
use serde::Serialize;
use std::{collections::HashMap, sync::Arc};

/// Template filter for converting a tag into a link to its tag page.
pub fn tag_link_filter(tag: &str) -> String {
    format!("/tags/{}/", tag)
}

/// The `url_for` template function supporting links to all supplied pages.
///
/// This is a custom, callable object because it needs to hold state to do its job.
#[derive(Debug, Serialize)]
pub struct UrlFor {
    pages: HashMap<PageSource, Page>,
}

impl UrlFor {
    /// Create a new instance of [`UrlFor`] supporting links to all pages passed.
    pub fn new(pages: &HashMap<PageSource, Page>) -> Self {
        Self {
            pages: pages.clone(),
        }
    }
}

impl minijinja::value::Object for UrlFor {
    fn call(self: &Arc<Self>, _state: &State<'_, '_>, args: &[Value]) -> Result<Value, Error> {
        let link: String = args
            .first()
            .ok_or(Error::new(ErrorKind::MissingArgument, "missing argument"))?
            .as_str()
            .ok_or(Error::new(
                ErrorKind::InvalidOperation,
                "argument is not a string",
            ))?
            .into();
        let (kind, name) = link.split_once(':').ok_or(Error::new(
            ErrorKind::InvalidOperation,
            "invalid link format",
        ))?;
        let key = match kind {
            "file" => Ok(PageSource::File(name.into())),
            "virtual" => Ok(PageSource::Virtual(name.into())),
            _ => Err(Error::new(ErrorKind::InvalidOperation, "invalid kind")),
        }?;
        let page = self.pages.get(&key).ok_or_else(|| {
            Error::new(
                ErrorKind::InvalidOperation,
                format!("page '{:?}' not found", &key),
            )
        })?;
        Ok(Value::from_safe_string(page.link.clone()))
    }
}
