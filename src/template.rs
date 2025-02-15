//! Template engine support

use crate::page::{Page, PageSource};
use jiff::{civil::Date, tz::TimeZone, Zoned};
use minijinja::{Error, ErrorKind, State, Value};
use serde::Serialize;
use std::{collections::HashMap, str::FromStr, sync::Arc};

/// Loads all custom filters into the Jinja environment.
pub fn load_filters(env: &mut minijinja::Environment) {
    env.add_filter("tag_link", tag_link_filter);
    env.add_filter("format_date", format_date_filter);
    env.add_filter("format_date_time", format_date_time_filter);
    env.add_filter("format_rfc3339", format_rfc3339_filter);
    env.add_filter("take", take);
}

/// Template filter for converting a tag into a link to its tag page.
fn tag_link_filter(tag: &str) -> String {
    format!("/tags/{}/", tag)
}

/// Template filter for printing a [`Date`] as `YYYY-mm-dd`.
fn format_date_filter(date: &str) -> String {
    Date::from_str(date)
        .expect("invalid date")
        .strftime("%Y-%m-%d")
        .to_string()
}

/// Template filter for printing a [`Zoned`] as `YYYY-mm-dd HH:MM`.
fn format_date_time_filter(date: &str) -> String {
    Zoned::from_str(date)
        .expect("invalid datetime")
        .strftime("%Y-%m-%d %H:%M")
        .to_string()
}

/// Template filter for printing a [`Date`] as RFC-3339 datetime, assuming midnight.
fn format_rfc3339_filter(date: &str) -> String {
    Date::from_str(date)
        .expect("invalid date")
        .to_zoned(TimeZone::UTC)
        .expect("date could not be represented as timestamp")
        .strftime("%Y-%m-%dT%H:%M:%SZ")
        .to_string()
}

/// Template filter that takes the first `n` characters.
fn take(s: &str, n: usize) -> String {
    s[..n].to_string()
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
        let key: PageSource = link.parse().map_err(|_| {
            Error::new(ErrorKind::InvalidOperation, format!("invalid link: {link}"))
        })?;
        let page = self.pages.get(&key).ok_or(Error::new(
            ErrorKind::InvalidOperation,
            format!("page '{:?}' not found", &key),
        ))?;
        Ok(Value::from_safe_string(page.link.to_owned()))
    }
}
