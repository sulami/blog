use crate::page::slugify;
use pulldown_cmark::{CodeBlockKind, Event, Tag, TagEnd};
use std::{collections::HashMap, sync::LazyLock};
use syntect::{
    html::{ClassStyle, ClassedHTMLGenerator},
    parsing::SyntaxSet,
    util::LinesWithEndings,
};

// These are somewhat expensive to load, so we use a lazy static to only load them once.
static SS: LazyLock<SyntaxSet> = LazyLock::new(SyntaxSet::load_defaults_newlines);

/// Renders the given markdown source to a string.
pub fn render(source: &str) -> String {
    let mut rendered = String::new();
    let mut code_language: Option<String> = None;
    let ss = &SS;

    let mut current_heading = None;

    let events = pulldown_cmark::Parser::new_ext(
        source,
        pulldown_cmark::Options::ENABLE_FOOTNOTES
            | pulldown_cmark::Options::ENABLE_TABLES
            | pulldown_cmark::Options::ENABLE_SMART_PUNCTUATION
            | pulldown_cmark::Options::ENABLE_HEADING_ATTRIBUTES,
    )
    .filter_map(|mut ev| match ev {
        // Syntax highlighting for code blocks.
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
                let mut generator = ClassedHTMLGenerator::new_with_class_style(
                    syntax,
                    ss,
                    ClassStyle::SpacedPrefixed { prefix: "c-" },
                );
                for line in LinesWithEndings::from(text) {
                    generator
                        .parse_html_for_line_which_includes_newline(line)
                        .expect("oh no");
                }
                let code = format!("<pre>{}</pre>", generator.finalize());
                Some(Event::Html(code.into()))
            } else {
                Some(ev)
            }
        }
        _ => Some(ev),
    })
    .filter_map(|mut ev| match ev {
        // Create linked anchors for headings that aren't manually linked.
        Event::Start(Tag::Heading {
            level, id: None, ..
        }) => {
            current_heading = Some(level);
            None
        }
        Event::End(TagEnd::Heading(level)) if current_heading == Some(level) => {
            current_heading = None;
            None
        }
        Event::Text(ref mut text) if current_heading.is_some() => {
            let slug = slugify(text);
            Some(Event::Html(
                format!(
                    r##"<{level} id="{slug}"><a href="#{slug}">{text}</a></{level}>"##,
                    level = current_heading.unwrap()
                )
                .into(),
            ))
        }
        _ => Some(ev),
    })
    .collect::<Vec<_>>();

    // NB Inlining footnotes is a two-pass process because the definitions can be in any order, and
    // potentially before the references(?). So we do a first pass where we collect all the
    // definitions under their respective labels, and then do a second pass where we replace the
    // references with the definitions and remove the original definitions.
    //
    // Another nuance here is that we strip paragrahps inside footnotes, because they are not HTML
    // phrasing content, and thus not allowed inside <p>.

    // Collect footnote definitions.
    let mut footnotes: HashMap<pulldown_cmark::CowStr<'_>, Vec<Event>> = HashMap::default();
    let mut current_footnote = None;

    for ev in events.iter() {
        match ev {
            Event::Start(Tag::FootnoteDefinition(label)) => {
                footnotes.insert(label.clone(), vec![]);
                current_footnote = Some(label);
            }
            Event::End(TagEnd::FootnoteDefinition) => {
                current_footnote = None;
            }
            Event::Start(Tag::Paragraph) if current_footnote.is_some() => {
                // Don't collect paragraphs inside footnotes.
            }
            Event::End(TagEnd::Paragraph) if current_footnote.is_some() => {
                // Don't collect paragraphs inside footnotes.
            }
            _ => {
                if let Some(label) = current_footnote {
                    footnotes.get_mut(label).unwrap().push(ev.clone());
                }
            }
        }
    }

    // Strip out footnote definitions and patch footnote references with them.
    let mut in_footnote = false;
    let events = events.into_iter().filter_map(|ev| match ev {
        Event::Start(Tag::FootnoteDefinition(_)) => {
            in_footnote = true;
            None
        }
        Event::End(TagEnd::FootnoteDefinition) => {
            in_footnote = false;
            None
        }
        Event::FootnoteReference(label) => {
            // Render just the definition of the footnote and re-insert it into the stream as an
            // HTML event.
            let mut definition = footnotes
                .get_mut(&label)
                .expect("footnote without definition found")
                .clone();
            definition.insert(
                0,
                Event::Html(
                    format!(
                        r#"<input type="checkbox" id="cb-{label}" /><label for="cb-{label}"><sup>{label}</sup></label><span class="footnote">"#
                    )
                    .into(),
                ),
            );

            definition.push(Event::Html("</span>".into()));
            let mut rendered_footnote = String::new();
            pulldown_cmark::html::push_html(&mut rendered_footnote, definition.into_iter());
            Some(Event::Html(rendered_footnote.into()))
        }
        _ if in_footnote => None,
        ev => Some(ev),
    });

    pulldown_cmark::html::push_html(&mut rendered, events);

    rendered
}
