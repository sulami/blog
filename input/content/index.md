title = "Weak Opinions, Strongly Held"
slug = "/"
kind = "page"
templated = true
---

Hi, my name is {{ site.author }}, and this is my blog. Find out more about me on
the [about page]({{ url_for('input/content/about.md') }}).

### Recent Posts

{% for post in posts[:5] %}
- [{{ post.title }}]({{ post.link }}) <span class="info">({{ post.created_at | format_date }})</span>
{%- endfor %}

### Best Posts

{% for post in best_posts %}
- [{{ post.title }}]({{ post.link }}) <span class="info">({{ post.created_at | format_date }})</span>
{%- endfor %}

... or visit [the archive]({{ url_for('input/content/archive.md') }}) 
for all posts, or [the tags page]({{ url_for('input/content/tags.md') }}
) for an overview of topics.
