title = "Weak Opinions, Strongly Held"
slug = "/"
kind = "page"
templated = true
---

Hi, my name is {{ site.author }}, and this is my blog. Find out more about me on
the [about page]({{ url_for('file:input/content/about.md') }}).

### Recent Posts

{% for post in posts[:5] %}
- [{{ post.title }}]({{ post.link }}) <span class="info">({{ post.timestamp | format_date }})</span>
{%- endfor %}

### Best Posts

{% for post in best_posts %}
- [{{ post.title }}]({{ post.link }}) <span class="info">({{ post.timestamp | format_date }})</span>
{%- endfor %}

... or visit [the archive]({{ url_for('file:input/content/posts.md') }}) 
for all
posts.
