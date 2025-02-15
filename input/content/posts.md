title = "Archive"
slug = "posts"
kind = "page"
templated = true
---
{% for post in posts %}
- [{{ post.title }}]({{ post.link }}) <span class="info">({{ post.timestamp | format_date }})</span>
{%- endfor %}
