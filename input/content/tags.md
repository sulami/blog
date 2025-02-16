title = "Tags"
slug = "tags"
kind = "page"
templated = true
---

{% for tag, count in tag_counts | dictsort %}
### {{ tag }} <span class="info">({{ count }} post{% if count > 1 %}s{% endif %})</span>
{% for post in posts if tag in post.tags %}
- [{{ post.title }}]({{ post.link }}) <span class="info">({{ post.timestamp | format_date }})</span>
{%- endfor %}
{%- endfor %}
