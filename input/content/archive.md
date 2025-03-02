title = "Archive"
slug = "posts"
kind = "page"
templated = true
---

{% set oldest = (posts | last).created_at[:4] | int %}
{% set newest = (posts | first).created_at[:4] | int %}

{% for year in range(oldest, newest + 1) | reverse %}
### {{ year }}
{% for post in posts if year in post.created_at %}
- [{{ post.title }}]({{ post.link }}) <span class="info">({{ post.created_at | format_date }})</span>
{%- endfor %}
{% endfor %}
