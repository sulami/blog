title = "Archive"
slug = "posts"
kind = "page"
templated = true
---

{% set oldest = (posts | last).timestamp[:4] | int %}
{% set newest = (posts | first).timestamp[:4] | int %}

{% for year in range(oldest, newest + 1) | reverse %}
### {{ year }}
{% for post in posts if year in post.timestamp %}
- [{{ post.title }}]({{ post.link }}) <span class="info">({{ post.timestamp | format_date }})</span>
{%- endfor %}
{% endfor %}
