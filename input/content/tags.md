title = "Tags"
slug = "tags"
kind = "page"
templated = true
---

{% macro linked_heading(text, extra='') -%}
[{{ text }}](#{{ text }}) {{ extra }} { #{{ text }} }
{%- endmacro %}

{% macro info(count) -%}
<span class="info">({{ count }} post{% if count > 1 %}s{% endif %})</span>
{%- endmacro %}

{% for tag, count in tag_counts | dictsort %}
### {{ linked_heading(tag, extra=info(count)) }}
{% for post in posts if tag in post.tags %}
- [{{ post.title }}]({{ post.link }}) <span class="info">({{ post.created_at | format_date }})</span>
{%- endfor %}
{%- endfor %}
