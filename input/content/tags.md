title = "Tags"
slug = "tags"
kind = "page"
templated = true
---
{% for tag, count in tag_counts %}
- [{{ tag }}]({{ tag | tag_link }}) <span class="info">({{ count }} post{% if count > 1 %}s{% endif %})</span>
{%- endfor %}
