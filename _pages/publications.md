---
layout: home
author_profile: true
title: Publications
---
The papers on this page are included to ensure timely dissemination on a noncommercial basis. Copyright and all rights therein are maintained by the authors or by other copyright holders, notwithstanding that they have offered their works here electronically. It is understood that all persons copying this information will adhere to the terms and constraints invoked by the copyrights. These works may not be reposted without the explicit permission of the copyright holder.

{% for publ in site.data.publications %}

  <strong>{{ publ.title }} </strong> {% if publ.link.url != %} (<a href="{{ publ.link.url }}">{{ publ.link.display }}</a>) {% endif %} <br /> 
  {{ publ.authors }}. {{ publ.proceedings }}, {{ publ.pages }}, {{ publ.publisher }}, {{ publ.year }}.


{% endfor %}