---
layout: page
title: Modules
permalink: /modules/
---

<!-- <ul id="archive">


{% for gallery in site.data.modules %}
  {% if modules.id == page.galleryid %}
    <h1>{{ modules.description }}</h1>
    {% for image in sortedimages %}
      <li class="archiveposturl">
        <span><a href="{{ site.url }}/graphs/{{ image.file }}">{{image.title }}</a></span><br>
<span class = "postlower">{{ image.caption }}<br />
<strong>Tags:</strong> {{ image.tags }}</span>
      </li>
    {% endfor %}
  {% endif %}
{% endfor %}

</ul> -->

This page contains links to the lectures and materials. It is organized around the topics units in the course. 
<!--
Clicking the title of the week's lecture will go to a PDF, embedded in the user's browser, by default. 
The bottom right icons link to the Github directory for the lecture (<i class="fab fa-github"></i>), the R Markdown document for the lecture (<i class="fab fa-r-project"></i>), and a PDF, embedded on Github, for the lecture (<i class="fas fa-file-pdf"></i>).
-->

<ul id="archive">
{% for modules in site.data.modules %}
      <li class="archiveposturl">
        <span><a href="{{ site.url }}{{ site.baseurl }}/{{ modules.dirname }}/{{ modules.filename }}.pdf">{{ modules.title }}</a></span><br>
<span class = "postlower">
{{ modules.desc }}</span>
<strong style="font-size:100%; font-family: 'Titillium Web', sans-serif; float:right; padding-right: .5em">
	<a href="https://github.com/{{ site.githubdir}}/tree/master/{{ modules.dirname }}"><i class="fab fa-github"></i></a>&nbsp;&nbsp;
<a href="https://github.com/{{ site.githubdir}}/tree/master/{{ modules.dirname }}/{{ modules.filename}}.Rmd"><i class="fab fa-r-project"></i></a>&nbsp;&nbsp;
<a href="https://github.com/{{ site.githubdir}}/blob/master/{{ modules.dirname }}/{{ modules.filename}}.pdf"><i class="fas fa-file-pdf"></i></a>
</strong> 
{% if modules.readings %}
  <br><strong>📖 Readings:</strong>
  <ul>
    {% for r in modules.readings %}
      <li><a href="{{ site.url }}{{ site.baseurl }}/{{ r.url }}">{{ r.title }}</a></li>
    {% endfor %}
  </ul>
{% endif %}

{% if modules.code %}
  <strong>💻 Code:</strong>
  <ul>
    {% for c in modules.code %}
      <li><a href="{{ site.url }}{{ site.baseurl }}/{{ c.url }}">{{ c.title }}</a></li>
    {% endfor %}
  </ul>
{% endif %}

<br>
      </li>
{% endfor %}
</ul>
