---
layout: page
title: "Course Schedule"
permalink: /schedule/
---

<table>
  <thead>
    <tr>
      <th style="width: 10%;">Date</th>
      <th style="width: 65%;">Topic</th>
      <th style="width: 25%;">Reading</th>
    </tr>
  </thead>
  <tbody>
    {% for item in site.data.schedule %}
    {% assign topic_lower = item.topic | downcase %}
    {% if topic_lower contains "exam" %}
      <tr class="exam-row">
    {% elsif topic_lower contains "no class" %}
      <tr class="noclass-row">
    {% elsif topic_lower == "presentation/discussion" %}
      <tr class="presentation-row">
    {% else %}
      <tr>
    {% endif %}
      <td>{{ item.date | date: "%b %d" }}</td>
      <td>{{ item.topic }}</td>
      <td>{{ item.reading }}</td>
    </tr>
    {% endfor %}
  </tbody>
</table>
