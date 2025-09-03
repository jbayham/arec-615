---
layout: page
title: "Course Schedule"
permalink: /schedule/
---

<table>
  <thead>
    <tr>
      <th style="width: 10%;">Date</th>
      <th style="width: 55%;">Topic</th>
      <th style="width: 20%;">Reading</th>
      <th style="width: 15%;">Due</th>
    </tr>
  </thead>
  <tbody>
    {% for item in site.data.schedule %}
    <tr>
      <td>{{ item.date | date: "%b %d" }}</td>
      <td>{{ item.topic }}</td>
      <td>{{ item.reading }}</td>
      <td>{{ item.due }}</td>
    </tr>
    {% endfor %}
  </tbody>
</table>
