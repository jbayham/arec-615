---
layout: page
title: "Course Schedule"
permalink: /schedule/
---

<table>
  <thead>
    <tr>
      <th>Date</th>
      <th>Topic</th>
      <th>Reading</th>
      <th>Due</th>
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
