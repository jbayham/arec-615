---
layout: page
title: "Course Schedule"
permalink: /schedule/
---

<table id="course-schedule">
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
    {% assign row_class = "" %}
    {% if topic_lower contains "wagar 107" %}
      {% assign row_class = "wagar-row" %}
    {% elsif topic_lower contains "exam" %}
      {% assign row_class = "exam-row" %}
    {% elsif topic_lower contains "no class" %}
      {% assign row_class = "noclass-row" %}
    {% elsif topic_lower == "presentation/discussion" %}
      {% assign row_class = "presentation-row" %}
    {% endif %}
    <tr class="{{ row_class }}" data-date="{{ item.date | date: '%Y-%m-%d' }}">
      <td>{{ item.date | date: "%b %d" }}</td>
      <td>{{ item.topic }}</td>
      <td>{{ item.reading }}</td>
    </tr>
    {% endfor %}
  </tbody>
</table>

<script>
  (function () {
    // Compare calendar dates in the course's time zone, regardless of the viewer's location.
    var parts = new Intl.DateTimeFormat('en-US', {
      timeZone: 'America/Denver',
      year: 'numeric',
      month: '2-digit',
      day: '2-digit'
    }).formatToParts(new Date());
    var date = {};
    parts.forEach(function (part) { date[part.type] = part.value; });
    var today = date.year + '-' + date.month + '-' + date.day;

    var nextClassFound = false;
    document.querySelectorAll('#course-schedule tbody tr[data-date]').forEach(function (row) {
      row.classList.toggle('past-day', row.dataset.date < today);
      // Keep today's class bold for the day, and skip entries marked "No class".
      var isNextClass = !nextClassFound && row.dataset.date >= today && !row.classList.contains('noclass-row');
      row.classList.toggle('next-class', isNextClass);
      if (isNextClass) nextClassFound = true;
    });
  })();
</script>
