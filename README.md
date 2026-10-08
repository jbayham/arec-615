AREC 615: Optimization Methods for Applied Economics
Course Website
=======================================================

This repository contains the course website built on Steve Miller's [No-Good-Very-Bad Course Website](https://github.com/svmiller/course-website) Jekyll template.

## Slide theme

All Quarto Reveal.js slide decks under `modules/` inherit the course theme
from `styles/slides.scss`, configured once in `modules/_quarto.yml`.
Edit that stylesheet to change the shared colors, fonts, and slide styling.

New decks only need `format: revealjs` in their YAML header. Add deck-specific
layout or presentation options inside the `revealjs` format mapping when needed, and
leave `theme` unset to use the shared theme. Keep any slide-specific CSS scoped
to its own classes. The existing PDF default for course notes is unchanged.

For a deck outside the `modules/` Quarto project, use
`theme: [simple, <relative-path-to>/styles/slides.scss]` inside that mapping.
