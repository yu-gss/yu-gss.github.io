# Graduate Student Seminar Website

A tiny static website for a graduate student seminar. The public site is one
plain HTML page plus a small CSS file, generated from text files by a
dependency-free OCaml program.

## What to Edit

- `data/site.txt` has the seminar name, term, meeting time, organizers, contact
  links, and basic department information.
- `data/talks.tsv` has the schedule. Keep the header row and separate columns
  with tabs.
- `assets/styles.css` controls the deliberately sparse visual design.

`email:` in `data/site.txt` can contain one address or a comma-separated list.

Common `data/site.txt` fields:

```text
name: Site title
term: Current term
meeting_time: Weekly meeting time
location: Room, Zoom, or TBA
description: One-sentence description
format: Talk format shown in the about section
audience: Audience note shown in the about section
organizers: Organizer names
email: organizer1@example.edu,organizer2@example.edu
calendar_url: seminar.ics
github_url: https://github.com/yu-gss/yu-gss.github.io
```

## Build Locally

You need OCaml and `make`.

```sh
make build
```

The generated site appears in `dist/`.

To preview it locally:

```sh
make serve
```

Then open `http://localhost:8000`.

## Deploy on GitHub Pages

The live site is:

```text
https://yu-gss.github.io/
```

The repository is:

```text
https://github.com/yu-gss/yu-gss.github.io
```

Push changes to `main`. The included workflow builds the OCaml generator,
publishes `dist/` to the `gh-pages` branch, and GitHub Pages serves that branch.

The site stays free to host on GitHub Pages as long as it fits GitHub's Pages
terms and limits.

## Schedule Format

`data/talks.tsv` columns:

```text
date	speaker	affiliation	title	abstract	location	url	status	materials
```

Use ISO dates like `2026-09-04`. The `materials` column can contain links in
this format:

```text
Slides|https://example.edu/slides.pdf; Paper|https://example.edu/paper.pdf
```

Blank fields are fine. Lines beginning with `#` are ignored.
