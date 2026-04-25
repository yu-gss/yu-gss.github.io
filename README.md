# Graduate Student Seminar Website

A tiny static website for a graduate student seminar. The public site is one
plain HTML page plus a small CSS file, generated from text files by a
dependency-free OCaml program.

## What to Edit

- `data/site.txt` has the seminar name, term, meeting time, contact links, and
  basic department information.
- `data/talks.tsv` has the schedule. Keep the header row and separate columns
  with tabs.
- `assets/styles.css` controls the deliberately sparse visual design.

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

1. Create a new GitHub repository.
2. Push this project to the repository's `main` branch.
3. In GitHub, open repository settings and enable Pages with GitHub Actions as
   the source.
4. The included workflow builds the OCaml generator and publishes `dist/`.

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
