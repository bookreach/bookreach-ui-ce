# BookReach UI (Community Edition)

[日本語](./README.ja.md)

A Book-Discovery Interface for School Librarians to Support Inquiry Learning.

From our paper (see the "Citations" section below):

> As a way of collaborating with teachers, school librarians select books that are useful for inquiry-based or exploratory learning classes.
>
> (...)
>
> To enable even less experienced school librarians to easily curate appropriate books, we developed a graphical user interface that directly shows the candidate books that are topically relevant to the inquiry-based class' subject, by making use of decimal classification classes assigned to books.

## How to Try

Open [https://bookreach.github.io/bookreach-ui-ce/](https://bookreach.github.io/bookreach-ui-ce/) in the browser.

### How to Use

1. Select the prefecture where your school library is located
2. Choose the school type, grade, subject, and describe the lesson topic in free text
3. The app predicts relevant NDC (Nippon Decimal Classification) codes via the NDL Predictor API
4. Click "Fetch books" to search your prefecture's libraries via the Calil Unitrad API
5. Browse results by NDC tab, view book details and library holdings
6. Select useful books and export the list as CSV, TSV, or print it

## Architecture

This is a single-page Elm application with no backend server. It uses only public APIs:

| API | Purpose |
|-----|---------|
| [Calil Unitrad](https://calil.jp/doc/api_ref.html) | Live library book search by NDC code |
| [NDL Predictor](https://lab.ndl.go.jp/ndc/) | Predict NDC codes from free-text keywords |
| [openBD](https://openbd.jp/) | Book cover images |

The app has three stages:

1. **Prefecture Selection** — Choose your prefecture (saved to localStorage)
2. **NDC Selection** — Pick school/subject/grade, enter a topic, and select predicted NDC codes
3. **Explorer** — Browse books, view details, filter, select, and export

## Development

Prerequisites:

- [Node.js](https://nodejs.org/en/) (v18+)
- [Elm](https://elm-lang.org/) (0.19.1, installed via npm)

### Setup

```bash
npm install          # Install dependencies (Elm, elm-watch, Sass, etc.)
npm run build-bulma  # Compile Bulma SCSS to CSS
```

### Commands

| Command | Description |
|---------|-------------|
| `npm start` | Start dev server with hot-reload (port 3000) |
| `npm run build-bulma` | Compile `br-bulma.scss` to `public/br-bulma.css` |
| `npm test` | Run Elm tests |
| `npm run format` | Format Elm source files with elm-format |

### Project Structure

```
src/
  Main.elm          # App entry point (three-stage Explorer)
  Api.elm           # Types, decoders, HTTP functions
  NdcSelect.elm     # NDC selection component (free-text → NDL prediction)
  BookFilter.elm    # Query and library filter
  School.elm        # School type/subject/grade definitions
  Utils.elm         # LocalStore, helpers

public/
  index.html        # HTML shell
  custom.js         # Unitrad search/polling, mapping, ports
  custom.css        # Custom styles
  data/
    prefectures.json   # 47 Japanese prefectures
    ndc9-lv3.json      # NDC level-3 labels

br-bulma.scss       # Bulma CSS configuration
```

### Tech Stack

- **Language**: [Elm](https://elm-lang.org/) 0.19.1
- **CSS Framework**: [Bulma](https://bulma.io) 1.0.1 (via SCSS)
- **Dev Server**: [elm-watch](https://lydell.github.io/elm-watch/) with hot-reload
- **Icons**: [Font Awesome](https://fontawesome.com/) 6 (CDN)

## Citations

Please cite the following paper if you use this software.

```bibtex
@INPROCEEDINGS{Yada2021-eo,
  title     = "{BookReach-UI}: A {Book-Curation} Interface for School
               Librarians to Support Inquiry Learning",
  booktitle = "Towards Open and Trustworthy Digital Societies",
  author    = "Yada, Shuntaro and Asaishi, Takuma and Miyata, Rei",
  publisher = "Springer International Publishing",
  pages     = "96--104",
  year      =  2021
}
```

See also `./CITATION.cff` and `./CITATIONS.bib`.

## Licence

MIT (see `./LICENCE`)

## Author

[Shuntaro Yada](https://shuntaroy.com)
