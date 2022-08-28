# BookReach UI (Community Edition)

[日本語](./README.ja.md)

A Book-Curation Interface for School Librarians to Support Inquiry Learning.

From our paper (see the below "Citations" section):

> As a way of collaborating with teachers, school librarians select books that are useful for inquiry-based or exploratory learning classes.
>
> (...)
>
> To enable even less experienced school librarians to easily curate appropriate books, we developed a graphical user interface that directly shows the candidate books that are topically relevant to the inquiry-based class’ subject, by making use of decimal classification classes assigned to books.

![Screenshot of BookReach-UI](/assets/bookreach-ui.png)

## How to try

Open [https://bookreach.github.io/bookreach-ui-ce/](https://bookreach.github.io/bookreach-ui-ce/) in the browser.

### How to use the app

1. Select a textbook used in the inquiry-based class
2. Select the chapters (curriculum units) targeted in the inquiry-based class
3. The UI returns the candidates of books relevant to the class subject are displayed along with their book covers
4. Select as many useful books as the user chooses
5. Selected books are automatically organised into a shareable list so that you can print it or download it as CSV and TSV for further editing

For more details, see the research article below.

## Development

> This repository contains just a reference implementation of the software proposed in the paper below.
> There's much room for improvement in app functionalities, code structures, development toolkits, etc. as you can see.
> Any sorts of pull requests are welcome!
> It's pleasure to have your interest, thank you so much!!

To start developing this project,
please install:

- [Elm (0.19.1)](https://elm-lang.org/)
- [`node.js`](https://nodejs.org/en/)

> Elm is the fantastic programming language to build a web application;
> this novice-friendly functional language enables us to write codes steadily and confidently, with fun.
> If you don't know Elm, please check its tutorial! It's impressive!!
>
> The bussiness logic of bookreach-ui does not rely on npm packages, but is closed inside the pure Elm world.
> So, you hardly need any knowledge about recent web-development toolkits at all.

First, to initialise the project:

```bash
npm install  # to install additional development tools
elm install  # to activate Elm packages used in this project
```

Then, every time you changed the Elm source files, run:

```bash
elm make src/Main.elm --debug --output=main.js  # compile Elm codes to a JS file
```

You'll see the `./index.html` file should show a working app.
You can access to the helpful debug console by clicking the Elm logo at the bottom right corner, which allows you to track the model state step by step (Elm's built-in feature!).
In production, remove `--debug` flag to disable this debugging mode.

Use the [`elm-live`](https://github.com/wking-io/elm-live) dev-server to compile and reload the app automatically when you make changes (I specified this tool in `./package.json` too).

```bash
npx elm-live src/Main.elm --start-page=index.html -- --output=main.js --debug
```

### Misc. info

- [Bulma](https://bulma.io) CSS is used for building UI blocks (loaded via CDN in `./index.html`)
- For book database API, the app assumes the [`json-server`](https://github.com/typicode/json-server)'s interface for simplicity
  - [A sample API](https://lean-hail-roast.glitch.me/) on [Glitch](https://glitch.com) returns a small size of sample data, some fields of which are flled with random meaningless values due to copyright issues. I do not assure these details about books, textbooks, etc. are authentic.
  - The sample API needs some seconds to run because it relies on the Glitch's free tier
  - You can use your own database and serve it with `json-server` locally
  - For the DB specification, see `./src/BookDB.elm`
- I use VSCode with the [Elm extension](https://github.com/elm-tooling/elm-language-client-vscode) for coding.
- I tested this app on Safari and Brave Browser (but not rigorously).

## Citations

Please cite the following paper if you used this software.

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

## Original Contributer

[Shuntaro Yada](https://shuntaroy.com)
