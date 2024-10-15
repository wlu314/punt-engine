---
title: Local Docsite Development
date: 10-15-2024
---

This is a walkthrough on how to clone this site, write a note, run the site on local, and update remote.

## Dependencies
- node v20
- npm v9.3.1
- git
- a GitHub account

Newer versions will likely work.

## Cloning and running the site

punt-engine is a monorepo, the docsite sits in a folder with all other modules in the engine.

Clone the entire thing and enter the docs folder:
```bash
git clone https://github.com/raquentin/punt-engine.git \
cd punt-engine/docs
```

In this folder, you'll need to use node to install dependencies and run the build script:
```bash
npm i \
npx quartz build --serve
```

You should have output in console directing you to (http://localhost:8080)[http://localhost:8080]. Open it in a browser and you'll see the `/docs` site running locally.

## Writing a note

Before editing the repo, make a new branch:

```bash
git checkout -b <branch-name>
```

If you're updating the docs to reflect changes in a new pr, just do this in the branch you're already in.

Navigate to `punt-engine/docs/content/notes`. You'll see many notes, including this one. Touch a new one with a descriptive file name, perhaps the same as the title. This file name is that same path that will be in the URL.

With this file touched, you can go back to the browser and open it up, you'll see a blank page to build on.

### Header

Each note needs a header defining its formatted title and date. The header for this note is:

```yaml
---
title: Local Docsite Development
date: 10-15-2024
---
```

Write yours similarly.

## Body Features

I'll quickly show some features you can do in these markdown files.

### LaTeX

You can write latex:

```latex
$a - b < b[a/b] <= a$
```

becomes

$a - b < b[a/b] <= a$

### Code

You can write code:

```bash
    ```haskell
    topEntity :: Clock System -> Reset System -> Enable System -> Signal System (Int, Int) -> Signal System Int
    topEntity = exposeClockResetEnable accumulatorMealy
    ```
```

becomes

```haskell
topEntity :: Clock System -> Reset System -> Enable System -> Signal System (Int, Int) -> Signal System Int
topEntity = exposeClockResetEnable accumulatorMealy
```

### Markdown

These files are written in markdown, which has many formatting features. See [this markdown cheat sheet](https://www.markdownguide.org/cheat-sheet/).

You can also inspect the source of other notes and see how they're constructed in the browser.

## Updating remote

With your note finished on local, `git push` it, make a pr, and it will be reviewed. This site will update when it's merged.
