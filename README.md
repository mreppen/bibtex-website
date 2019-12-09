# bibtex-website

Uses [bibtex2html](https://www.lri.fr/~filliatr/bibtex2html/) for parsing the bib files.
This code was written for better control over the output.

Replaces any tag ```<bibtex item="bibkey">``` with a reference from ```@article{bibkey, ...}``` in supplied bib files.

## Installation
Install `ocaml` and `opam`.

If opam is not already initialized: `opam init`

Then run
```
opam pin bibtex-website https://github.com/mreppen/bibtex-website.git
opam install bibtex-website
```

## Usage
```bibtex-website --bibdir=bib *.bib template.html > index.html```
generates .bib files in "bib/" and index.html with formatted references and links to the .bib files.

## Example
See [max.reppen.ch](https://max.reppen.ch) and [mreppen/mreppen.github.io](https://github.com/mreppen/mreppen.github.io).
