[![Build Status](https://travis-ci.org/jwoudenberg/groceries.svg?branch=trunk)](https://travis-ci.org/jwoudenberg/groceries)

# Grocery Management System

A very serious system for generating grocery shopping lists. It parses a set of markdown recipe files into a Prolog knowledge base, which can then be queried to generate a shopping list.

More a demo than a library. I use this myself and I'm Dutch, and some of the parsing codes makes special allowances for the Dutch language.

## Setup

Either use [Nix][nix] or download [SWI-Prolog][swi-prolog] by hand.

## Generating the ingredient knowledge base

```
scripts/parse_recipes.pl /path/to/your/recipes/*.md
```

For parsing to succeed each recipe markdown file must contain a single root-level unordered list, each item corresponding to an ingredient.

The script will write the recipe knowledge base to `recipes.pl`. It's a human-readable file, so feel free to take a look!

## Create a meal planning module

In this file you'll do your meal planning. There's an example file bundled with this repo to get you started. To use it:

```
cp example/planning.pl planning.pl
```

## Getting your grocery list

This is still a work in progress. There's some functions already defined in `groceries.pl` if you want to take a look!

## Update prolog dependencies

These are comitted to guard against pack website downtime.

Then run the `scripts/install.pl` to fetch Prolog dependencies.

## Development

Run tests by calling `scripts/run_tests.pl`.

[nix]: https://nixos.org/
[swi-prolog]: https://www.swi-prolog.org/Download.html
