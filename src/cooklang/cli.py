#!/usr/bin/env python3

import pathlib
import sys
import cooklang.exporter_html as exporter_html
import cooklang.exporter_groceries as exporter_groceries
import argparse


def main():
    parser = argparse.ArgumentParser(
        description="Generate grocery lists for cooklang recipes",
        usage="%(prog)s [file]",
    )
    parser.add_argument(
        "recipe",
        nargs="?",
        type=argparse.FileType("r"),
        default=sys.stdin,
        help="path to recipe file. Reads from stdin by default",
    )
    parser.add_argument(
        "--output",
        metavar="path",
        type=pathlib.Path,
        help="path to output file. Writes to stdout by default",
    )
    parser.add_argument(
        "--html",
        action="store_true",
        help="output the recipe as an html page",
    )
    parser.add_argument(
        "--portions",
        metavar="no",
        type=float,
        help="adapt ingredients to a certain amount of portions",
    )
    parser.add_argument(
        "--multiplier",
        metavar="no",
        type=float,
        help="multiple ingredient amounts by a factor",
    )
    parser.add_argument(
        "--s-ingredients",
        metavar="str",
        default="Ingredients",
        help="string to use as ingredients header",
    )
    parser.add_argument(
        "--s-instructions",
        metavar="str",
        default="Instructions",
        help="string to use as instructions header",
    )
    parser.add_argument(
        "--s-servings",
        metavar="str",
        default="Serves $servings",
        help="string to use as servings description",
    )
    parser.add_argument(
        "--s-lang",
        metavar="str",
        default="en-US",
        help="language code of the generated recipe",
    )
    args = parser.parse_args()
    if args.html:
        html = exporter_html.toHtml(
            args.recipe.buffer.read(),
            portions=args.portions,
            multiplier=args.multiplier,
            i18n_ingredients=args.s_ingredients,
            i18n_instructions=args.s_instructions,
            i18n_servings=args.s_servings,
            l10n_lang=args.s_lang,
        )
        write_to_path_or_stdout(args.output, html)
    else:
        groceries = exporter_groceries.to_groceries(
            args.recipe.buffer.read(),
            existing_groceries=read_from_path_or_none(args.output),
            portions=args.portions,
            multiplier=args.multiplier,
        )
        write_to_path_or_stdout(args.output, groceries)


def read_from_path_or_none(path):
    if path is None:
        return None
    else:
        try:
            with open(path, "r") as output:
                return output.read()
        except FileNotFoundError:
            return ""


def write_to_path_or_stdout(path, content):
    if path is None:
        sys.stdout.buffer.write(content)
    else:
        with open(path, "wb") as output:
            output.write(content)


if __name__ == "__main__":
    main()
