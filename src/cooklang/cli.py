#!/usr/bin/env python3

import pathlib
import sys
import cooklang.exporter_html as exporter_html
import cooklang.exporter_groceries as exporter_groceries
import argparse


def main():
    parser = argparse.ArgumentParser(
        description="Generate html pages for cooklang recipes",
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
        "--groceries",
        action="store_true",
        help="output a grocery list in todo.txt format",
    )
    parser.add_argument(
        "--portions",
        metavar="no",
        type=int,
        help="adapt ingredients to a certain amount of portions",
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
    if args.groceries:
        groceries = exporter_groceries.to_groceries(
            args.recipe.buffer.read(),
            existing_groceries=read_from_path_or_none(args.output),
            portions=args.portions,
        )
        write_to_path_or_stdout(args.output, groceries)
    else:
        html = exporter_html.toHtml(
            args.recipe.buffer.read(),
            portions=args.portions,
            i18n_ingredients=args.s_ingredients,
            i18n_instructions=args.s_instructions,
            i18n_servings=args.s_servings,
            l10n_lang=args.s_lang,
        )
        write_to_path_or_stdout(args.output, html)


def read_from_path_or_none(path):
    if path is None:
        return None
    else:
        with open(path, "r") as output:
            return output.read()


def write_to_path_or_stdout(path, content):
    if path is None:
        sys.stdout.buffer.write(content)
    else:
        with open(path, "wb") as output:
            output.write(content)


if __name__ == "__main__":
    main()
