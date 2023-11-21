#!/usr/bin/env python3

import sys
import cooklang_to_html.recipe_html as recipe_html
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
        type=argparse.FileType("w"),
        default=sys.stdout,
        help="path to output file. Writes to stdout by default",
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
    html = recipe_html.toHtml(
        args.recipe.buffer.read(),
        portions=args.portions,
        i18n_ingredients=args.s_ingredients,
        i18n_instructions=args.s_instructions,
        i18n_servings=args.s_servings,
        l10n_lang=args.s_lang,
    )
    args.output.buffer.write(html)


if __name__ == "__main__":
    main()
