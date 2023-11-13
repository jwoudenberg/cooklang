#!/usr/bin/env python3

import sys
import cooklang_to_html.recipe_html as recipe_html


def main():
    recipeText = sys.stdin.buffer.read()
    html = recipe_html.toHtml(recipeText)
    sys.stdout.buffer.write(html)


if __name__ == "__main__":
    main()
