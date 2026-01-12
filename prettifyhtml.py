#!/usr/bin/env python3
# A simple script to 'prettify' HTML docs
# This utility reformats HTML files so their structure is easier to read.
# It uses BeautifulSoup to parse the document and then rewrites it with
# consistent indentation, defaulting to two spaces per level.

import sys
import bs4 # Formatter + BeautifulSoup for parsing and formatting tools

def prettify_html(filename: str, indent: int = 2) -> None:
  # Read the HTML file
  # Open the given file in text mode with UTF-8 encoding and load its contents.
  with open(filename, 'r', encoding='utf-8') as f:
    html = f.read()

  # Parse and prettify
  # Replace the default 1 space with the specified indent
  # BeautifulSoup defaults to one space; here we adjust to the user’s choice.
  formatter = bs4.formatter.HTMLFormatter(indent=indent)

  # Feed the raw HTML into BeautifulSoup using the html parser.
  # The prettify method outputs a formatted version with standard indentation.
  pretty_html = bs4.BeautifulSoup(html, 'html.parser').prettify(formatter=formatter)

  # Write back to the same file
  # Overwrite the original file with the newly formatted HTML.
  with open(filename, 'w', encoding='utf-8') as f:
    f.write(pretty_html)

def main():
  # Check command-line arguments
  # Expect either one argument (the file name) or two (file name plus indent size).
  if len(sys.argv) < 2 or len(sys.argv) > 3:
    print(f'Usage: {sys.argv[0]} <htmlfile> [indent_spaces]')
    sys.exit(1)

  # Extract the file name from the first argument
  filename = sys.argv[1]
  # If a second argument is given, interpret it as the indent size; otherwise default to 2.
  indent = int(sys.argv[2]) if len(sys.argv) == 3 else 2

  # Perform the prettifying operation
  prettify_html(filename, indent)

# Standard Python entry point
# Ensures that main() runs only when the script is executed directly.
if __name__ == '__main__':
  main()
