# Fountain Parser (three‑and‑a‑half programs) 🎬

A loosely unified toolkit for working with the [Fountain](https://fountain.io) screenplay format—implemented as three (‑and‑a‑half, if CSS counts as programming) self‑contained components. Each part does one job well, and they compose into a clean pipeline from `.fountain` to polished `.html`.

---

## Components

- **Haskell parser:** Parses `.fountain` into an intermediate `.ast` (Cabal project, developed with TDD).
- **`htmlfier.lua`: Lua converter:** Transforms the `.ast` into raw `.html` (no indentation for simplicity).
- **`prettifyhtml.py`: Python prettifier:** Adds indentation to the generated `.html` for readability.
- **`assets/screenplay.css`: CSS stylesheet:** Screenplay‑esque formatting for the final HTML.

(Together, they make what is effectively a code-to-code compiler.)

---

## Features ✨

- **Separation of concerns:** Clear boundaries between parsing, conversion, and formatting.
- **Intermediate representation:** Stable `.ast` for tooling, testing, and future extensions.
- **Minimal dependencies:** Lightweight Lua and Python scripts; Haskell build via Cabal.
- **Composable pipeline:** Run end‑to‑end or use components independently.
