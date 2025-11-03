> Status: **not ready for use but trying it on small projects probably works**

Minimal LSP language server for [elm](https://elm-lang.org/).
To use, [install rust](https://rust-lang.org/tools/install/), clone this project and run
```bash
cargo build --release
```
Now you can point your editor to the created `target/release/elm-language-server-rs`.
If you're using vs code (or forks) (which do not appear to have a built-in way to just integrate a language server) you can use extensions like https://github.com/mjmorales/vscode-generic-lsp-proxy

## goals
- fast
- reliable
- tolerate code that only partially parses
- getting to know the language server protocol :)

## not planned
- support for elm version <= 0.18
- type inference
- directly integrating `elm-test`. `elm-review` or `elm-format`
- codelens
- outline symbols
- code folding
- linked editing
- `elm.json` help
- inline GLSL blocks

## not sure (Please give me feedback on this)
- adding necessary imports when autocompleting vs making it a separate code action vs nothing vs ? (leaning towards separate code action)
- show function parameter names (leaning towards no, as they are often confusing if they are curried, reveal non-exposed variant patterns, have more parameters than the type suggests, are undescriptive etc)
- add code actions like "expose (including variants)", "inline", "inline all uses" (leaning towards no as it is fairly complicated, though it it very useful for sure)
- your idea 👀

## TODO
- add suggestions for import aliases when typing uppercase reference
- add suggestions for expose-imported variables and types
- after typing an import alias, suggest exposed members from its associated module
- do not suggest member names already listed in explicit exposing
- do not suggest anything for declaration names
- incremental reparsing
- show errors inline
- support elm projects with non-root `elm.json`
- support `tests/`
- support when new modules get created or the `elm.json` changes

## setup for developing
Rebuild the project with
```bash
cargo build
```
