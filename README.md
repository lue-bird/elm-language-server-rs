> Status: **not ready for use but trying it on small projects probably works**

A minimal LSP language server for [elm](https://elm-lang.org/), written in rust.

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

## not sure
Please give me feedback on this
- adding necessary imports when autocompleting
- when suggesting module names, only show those that share the prefix before the last . 
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

## setup for developing
Rebuild the project with
```bash
cargo build
```
