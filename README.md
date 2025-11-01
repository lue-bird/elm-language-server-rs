> Status: **not ready for use but trying it on small projects probably works**

A minimal LSP language server for [elm](https://elm-lang.org/), written in rust and elm (which itself gets transpiled into rust when building).

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
- adding necessary imports when autocompleting
- your idea 👀

## TODO
- do not suggest member names already listed in explicit exposing
- trigger autocomplete on . without name
- incremental reparsing
- show errors inline
- support elm projects with non-root `elm.json`
- support `tests/`

## installation (warning: not trivial)
You'll need to [install `elm-to-rust`](https://github.com/lue-bird/elm-syntax-to-rust/tree/main/node-elm-to-rust) and rust itself.

Build the project with
```bash
elm-to-rust && cargo build --release
```

and point your editor to the created `target/release/elm-language-server-rs`.
If you're using vs code (or forks) (which do not appear to have a built-in way to just integrate a language server) you can use extensions like https://github.com/mjmorales/vscode-generic-lsp-proxy


## setup for developing
Rebuild the project with
```bash
elm-to-rust && cargo build
```
