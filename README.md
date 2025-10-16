> Status: **do not use 💩**

A minimal LSP language server for [elm](https://elm-lang.org/), written in a bit of rust and mostly elm (which itself gets transpiled into rust when building).

## goals
- fast
- reliable
- tolerate code that only partially parses
- getting to know the language server protocol :)

## not planned
- support for elm version <= 0.18
- type inference
- directly integrating `elm-review` or `elm-format`
- `elm.json` help
- inline GLSL blocks

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

If you're working on the rust part and notice that
`rustfmt` is not doing it's job in some function bodies,
consider `cargo install`ing [genemichaels](https://github.com/andrewbaxter/genemichaels/blob/master/crates/genemichaels/readme.md)
or other alternative formatters :)
