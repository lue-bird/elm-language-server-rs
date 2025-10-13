> Status: **do not use 💩**

A minimal LSP language server for [elm](https://elm-lang.org/), written in a bit of rust and mostly elm (which itself gets transpiled into rust when building).

## goals
- fast
- reliable
- tolerate code that only partially parses
- getting to know the language server protocol :)

## not planned
- inline GLSL blocks
- type inference
- `elm.json` help
- directly integrating `elm-review` or `elm-format`

## current issues (TODO investigate)
- high idle memory footprint (around 600MB for a somewhat small project alone).
  Caused by (I assume) parsing-produced garbage always being persisted.
  Potential alternatives:
    - convert between the parsed `ElmSyntaxFile` and a persistent rust type (a lot of work (now and when the elm types change) and not "free" performance-wise).
      This is arguably the most actionable, future-proof and least invasive option because it also leaves open the option of opening up more incremental "updates" of parsed state instead of "from scratch" replacing.
    - always generate persistent (usually just Rc/Arc instead of &) versions of elm functions and types from elm-syntax-to-rust (bloats the default declarations, not trivial to implement, likely very useful and performant). This is currently not (easily) possible because by default rust does not support pattern matching inside an Rc/Arc
    - other ideas?

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
