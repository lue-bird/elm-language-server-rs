> Status: should work, with some features still to come

Small LSP language server for [elm](https://elm-lang.org/).
To use, [install rust](https://rust-lang.org/tools/install/) and
```bash
cargo install --git https://github.com/lue-bird/elm-language-server-rs
```
Then point your editor to `elm-language-server-rs`.
To use in vs code (or forks) (which appear to have no built-in language server bridge) use extensions like [vscode-generic-lsp-proxy](https://github.com/mjmorales/vscode-generic-lsp-proxy).

Works with [`elm-format`, install it](https://github.com/avh4/elm-format?tab=readme-ov-file#installation-) if you haven't already.

## goals
- fast (e.g. rename must be instant no matter the project size)
- reliable (e.g. work well with code that only partially parses)
- getting to know the language server protocol :)

## not planned
- support for elm version <= 0.18 or inline GLSL blocks
- type inference
- directly integrating `elm-test` running
- codelens, outline symbols, code folding, linked editing
- `elm.json` help

## not sure (Please give me feedback on this)
- configuring a custom elm compiler and elm formatter path. Is there an established way to set it, preferably something like an environment variable outside of LSP configuration? (If there is, I will support it)
- switch to `position_encoding: Some(lsp_types::PositionEncodingKind::UTF8)`. This makes source edits and parsing easier and faster at the cost of compatibility with lsp clients below version 3.17.0. Is that acceptable? (leaning towards yes).
  Also validate if elm --report region column is UTF-8 or UTF-16 (seems to be UTF-16 strangely)
- adding necessary imports when autocompleting vs making it a separate code action vs nothing vs ? (leaning towards separate code action)
- show function parameter names (leaning towards no, as they are often confusing if they are curried, reveal non-exposed variant patterns, have more parameters than the type suggests, are undescriptive etc)
- add "find references"
- add support for `elm-review` (depends on how easy it is, which I assume it isn't)
- add code actions like "expose (including variants)", "inline", "inline all uses" (leaning towards no as it is fairly complicated, though it it very useful for sure)
- when showing module documentation inline actual info for `@docs` members (these are not super easy to parse but would be useful, only if I have time and there is interest)
- show all module exposes when hovering `(..)` (only if I have time and there is interest)
- support when the `elm.json` changes (only if I have time and there is interest)
- It is possible that an elm module belongs to multiple projects when source directory paths overlap between projects. This throws a wrench in pretty much all existing code (likely internal document source desync and a more limited lsp feature range in one of the containing projects). This situation is, I assume, fixable by special-casing their storage and handling but it would require a _lot_ of work
- add a vs code extension as glue (would prefer not to, especially because I dislike distributing binaries. But open to the idea, as it would enable syntax highlighting inside doc comments and make installing easier)
- add support for using [elm-dev](https://github.com/mdgriffith/elm-dev) as the compiler to speed up compile times and retrieve type info (I assume integrating it is hard and to me it seems kind of bloated with MCP bs etc, currently leaning towards no)
- your idea 👀

## TODO
- add suggestions for expose-imported variables and types
- do not suggest exposed/module-declared members when local module-declared/binding has the same name
- incremental reparsing (somehow it's pretty much fast enough already without?)
- do not support goto definition on let declaration name and choice type
- support `tests/` (dev hint: just add the tests directory as a source file and test dependencies as dependencies to the regular project state, do not add it to a separate project state to avoid module state syncing issues)
- support elm projects with non-workspace-root `elm.json`

## setup for developing
Rebuild the project with
```bash
cargo build
```
Now you can point your editor to the created `target/release/elm-language-server-rs`.
If you're using vs code (or forks) (which do not appear to have a built-in way to just integrate a language server) you can use extensions like [vscode-generic-lsp-proxy](https://github.com/mjmorales/vscode-generic-lsp-proxy)
