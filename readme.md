Small LSP language server for [elm](https://elm-lang.org/).
To use, [install rust](https://rust-lang.org/tools/install/) and
```bash
cargo +nightly install --git https://github.com/lue-bird/elm-language-server-rs
```
Then point your editor to `elm-language-server-rs`, see also [specific setups](#editor-setups).

Works with [`elm-format`](https://github.com/avh4/elm-format?tab=readme-ov-file#installation) and [`elm-test`](https://github.com/rtfeldman/node-test-runner?tab=readme-ov-file#installation), install them if you haven't already.

## goals
- fast (e.g. rename must be instant no matter the project size)
- reliable (e.g. work well with code that only partially parses)
- getting to know the language server protocol :)

## not planned
- support for elm version <= 0.18 or inline GLSL blocks
- type inference
- directly integrating `elm-test` running
- codelens, workspace symbols, code folding, linked editing
- `elm.json` help

## not sure (Please give me feedback on this)
- configuring a custom elm compiler, formatter and test runner path. Is there an established way to set it, preferably something like an environment variable outside of LSP configuration? (If there is, I will support it)
- reparse incrementally (somewhat easy to implement but somehow it's for me at least pretty much fast enough already without? More data points welcome)
- add support for `elm-review` (depends on how easy it is, which I assume it isn't)
- show all module exposes when hovering `(..)` (only if I have time and there is interest)
- add code actions like "expose (including variants)", "inline", "inline all uses" (leaning towards no as it is fairly complicated, though it is very useful for sure)
- support when the `elm.json` changes (only if I have time and there is interest)
- switch to `position_encoding: Some(lsp_types::PositionEncodingKind::UTF8)`. This makes source edits and parsing easier and faster at the cost of compatibility with lsp clients below version 3.17.0. Is that acceptable? (leaning towards yes).
  Also validate if elm --report region column is UTF-8 or UTF-16 (seems to be UTF-16 strangely)
- show function parameter names (leaning towards no, as they are often confusing if they are curried, reveal non-exposed variant patterns, have more parameters than the type suggests, are undescriptive etc)
- currently, an exposed member will still be suggested even when a local module-declared reference/local binding with the same name exists. Likewise, a local module-declared reference will still be suggested even when a local binding with the same name exists. (somewhat easily fixable but I don't really see the harm in directly showing this shadowing in your face)
- add support for using [elm-dev](https://github.com/mdgriffith/elm-dev) as the compiler to speed up compile times and retrieve type info (I assume integrating it is hard and to me it seems kind of bloated with MCP bs etc, currently leaning towards no)
- your idea 👀

## known limitations
- when moving a module into an existing project, no syntax highlighting will be shown before you interact with the file, as semantic tokens are requested before the "did change watched files" notification is sent.
  A band-aid fix would be pre-maturely assuming that requesting semantic tokens on an unknown file means that that file should be added to a project. This would however potentially lead to duplicate parsing etc.
  There must surely be a better way, right?
- It is possible that an elm module belongs to multiple projects when source directory paths overlap between projects. This throws a wrench in pretty much all existing code (likely internal document source desync and a more limited lsp feature range in one of the containing projects).
  This situation is, I assume, fixable by special-casing their storage and handling but it would require a _lot_ of work

## editor setups
feel free to contribute, as I only use vscodium

### vscode-like
#### pre-built
1. download https://github.com/lue-bird/elm-language-server-rs/blob/main/vscode/elm-language-server-rs-0.0.1.vsix
2. open the command bar at the top and select: `>Extensions: Install from VSIX`
#### build from source
1. clone this repo
2. open `vscode/`
3. run `npm run package` to create the `.vsix`
4. open the command bar at the top and select: `>Extensions: Install from VSIX`
#### server only
There is no built-in language server bridge as far as I know but you can install an extension like [vscode-generic-lsp-proxy](https://github.com/mjmorales/vscode-generic-lsp-proxy) that will work for any language server.
Then add a `.vscode/lsp-proxy.json` like
```json
[
  {
    "languageId": "elm",
    "command": "elm-language-server-rs",
    "fileExtensions": [
      ".json",
      ".elm"
    ]
  }
]
```

### helix
write to `~/.config/helix/languages.toml`:
```toml
[language-server.elm-language-server-rs]
command = "elm-language-server-rs"
[[language]]
name = "elm"
scope = "source.elm"
injection-regex = "elm"
roots = ["elm.json"]
file-types = ["elm", "json"]
comment-token = "--"
block-comment-tokens = { start = "{-", end = "-}" }
indent = { tab-width = 4, unit = "    " }
language-servers = [ "elm-language-server-rs" ]
auto-format = true
```

## setup for developing
Rebuild the project with
```bash
cargo build
```
Then point your editor to the created `???/target/debug/elm-language-server-rs`.
