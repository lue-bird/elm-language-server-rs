vscode extension for [elm](https://elm-lang.org/) using [elm-language-server-rs](https://github.com/lue-bird/elm-language-server-rs). Having it installed is a strict requirement.

## settings
- `elm-language-server-rs.elmPath: string`: compiler executable, default `"elm"`
- `elm-language-server-rs.elmTestPath: string`: test runner executable, default `"elm-test"`
- `elm-language-server-rs.elmFormatPath: "builtin" | string`: formatter executable, default `"elm-format"`. `"builtin"` is a fast rust formatter that is mostly but not fully compatible

## setup for developing
```bash
npm install
```
Open in vscode and press `F5` (or navigate to "run and debug" and click the start button) to open a new window with the compiled extension enabled.
