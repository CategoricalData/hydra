# Hydra language support for VS Code

Syntax highlighting and basic editing support for [Hydra](https://github.com/CategoricalData/hydra)
textual-syntax files (`.hydra`) — the neutral notation produced by the kernel's
`hydra.core.print.*` functions and specified in
[`docs/specification/syntax.md`](../../docs/specification/syntax.md).

Hydra is a translingual, functional graph language based on the LambdaGraph data model.
Its own kernel sources now live in `.hydra` files, so this extension exists to give that
source — and any `.hydra` you write — proper highlighting on first contact.

## Features

- **Syntax highlighting** via a TextMate grammar (`source.hydra`), derived directly from
  `syntax.md` §2–§3 (lexical structure, terms, types, type schemes) and §6 (module grammar):
  line comments (`#`) and structured doc comments (`##`, with `@key` metadata directives);
  JSON-style strings and escapes; backticked names; width/precision-suffixed literals
  (`42:int32`, `0.42:float32`, `NaN:float64`, `"…":binary`) and unsuffixed decimals; the
  reserved words of §2.7; the Unicode operators `λ Λ ∀ → ⇒ ⟨ ⟩` and the ASCII tokens
  `:= = : @`; and greedily-read qualified names, including the special dot handling in
  binder heads.
- **Editing support** via `language-configuration.json`: `#` line-comment toggling, bracket
  matching and auto-closing for `()`, `⟨⟩`, `{}`, `[]`, `<>`, and auto-closing quotes and
  backticks.
- **Unicode input** (see below): type `\lambda`, `\to`, `\forall`, … and accept the
  completion to insert `λ`, `→`, `∀`. Snippets for the common forms (module header,
  definition, lambda, `let … in`, `cases`) are included too.

Language intelligence (diagnostics, go-to-definition, hover, name completion) is **not**
part of this extension; it belongs to the Hydra language server
([#808](https://github.com/CategoricalData/hydra/issues/808)). The extension is built so it
can later start that server.

## Unicode input

Hydra's grammar is Unicode-only — there are no ASCII synonyms for `λ Λ ∀ → ⇒ ⟨ ⟩`
(`syntax.md` §2.3). Two ways to type them:

1. **Abbreviation completion.** Type a backslash followed by a name and accept the
   suggestion:

   | Type | Inserts | | Type | Inserts |
   |------|---------|-|------|---------|
   | `\lambda` or `\l` | `λ` | | `\to` or `\r` | `→` |
   | `\Lambda` or `\L` | `Λ` | | `\Rightarrow` or `\R` | `⇒` |
   | `\forall` or `\all` | `∀` | | `\langle` / `\<` | `⟨` |
   | | | | `\rangle` / `\>` | `⟩` |

2. **Snippets.** The same prefixes are also available as plain snippets, plus structural
   snippets: `module`, `import`, `type`, `term`, `primitive`, `lambda`, `typelambda`,
   `let`, `cases`.

## The `.hydra` file-association clash

The `.hydra` extension is also used by the [hydra-synth](https://hydra.ojack.xyz/)
live-coding community for JavaScript sketches, and at least one VS Code extension associates
`*.hydra` with JavaScript. If your `.hydra` files open as JavaScript (or vice versa), pin the
association explicitly in your settings.

To treat `.hydra` as **Hydra** (this extension):

```jsonc
// .vscode/settings.json or user settings
"files.associations": {
  "*.hydra": "hydra"
}
```

To treat `.hydra` as **JavaScript** (hydra-synth) while this extension is installed:

```jsonc
"files.associations": {
  "*.hydra": "javascript"
}
```

Per-folder settings (`.vscode/settings.json`) override user settings, so you can keep Hydra
globally and opt a hydra-synth project back to JavaScript, or the reverse.

## Development

```sh
npm install
npm run compile        # typecheck + build the extension host (out/)
npm run test:grammar   # scope-assertion tests (tests/scopes/*.hydra)
npm run test:corpus    # snapshot tests against the kernel's own .hydra sources
npm test               # both
npm run package         # produce a .vsix
```

### Grammar tests

Two layers:

- **`tests/scopes/*.hydra`** — hand-authored `vscode-tmgrammar-test` scope assertions, the
  stable unit suite. Each file is itself valid `.hydra` (assertions ride on `#` comment
  lines). Covers forms the kernel corpus does not exercise: suffixed/binary/decimal
  literals, `let … in`, `cases`, nominal sugar (`Name{…}` / `Name[…]`, #809), and the
  forward-looking #811 operator sugar.
- **`tests/corpus/*.hydra` + `*.snap`** — snapshot tests run against a copy of the kernel's
  own `.hydra` sources (`packages/hydra-kernel/src/main/hydra/`). `npm run test:corpus:refresh`
  re-copies the sources and regenerates the snapshots; CI re-runs it and fails if they
  drift. This is the intended coupling: a change to `syntax.md` / the kernel sources breaks
  the grammar tests until the grammar catches up.

The grammar is the **single source of truth** for Hydra highlighting. The IntelliJ plugin,
the docs site (e.g. via Shiki) and a future GitHub Linguist entry can all reuse
`syntaxes/hydra.tmLanguage.json`.

## Publishing

Targets both the VS Code Marketplace and [Open VSX](https://open-vsx.org/) (which serves
VSCodium, Cursor and other editors). The extension version is tied to Hydra's single
version number. Publisher ID, display name and icon are pending the Hydra branding issue
([#495](https://github.com/CategoricalData/hydra/issues/495)).

## License

Apache-2.0, matching the Hydra project.
