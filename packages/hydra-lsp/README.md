# hydra-lsp

The Hydra language server (#808, increment 1: #836), written in Hydra.

The translingual layers live here as Hydra modules and generate to every target
(currently Haskell and TypeScript):

- `hydra.ext.lsp.protocol` — the subset of the Language Server Protocol types the server uses.
  Enumerations (e.g. `DiagnosticSeverity`) are unions, per Hydra house style.
- `hydra.ext.lsp.wire` — the explicit translation between those types and LSP's JSON wire format
  (enum to fixed-integer tables, absent optionals omitted). Hydra's native JSON format is for data
  exchange within Hydra and is deliberately not used on the wire.
- `hydra.lsp.analysis` — pure analysis over document text (`format`; diagnostics follow #834's span API).

The thin per-host transport shim (`vscode-languageserver` for TypeScript) lives under
`overlay/typescript/hydra-lsp/`; the editor client is `editors/vscode/`.

The package is `"publishable": false` until the server is usable.

## Layout

```
packages/hydra-lsp/
  src/main/haskell/Hydra/Sources/Lsp/
    Analysis.hs   # hydra.lsp.analysis
    Manifest.hs   # module list
    Protocol.hs   # hydra.ext.lsp.protocol
    Wire.hs       # hydra.ext.lsp.wire
```
