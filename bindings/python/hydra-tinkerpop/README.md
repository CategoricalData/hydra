# hydra-tinkerpop (Python)

A bidirectional bridge between Hydra property graphs and Apache TinkerPop (Gremlin) graphs, in Python.
It is the Python counterpart of the Java `hydra.overlay.java.tinkerpop.coder` package: it converts between
a live TinkerPop graph (read/written via a gremlinpython `GraphTraversalSource`) and Hydra's
`hydra.pg.model.Graph`, and validates a TinkerPop graph against a Hydra `GraphSchema`.

## Why it lives under `hydra.overlay.python`

The bridge depends on gremlinpython — it is host-specific, not translingual. Per Hydra's namespace rule,
only translingual (generated) code lives outside `hydra.overlay`; everything host-specific lives under
`hydra.overlay.<lang>`. So the module is `hydra.overlay.python.tinkerpop.coder`. (It is currently packaged as
a binding; it is authored at its eventual overlay namespace so migrating it into `overlay/` later is a file
move, not a rewrite.)

## What it provides

`hydra.overlay.python.tinkerpop.coder`:

- `gremlin_to_hydra(g, value_mapper=object_to_literal)` — read a TinkerPop graph (via a traversal source)
  into a `hydra.pg.model.Graph`.
- `hydra_to_gremlin(graph, g, value_unmapper=literal_to_object)` — write a Hydra graph into a live
  TinkerPop graph (upsert by id; idempotent).
- `object_to_literal` / `literal_to_object` — value mappers between plain Python values and Hydra `Literal`s.
- `validate(schema, g)` — validate a TinkerPop graph against a Hydra `GraphSchema`, returning a `Result`
  (`is_valid`, `error`, printable `repr`). Wraps the `hydra.validate.pg` engine.
- `check_literal`, `show_literal`, `show_literal_type` — the validation callback and human-readable renderers.

## Testing

gremlinpython is a remote-only client (there is no embedded TinkerGraph in Python, unlike Java), so any test
that touches a live graph requires a running Gremlin Server and is therefore an **integration** test.

- `test/test_coder_unit.py` — server-independent unit tests of the value-mapping and validation logic.
- `test/test_coder_integration.py` — full round-trip and validation against a live server; skipped unless
  `HYDRA_TINKERPOP_INTEGRATION=1` and a Gremlin Server is running on `localhost:8182`.

The graph round-trip and validation are exercised against the built-in `hydra.tinkerpop.examples.modern`
sample graph (the canonical TinkerPop "Modern" graph).

Run the unit tests with `dist/python` (hydra-kernel, hydra-pg, hydra-rdf) and this binding's `src` on
`PYTHONPATH`, under Python 3.10+ (the generated code uses `typing.TypeAlias`).

## Status (0.17.0)

Source-correct and tested, but **not published**. Hydra's bindings are migrating into `overlay/` before the
0.17.0 release, so PyPI packaging is intentionally deferred.
