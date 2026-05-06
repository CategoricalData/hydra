# hydra-rdf4j

Java binding between Hydra's RDF model (`hydra.rdf.syntax.*`) and Eclipse RDF4j.

Provides `hydra.rdf.SerdeNative`, which serializes Hydra RDF graphs to standards-compliant
N-Triples (and other formats supported by RDF4j Rio) and deserializes the reverse.

## Maven coordinates

```
net.fortytwo.hydra:hydra-rdf4j:0.16.0
```

## Dependencies

- `net.fortytwo.hydra:hydra-rdf` — the Hydra RDF model package (transitively `hydra-kernel`)
- `org.eclipse.rdf4j:rdf4j-rio-ntriples` — RDF4j's N-Triples Rio module (transitively `rdf4j-model`)

## Building locally

```sh
gradle :hydra-rdf4j:build
```

## See also

- The Hydra `bindings/` philosophy: handwritten host-language adapters that wire Hydra
  packages to external libraries, kept separate from `heads/` runtimes (which stay
  third-party-free) and from `packages/` (which contain DSL-based module definitions).
- [`docs/implementation.md`](../../../docs/implementation.md) principle 7.
