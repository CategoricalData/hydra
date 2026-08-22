# C POC — structural tree-ownership via arena (#678)

Proof-of-concept for [#678](https://github.com/CategoricalData/hydra/issues/678): a recursive,
Hydra-shaped `Term` struct graph in C, built in an **arena/region** and freed wholesale, demonstrated
**leak-free**.

See the design note (`docs/nongc-memory-discipline.md`, C section) for the full analysis.

## What it demonstrates

- A minimal bump-allocating arena (`arena_alloc` / `arena_free_all`) growing in blocks.
- Every `Term` node, name, and child-pointer array lives in the arena; recursive children are plain `Term*`
  into the same region (safe — shared arena lifetime).
- `arena_free_all` frees the whole region at once: no per-node `free`, no leak, no double-free.
- Two signals: byte accounting (`bytes_malloced == bytes_freed`) and LSan (zero leaks). A multi-block stress
  (500 nodes into tiny 256-byte blocks) exercises the block-growth + wholesale-free path, not just the
  single-block case.
- **API implication (the honest cost):** the consumer holds and frees an arena handle; node lifetimes are
  arena-scoped. Acceptable for immutable tree data (no node ever needs early freeing).

## Run (leak-checked with AddressSanitizer + LeakSanitizer)

```sh
gcc -std=c11 -fsanitize=address,leak -g -O0 term_arena.c -o /tmp/c_arena && /tmp/c_arena
```

Clean exit (code 0, no sanitizer report) means byte accounting balanced **and** LSan found zero leaks.

## Result on this box (gcc 12.2)

10 nodes in one region (65560 bytes malloced == freed); multi-block stress 500 nodes across many tiny
blocks, all freed; LSan clean.
