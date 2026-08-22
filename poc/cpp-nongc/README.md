# C++ POC — structural tree-ownership (#678)

Proof-of-concept for [#678](https://github.com/CategoricalData/hydra/issues/678): a recursive,
Hydra-shaped `Term` type in C++, constructed and dropped, demonstrated **leak-free** under two disciplines.

See the design note (`docs/nongc-memory-discipline.md`, C++ section) for the full analysis.

## Files

- `term_unique_ptr.cpp` — the **recommended default**: `std::unique_ptr<Term>` recursive children, RAII
  destruction. A static drop-counter proves every node's destructor runs exactly once; ASan/LSan confirms
  no leak and no double-free.
- `term_arena.cpp` — the **arena option**: build the whole tree in a bump-allocated region, free the region
  wholesale. Byte accounting (handed-out == reclaimed) plus LSan back the "arena API implication" claim with
  compiled code.

## Run (both are leak-checked with AddressSanitizer + LeakSanitizer)

```sh
g++ -std=c++17 -fsanitize=address,leak -g -O0 term_unique_ptr.cpp -o /tmp/cpp_uptr && /tmp/cpp_uptr
g++ -std=c++17 -fsanitize=address,leak -g -O0 term_arena.cpp     -o /tmp/cpp_arena && /tmp/cpp_arena
```

A clean exit (code 0, no sanitizer report) means: drop-counter matched the node count (unique_ptr) or
malloced-bytes matched freed-bytes (arena), **and** LSan found zero leaks. valgrind is an alternative if
present (`-fsanitize` is used here because valgrind is not installed on the build box).

## Result on this box (g++ 12.2)

- `term_unique_ptr`: 12 nodes constructed, 12 destroyed exactly once, LSan clean.
- `term_arena`: 10 nodes built in one region, freed wholesale, LSan clean.
