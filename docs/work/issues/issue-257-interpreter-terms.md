# Issue #257: interpreter terms

## Naming discussion: `hydra.accessors` rename

The term "accessors" isn't ideal for the `hydra.accessors` module. The natural replacement
terms "step" and "path" are already taken by `hydra.query`, which would cause name collisions
when importing `Hydra.Kernel`.

### Options considered

**Rename in `hydra.accessors`** (newer/less established):
- **`hydra.traversal`** -- `TraversalStep`, `TraversalPath`, `TraversalGraph`.
  Emphasizes the navigation aspect.
- **`hydra.navigation`** -- `NavigationStep`, `NavigationPath`. Similar idea.
- **`hydra.cursor`** -- `CursorStep`, `CursorPath`, `CursorGraph`.
  Evokes the idea of a pointer moving through structure (like a zipper cursor).
- **`hydra.selector`** -- `Selector`, `SelectorPath`. CSS/jQuery-inspired.

**Rename in `hydra.query`** (more domain-specific):
- Query's `Step` -> `QueryStep` or `Traversal`, `Path` -> `QueryPath` or
  `PathExpression`/`RegexPath`

### Recommendation

Rename `hydra.accessors` -> **`hydra.traversal`** with types like `TraversalStep`
(was `TermAccessor`), `TraversalPath`, `TraversalEdge`, `TraversalGraph`, `TraversalNode`.
Reasoning:
- "Traversal" is well-established in FP (lens/optics) and graph theory
- It naturally pairs with "step" and "path" without collision --
  `TraversalStep` vs `Step`, `TraversalPath` vs `Path`
- It's more descriptive than "accessor" about what the types actually do
  (navigating into subterms)
- The query module's `Step`/`Path` names are clean and worth keeping as-is
