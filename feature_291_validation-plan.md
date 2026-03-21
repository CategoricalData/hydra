# Feature 291: Standardize Validation Patterns

## Goal

Standardize validation in Hydra with parallel module hierarchies:
- `hydra.errors.*` -- type-level error definitions
- `hydra.validate.*` -- term-level validation functions returning `maybe<e>`

Focus on hydra-haskell only for now. hydra-ext (PG validation) deferred to a later pass.
`hydra.context` stays as-is (tracked separately in issue #292).

## Architecture

**Type-level modules** (error type definitions, kernel types):
- `hydra.errors.checking` -- CheckingError union + 10 sub-error types (moved from hydra.error)
- `hydra.errors.core` -- DecodingError, DuplicateBindingError, DuplicateFieldError,
  UndefinedFieldError, UndefinedTermError, UndefinedTypeError,
  UnexpectedTermVariantError, UnexpectedTypeVariantError (moved from hydra.error)
- `hydra.errors.module` -- (empty for now, ready for module validation errors)

**Kept in `hydra.error`:**
- `Error` (top-level union referencing types from new modules)
- `OtherError` (catch-all)
- `UnificationError`

**Term-level modules** (validation functions, kernel terms) -- not yet created:
- `hydra.validate.core`
- `hydra.validate.module`

## Progress

- [x] Create `hydra.errors.checking` type module
- [x] Create `hydra.errors.core` type module
- [x] Create `hydra.errors.module` type module (empty)
- [x] Update `hydra.error` to import from new modules
- [x] Register new modules in Types/All.hs
- [x] Build succeeds
- [x] Tests pass (4397 examples, 0 failures)
- [x] Update source files to use new DSL imports (Checking.hs, Extract/Helpers.hs, Decoding.hs)
- [x] Add Hydra.Errors.Checking and Hydra.Errors.Core to Kernel.hs re-exports
- [x] Move DecodingError back to hydra.error (where it belongs alongside OtherError/UnificationError)
- [x] sync-haskell.sh passes (4397 tests, 0 failures)
- [x] sync-ext.sh passes
- [x] Create `hydra.validate.core` term module (functions: term, checkTerm, checkDuplicateBindings, checkDuplicateFields, findDuplicate)
- [x] Create `foldTermWithGraphAndPath` in hydra.rewriting
- [x] Move extendGraphFor* functions to hydra.rewriting, rename rewrite functions
- [x] Create `hydra.show.errors.core` show module
- [x] Add `InvalidTermError` union type and `location :: AccessorPath` field to DuplicateBindingError/DuplicateFieldError
- [x] Add `ValidateCoreTermTestCase` type to hydra.testing
- [ ] Fix test generation errors (Phase 2 fails with type unification issues in test data)
- [ ] Create `hydra.validate.module` term module
- [ ] Get validation tests running end-to-end
