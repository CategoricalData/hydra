# Hydra Documentation Style Guide

This document defines the writing style and formatting conventions for Hydra documentation.

## Capitalization

### Headings

Use sentence case for all headings (capitalize only the first word and proper nouns).
Acronyms like DSL, API, etc. remain capitalized, but words following them use lowercase:

**Correct:**
- `## Architecture overview`
- `## DSL system`
- `### Key design principles`
- `### Adding a new primitive function`
- `### API reference guide`

**Incorrect:**
- `## Architecture Overview`
- `## DSL System`
- `### Key Design Principles`
- `### Adding a New Primitive Function`
- `### API Reference Guide`

### Proper nouns

Always capitalize proper nouns, even in sentence case:

**Correct:**
- Hydra
- Haskell
- Java
- Python
- Hydra-Haskell

**Examples in context:**
- `### Relationship to core language` (common noun)
- `### DSL system` (acronym stays capitalized)
- `### Multi-language generation` (compound adjective, only first word capitalized)

### Bold emphasis

In bold text, use sentence case:

**Correct:**
- **Multi-language by design**
- **Type safety**
- **Self-hosting**

**Incorrect:**
- **Multi-Language By Design**
- **Type Safety**
- **Self-Hosting**

## Line length and wrapping

### Maximum line length

Keep lines to a maximum of 120 characters.

### Sentence-based wrapping

Wrap text at sentence boundaries when possible:

```markdown
Hydra is a strongly-typed functional programming language that executes in multiple language environments.
By design, developers can write Hydra source code in any of the supported host languages.
```

### Mid-sentence wrapping

For sentences longer than 120 characters, break mid-sentence at natural points (clauses, phrases):

```markdown
Coders enable cross-compilation of Hydra programs between different language implementations.
They transform Hydra modules (types and terms) from one language's representation to another, allowing developers
to write Hydra code in their preferred language and compile it to any other supported language.
```

### Code blocks

Code blocks and ASCII diagrams are exempt from the 120-character limit.

## Lists

### Bulleted lists

- Use sentence case for list items
- End with period only if the item is a complete sentence
- Indent continuation lines to align with the first line of text

**Example:**
```markdown
1. **Multi-language by design**: Hydra programs can be written in any supported host language and cross-compiled
   to others
2. **Unified type system**: All implementations share the same Hydra kernel (types, primitives, semantics)
```

## Links

### First mention

Link to relevant documentation on first mention:

```markdown
[Hydra-Haskell](https://github.com/CategoricalData/hydra/tree/main/hydra-haskell) serves as the source of truth
for the Hydra kernel.
```

### Internal links

Use relative links for internal documentation:

```markdown
See the [Concepts](concepts.md) documentation for more details.
```

## Code formatting

### Inline code

Use backticks for:
- Function names: `inferType`
- Type names: `Term`, `Type`
- File names: `Core.hs`
- Module names: `hydra.core`
- Language keywords: `lambda`, `let`

### Code blocks

Use fenced code blocks with language specification:

````markdown
```haskell
def "Term" $
  doc "A data term" $
  union [...]
```
````

## Language-specific conventions

### Referring to implementations

- Hydra-Haskell (with hyphen, capitalized)
- Hydra-Java (with hyphen, capitalized)
- Hydra-Python (with hyphen, capitalized)

### Referring to languages

- Haskell (capitalized)
- Java (capitalized)
- Python (capitalized)

### Maturity levels

When referring to implementation maturity:

- "mature" or "production-ready" (lowercase)
- "experimental" (lowercase)
- "in development" (lowercase)

Do not refer to immature implementations in user-facing documentation unless context is necessary.

## Technical writing

### Voice

- Use active voice when possible
- Use second person ("you") for recipes and tutorials
- Use third person for reference documentation

### Tense

- Present tense for describing how things work
- Imperative for instructions

**Examples:**
- "Hydra uses embedded domain-specific languages" (present, descriptive)
- "Add the following code to your module" (imperative, instructional)

### Clarity

- Define acronyms on first use
- Link to detailed explanations for complex concepts
- Provide examples for abstract concepts

## File organization

### Documentation structure

```
docs/
└── src/
    ├── documentation-style-guide.md  # This file
    └── recipes/            # Task-oriented guides
        ├── index.md
        ├── adding-primitives.md
        └── extending-hydra-core.md

wiki/
├── Home.md                 # Documentation index
├── Concepts.md             # Theoretical foundations
├── Implementation.md       # Implementation details
└── ...                     # Other reference docs
```

### File naming

- Use lowercase with hyphens: `adding-primitives.md`
- Be descriptive: `implementation-overview.md` not `impl.md`
- Use `.md` extension for Markdown files

## Markdown conventions

### Emphasis

- Use **bold** for key terms and emphasis
- Use *italic* sparingly for subtle emphasis
- Use `code formatting` for technical terms

### Section breaks

Use horizontal rules (`---`) to separate major sections:

```markdown
## Section One

Content here.

---

## Section Two

Content here.
```

## Examples and code samples

### Complete examples

Provide complete, runnable examples when possible:

```haskell
-- Complete example with imports
module Example where

import Hydra.Core

myFunction :: Term -> Term
myFunction = ...
```

### Partial examples

Mark partial examples clearly:

```haskell
-- ... existing code ...
def "NewType" $
  doc "Description" $
  record [...]
-- ... more code ...
```

## Reviewing documentation

When reviewing documentation, check for:

1. Sentence case in all headings
2. Lines under 120 characters
3. Sentence-based line wrapping
4. Proper capitalization of proper nouns
5. Consistent link formatting
6. Complete code examples
7. Clear, active voice
