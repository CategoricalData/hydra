# Hydra Developer Recipes

Step-by-step guides for common Hydra development tasks. These recipes provide practical, hands-on instructions for extending Hydra, implementing new features, and working with Hydra's architecture.

## Available Recipes

### Core Development

- **[Adding new type and term constructors to Hydra Core](extending-hydra-core.md)** - Complete process including schema updates, type inference, rewriting functions, and solving the bootstrap problem.
- **[Adding new primitive functions](adding-primitives.md)** - Guide for implementing primitive functions across Haskell, Java, and Python implementations.

### Testing

- **[Extending the common test suite](extending-tests.md)** - How to add new test cases to Hydra's cross-language test suite.

### Implementations

- **[Creating a new Hydra implementation](new-implementation.md)** - Step-by-step guide for implementing Hydra in a new language
- **[Synchronizing Hydra-Python](syncing-python.md)** - How to regenerate Python artifacts from Hydra-Haskell sources
- **[Exporting the kernel to JSON](json-kernel.md)** - How to export and verify the Hydra kernel as JSON for cross-language access

### Refactoring

- **[Refactoring Hydra namespaces](refactoring-namespaces.md)** - How to rename or move a Hydra namespace across all implementations

### Development Workflow

- **[LLM-assisted development](llm-assisted-development.md)** - Best practices for using AI assistants when working with Hydra

## About Recipes

Recipes are practical, task-oriented guides that walk through specific development scenarios. Each recipe includes:

- Clear prerequisites and context
- Step-by-step instructions
- Code examples and file locations
- Common pitfalls and troubleshooting tips
- Verification steps

These complement Hydra's reference documentation by focusing on "how to accomplish X" rather than "what is X."

## Contributing Recipes

Have a common development task that would make a good recipe? Contributions are welcome! Recipes should:

- Focus on a specific, well-defined task
- Include concrete examples from the Hydra codebase
- Provide complete, tested instructions
- Note any version-specific considerations

See the existing recipes for examples of structure and style.
