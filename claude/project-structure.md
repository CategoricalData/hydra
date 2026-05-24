# Project structure (pointer)

The canonical, user-facing project structure lives on the wiki:
[Code organization](https://github.com/CategoricalData/hydra/wiki/Code-organization),
including the `packages/heads/dist` split, the per-package layout, and the
two flavors of `bindings/` (third-party adapters vs per-package host DSL
helpers).

CLAUDE.md's "Project structure" section gives the orientation summary;
the wiki page gives the full picture. There is no Claude-specific
content here.

For build artifacts, the per-worktree convention (`.stack-work/`, `build/`,
`.gradle/` live per worktree, not shared across branches), and the bare-repo
+ worktrees layout, see CLAUDE.md "Project structure" and
[`claude/worktree-workflow.md`](worktree-workflow.md).
