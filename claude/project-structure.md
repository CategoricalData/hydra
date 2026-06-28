# Project structure (pointer)

The canonical, user-facing project structure lives on the wiki:
[Code organization](https://github.com/CategoricalData/hydra/wiki/Code-organization),
including the `packages/heads/overlay/dist` split and the per-package layout.
`overlay/<lang>/<package>/` holds hand-written source overlaid onto the generated
distribution packages (#418), including host-specific third-party integrations
(the former `bindings/` tree, folded into overlays in #511 — see
[docs/overlays.md](../docs/overlays.md)).

CLAUDE.md's "Project structure" section gives the orientation summary;
the wiki page gives the full picture. There is no Claude-specific
content here.

For build artifacts, the per-worktree convention (`.stack-work/`, `build/`,
`.gradle/` live per worktree, not shared across branches), and the bare-repo
+ worktrees layout, see CLAUDE.md "Project structure" and
[`claude/worktree-workflow.md`](worktree-workflow.md).
