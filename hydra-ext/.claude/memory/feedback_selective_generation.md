---
name: Selective module generation
description: Only regenerate specific modules, not the entire hydra-ext universe, to save time and resources
type: feedback
---

When generating Haskell code from hydra-ext type modules, do NOT use the update-haskell-ext-main executable, which regenerates every module. Instead, use GHCi to generate only the specific modules needed:

```haskell
import Hydra.Ext.Generation
writeHaskell "src/gen-main/haskell" mainModules [MyModule.module_]
```

**Why:** Full regeneration is very time-consuming and resource-consuming. Selective generation is much faster.

**How to apply:** Whenever adding or modifying a single type module in hydra-ext, generate only that module.
