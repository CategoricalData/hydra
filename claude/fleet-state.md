# Fleet coordination state (durable — edit in place)

Do not append here; LOG.md is the event stream. This file holds current truth only.

## Machines
- alpha (GCE): staging-gce + release_508_hydra_0_17 + issue agents
- marvin7 (laptop): staging + issue agents

## Standing arrangements
- marvin7-staging is acting release_508 coordinator for #508-children spawned on
  marvin7 (currently: #538). The 0.17 finalize gate remains with alpha release_508.

## Protocol (summary; full text in [fleet-coordination.md](fleet-coordination.md))
- Read-before-act: fetch this branch before pushes to main, red-CI claims, contested spawns.
- Claims: append to LOG.md + push. Push success = claim acquired; rejection = pull and defer.
- Idle poll: fetch every ~10-15 min inside long watcher loops.
- Rollover: this branch is periodically deleted and recreated (STATE.md carried, LOG.md fresh).
