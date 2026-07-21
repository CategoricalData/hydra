"""Driver-level fail-loud tests for hydra.generation's routing wrapper (#560).

hydra.test.build.routing (packages/hydra-build) already exercises the
GENERATED module (hydra.build.routing) directly for unrouted-namespace
failures. This test closes the remaining gap: proving generation.py's own
Either-to-RuntimeError conversion (namespace_to_package, group_by_package)
actually propagates that fail-loud behavior rather than swallowing it.
"""

import json
import os

import pytest
from hydra.generation import group_by_package, namespace_to_package
from hydra.packaging import Module, ModuleName


def _write_fixture_dist_json_root(tmp_path):
    """A minimal dist/json-shaped tree with one package declaring one
    module, so _build_routing_map has a non-empty (but incomplete) routing
    map to fail against."""
    manifest_dir = tmp_path / "hydra-kernel" / "src" / "main" / "json"
    manifest_dir.mkdir(parents=True)
    manifest = {"package": "hydra-kernel", "mainModules": ["hydra.only.module"]}
    (manifest_dir / "manifest.json").write_text(json.dumps(manifest))
    return str(tmp_path)


def test_namespace_to_package_raises_on_unrouted_namespace(tmp_path):
    dist_json_root = _write_fixture_dist_json_root(tmp_path)
    with pytest.raises(RuntimeError, match="^namespace_to_package:"):
        namespace_to_package(dist_json_root, ModuleName("hydra.totally.unrouted"))


def test_group_by_package_raises_if_any_module_is_unrouted(tmp_path):
    dist_json_root = _write_fixture_dist_json_root(tmp_path)
    unrouted = Module(
        name=ModuleName("hydra.totally.unrouted"),
        metadata=None,
        dependencies=[],
        definitions=[],
    )
    with pytest.raises(RuntimeError, match="^group_by_package:"):
        group_by_package(dist_json_root, [unrouted])
