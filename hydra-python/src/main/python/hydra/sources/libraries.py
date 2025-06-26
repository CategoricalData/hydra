"""Python implementations of primitive name constants and registration functions."""


from hydra.core import Name
from hydra.graph import Primitive


def qname(namespace: str, local_name: str) -> Name:
    """Qualified name constructor."""
    return Name(f"{namespace}.{local_name}")


def register_string_primitives() -> dict[Name, Primitive]:
    """Register all string primitive functions."""
    all_primitives: dict[Name, Primitive] = {}
    return all_primitives


def standard_library() -> dict[Name, Primitive]:
    """Get all standard library primitives."""
    primitives: dict[Name, Primitive] = {}
    primitives.update(register_string_primitives())
    return primitives
