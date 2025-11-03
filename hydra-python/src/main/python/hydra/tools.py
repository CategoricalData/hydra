"""Tools for Hydra."""

import inspect
import os
from collections.abc import Callable
from typing import Any, TypeVar, cast, get_type_hints, overload

from hydra.core import Name
from hydra.dsl.phantoms import primitive1
from hydra.dsl.prims import int32, prim1, string
from hydra.graph import TermCoder
from hydra.phantoms import TTerm

A = TypeVar("A")
B = TypeVar("B")

# Type mappings from Python types to TermCoder factories
TYPE_CODERS: dict[type, Callable[[], TermCoder[Any]]] = {
    str: string,
    int: int32,
}


@overload
def primitive(func: Callable[[A], B]) -> Callable[[TTerm[A]], TTerm[B]]: ...


@overload
def primitive(
    *, name: str | None = None, namespace: str | None = None
) -> Callable[[Callable[[A], B]], Callable[[TTerm[A]], TTerm[B]]]: ...


def primitive(
    func: Callable[[A], B] | None = None,
    *,
    name: str | None = None,
    namespace: str | None = None,
) -> (
    Callable[[TTerm[A]], TTerm[B]]
    | Callable[[Callable[[A], B]], Callable[[TTerm[A]], TTerm[B]]]
):
    """Lift a regular Python function to a Hydra DSL primitive.

    Takes a function like:
        def to_lower(s: str) -> str:
            return s.lower()

    And transforms it into:
        def to_lower(s: TTerm[str]) -> TTerm[str]:
            return primitive1(to_lower_name, s)

    Parameters
    ----------
    func: Callable[[A], B] | None
        The function to lift.
    name: str | None
        The name of the primitive.
    namespace: str | None
        The namespace of the primitive.

    Returns
    -------
        Either the lifted function directly or a decorator function.
    """
    if func is not None:
        # Used as @primitive_lift without parentheses
        return _lift_function(func, name, namespace)
    else:
        # Used as @primitive_lift() with parentheses
        def decorator(f: Callable[[A], B]) -> Callable[[TTerm[A]], TTerm[B]]:
            return _lift_function(f, name, namespace)

        return decorator


def _infer_namespace_from_file(func: Any) -> str:
    """Infer the namespace from the file path where the function is defined."""
    try:
        # Get the file path where the function is defined
        file_path = inspect.getfile(func)
        
        # Convert to relative path and extract the relevant parts
        # Look for patterns like: .../hydra/dsl/lib/strings.py
        path_parts = file_path.replace(os.path.sep, '/').split('/')
        
        # Find the hydra/dsl/lib part and extract the module name
        try:
            hydra_idx = path_parts.index('hydra')
            if (hydra_idx + 2 < len(path_parts) and 
                path_parts[hydra_idx + 1] == 'dsl' and 
                path_parts[hydra_idx + 2] == 'lib'):
                # Extract the module name (without .py extension)
                module_file = path_parts[hydra_idx + 3]
                module_name = os.path.splitext(module_file)[0]
                return f"hydra.lib.{module_name}"
        except (ValueError, IndexError):
            pass
            
        # Fallback: try to extract any module name from the file
        file_name = os.path.basename(file_path)
        module_name = os.path.splitext(file_name)[0]
        return f"hydra.lib.{module_name}"
        
    except (OSError, TypeError):
        # Fallback to default namespace
        return "hydra.lib.lifted"


def _lift_function(
    func: Callable[[A], B], name: str | None, namespace: str | None
) -> Callable[[TTerm[A]], TTerm[B]]:
    """Lift a function to a Hydra DSL primitive."""
    # Get function name and infer namespace if not provided
    func_name = name or func.__name__
    inferred_namespace = namespace or _infer_namespace_from_file(func)
    primitive_name = Name(f"{inferred_namespace}.{func_name}")

    # Get type hints
    type_hints = get_type_hints(func)

    # Get function signature
    sig = inspect.signature(func)
    params = list(sig.parameters.values())

    if len(params) == 0:
        raise ValueError("Cannot lift functions with no parameters")

    # For now, only support single-parameter functions
    if len(params) != 1:
        raise ValueError("Currently only single-parameter functions are supported")

    # Get input and output types
    param = params[0]
    input_type = type_hints.get(param.name)
    return_type = type_hints.get("return")

    if input_type is None:
        raise ValueError(f"Parameter {param.name} must have a type annotation")
    if return_type is None:
        raise ValueError("Function must have a return type annotation")

    # Get coders for input and output types
    if input_type not in TYPE_CODERS:
        raise ValueError(f"Unsupported input type: {input_type}")
    if return_type not in TYPE_CODERS:
        raise ValueError(f"Unsupported return type: {return_type}")

    input_coder_factory = TYPE_CODERS[input_type]
    output_coder_factory = TYPE_CODERS[return_type]

    # Create the primitive
    primitive = prim1(
        name=primitive_name,
        compute=func,
        variables=[],
        input1=input_coder_factory(),
        output=output_coder_factory(),
    )

    # Register the primitive by namespace
    # TODO: Add in registration to the standard library

    # Create the lifted function with proper typing
    def lifted_with_typing(arg: TTerm[A]) -> TTerm[B]:
        """Lifted version of the original function."""
        return cast(TTerm[B], primitive1(primitive_name, arg))

    # Preserve original function metadata
    lifted_with_typing.__name__ = func.__name__
    lifted_with_typing.__doc__ = func.__doc__ or f"Lifted version of {func_name}."
    lifted_with_typing.__annotations__ = {
        next(iter(sig.parameters.keys())): TTerm[input_type],
        "return": TTerm[return_type],
    }

    # Store reference to the primitive name on the function
    setattr(lifted_with_typing, "_primitive_name", primitive_name)
    setattr(lifted_with_typing, "_primitive", primitive)

    return cast(Callable[[TTerm[A]], TTerm[B]], lifted_with_typing)
