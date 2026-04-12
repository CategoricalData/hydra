"""A DSL which is used as a basis for some of the other DSLs."""

import hydra.constants
import hydra.dsl.terms as terms
import hydra.dsl.types as types
from hydra.core import Name, Term, Type
from hydra.dsl.python import Maybe, Just, Nothing


def annotate_term(key: Name, mvalue: Maybe[Term], term: Term) -> Term:
    """Add an annotation to a term."""
    return set_term_annotation(key, mvalue, term)


def annotate_type(key: Name, mvalue: Maybe[Term], typ: Type) -> Type:
    """Add an annotation to a type."""
    return set_type_annotation(key, mvalue, typ)


def bounded(min_len: Maybe[int], max_len: Maybe[int], typ: Type) -> Type:
    """Apply minimum and maximum length bounds to a type."""
    def annot_min(t: Type) -> Type:
        match min_len:
            case Nothing():
                return t
            case Just(m):
                return set_min_length(m, t)
    
    def annot_max(t: Type) -> Type:
        match max_len:
            case Nothing():
                return t
            case Just(m):
                return set_max_length(m, t)
    
    return annot_min(annot_max(typ))


def bounded_list(min_len: Maybe[int], max_len: Maybe[int], et: Type) -> Type:
    """Create a bounded list type."""
    return bounded(min_len, max_len, types.list_(et))


def bounded_map(min_len: Maybe[int], max_len: Maybe[int], kt: Type, vt: Type) -> Type:
    """Create a bounded map type."""
    return bounded(min_len, max_len, types.map_(kt, vt))


def bounded_set(min_len: Maybe[int], max_len: Maybe[int], et: Type) -> Type:
    """Create a bounded set type."""
    return bounded(min_len, max_len, types.set_(et))


def bounded_string(min_len: Maybe[int], max_len: Maybe[int]) -> Type:
    """Create a bounded string type."""
    return bounded(min_len, max_len, types.string())


def deprecated(typ: Type) -> Type:
    """Mark a type as deprecated."""
    return set_type_annotation(
        hydra.constants.key_deprecated,
        Just(terms.boolean(True)),
        typ
    )


def doc(s: str, typ: Type) -> Type:
    """Add documentation to a type."""
    return set_type_description(Just(s), typ)


def doc70(s: str, typ: Type) -> Type:
    """Add documentation with line wrapping at 70 characters."""
    return doc(wrap_line(70, s), typ)


def doc80(s: str, typ: Type) -> Type:
    """Add documentation with line wrapping at 80 characters."""
    return doc(wrap_line(80, s), typ)


def data_doc(s: str, term: Term) -> Term:
    """Add documentation to a term."""
    return set_term_description(Just(s), term)


def exclude(typ: Type) -> Type:
    """Mark a type to be excluded."""
    return set_type_annotation(
        hydra.constants.key_exclude,
        Just(terms.boolean(True)),
        typ
    )


def min_length_list(length: int, et: Type) -> Type:
    """Create a list type with minimum length."""
    return bounded_list(Just(length), Nothing(), et)


def nonempty_list(et: Type) -> Type:
    """Create a non-empty list type."""
    return min_length_list(1, et)


def nonempty_map(kt: Type, vt: Type) -> Type:
    """Create a non-empty map type."""
    return bounded_map(Just(1), Nothing(), kt, vt)


def note(s: str, typ: Type) -> Type:
    """Add a note to a type."""
    return doc(f"Note: {s}", typ)


def preserve_field_name(typ: Type) -> Type:
    """Mark a field name to be preserved."""
    return set_type_annotation(
        hydra.constants.key_preserve_field_name,
        Just(terms.boolean(True)),
        typ
    )


def see(s: str, typ: Type) -> Type:
    """Add a 'see' reference to a type."""
    return doc(f"See {s}", typ)


def set_max_length(m: int, typ: Type) -> Type:
    """Set the maximum length annotation."""
    return set_type_annotation(
        hydra.constants.key_max_length,
        Just(terms.int32(m)),
        typ
    )


def set_min_length(m: int, typ: Type) -> Type:
    """Set the minimum length annotation."""
    return set_type_annotation(
        hydra.constants.key_min_length,
        Just(terms.int32(m)),
        typ
    )


def two_or_more_list(et: Type) -> Type:
    """Create a list type with at least two elements."""
    return bounded_list(Just(2), Nothing(), et)


# Core annotation functions - these need to be implemented based on Hydra's annotation system

def set_term_annotation(key: Name, mvalue: Maybe[Term], term: Term) -> Term:
    """Set an annotation on a term."""
    # TODO: This needs to interact with Hydra's annotation system
    # For now, this is a placeholder that returns the term unchanged
    return term


def set_type_annotation(key: Name, mvalue: Maybe[Term], typ: Type) -> Type:
    """Set an annotation on a type."""
    # TODO: This needs to interact with Hydra's annotation system
    # For now, this is a placeholder that returns the type unchanged
    return typ


def set_term_description(desc: Maybe[str], term: Term) -> Term:
    """Set the description annotation on a term."""
    match desc:
        case Nothing():
            return term
        case Just(s):
            return set_term_annotation(
                hydra.constants.key_description,
                Just(terms.string(s)),
                term
            )


def set_type_description(desc: Maybe[str], typ: Type) -> Type:
    """Set the description annotation on a type."""
    match desc:
        case Nothing():
            return typ
        case Just(s):
            return set_type_annotation(
                hydra.constants.key_description,
                Just(terms.string(s)),
                typ
            )


def wrap_line(width: int, text: str) -> str:
    """Wrap a line of text at the specified width."""
    # Simple word-wrapping implementation
    words = text.split()
    if not words:
        return text
    
    lines = []
    current_line = []
    current_length = 0
    
    for word in words:
        word_length = len(word)
        # +1 for the space before the word (except for first word in line)
        needed_length = word_length + (1 if current_line else 0)
        
        if current_length + needed_length <= width:
            current_line.append(word)
            current_length += needed_length
        else:
            if current_line:
                lines.append(" ".join(current_line))
            current_line = [word]
            current_length = word_length
    
    if current_line:
        lines.append(" ".join(current_line))
    
    return "\n".join(lines)

