"""Complete type-level definitions for hydra.core, using the Python Types DSL.

This mirrors the Haskell source at:
  hydra-haskell/src/main/haskell/Hydra/Sources/Kernel/Types/Core.hs

Types reference each other directly via T.field("name", name_binding) and
T.list_(field_binding), mirroring Haskell's AsType Binding typeclass.
Definitions are ordered so that each type is defined before it is referenced.
For the mutually recursive term/type pair, forward references are used.
"""

import hydra.annotations
import hydra.dsl.types as T
from hydra.core import Binding, Name
from hydra.module import Module, Namespace
from hydra.dsl.python import Maybe, Just, Nothing

ns = Namespace("hydra.core")


def define(local_name: str, typ) -> Binding:
    """Define a type in the hydra.core namespace."""
    return hydra.annotations.type_element(Name(ns.value + "." + local_name), typ)


# =========================================================================
# Forward references — only for the mutually recursive term/type pair
# =========================================================================

_Term = T.variable(ns.value + ".Term")
_Type = T.variable(ns.value + ".Type")

# =========================================================================
# Type definitions — ordered so each type is defined before use
# =========================================================================

# A unique identifier; a string-valued key.
name: Binding = define("Name",
    T.wrap(T.string()))

# A floating-point type.
float_type: Binding = define("FloatType",
    T.union([
        T.field("bigfloat", T.unit()),
        T.field("float32", T.unit()),
        T.field("float64", T.unit())]))

# A floating-point literal value.
float_value: Binding = define("FloatValue",
    T.union([
        T.field("bigfloat", T.bigfloat()),
        T.field("float32", T.float32()),
        T.field("float64", T.float64())]))

# An integer type.
integer_type: Binding = define("IntegerType",
    T.union([
        T.field("bigint", T.unit()),
        T.field("int8", T.unit()),
        T.field("int16", T.unit()),
        T.field("int32", T.unit()),
        T.field("int64", T.unit()),
        T.field("uint8", T.unit()),
        T.field("uint16", T.unit()),
        T.field("uint32", T.unit()),
        T.field("uint64", T.unit())]))

# An integer literal value.
integer_value: Binding = define("IntegerValue",
    T.union([
        T.field("bigint", T.bigint()),
        T.field("int8", T.int8()),
        T.field("int16", T.int16()),
        T.field("int32", T.int32()),
        T.field("int64", T.int64()),
        T.field("uint8", T.uint8()),
        T.field("uint16", T.uint16()),
        T.field("uint32", T.uint32()),
        T.field("uint64", T.uint64())]))

# Any of a fixed set of literal types.
literal_type: Binding = define("LiteralType",
    T.union([
        T.field("binary", T.unit()),
        T.field("boolean", T.unit()),
        T.field("float", float_type),
        T.field("integer", integer_type),
        T.field("string", T.unit())]))

# A term constant; an instance of a literal type.
literal: Binding = define("Literal",
    T.union([
        T.field("binary", T.binary()),
        T.field("boolean", T.boolean()),
        T.field("float", float_value),
        T.field("integer", integer_value),
        T.field("string", T.string())]))

# A name/term pair.
field: Binding = define("Field",
    T.record([
        T.field("name", name),
        T.field("term", _Term)]))

# A name/type pair.
field_type: Binding = define("FieldType",
    T.record([
        T.field("name", name),
        T.field("type", _Type)]))

# A record elimination; a projection.
projection: Binding = define("Projection",
    T.record([
        T.field("typeName", name),
        T.field("field", name)]))

# A union elimination; a case statement.
case_statement: Binding = define("CaseStatement",
    T.record([
        T.field("typeName", name),
        T.field("default", T.optional(_Term)),
        T.field("cases", T.list_(field))]))

# A corresponding elimination for an introduction term.
elimination: Binding = define("Elimination",
    T.union([
        T.field("record", projection),
        T.field("union", case_statement),
        T.field("wrap", name)]))

# A function abstraction (lambda).
lambda_: Binding = define("Lambda",
    T.record([
        T.field("parameter", name),
        T.field("domain", T.optional(_Type)),
        T.field("body", _Term)]))

# A function.
function: Binding = define("Function",
    T.union([
        T.field("elimination", elimination),
        T.field("lambda", lambda_),
        T.field("primitive", name)]))

# Metadata associated with a type variable, including typeclass constraints.
type_variable_metadata: Binding = define("TypeVariableMetadata",
    T.record([
        T.field("classes", T.set_(name))]))

# A type expression together with free type variables.
type_scheme: Binding = define("TypeScheme",
    T.record([
        T.field("variables", T.list_(name)),
        T.field("type", _Type),
        T.field("constraints",
            T.optional(T.map_(name, type_variable_metadata)))]))

# A field with an optional type scheme, used to bind variables in a 'let' expression.
binding: Binding = define("Binding",
    T.record([
        T.field("name", name),
        T.field("term", _Term),
        T.field("type", T.optional(type_scheme))]))

# A set of (possibly recursive) 'let' bindings together with a body.
let: Binding = define("Let",
    T.record([
        T.field("bindings", T.list_(binding)),
        T.field("body", _Term)]))

# A term which applies a function to an argument.
application: Binding = define("Application",
    T.record([
        T.field("function", _Term),
        T.field("argument", _Term)]))

# An instance of a union type.
injection: Binding = define("Injection",
    T.record([
        T.field("typeName", name),
        T.field("field", field)]))

# A record: a map of field names to terms.
record: Binding = define("Record",
    T.record([
        T.field("typeName", name),
        T.field("fields", T.list_(field))]))

# A term together with an annotation.
annotated_term: Binding = define("AnnotatedTerm",
    T.record([
        T.field("body", _Term),
        T.field("annotation", T.map_(name, _Term))]))

# A term applied to a type; a type application.
type_application_term: Binding = define("TypeApplicationTerm",
    T.record([
        T.field("body", _Term),
        T.field("type", _Type)]))

# A System F type abstraction term.
type_lambda: Binding = define("TypeLambda",
    T.record([
        T.field("parameter", name),
        T.field("body", _Term)]))

# A term wrapped in a type name.
wrapped_term: Binding = define("WrappedTerm",
    T.record([
        T.field("typeName", name),
        T.field("body", _Term)]))

# A data term.
term: Binding = define("Term",
    T.union([
        T.field("annotated", annotated_term),
        T.field("application", application),
        T.field("either", T.either(_Term, _Term)),
        T.field("function", function),
        T.field("let", let),
        T.field("list", T.list_(_Term)),
        T.field("literal", literal),
        T.field("map", T.map_(_Term, _Term)),
        T.field("maybe", T.optional(_Term)),
        T.field("pair", T.pair(_Term, _Term)),
        T.field("record", record),
        T.field("set", T.set_(_Term)),
        T.field("typeApplication", type_application_term),
        T.field("typeLambda", type_lambda),
        T.field("union", injection),
        T.field("unit", T.unit()),
        T.field("variable", name),
        T.field("wrap", wrapped_term)]))

# A type together with an annotation.
annotated_type: Binding = define("AnnotatedType",
    T.record([
        T.field("body", _Type),
        T.field("annotation", T.map_(name, _Term))]))

# The type-level analog of an application term.
application_type: Binding = define("ApplicationType",
    T.record([
        T.field("function", _Type),
        T.field("argument", _Type)]))

# A type providing a choice between a 'left' type and a 'right' type.
either_type: Binding = define("EitherType",
    T.record([
        T.field("left", _Type),
        T.field("right", _Type)]))

# A type pairing a 'first' type and a 'second' type.
pair_type: Binding = define("PairType",
    T.record([
        T.field("first", _Type),
        T.field("second", _Type)]))

# A universally quantified type.
forall_type: Binding = define("ForallType",
    T.record([
        T.field("parameter", name),
        T.field("body", _Type)]))

# A function type, also known as an arrow type.
function_type: Binding = define("FunctionType",
    T.record([
        T.field("domain", _Type),
        T.field("codomain", _Type)]))

# A map type.
map_type: Binding = define("MapType",
    T.record([
        T.field("keys", _Type),
        T.field("values", _Type)]))

# A data type.
type: Binding = define("Type",
    T.union([
        T.field("annotated", annotated_type),
        T.field("application", application_type),
        T.field("either", either_type),
        T.field("forall", forall_type),
        T.field("function", function_type),
        T.field("list", _Type),
        T.field("literal", literal_type),
        T.field("map", map_type),
        T.field("maybe", _Type),
        T.field("pair", pair_type),
        T.field("record", T.list_(field_type)),
        T.field("set", _Type),
        T.field("union", T.list_(field_type)),
        T.field("unit", T.unit()),
        T.field("variable", name),
        T.field("wrap", _Type)]))

# =========================================================================
# Module definition
# =========================================================================

elements = [
    annotated_term,
    annotated_type,
    application,
    application_type,
    binding,
    case_statement,
    either_type,
    pair_type,
    elimination,
    field,
    field_type,
    float_type,
    float_value,
    forall_type,
    function,
    function_type,
    injection,
    integer_type,
    integer_value,
    lambda_,
    let,
    literal,
    literal_type,
    map_type,
    name,
    projection,
    record,
    term,
    type,
    type_application_term,
    type_lambda,
    type_scheme,
    type_variable_metadata,
    wrapped_term]

# Note: hydra.core uniquely takes itself as a type-level dependency.
module = Module(
    ns,
    tuple(elements),
    (),
    (ns,),
    Just("Hydra's core data model, consisting of the fundamental "
         "hydra.core.Term type and all of its dependencies."))
