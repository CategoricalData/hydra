package hydra.dsl.meta.examples;

import hydra.core.Binding;
import hydra.core.Name;
import hydra.core.Type;
import hydra.dsl.Types;
import hydra.module.Module;
import hydra.module.Namespace;
import hydra.util.Maybe;

import java.util.Arrays;
import java.util.Collections;
import java.util.List;

import static hydra.dsl.Core.name;

/**
 * Complete type-level definitions for hydra.core, using the Java Types DSL.
 *
 * <p>This mirrors the Haskell source at:
 * {@code hydra-haskell/src/main/haskell/Hydra/Sources/Kernel/Types/Core.hs}
 *
 * <p>Types reference each other directly via {@link Types#field(String, Binding)}
 * and {@link Types#list(Binding)}, mirroring Haskell's {@code AsType Binding} typeclass.
 * Definitions are ordered so that each type is defined before it is referenced.
 * For the mutually recursive {@code term}/{@code type} pair, forward references are used.
 */
public interface CoreTypes {

    Namespace ns = new Namespace("hydra.core");

    /**
     * Define a type in the hydra.core namespace.
     */
    static Binding define(String localName, Type type) {
        return hydra.annotations.Annotations.typeElement(name(ns.value + "." + localName), type);
    }

    // =========================================================================
    // Forward references — only for the mutually recursive term/type pair
    // =========================================================================

    Type _Term = Types.variable(ns.value + ".Term");
    Type _Type = Types.variable(ns.value + ".Type");

    // =========================================================================
    // Type definitions — ordered so each type is defined before use
    // =========================================================================

    /**
     * A unique identifier; a string-valued key.
     */
    Binding name = define("Name",
        Types.wrap(Types.string()));

    /**
     * A floating-point type.
     */
    Binding floatType = define("FloatType",
        Types.union(
            Types.field("bigfloat", Types.unit()),
            Types.field("float32", Types.unit()),
            Types.field("float64", Types.unit())));

    /**
     * A floating-point literal value.
     */
    Binding floatValue = define("FloatValue",
        Types.union(
            Types.field("bigfloat", Types.bigfloat()),
            Types.field("float32", Types.float32()),
            Types.field("float64", Types.float64())));

    /**
     * An integer type.
     */
    Binding integerType = define("IntegerType",
        Types.union(
            Types.field("bigint", Types.unit()),
            Types.field("int8", Types.unit()),
            Types.field("int16", Types.unit()),
            Types.field("int32", Types.unit()),
            Types.field("int64", Types.unit()),
            Types.field("uint8", Types.unit()),
            Types.field("uint16", Types.unit()),
            Types.field("uint32", Types.unit()),
            Types.field("uint64", Types.unit())));

    /**
     * An integer literal value.
     */
    Binding integerValue = define("IntegerValue",
        Types.union(
            Types.field("bigint", Types.bigint()),
            Types.field("int8", Types.int8()),
            Types.field("int16", Types.int16()),
            Types.field("int32", Types.int32()),
            Types.field("int64", Types.int64()),
            Types.field("uint8", Types.uint8()),
            Types.field("uint16", Types.uint16()),
            Types.field("uint32", Types.uint32()),
            Types.field("uint64", Types.uint64())));

    /**
     * Any of a fixed set of literal types.
     */
    Binding literalType = define("LiteralType",
        Types.union(
            Types.field("binary", Types.unit()),
            Types.field("boolean", Types.unit()),
            Types.field("float", floatType),
            Types.field("integer", integerType),
            Types.field("string", Types.unit())));

    /**
     * A term constant; an instance of a literal type.
     */
    Binding literal = define("Literal",
        Types.union(
            Types.field("binary", Types.binary()),
            Types.field("boolean", Types.boolean_()),
            Types.field("float", floatValue),
            Types.field("integer", integerValue),
            Types.field("string", Types.string())));

    /**
     * A name/term pair.
     */
    Binding field = define("Field",
        Types.record(
            Types.field("name", name),
            Types.field("term", _Term)));

    /**
     * A name/type pair.
     */
    Binding fieldType = define("FieldType",
        Types.record(
            Types.field("name", name),
            Types.field("type", _Type)));

    /**
     * A record elimination; a projection.
     */
    Binding projection = define("Projection",
        Types.record(
            Types.field("typeName", name),
            Types.field("field", name)));

    /**
     * A union elimination; a case statement.
     */
    Binding caseStatement = define("CaseStatement",
        Types.record(
            Types.field("typeName", name),
            Types.field("default", Types.optional(_Term)),
            Types.field("cases", Types.list(field))));

    /**
     * A corresponding elimination for an introduction term.
     */
    Binding elimination = define("Elimination",
        Types.union(
            Types.field("record", projection),
            Types.field("union", caseStatement),
            Types.field("wrap", name)));

    /**
     * A function abstraction (lambda).
     */
    Binding lambda = define("Lambda",
        Types.record(
            Types.field("parameter", name),
            Types.field("domain", Types.optional(_Type)),
            Types.field("body", _Term)));

    /**
     * A function.
     */
    Binding function = define("Function",
        Types.union(
            Types.field("elimination", elimination),
            Types.field("lambda", lambda),
            Types.field("primitive", name)));

    /**
     * Metadata associated with a type variable, including typeclass constraints.
     */
    Binding typeVariableMetadata = define("TypeVariableMetadata",
        Types.record(
            Types.field("classes", Types.set(name))));

    /**
     * A type expression together with free type variables.
     */
    Binding typeScheme = define("TypeScheme",
        Types.record(
            Types.field("variables", Types.list(name)),
            Types.field("type", _Type),
            Types.field("constraints",
                Types.optional(Types.map(name, typeVariableMetadata)))));

    /**
     * A field with an optional type scheme, used to bind variables in a 'let' expression.
     */
    Binding binding = define("Binding",
        Types.record(
            Types.field("name", name),
            Types.field("term", _Term),
            Types.field("type", Types.optional(typeScheme))));

    /**
     * A set of (possibly recursive) 'let' bindings together with a body.
     */
    Binding let = define("Let",
        Types.record(
            Types.field("bindings", Types.list(binding)),
            Types.field("body", _Term)));

    /**
     * A term which applies a function to an argument.
     */
    Binding application = define("Application",
        Types.record(
            Types.field("function", _Term),
            Types.field("argument", _Term)));

    /**
     * An instance of a union type.
     */
    Binding injection = define("Injection",
        Types.record(
            Types.field("typeName", name),
            Types.field("field", field)));

    /**
     * A record: a map of field names to terms.
     */
    Binding record = define("Record",
        Types.record(
            Types.field("typeName", name),
            Types.field("fields", Types.list(field))));

    /**
     * A term together with an annotation.
     */
    Binding annotatedTerm = define("AnnotatedTerm",
        Types.record(
            Types.field("body", _Term),
            Types.field("annotation", Types.map(name, _Term))));

    /**
     * A term applied to a type; a type application.
     */
    Binding typeApplicationTerm = define("TypeApplicationTerm",
        Types.record(
            Types.field("body", _Term),
            Types.field("type", _Type)));

    /**
     * A System F type abstraction term.
     */
    Binding typeLambda = define("TypeLambda",
        Types.record(
            Types.field("parameter", name),
            Types.field("body", _Term)));

    /**
     * A term wrapped in a type name.
     */
    Binding wrappedTerm = define("WrappedTerm",
        Types.record(
            Types.field("typeName", name),
            Types.field("body", _Term)));

    /**
     * A data term.
     */
    Binding term = define("Term",
        Types.union(
            Types.field("annotated", annotatedTerm),
            Types.field("application", application),
            Types.field("either", Types.either_(_Term, _Term)),
            Types.field("function", function),
            Types.field("let", let),
            Types.field("list", Types.list(_Term)),
            Types.field("literal", literal),
            Types.field("map", Types.map(_Term, _Term)),
            Types.field("maybe", Types.optional(_Term)),
            Types.field("pair", Types.pair(_Term, _Term)),
            Types.field("record", record),
            Types.field("set", Types.set(_Term)),
            Types.field("typeApplication", typeApplicationTerm),
            Types.field("typeLambda", typeLambda),
            Types.field("union", injection),
            Types.field("unit", Types.unit()),
            Types.field("variable", name),
            Types.field("wrap", wrappedTerm)));

    /**
     * A labeled record or union type.
     */
    Binding rowType = define("RowType",
        Types.record(
            Types.field("typeName", name),
            Types.field("fields", Types.list(fieldType))));

    /**
     * A type together with an annotation.
     */
    Binding annotatedType = define("AnnotatedType",
        Types.record(
            Types.field("body", _Type),
            Types.field("annotation", Types.map(name, _Term))));

    /**
     * The type-level analog of an application term.
     */
    Binding applicationType = define("ApplicationType",
        Types.record(
            Types.field("function", _Type),
            Types.field("argument", _Type)));

    /**
     * A type providing a choice between a 'left' type and a 'right' type.
     */
    Binding eitherType = define("EitherType",
        Types.record(
            Types.field("left", _Type),
            Types.field("right", _Type)));

    /**
     * A type pairing a 'first' type and a 'second' type.
     */
    Binding pairType = define("PairType",
        Types.record(
            Types.field("first", _Type),
            Types.field("second", _Type)));

    /**
     * A universally quantified type.
     */
    Binding forallType = define("ForallType",
        Types.record(
            Types.field("parameter", name),
            Types.field("body", _Type)));

    /**
     * A function type, also known as an arrow type.
     */
    Binding functionType = define("FunctionType",
        Types.record(
            Types.field("domain", _Type),
            Types.field("codomain", _Type)));

    /**
     * A map type.
     */
    Binding mapType = define("MapType",
        Types.record(
            Types.field("keys", _Type),
            Types.field("values", _Type)));

    /**
     * A type wrapped in a type name; a newtype.
     */
    Binding wrappedType = define("WrappedType",
        Types.record(
            Types.field("typeName", name),
            Types.field("body", _Type)));

    /**
     * A data type.
     */
    Binding type = define("Type",
        Types.union(
            Types.field("annotated", annotatedType),
            Types.field("application", applicationType),
            Types.field("either", eitherType),
            Types.field("forall", forallType),
            Types.field("function", functionType),
            Types.field("list", _Type),
            Types.field("literal", literalType),
            Types.field("map", mapType),
            Types.field("maybe", _Type),
            Types.field("pair", pairType),
            Types.field("record", rowType),
            Types.field("set", _Type),
            Types.field("union", rowType),
            Types.field("unit", Types.unit()),
            Types.field("variable", name),
            Types.field("wrap", wrappedType)));

    // =========================================================================
    // Module definition
    // =========================================================================

    /**
     * All type definitions in this module.
     */
    List<Binding> elements = Arrays.asList(
        annotatedTerm,
        annotatedType,
        application,
        applicationType,
        binding,
        caseStatement,
        eitherType,
        pairType,
        elimination,
        field,
        fieldType,
        floatType,
        floatValue,
        forallType,
        function,
        functionType,
        injection,
        integerType,
        integerValue,
        lambda,
        let,
        literal,
        literalType,
        mapType,
        name,
        projection,
        record,
        rowType,
        term,
        type,
        typeApplicationTerm,
        typeLambda,
        typeScheme,
        typeVariableMetadata,
        wrappedTerm,
        wrappedType);

    /**
     * The hydra.core module, containing all core type definitions.
     *
     * <p>Note: hydra.core uniquely takes itself as a type-level dependency.
     */
    Module module = new Module(
        ns,
        hydra.util.ConsList.fromList(elements),
        hydra.util.ConsList.empty(),
        hydra.util.ConsList.of(ns),
        Maybe.just("Hydra's core data model, consisting of the fundamental "
            + "hydra.core.Term type and all of its dependencies."));

}
