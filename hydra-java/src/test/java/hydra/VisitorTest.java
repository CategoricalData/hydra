package hydra;

import hydra.core.FieldType;
import hydra.core.FloatType;
import hydra.core.IntegerType;
import hydra.core.LiteralType;
import hydra.core.Name;
import hydra.core.RowType;
import hydra.core.Type;
import hydra.core.WrappedType;

import java.util.Arrays;
import java.util.stream.Collectors;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertFalse;
import static org.junit.jupiter.api.Assertions.assertTrue;


/**
 * This test illustrates constructing and visiting Hydra types using the generated visitor API.
 */
public class VisitorTest {
    // A few example types
    private final Type stringType = new Type.Literal(new LiteralType.String_());
    private final Type int32Type = new Type.Literal(new LiteralType.Integer_(new IntegerType.Int32()));
    private final Type float64Type = new Type.Literal(new LiteralType.Float_(new FloatType.Float64()));
    private final Type listOfStrings = new Type.List(stringType);
    private final Type maybeInt = new Type.Maybe(int32Type);
    private final Type recordType = new Type.Record(new RowType(
            new Name("LatLon"),
            hydra.util.ConsList.of(
                    new FieldType(new Name("lat"), float64Type),
                    new FieldType(new Name("lon"), float64Type))));

    // Example visitor: format a type as a human-readable string
    private final Type.Visitor<String> typeToString = new Type.Visitor<String>() {
        @Override
        public String visit(Type.Annotated instance) {
            return instance.value.body.accept(typeToString) + " (annotated)";
        }

        @Override
        public String visit(Type.Application instance) {
            return "apply(" + instance.value.function.accept(typeToString)
                    + ", " + instance.value.argument.accept(typeToString) + ")";
        }

        @Override
        public String visit(Type.Either instance) {
            return "either(" + instance.value.left.accept(typeToString)
                    + ", " + instance.value.right.accept(typeToString) + ")";
        }

        @Override
        public String visit(Type.Forall instance) {
            return "forall " + instance.value.parameter.value + ". " + instance.value.body.accept(typeToString);
        }

        @Override
        public String visit(Type.Function instance) {
            return instance.value.domain.accept(typeToString) + " -> " + instance.value.codomain.accept(typeToString);
        }

        @Override
        public String visit(Type.List instance) {
            return "[" + instance.value.accept(typeToString) + "]";
        }

        @Override
        public String visit(Type.Literal instance) {
            return instance.value.accept(new LiteralType.Visitor<String>() {
                @Override
                public String visit(LiteralType.Binary instance) {
                    return "binary";
                }

                @Override
                public String visit(LiteralType.Boolean_ instance) {
                    return "boolean";
                }

                @Override
                public String visit(LiteralType.Float_ instance) {
                    return "float64";
                }

                @Override
                public String visit(LiteralType.Integer_ instance) {
                    return "int32";
                }

                @Override
                public String visit(LiteralType.String_ instance) {
                    return "string";
                }
            });
        }

        @Override
        public String visit(Type.Map instance) {
            return "map(" + instance.value.keys.accept(typeToString)
                    + ", " + instance.value.values.accept(typeToString) + ")";
        }

        @Override
        public String visit(Type.Maybe instance) {
            return instance.value.accept(typeToString) + "?";
        }

        @Override
        public String visit(Type.Pair instance) {
            return "pair(" + instance.value.first.accept(typeToString)
                    + ", " + instance.value.second.accept(typeToString) + ")";
        }

        @Override
        public String visit(Type.Record instance) {
            String fields = instance.value.fields.stream()
                    .map(f -> f.name.value + ": " + f.type.accept(typeToString))
                    .collect(Collectors.joining(", "));
            return "{" + fields + "}";
        }

        @Override
        public String visit(Type.Set instance) {
            return "set(" + instance.value.accept(typeToString) + ")";
        }

        @Override
        public String visit(Type.Union instance) {
            String fields = instance.value.fields.stream()
                    .map(f -> f.name.value + ": " + f.type.accept(typeToString))
                    .collect(Collectors.joining(" | "));
            return "(" + fields + ")";
        }

        @Override
        public String visit(Type.Unit instance) {
            return "unit";
        }

        @Override
        public String visit(Type.Variable instance) {
            return instance.value.value;
        }

        @Override
        public String visit(Type.Wrap instance) {
            return "wrap(" + instance.value.typeName.value + ", " + instance.value.body.accept(typeToString) + ")";
        }
    };

    // Example partial visitor: check whether a type is a numeric literal type
    private final Type.PartialVisitor<Boolean> isNumericType = new Type.PartialVisitor<Boolean>() {
        @Override
        public Boolean visit(Type.Literal instance) {
            return instance.value.accept(new LiteralType.PartialVisitor<Boolean>() {
                @Override
                public Boolean visit(LiteralType.Float_ instance) {
                    return true;
                }

                @Override
                public Boolean visit(LiteralType.Integer_ instance) {
                    return true;
                }

                @Override
                public Boolean otherwise(LiteralType ignored) {
                    return false;
                }
            });
        }

        @Override
        public Boolean otherwise(Type instance) {
            return false;
        }
    };

    @Test
    public void demonstrateVisitor() {
        assertEquals("string", stringType.accept(typeToString));
        assertEquals("int32", int32Type.accept(typeToString));
        assertEquals("[string]", listOfStrings.accept(typeToString));
        assertEquals("int32?", maybeInt.accept(typeToString));
        assertEquals("{lat: float64, lon: float64}", recordType.accept(typeToString));
    }

    @Test
    public void demonstratePartialVisitor() {
        assertFalse(stringType.accept(isNumericType));
        assertTrue(int32Type.accept(isNumericType));
        assertTrue(float64Type.accept(isNumericType));
        assertFalse(listOfStrings.accept(isNumericType));
        assertFalse(recordType.accept(isNumericType));
    }
}
