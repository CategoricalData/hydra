package hydra.json;

import hydra.HydraTestBase;
import hydra.util.Unit;
import hydra.json.model.Value;
import org.junit.jupiter.api.Test;

import java.math.BigDecimal;
import java.util.Arrays;
import java.util.HashMap;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertTrue;
import static org.junit.jupiter.api.Assertions.fail;


public class JsonSerdeTest extends HydraTestBase {
    private static final String JSON_STRING_1 = "{"
            + "\"numVal\": 42, "
            + "\"boolVal\": true, "
            + "\"nullVal\": null, "
            + "\"objVal\": {\"foo\": \"bar\"}, "
            + "\"arrayval\": [1,2,3.0]}";

    private static final Value JSON_VALUE_0 = new Value.Object_(new HashMap<String, Value>() {{
        put("boolVal", new Value.Boolean_(true));
    }});

    private static final Value JSON_VALUE_1 = new Value.Object_(new HashMap<String, Value>() {{
        put("numVal", new Value.Number_(BigDecimal.valueOf(42.0)));
        put("boolVal", new Value.Boolean_(true));
        put("nullVal", new Value.Null(false));
        put("objVal", new Value.Object_(new HashMap<String, Value>() {{
            put("foo", new Value.String_("bar"));
        }}));
        put("arrayval", new Value.Array(Arrays.asList(new Value.Number_(BigDecimal.valueOf(1.0)),
                new Value.Number_(BigDecimal.valueOf(2.0)),
                new Value.Number_(BigDecimal.valueOf(3.0)))));
    }});

    @Test
    public void checkDecoding() {
        checkFlow(JsonSerde.decode(JSON_STRING_1), value -> value.accept(new Value.PartialVisitor<Void>() {
            @Override
            public Void otherwise(Value instance) {
                fail("not an object");
                return null;
            }

            @Override
            public Void visit(Value.Object_ instance) {
                Map<String, Value> m = instance.value;
                assertEquals(new Value.Number_(BigDecimal.valueOf(42.0)), m.get("numVal"));
                assertEquals(new Value.Boolean_(true), m.get("boolVal"));
                assertEquals(new Value.Null(false), m.get("nullVal"));
                assertTrue(m.get("objVal") instanceof Value.Object_);
                assertTrue(m.get("arrayval") instanceof Value.Array);
                assertEquals(3, ((Value.Array) m.get("arrayval")).value.size());
                return null;
            }
        }));
    }

    @Test
    public void checkEncoding() {
        assertSucceedsWith("\"foo\"", JsonSerde.encode(new Value.String_("foo")));
        assertSucceedsWith("{\"foo\":42.0}",
                JsonSerde.encode(new Value.Object_(new HashMap<String, Value>() {{
                    put("foo", new Value.Number_(BigDecimal.valueOf(42.0)));
                }})));

        // Raw values (other than null) are wrapped as objects by json-io
        assertSucceedsWith("{\"value\":42.0}", JsonSerde.encode(new Value.Number_(BigDecimal.valueOf(42.0))));
        assertSucceedsWith("{\"value\":true}", JsonSerde.encode(new Value.Boolean_(true)));

        // Raw nulls are *not* wrapped by json-io
        assertSucceedsWith("null", JsonSerde.encode(new Value.Null(false)));

        assertSucceedsWith("[\"foo\"]",
                JsonSerde.encode(new Value.Array(Arrays.asList(new Value.String_("foo")))));
    }

    @Test
    // Note: we can't easily check round trips from strings, as json-io serializes map entries in an arbitrary order
    public void checkRoundTripFromValue() {
        assertRoundTripIsNoop(new JsonSerde<Unit, Unit>(), JSON_VALUE_0);
        assertRoundTripIsNoop(new JsonSerde<Unit, Unit>(), JSON_VALUE_1);
    }

    @Test
    public void checkFailOnInvalidJsonString() {
        assertFails(JsonSerde.decode("{{"));
    }
}
