package hydra.langs.json;

import hydra.json.Value;
import java.util.Collections;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;


/**
 * Decoding functions for Hydra's native JSON values which are intended for use in Java.
 * Decoding failures result in Java exceptions rather than failure flows.
 */
public abstract class JsonDecoding {
    /**
     * Decode a list from JSON.
     */
    public static <A> List<A> decodeList(Function<Value, A> mapping, Value json) {
        return json.accept(new Value.PartialVisitor<List<A>>() {
            @Override
            public List<A> otherwise(Value instance) {
                throw unexpected("array", instance);
            }

            @Override
            public List<A> visit(Value.Array instance) {
                return instance.value.stream().map(mapping).collect(Collectors.toList());
            }

            @Override
            public List<A> visit(Value.Null instance) {
                return Collections.emptyList();
            }
        });
    }

    /**
     * Decode a boolean value from JSON.
     */
    public static boolean decodeBoolean(Value json) {
        return json.accept(new Value.PartialVisitor<Boolean>() {
            @Override
            public Boolean otherwise(Value instance) {
                throw unexpected("boolean", instance);
            }

            @Override
            public Boolean visit(Value.Boolean_ instance) {
                return instance.value;
            }
        });
    }

    /**
     * Decode a double value from JSON.
     */
    public static double decodeDouble(Value json) {
        Number num = decodeNumber(json);
        return num.doubleValue();
    }

    /**
     * Decode an enumerated value from JSON.
     */
    public static <A> A decodeEnum(Map<String, A> values, Value json) {
        String key = decodeString(json);
        A value = values.get(key);
        if (value == null) {
            throw new RuntimeException("no such enum value: " + key);
        } else {
            return value;
        }
    }

    /**
     * Decode a float value from JSON.
     */
    public static float decodeFloat(Value json) {
        Number num = decodeNumber(json);
        return num.floatValue();
    }

    /**
     * Decode an integer value from JSON.
     */
    public static int decodeInteger(Value json) {
        Number num = decodeNumber(json);
        return num.intValue();
    }

    /**
     * Decode a list field from JSON.
     */
    public static <A> List<A> decodeListField(String name, Function<Value, A> mapping, Value json) {
        // Note: this allows the field to be omitted, and also allows a null value, both resulting in an empty list
        Optional<List<A>> opt = decodeOptionalField(name, v -> decodeList(mapping, v), json, Collections.emptyList());
        return opt.orElse(Collections.emptyList());
    }

    /**
     * Decode a number from JSON.
     */
    public static double decodeNumber(Value json) {
        return json.accept(new Value.PartialVisitor<Double>() {
            @Override
            public Double otherwise(Value instance) {
                throw unexpected("number", instance);
            }

            @Override
            public Double visit(Value.Number_ instance) {
                return instance.value;
            }
        });
    }

    /**
     * Decode an object (key/value map) from JSON.
     */
    public static Map<String, Value> decodeObject(Value json) {
        return json.accept(new Value.PartialVisitor<Map<String, Value>>() {
            @Override
            public Map<String, Value> otherwise(Value instance) {
                throw unexpected("object", instance);
            }

            @Override
            public Map<String, Value> visit(Value.Object_ instance) {
                return instance.value;
            }
        });
    }

    /**
     * Decode an optional double-valued field from JSON.
     */
    public static Optional<Double> decodeOptionalDoubleField(String name, Value json) {
        return decodeOptionalField(name, JsonDecoding::decodeDouble, json, null);
    }

    /**
     * Decode an optional field from JSON.
     */
    public static <A> Optional<A> decodeOptionalField(String name,
                                                      Function<Value, A> mapping,
                                                      Value json,
                                                      A defaultValue) {
        Map<String, Value> map = decodeObject(json);
        Value fieldValue = map.get(name);
        if (fieldValue == null) {
            return defaultValue == null ? Optional.empty() : Optional.of(defaultValue);
        } else {
            return fieldValue.accept(new Value.PartialVisitor<Optional<A>>() {
                @Override
                public Optional<A> otherwise(Value instance) {
                    return Optional.of(mapping.apply(fieldValue));
                }

                @Override
                public Optional<A> visit(Value.Null instance) {
                    return defaultValue == null ? Optional.empty() : Optional.of(defaultValue);
                }
            });
        }
    }

    /**
     * Decode an optional field from JSON.
     */
    public static <A> Optional<A> decodeOptionalField(String name, Function<Value, A> mapping, Value json) {
        return decodeOptionalField(name, mapping, json, null);
    }

    /**
     * Decode an optional integer-valued field from JSON.
     */
    public static Optional<Integer> decodeOptionalIntegerField(String name, Value json) {
        return decodeOptionalField(name, JsonDecoding::decodeInteger, json, null);
    }

    /**
     * Decode an optional set-valued field from JSON.
     */
    public static <A> Optional<Set<A>> decodeOptionalSetField(String name, Function<Value, A> mapping, Value json) {
        return decodeOptionalField(name, v -> decodeSet(mapping, v), json);
    }

    /**
     * Decode a required boolean-valued field from JSON.
     */
    public static boolean decodeRequiredBooleanField(String name, Value json) {
        return decodeRequiredField(name, JsonDecoding::decodeBoolean, json);
    }

    /**
     * Decode a required field from JSON.
     */
    public static <A> A decodeRequiredField(String name, Function<Value, A> mapping, Value json, A defaultValue) {
        Optional<A> opt = decodeOptionalField(name, mapping, json);
        if (opt.isPresent()) {
            return opt.get();
        } else {
            throw new RuntimeException("missing required field \"" + name + "\"");
        }
    }

    /**
     * Decode a required field from JSON.
     */
    public static <A> A decodeRequiredField(String name, Function<Value, A> mapping, Value json) {
        return decodeRequiredField(name, mapping, json, null);
    }

    /**
     * Decode a required int32-valued field from JSON.
     */
    public static int decodeRequiredIntField(String name, Value json) {
        return decodeRequiredField(name, JsonDecoding::decodeInteger, json);
    }

    /**
     * Decode a required list-valued field from JSON.
     */
    public static <A> List<A> decodeRequiredListField(String name, Function<Value, A> mapping, Value json) {
        return decodeRequiredField(name, v -> decodeList(mapping, v), json, Collections.emptyList());
    }

    /**
     * Decode a set from JSON.
     */
    public static <A> Set<A> decodeSet(Function<Value, A> mapping, Value json) {
        return new HashSet<>(decodeList(mapping, json));
    }

    /**
     * Decode a string value from JSON.
     */
    public static String decodeString(Value json) {
        return json.accept(new Value.PartialVisitor<String>() {
            @Override
            public String otherwise(Value instance) {
                throw unexpected("string", instance);
            }

            @Override
            public String visit(Value.String_ instance) {
                return instance.value;
            }
        });
    }

    /**
     * Decode a list of string values from JSON.
     */
    public static List<String> decodeStringList(Value json) {
        return decodeList(JsonDecoding::decodeString, json);
    }

    /**
     * Decode a union (injection) from JSON.
     */
    public static <A> A decodeUnion(Map<String, Function<Value, A>> mappings, Value json) {
        return json.accept(new Value.PartialVisitor<A>() {
            @Override
            public A otherwise(Value instance) {
                throw unexpected("string or object", instance);
            }

            @Override
            public A visit(Value.Object_ instance) {
                Map<String, Value> map = instance.value;
                if (map.size() != 1) {
                    throw new RuntimeException("expected union, found object with " + map.size() + " fields");
                }
                Map.Entry<String, Value> entry = map.entrySet().iterator().next();
                Function<Value, A> mapping = mappings.get(entry.getKey());
                if (mapping == null) {
                    throw new RuntimeException("unexpected union value: " + entry.getKey());
                } else {
                    return mapping.apply(entry.getValue());
                }
            }

            @Override
            public A visit(Value.String_ instance) {
                Function<Value, A> mapping = mappings.get(instance.value);
                if (mapping == null) {
                    throw new RuntimeException("unexpected union value: " + instance.value);
                } else {
                    return mapping.apply(new Value.Null());
                }
            }
        });
    }

    /**
     * Fail on an unexpected JSON value.
     */
    public static RuntimeException unexpected(String expected, Value actual) {
        return new RuntimeException("expected " + expected + ", found " + actual);
    }
}
