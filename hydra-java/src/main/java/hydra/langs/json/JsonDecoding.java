package hydra.langs.json;

import hydra.json.Value;
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
    });
  }

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

  public static double decodeDouble(Value json) {
    Number num = decodeNumber(json);
    return num.doubleValue();
  }

  public static float decodeFloat(Value json) {
    Number num = decodeNumber(json);
    return num.floatValue();
  }

  public static int decodeInteger(Value json) {
    Number num = decodeNumber(json);
    return num.intValue();
  }

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

  public static Optional<Double> decodeOptionalDoubleField(String name, Value json) {
    return decodeOptionalField(name, JsonDecoding::decodeDouble, json);
  }

  public static <A> Optional<A> decodeOptionalField(String name, Function<Value, A> mapping, Value json) {
    Map<String, Value> map = decodeObject(json);
    Value fieldValue = map.get(name);
    if (fieldValue == null) {
      return Optional.empty();
    } else {
      return Optional.of(mapping.apply(fieldValue));
    }
  }

  public static <A> Optional<Set<A>> decodeOptionalSetField(String name, Function<Value, A> mapping, Value json) {
    return decodeOptionalField(name, v -> decodeSet(mapping, v), json);
  }

  public static <A> A decodeRequiredField(String name, Function<Value, A> mapping, Value json) {
    Optional<A> opt = decodeOptionalField(name, mapping, json);
    if (opt.isPresent()) {
      return opt.get();
    } else {
      throw new RuntimeException("missing required field " + name);
    }
  }

  public static <A> List<A> decodeRequiredListField(String name, Function<Value, A> mapping, Value json) {
    return decodeRequiredField(name, v -> decodeList(mapping, v), json);
  }

  public static <A> Set<A> decodeSet(Function<Value, A> mapping, Value json) {
    return new HashSet<>(decodeList(mapping, json));
  }

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

  public static List<String> decodeStringList(Value json) {
    return decodeList(JsonDecoding::decodeString, json);
  }

  public static <A> A decodeEnum(Map<String, A> values, Value json) {
    Map<String, Value> map = decodeObject(json);
    if (map.size() != 1) {
      throw new RuntimeException("expected union, found object with " + map.size() + " fields");
    }
    Map.Entry<String, Value> entry = map.entrySet().iterator().next();
    A value = values.get(entry.getKey());
    if (value == null) {
      throw new RuntimeException("expected union, found object with unknown field " + entry.getKey());
    } else {
      return value;
    }
  }

  public static <A> A decodeUnion(Map<String, Function<Value, A>> mappings, Value json) {
    Map<String, Value> map = decodeObject(json);
    if (map.size() != 1) {
      throw new RuntimeException("expected union, found object with " + map.size() + " fields");
    }
    Map.Entry<String, Value> entry = map.entrySet().iterator().next();
    Function<Value, A> mapping = mappings.get(entry.getKey());
    if (mapping == null) {
      throw new RuntimeException("expected union, found object with unknown field " + entry.getKey());
    } else {
      return mapping.apply(entry.getValue());
    }
  }

  public static RuntimeException unexpected(String expected, Value actual) {
    return new RuntimeException("expected " + expected + ", found " + actual);
  }
}
