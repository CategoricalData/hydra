package hydra.ext.json;

import hydra.core.Name;
import hydra.json.Value;
import java.util.ArrayList;
import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;
import hydra.util.Opt;
import java.util.Set;
import java.util.function.Function;
import java.util.stream.Collectors;


/**
 * Encoding functions for Hydra's native JSON values
 */
public abstract class JsonEncoding {

  protected static Value toJson(boolean b) {
    return new Value.Boolean_(b);
  }

  protected static Value toJson(double d) {
    return new Value.Number_(String.valueOf(d));
  }

  protected static Value toJson(int i) {
    return new Value.Number_(String.valueOf(i));
  }

  protected static Value toJson(String value) {
    return new Value.String_(value);
  }

  protected static <L> Value toJson(List<L> list, Function<L, Value> mapping) {
    return new Value.Array(list.stream().map(mapping).collect(Collectors.toList()));
  }

  protected static <L> Value toJson(Opt<L> opt, Function<L, Value> mapping) {
    return opt.map(mapping).orElse(null);
  }

  protected static Value toJson(List<String> strings) {
    return toJson(strings, JsonEncoding::toJson);
  }

  protected static Value unit() {
    return new Value.Object_(Collections.emptyMap());
  }

  protected static Value unitVariant(String key) {
    return variant(key, unit());
  }

  protected static Value unitVariant(Name key) {
    return variant(key.value, unit());
  }

  protected static Value variant(String key, Value value) {
    return new ObjectBuilder().put(key, value).build();
  }

  protected static Value variant(Name key, Value value) {
    return new ObjectBuilder().put(key.value, value).build();
  }

  public static class ObjectBuilder {
    // Note: use LinkedHashMap for the sake of predictable ordering
    private final Map<String, Value> map = new LinkedHashMap<>();

    public ObjectBuilder put(String key, boolean value) {
      return put(key, toJson(value));
    }

    public ObjectBuilder put(Name key, boolean value) {
      return put(key.value, value);
    }

    public ObjectBuilder put(String key, double value) {
      return put(key, toJson(value));
    }

    public ObjectBuilder put(Name key, double value) {
      return put(key.value, value);
    }

    public ObjectBuilder put(String key, int value) {
      return put(key, toJson(value));
    }

    public ObjectBuilder put(Name key, int value) {
      return put(key.value, value);
    }

    public ObjectBuilder put(String key, String value) {
      return put(key, toJson(value));
    }

    public ObjectBuilder put(Name key, String value) {
      return put(key.value, value);
    }

    public ObjectBuilder put(String key, Value value) {
      if (value != null) {
        map.put(key, value);
      }
      return this;
    }

    public ObjectBuilder put(Name key, Value value) {
      return put(key.value, value);
    }

    public <L> ObjectBuilder put(String key, L value, Function<L, Value> mapping) {
      return put(key, mapping.apply(value));
    }

    public <L> ObjectBuilder put(Name key, L value, Function<L, Value> mapping) {
      return put(key.value, value, mapping);
    }

    public <L> ObjectBuilder putList(String key, List<L> value, Function<L, Value> mapping) {
      return put(key, toJson(value, mapping));
    }

    public <L> ObjectBuilder putList(Name key, List<L> value, Function<L, Value> mapping) {
      return putList(key.value, value, mapping);
    }

    public ObjectBuilder putOpt(String key, Opt<Boolean> value) {
      return putOpt(key, value, JsonEncoding::toJson);
    }

    public ObjectBuilder putOpt(Name key, Opt<Boolean> value) {
      return putOpt(key, value, JsonEncoding::toJson);
    }

    public <L> ObjectBuilder putOpt(String key, Opt<L> value, Function<L, Value> mapping) {
      return put(key, toJson(value, mapping));
    }

    public <L> ObjectBuilder putOpt(Name key, Opt<L> value, Function<L, Value> mapping) {
      return putOpt(key.value, value, mapping);
    }

    public <L> ObjectBuilder putOptSet(String key, Opt<Set<L>> values, Function<L, Value> mapping) {
      return put(key, toJson(new ArrayList<>(values.orElse(Collections.emptySet())), mapping));
    }

    public <L> ObjectBuilder putOptSet(Name key, Opt<Set<L>> values, Function<L, Value> mapping) {
      return putOptSet(key.value, values, mapping);
    }

    public Value build() {
      return new Value.Object_(map);
    }
  }
}
