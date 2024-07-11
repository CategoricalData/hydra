package hydra.langs.json;

import hydra.json.Value;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
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
    return new Value.Number_(d);
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

  protected static Value variant(String key, Value value) {
    return new ObjectBuilder().put(key, value).build();
  }

  public static class ObjectBuilder {
    private final Map<String, Value> map = new HashMap<>();

    public ObjectBuilder put(String key, String value) {
      return put(key, new Value.String_(value));
    }

    public ObjectBuilder put(String key, Value value) {
      if (value != null) {
        map.put(key, value);
      }
      return this;
    }

    public <L> ObjectBuilder put(String key, L value, Function<L, Value> mapping) {
      return put(key, mapping.apply(value));
    }

    public <L> ObjectBuilder putList(String key, List<L> value, Function<L, Value> mapping) {
      return put(key, toJson(value, mapping));
    }

    public <L> ObjectBuilder putOpt(String key, Opt<L> value, Function<L, Value> mapping) {
      return put(key, toJson(value, mapping));
    }

    public <L> ObjectBuilder putOptSet(String key, Opt<Set<L>> values, Function<L, Value> mapping) {
      return put(key, toJson(new ArrayList<>(values.orElse(Collections.emptySet())), mapping));
    }

    public Value build() {
      return new Value.Object_(map);
    }
  }
}
