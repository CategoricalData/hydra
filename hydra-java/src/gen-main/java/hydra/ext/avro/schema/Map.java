package hydra.ext.avro.schema;

public class Map {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/avro/schema.Map");
  
  public final hydra.ext.avro.schema.Schema values;
  
  public Map (hydra.ext.avro.schema.Schema values) {
    this.values = values;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Map)) {
      return false;
    }
    Map o = (Map) (other);
    return values.equals(o.values);
  }
  
  @Override
  public int hashCode() {
    return 2 * values.hashCode();
  }
}