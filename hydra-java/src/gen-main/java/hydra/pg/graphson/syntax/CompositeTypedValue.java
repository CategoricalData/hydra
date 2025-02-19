// Note: this is an automatically generated file. Do not edit.

package hydra.pg.graphson.syntax;

import java.io.Serializable;

public class CompositeTypedValue implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.pg.graphson.syntax.CompositeTypedValue");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_FIELDS = new hydra.core.Name("fields");
  
  public final hydra.pg.graphson.syntax.TypeName type;
  
  public final hydra.pg.graphson.syntax.Map fields;
  
  public CompositeTypedValue (hydra.pg.graphson.syntax.TypeName type, hydra.pg.graphson.syntax.Map fields) {
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((fields));
    this.type = type;
    this.fields = fields;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CompositeTypedValue)) {
      return false;
    }
    CompositeTypedValue o = (CompositeTypedValue) (other);
    return type.equals(o.type) && fields.equals(o.fields);
  }
  
  @Override
  public int hashCode() {
    return 2 * type.hashCode() + 3 * fields.hashCode();
  }
  
  public CompositeTypedValue withType(hydra.pg.graphson.syntax.TypeName type) {
    java.util.Objects.requireNonNull((type));
    return new CompositeTypedValue(type, fields);
  }
  
  public CompositeTypedValue withFields(hydra.pg.graphson.syntax.Map fields) {
    java.util.Objects.requireNonNull((fields));
    return new CompositeTypedValue(type, fields);
  }
}