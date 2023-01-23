package hydra.ext.graphql.syntax;

public class FieldsDefinition {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.FieldsDefinition");
  
  public final java.util.List<hydra.ext.graphql.syntax.FieldDefinition> value;
  
  public FieldsDefinition (java.util.List<hydra.ext.graphql.syntax.FieldDefinition> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FieldsDefinition)) {
      return false;
    }
    FieldsDefinition o = (FieldsDefinition) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}