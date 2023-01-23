package hydra.ext.graphql.syntax;

public class EnumValuesDefinition {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.EnumValuesDefinition");
  
  public final java.util.List<hydra.ext.graphql.syntax.EnumValueDefinition> value;
  
  public EnumValuesDefinition (java.util.List<hydra.ext.graphql.syntax.EnumValueDefinition> value) {
    this.value = value;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EnumValuesDefinition)) {
      return false;
    }
    EnumValuesDefinition o = (EnumValuesDefinition) (other);
    return value.equals(o.value);
  }
  
  @Override
  public int hashCode() {
    return 2 * value.hashCode();
  }
}