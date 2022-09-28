package hydra.ext.graphql.syntax;

public class VariablesDefinition {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.VariablesDefinition");
  
  public final hydra.ext.graphql.syntax.Variable variable;
  
  public final hydra.ext.graphql.syntax.Type type;
  
  public final java.util.Optional<hydra.ext.graphql.syntax.DefaultValue> defaultValue;
  
  public final java.util.Optional<hydra.ext.graphql.syntax.Directives> directives;
  
  public VariablesDefinition (hydra.ext.graphql.syntax.Variable variable, hydra.ext.graphql.syntax.Type type, java.util.Optional<hydra.ext.graphql.syntax.DefaultValue> defaultValue, java.util.Optional<hydra.ext.graphql.syntax.Directives> directives) {
    this.variable = variable;
    this.type = type;
    this.defaultValue = defaultValue;
    this.directives = directives;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof VariablesDefinition)) {
      return false;
    }
    VariablesDefinition o = (VariablesDefinition) (other);
    return variable.equals(o.variable) && type.equals(o.type) && defaultValue.equals(o.defaultValue) && directives.equals(o.directives);
  }
  
  @Override
  public int hashCode() {
    return 2 * variable.hashCode() + 3 * type.hashCode() + 5 * defaultValue.hashCode() + 7 * directives.hashCode();
  }
  
  public VariablesDefinition withVariable(hydra.ext.graphql.syntax.Variable variable) {
    return new VariablesDefinition(variable, type, defaultValue, directives);
  }
  
  public VariablesDefinition withType(hydra.ext.graphql.syntax.Type type) {
    return new VariablesDefinition(variable, type, defaultValue, directives);
  }
  
  public VariablesDefinition withDefaultValue(java.util.Optional<hydra.ext.graphql.syntax.DefaultValue> defaultValue) {
    return new VariablesDefinition(variable, type, defaultValue, directives);
  }
  
  public VariablesDefinition withDirectives(java.util.Optional<hydra.ext.graphql.syntax.Directives> directives) {
    return new VariablesDefinition(variable, type, defaultValue, directives);
  }
}