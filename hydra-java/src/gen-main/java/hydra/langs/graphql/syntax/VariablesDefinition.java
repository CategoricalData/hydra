// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class VariablesDefinition implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.VariablesDefinition");
  
  public final hydra.langs.graphql.syntax.Variable variable;
  
  public final hydra.langs.graphql.syntax.Type type;
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.DefaultValue> defaultValue;
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives;
  
  public VariablesDefinition (hydra.langs.graphql.syntax.Variable variable, hydra.langs.graphql.syntax.Type type, hydra.util.Opt<hydra.langs.graphql.syntax.DefaultValue> defaultValue, hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives) {
    if (variable == null) {
      throw new IllegalArgumentException("null value for 'variable' argument");
    }
    if (type == null) {
      throw new IllegalArgumentException("null value for 'type' argument");
    }
    if (defaultValue == null) {
      throw new IllegalArgumentException("null value for 'defaultValue' argument");
    }
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
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
  
  public VariablesDefinition withVariable(hydra.langs.graphql.syntax.Variable variable) {
    if (variable == null) {
      throw new IllegalArgumentException("null value for 'variable' argument");
    }
    return new VariablesDefinition(variable, type, defaultValue, directives);
  }
  
  public VariablesDefinition withType(hydra.langs.graphql.syntax.Type type) {
    if (type == null) {
      throw new IllegalArgumentException("null value for 'type' argument");
    }
    return new VariablesDefinition(variable, type, defaultValue, directives);
  }
  
  public VariablesDefinition withDefaultValue(hydra.util.Opt<hydra.langs.graphql.syntax.DefaultValue> defaultValue) {
    if (defaultValue == null) {
      throw new IllegalArgumentException("null value for 'defaultValue' argument");
    }
    return new VariablesDefinition(variable, type, defaultValue, directives);
  }
  
  public VariablesDefinition withDirectives(hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives) {
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
    return new VariablesDefinition(variable, type, defaultValue, directives);
  }
}