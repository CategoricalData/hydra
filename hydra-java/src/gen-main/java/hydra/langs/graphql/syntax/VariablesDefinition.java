// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class VariablesDefinition implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/graphql/syntax.VariablesDefinition");
  
  public static final hydra.core.Name FIELD_NAME_VARIABLE = new hydra.core.Name("variable");
  
  public static final hydra.core.Name FIELD_NAME_TYPE = new hydra.core.Name("type");
  
  public static final hydra.core.Name FIELD_NAME_DEFAULT_VALUE = new hydra.core.Name("defaultValue");
  
  public static final hydra.core.Name FIELD_NAME_DIRECTIVES = new hydra.core.Name("directives");
  
  public final hydra.langs.graphql.syntax.Variable variable;
  
  public final hydra.langs.graphql.syntax.Type type;
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.DefaultValue> defaultValue;
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives;
  
  public VariablesDefinition (hydra.langs.graphql.syntax.Variable variable, hydra.langs.graphql.syntax.Type type, hydra.util.Opt<hydra.langs.graphql.syntax.DefaultValue> defaultValue, hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives) {
    java.util.Objects.requireNonNull((variable));
    java.util.Objects.requireNonNull((type));
    java.util.Objects.requireNonNull((defaultValue));
    java.util.Objects.requireNonNull((directives));
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
    java.util.Objects.requireNonNull((variable));
    return new VariablesDefinition(variable, type, defaultValue, directives);
  }
  
  public VariablesDefinition withType(hydra.langs.graphql.syntax.Type type) {
    java.util.Objects.requireNonNull((type));
    return new VariablesDefinition(variable, type, defaultValue, directives);
  }
  
  public VariablesDefinition withDefaultValue(hydra.util.Opt<hydra.langs.graphql.syntax.DefaultValue> defaultValue) {
    java.util.Objects.requireNonNull((defaultValue));
    return new VariablesDefinition(variable, type, defaultValue, directives);
  }
  
  public VariablesDefinition withDirectives(hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives) {
    java.util.Objects.requireNonNull((directives));
    return new VariablesDefinition(variable, type, defaultValue, directives);
  }
}