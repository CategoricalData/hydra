// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class ScalarTypeDefinition implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/graphql/syntax.ScalarTypeDefinition");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_DIRECTIVES = new hydra.core.Name("directives");
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Description> description;
  
  public final hydra.langs.graphql.syntax.Name name;
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives;
  
  public ScalarTypeDefinition (hydra.util.Opt<hydra.langs.graphql.syntax.Description> description, hydra.langs.graphql.syntax.Name name, hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives) {
    java.util.Objects.requireNonNull((description));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((directives));
    this.description = description;
    this.name = name;
    this.directives = directives;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ScalarTypeDefinition)) {
      return false;
    }
    ScalarTypeDefinition o = (ScalarTypeDefinition) (other);
    return description.equals(o.description) && name.equals(o.name) && directives.equals(o.directives);
  }
  
  @Override
  public int hashCode() {
    return 2 * description.hashCode() + 3 * name.hashCode() + 5 * directives.hashCode();
  }
  
  public ScalarTypeDefinition withDescription(hydra.util.Opt<hydra.langs.graphql.syntax.Description> description) {
    java.util.Objects.requireNonNull((description));
    return new ScalarTypeDefinition(description, name, directives);
  }
  
  public ScalarTypeDefinition withName(hydra.langs.graphql.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new ScalarTypeDefinition(description, name, directives);
  }
  
  public ScalarTypeDefinition withDirectives(hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives) {
    java.util.Objects.requireNonNull((directives));
    return new ScalarTypeDefinition(description, name, directives);
  }
}