// Note: this is an automatically generated file. Do not edit.

package hydra.ext.graphql.syntax;

import java.io.Serializable;

public class InputObjectTypeDefinition_Sequence2 implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/graphql/syntax.InputObjectTypeDefinition.Sequence2");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_DIRECTIVES = new hydra.core.Name("directives");
  
  public final hydra.util.Opt<hydra.ext.graphql.syntax.Description> description;
  
  public final hydra.ext.graphql.syntax.Name name;
  
  public final hydra.util.Opt<hydra.ext.graphql.syntax.Directives> directives;
  
  public InputObjectTypeDefinition_Sequence2 (hydra.util.Opt<hydra.ext.graphql.syntax.Description> description, hydra.ext.graphql.syntax.Name name, hydra.util.Opt<hydra.ext.graphql.syntax.Directives> directives) {
    java.util.Objects.requireNonNull((description));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((directives));
    this.description = description;
    this.name = name;
    this.directives = directives;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InputObjectTypeDefinition_Sequence2)) {
      return false;
    }
    InputObjectTypeDefinition_Sequence2 o = (InputObjectTypeDefinition_Sequence2) (other);
    return description.equals(o.description) && name.equals(o.name) && directives.equals(o.directives);
  }
  
  @Override
  public int hashCode() {
    return 2 * description.hashCode() + 3 * name.hashCode() + 5 * directives.hashCode();
  }
  
  public InputObjectTypeDefinition_Sequence2 withDescription(hydra.util.Opt<hydra.ext.graphql.syntax.Description> description) {
    java.util.Objects.requireNonNull((description));
    return new InputObjectTypeDefinition_Sequence2(description, name, directives);
  }
  
  public InputObjectTypeDefinition_Sequence2 withName(hydra.ext.graphql.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new InputObjectTypeDefinition_Sequence2(description, name, directives);
  }
  
  public InputObjectTypeDefinition_Sequence2 withDirectives(hydra.util.Opt<hydra.ext.graphql.syntax.Directives> directives) {
    java.util.Objects.requireNonNull((directives));
    return new InputObjectTypeDefinition_Sequence2(description, name, directives);
  }
}
