// Note: this is an automatically generated file. Do not edit.

package hydra.ext.graphql.syntax;

import java.io.Serializable;

public class InputObjectTypeExtension_Sequence2 implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/graphql/syntax.InputObjectTypeExtension.Sequence2");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_DIRECTIVES = new hydra.core.Name("directives");
  
  public final hydra.ext.graphql.syntax.Name name;
  
  public final hydra.ext.graphql.syntax.Directives directives;
  
  public InputObjectTypeExtension_Sequence2 (hydra.ext.graphql.syntax.Name name, hydra.ext.graphql.syntax.Directives directives) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((directives));
    this.name = name;
    this.directives = directives;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof InputObjectTypeExtension_Sequence2)) {
      return false;
    }
    InputObjectTypeExtension_Sequence2 o = (InputObjectTypeExtension_Sequence2) (other);
    return name.equals(o.name) && directives.equals(o.directives);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * directives.hashCode();
  }
  
  public InputObjectTypeExtension_Sequence2 withName(hydra.ext.graphql.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new InputObjectTypeExtension_Sequence2(name, directives);
  }
  
  public InputObjectTypeExtension_Sequence2 withDirectives(hydra.ext.graphql.syntax.Directives directives) {
    java.util.Objects.requireNonNull((directives));
    return new InputObjectTypeExtension_Sequence2(name, directives);
  }
}