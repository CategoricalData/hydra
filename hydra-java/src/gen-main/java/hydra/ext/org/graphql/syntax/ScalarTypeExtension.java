// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.graphql.syntax;

import java.io.Serializable;

public class ScalarTypeExtension implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.graphql.syntax.ScalarTypeExtension");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_DIRECTIVES = new hydra.core.Name("directives");
  
  public final hydra.ext.org.graphql.syntax.Name name;
  
  public final hydra.ext.org.graphql.syntax.Directives directives;
  
  public ScalarTypeExtension (hydra.ext.org.graphql.syntax.Name name, hydra.ext.org.graphql.syntax.Directives directives) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((directives));
    this.name = name;
    this.directives = directives;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof ScalarTypeExtension)) {
      return false;
    }
    ScalarTypeExtension o = (ScalarTypeExtension) (other);
    return name.equals(o.name) && directives.equals(o.directives);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * directives.hashCode();
  }
  
  public ScalarTypeExtension withName(hydra.ext.org.graphql.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new ScalarTypeExtension(name, directives);
  }
  
  public ScalarTypeExtension withDirectives(hydra.ext.org.graphql.syntax.Directives directives) {
    java.util.Objects.requireNonNull((directives));
    return new ScalarTypeExtension(name, directives);
  }
}