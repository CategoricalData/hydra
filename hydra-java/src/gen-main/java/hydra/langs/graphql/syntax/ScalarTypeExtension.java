// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class ScalarTypeExtension implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.ScalarTypeExtension");
  
  public final hydra.langs.graphql.syntax.Name name;
  
  public final hydra.langs.graphql.syntax.Directives directives;
  
  public ScalarTypeExtension (hydra.langs.graphql.syntax.Name name, hydra.langs.graphql.syntax.Directives directives) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
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
  
  public ScalarTypeExtension withName(hydra.langs.graphql.syntax.Name name) {
    if (name == null) {
      throw new IllegalArgumentException("null value for 'name' argument");
    }
    return new ScalarTypeExtension(name, directives);
  }
  
  public ScalarTypeExtension withDirectives(hydra.langs.graphql.syntax.Directives directives) {
    if (directives == null) {
      throw new IllegalArgumentException("null value for 'directives' argument");
    }
    return new ScalarTypeExtension(name, directives);
  }
}