// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class ScalarTypeExtension implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.ScalarTypeExtension");
  
  public final hydra.langs.graphql.syntax.Name name;
  
  public final hydra.langs.graphql.syntax.Directives directives;
  
  public ScalarTypeExtension (hydra.langs.graphql.syntax.Name name, hydra.langs.graphql.syntax.Directives directives) {
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
  
  public ScalarTypeExtension withName(hydra.langs.graphql.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new ScalarTypeExtension(name, directives);
  }
  
  public ScalarTypeExtension withDirectives(hydra.langs.graphql.syntax.Directives directives) {
    java.util.Objects.requireNonNull((directives));
    return new ScalarTypeExtension(name, directives);
  }
}