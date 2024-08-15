// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class UnionTypeExtension_Sequence2 implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/graphql/syntax.UnionTypeExtension.Sequence2");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_DIRECTIVES = new hydra.core.Name("directives");
  
  public final hydra.langs.graphql.syntax.Name name;
  
  public final hydra.langs.graphql.syntax.Directives directives;
  
  public UnionTypeExtension_Sequence2 (hydra.langs.graphql.syntax.Name name, hydra.langs.graphql.syntax.Directives directives) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((directives));
    this.name = name;
    this.directives = directives;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnionTypeExtension_Sequence2)) {
      return false;
    }
    UnionTypeExtension_Sequence2 o = (UnionTypeExtension_Sequence2) (other);
    return name.equals(o.name) && directives.equals(o.directives);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * directives.hashCode();
  }
  
  public UnionTypeExtension_Sequence2 withName(hydra.langs.graphql.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new UnionTypeExtension_Sequence2(name, directives);
  }
  
  public UnionTypeExtension_Sequence2 withDirectives(hydra.langs.graphql.syntax.Directives directives) {
    java.util.Objects.requireNonNull((directives));
    return new UnionTypeExtension_Sequence2(name, directives);
  }
}