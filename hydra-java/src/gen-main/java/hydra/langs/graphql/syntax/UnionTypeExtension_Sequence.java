// Note: this is an automatically generated file. Do not edit.

package hydra.langs.graphql.syntax;

import java.io.Serializable;

public class UnionTypeExtension_Sequence implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/graphql/syntax.UnionTypeExtension.Sequence");
  
  public final hydra.langs.graphql.syntax.Name name;
  
  public final hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives;
  
  public final hydra.langs.graphql.syntax.UnionMemberTypes unionMemberTypes;
  
  public UnionTypeExtension_Sequence (hydra.langs.graphql.syntax.Name name, hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives, hydra.langs.graphql.syntax.UnionMemberTypes unionMemberTypes) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((directives));
    java.util.Objects.requireNonNull((unionMemberTypes));
    this.name = name;
    this.directives = directives;
    this.unionMemberTypes = unionMemberTypes;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnionTypeExtension_Sequence)) {
      return false;
    }
    UnionTypeExtension_Sequence o = (UnionTypeExtension_Sequence) (other);
    return name.equals(o.name) && directives.equals(o.directives) && unionMemberTypes.equals(o.unionMemberTypes);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * directives.hashCode() + 5 * unionMemberTypes.hashCode();
  }
  
  public UnionTypeExtension_Sequence withName(hydra.langs.graphql.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new UnionTypeExtension_Sequence(name, directives, unionMemberTypes);
  }
  
  public UnionTypeExtension_Sequence withDirectives(hydra.util.Opt<hydra.langs.graphql.syntax.Directives> directives) {
    java.util.Objects.requireNonNull((directives));
    return new UnionTypeExtension_Sequence(name, directives, unionMemberTypes);
  }
  
  public UnionTypeExtension_Sequence withUnionMemberTypes(hydra.langs.graphql.syntax.UnionMemberTypes unionMemberTypes) {
    java.util.Objects.requireNonNull((unionMemberTypes));
    return new UnionTypeExtension_Sequence(name, directives, unionMemberTypes);
  }
}