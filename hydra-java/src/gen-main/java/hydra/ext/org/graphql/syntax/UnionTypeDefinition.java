// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.graphql.syntax;

import java.io.Serializable;

public class UnionTypeDefinition implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.graphql.syntax.UnionTypeDefinition");
  
  public static final hydra.core.Name FIELD_NAME_DESCRIPTION = new hydra.core.Name("description");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_DIRECTIVES = new hydra.core.Name("directives");
  
  public static final hydra.core.Name FIELD_NAME_UNION_MEMBER_TYPES = new hydra.core.Name("unionMemberTypes");
  
  public final hydra.util.Opt<hydra.ext.org.graphql.syntax.Description> description;
  
  public final hydra.ext.org.graphql.syntax.Name name;
  
  public final hydra.util.Opt<hydra.ext.org.graphql.syntax.Directives> directives;
  
  public final hydra.util.Opt<hydra.ext.org.graphql.syntax.UnionMemberTypes> unionMemberTypes;
  
  public UnionTypeDefinition (hydra.util.Opt<hydra.ext.org.graphql.syntax.Description> description, hydra.ext.org.graphql.syntax.Name name, hydra.util.Opt<hydra.ext.org.graphql.syntax.Directives> directives, hydra.util.Opt<hydra.ext.org.graphql.syntax.UnionMemberTypes> unionMemberTypes) {
    java.util.Objects.requireNonNull((description));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((directives));
    java.util.Objects.requireNonNull((unionMemberTypes));
    this.description = description;
    this.name = name;
    this.directives = directives;
    this.unionMemberTypes = unionMemberTypes;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof UnionTypeDefinition)) {
      return false;
    }
    UnionTypeDefinition o = (UnionTypeDefinition) (other);
    return description.equals(o.description) && name.equals(o.name) && directives.equals(o.directives) && unionMemberTypes.equals(o.unionMemberTypes);
  }
  
  @Override
  public int hashCode() {
    return 2 * description.hashCode() + 3 * name.hashCode() + 5 * directives.hashCode() + 7 * unionMemberTypes.hashCode();
  }
  
  public UnionTypeDefinition withDescription(hydra.util.Opt<hydra.ext.org.graphql.syntax.Description> description) {
    java.util.Objects.requireNonNull((description));
    return new UnionTypeDefinition(description, name, directives, unionMemberTypes);
  }
  
  public UnionTypeDefinition withName(hydra.ext.org.graphql.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new UnionTypeDefinition(description, name, directives, unionMemberTypes);
  }
  
  public UnionTypeDefinition withDirectives(hydra.util.Opt<hydra.ext.org.graphql.syntax.Directives> directives) {
    java.util.Objects.requireNonNull((directives));
    return new UnionTypeDefinition(description, name, directives, unionMemberTypes);
  }
  
  public UnionTypeDefinition withUnionMemberTypes(hydra.util.Opt<hydra.ext.org.graphql.syntax.UnionMemberTypes> unionMemberTypes) {
    java.util.Objects.requireNonNull((unionMemberTypes));
    return new UnionTypeDefinition(description, name, directives, unionMemberTypes);
  }
}