// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class QualifiedAliasMember implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.QualifiedAliasMember");
  
  public static final hydra.core.Name FIELD_NAME_ALIAS = new hydra.core.Name("alias");
  
  public static final hydra.core.Name FIELD_NAME_MEMBER = new hydra.core.Name("member");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENTS = new hydra.core.Name("arguments");
  
  public final hydra.ext.csharp.syntax.Identifier alias;
  
  public final hydra.ext.csharp.syntax.Identifier member;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.TypeArgumentList> arguments;
  
  public QualifiedAliasMember (hydra.ext.csharp.syntax.Identifier alias, hydra.ext.csharp.syntax.Identifier member, hydra.util.Opt<hydra.ext.csharp.syntax.TypeArgumentList> arguments) {
    java.util.Objects.requireNonNull((alias));
    java.util.Objects.requireNonNull((member));
    java.util.Objects.requireNonNull((arguments));
    this.alias = alias;
    this.member = member;
    this.arguments = arguments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof QualifiedAliasMember)) {
      return false;
    }
    QualifiedAliasMember o = (QualifiedAliasMember) (other);
    return alias.equals(o.alias) && member.equals(o.member) && arguments.equals(o.arguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * alias.hashCode() + 3 * member.hashCode() + 5 * arguments.hashCode();
  }
  
  public QualifiedAliasMember withAlias(hydra.ext.csharp.syntax.Identifier alias) {
    java.util.Objects.requireNonNull((alias));
    return new QualifiedAliasMember(alias, member, arguments);
  }
  
  public QualifiedAliasMember withMember(hydra.ext.csharp.syntax.Identifier member) {
    java.util.Objects.requireNonNull((member));
    return new QualifiedAliasMember(alias, member, arguments);
  }
  
  public QualifiedAliasMember withArguments(hydra.util.Opt<hydra.ext.csharp.syntax.TypeArgumentList> arguments) {
    java.util.Objects.requireNonNull((arguments));
    return new QualifiedAliasMember(alias, member, arguments);
  }
}