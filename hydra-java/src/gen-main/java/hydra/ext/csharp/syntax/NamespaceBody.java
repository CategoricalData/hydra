// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class NamespaceBody implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.NamespaceBody");
  
  public static final hydra.core.Name FIELD_NAME_EXTERNS = new hydra.core.Name("externs");
  
  public static final hydra.core.Name FIELD_NAME_USINGS = new hydra.core.Name("usings");
  
  public static final hydra.core.Name FIELD_NAME_MEMBERS = new hydra.core.Name("members");
  
  public final java.util.List<hydra.ext.csharp.syntax.ExternAliasDirective> externs;
  
  public final java.util.List<hydra.ext.csharp.syntax.UsingDirective> usings;
  
  public final java.util.List<hydra.ext.csharp.syntax.NamespaceMemberDeclaration> members;
  
  public NamespaceBody (java.util.List<hydra.ext.csharp.syntax.ExternAliasDirective> externs, java.util.List<hydra.ext.csharp.syntax.UsingDirective> usings, java.util.List<hydra.ext.csharp.syntax.NamespaceMemberDeclaration> members) {
    java.util.Objects.requireNonNull((externs));
    java.util.Objects.requireNonNull((usings));
    java.util.Objects.requireNonNull((members));
    this.externs = externs;
    this.usings = usings;
    this.members = members;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NamespaceBody)) {
      return false;
    }
    NamespaceBody o = (NamespaceBody) (other);
    return externs.equals(o.externs) && usings.equals(o.usings) && members.equals(o.members);
  }
  
  @Override
  public int hashCode() {
    return 2 * externs.hashCode() + 3 * usings.hashCode() + 5 * members.hashCode();
  }
  
  public NamespaceBody withExterns(java.util.List<hydra.ext.csharp.syntax.ExternAliasDirective> externs) {
    java.util.Objects.requireNonNull((externs));
    return new NamespaceBody(externs, usings, members);
  }
  
  public NamespaceBody withUsings(java.util.List<hydra.ext.csharp.syntax.UsingDirective> usings) {
    java.util.Objects.requireNonNull((usings));
    return new NamespaceBody(externs, usings, members);
  }
  
  public NamespaceBody withMembers(java.util.List<hydra.ext.csharp.syntax.NamespaceMemberDeclaration> members) {
    java.util.Objects.requireNonNull((members));
    return new NamespaceBody(externs, usings, members);
  }
}