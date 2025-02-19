// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class CompilationUnit implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.CompilationUnit");
  
  public static final hydra.core.Name FIELD_NAME_EXTERNS = new hydra.core.Name("externs");
  
  public static final hydra.core.Name FIELD_NAME_USINGS = new hydra.core.Name("usings");
  
  public static final hydra.core.Name FIELD_NAME_ATTRIBUTES = new hydra.core.Name("attributes");
  
  public static final hydra.core.Name FIELD_NAME_MEMBERS = new hydra.core.Name("members");
  
  public final java.util.List<hydra.ext.csharp.syntax.ExternAliasDirective> externs;
  
  public final java.util.List<hydra.ext.csharp.syntax.UsingDirective> usings;
  
  public final java.util.List<hydra.ext.csharp.syntax.GlobalAttributeSection> attributes;
  
  public final java.util.List<hydra.ext.csharp.syntax.NamespaceMemberDeclaration> members;
  
  public CompilationUnit (java.util.List<hydra.ext.csharp.syntax.ExternAliasDirective> externs, java.util.List<hydra.ext.csharp.syntax.UsingDirective> usings, java.util.List<hydra.ext.csharp.syntax.GlobalAttributeSection> attributes, java.util.List<hydra.ext.csharp.syntax.NamespaceMemberDeclaration> members) {
    java.util.Objects.requireNonNull((externs));
    java.util.Objects.requireNonNull((usings));
    java.util.Objects.requireNonNull((attributes));
    java.util.Objects.requireNonNull((members));
    this.externs = externs;
    this.usings = usings;
    this.attributes = attributes;
    this.members = members;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CompilationUnit)) {
      return false;
    }
    CompilationUnit o = (CompilationUnit) (other);
    return externs.equals(o.externs) && usings.equals(o.usings) && attributes.equals(o.attributes) && members.equals(o.members);
  }
  
  @Override
  public int hashCode() {
    return 2 * externs.hashCode() + 3 * usings.hashCode() + 5 * attributes.hashCode() + 7 * members.hashCode();
  }
  
  public CompilationUnit withExterns(java.util.List<hydra.ext.csharp.syntax.ExternAliasDirective> externs) {
    java.util.Objects.requireNonNull((externs));
    return new CompilationUnit(externs, usings, attributes, members);
  }
  
  public CompilationUnit withUsings(java.util.List<hydra.ext.csharp.syntax.UsingDirective> usings) {
    java.util.Objects.requireNonNull((usings));
    return new CompilationUnit(externs, usings, attributes, members);
  }
  
  public CompilationUnit withAttributes(java.util.List<hydra.ext.csharp.syntax.GlobalAttributeSection> attributes) {
    java.util.Objects.requireNonNull((attributes));
    return new CompilationUnit(externs, usings, attributes, members);
  }
  
  public CompilationUnit withMembers(java.util.List<hydra.ext.csharp.syntax.NamespaceMemberDeclaration> members) {
    java.util.Objects.requireNonNull((members));
    return new CompilationUnit(externs, usings, attributes, members);
  }
}