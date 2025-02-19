// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class QualifiedNamespaceOrTypeName implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.QualifiedNamespaceOrTypeName");
  
  public static final hydra.core.Name FIELD_NAME_NAMESPACE_OR_TYPE = new hydra.core.Name("namespaceOrType");
  
  public static final hydra.core.Name FIELD_NAME_IDENTIFIER = new hydra.core.Name("identifier");
  
  public static final hydra.core.Name FIELD_NAME_ARGUMENTS = new hydra.core.Name("arguments");
  
  public final hydra.ext.csharp.syntax.NamespaceOrTypeName namespaceOrType;
  
  public final hydra.ext.csharp.syntax.Identifier identifier;
  
  public final hydra.util.Opt<hydra.ext.csharp.syntax.TypeArgumentList> arguments;
  
  public QualifiedNamespaceOrTypeName (hydra.ext.csharp.syntax.NamespaceOrTypeName namespaceOrType, hydra.ext.csharp.syntax.Identifier identifier, hydra.util.Opt<hydra.ext.csharp.syntax.TypeArgumentList> arguments) {
    java.util.Objects.requireNonNull((namespaceOrType));
    java.util.Objects.requireNonNull((identifier));
    java.util.Objects.requireNonNull((arguments));
    this.namespaceOrType = namespaceOrType;
    this.identifier = identifier;
    this.arguments = arguments;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof QualifiedNamespaceOrTypeName)) {
      return false;
    }
    QualifiedNamespaceOrTypeName o = (QualifiedNamespaceOrTypeName) (other);
    return namespaceOrType.equals(o.namespaceOrType) && identifier.equals(o.identifier) && arguments.equals(o.arguments);
  }
  
  @Override
  public int hashCode() {
    return 2 * namespaceOrType.hashCode() + 3 * identifier.hashCode() + 5 * arguments.hashCode();
  }
  
  public QualifiedNamespaceOrTypeName withNamespaceOrType(hydra.ext.csharp.syntax.NamespaceOrTypeName namespaceOrType) {
    java.util.Objects.requireNonNull((namespaceOrType));
    return new QualifiedNamespaceOrTypeName(namespaceOrType, identifier, arguments);
  }
  
  public QualifiedNamespaceOrTypeName withIdentifier(hydra.ext.csharp.syntax.Identifier identifier) {
    java.util.Objects.requireNonNull((identifier));
    return new QualifiedNamespaceOrTypeName(namespaceOrType, identifier, arguments);
  }
  
  public QualifiedNamespaceOrTypeName withArguments(hydra.util.Opt<hydra.ext.csharp.syntax.TypeArgumentList> arguments) {
    java.util.Objects.requireNonNull((arguments));
    return new QualifiedNamespaceOrTypeName(namespaceOrType, identifier, arguments);
  }
}