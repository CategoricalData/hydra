// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public class NamespaceDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.NamespaceDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_BODY = new hydra.core.Name("body");
  
  public final hydra.ext.csharp.syntax.QualifiedIdentifier name;
  
  public final hydra.ext.csharp.syntax.NamespaceBody body;
  
  public NamespaceDeclaration (hydra.ext.csharp.syntax.QualifiedIdentifier name, hydra.ext.csharp.syntax.NamespaceBody body) {
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((body));
    this.name = name;
    this.body = body;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NamespaceDeclaration)) {
      return false;
    }
    NamespaceDeclaration o = (NamespaceDeclaration) (other);
    return name.equals(o.name) && body.equals(o.body);
  }
  
  @Override
  public int hashCode() {
    return 2 * name.hashCode() + 3 * body.hashCode();
  }
  
  public NamespaceDeclaration withName(hydra.ext.csharp.syntax.QualifiedIdentifier name) {
    java.util.Objects.requireNonNull((name));
    return new NamespaceDeclaration(name, body);
  }
  
  public NamespaceDeclaration withBody(hydra.ext.csharp.syntax.NamespaceBody body) {
    java.util.Objects.requireNonNull((body));
    return new NamespaceDeclaration(name, body);
  }
}