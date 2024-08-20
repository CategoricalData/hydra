// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

public class QualifiedName implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/haskell/ast.QualifiedName");
  
  public static final hydra.core.Name FIELD_NAME_QUALIFIERS = new hydra.core.Name("qualifiers");
  
  public static final hydra.core.Name FIELD_NAME_UNQUALIFIED = new hydra.core.Name("unqualified");
  
  public final java.util.List<hydra.ext.haskell.ast.NamePart> qualifiers;
  
  public final hydra.ext.haskell.ast.NamePart unqualified;
  
  public QualifiedName (java.util.List<hydra.ext.haskell.ast.NamePart> qualifiers, hydra.ext.haskell.ast.NamePart unqualified) {
    java.util.Objects.requireNonNull((qualifiers));
    java.util.Objects.requireNonNull((unqualified));
    this.qualifiers = qualifiers;
    this.unqualified = unqualified;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof QualifiedName)) {
      return false;
    }
    QualifiedName o = (QualifiedName) (other);
    return qualifiers.equals(o.qualifiers) && unqualified.equals(o.unqualified);
  }
  
  @Override
  public int hashCode() {
    return 2 * qualifiers.hashCode() + 3 * unqualified.hashCode();
  }
  
  public QualifiedName withQualifiers(java.util.List<hydra.ext.haskell.ast.NamePart> qualifiers) {
    java.util.Objects.requireNonNull((qualifiers));
    return new QualifiedName(qualifiers, unqualified);
  }
  
  public QualifiedName withUnqualified(hydra.ext.haskell.ast.NamePart unqualified) {
    java.util.Objects.requireNonNull((unqualified));
    return new QualifiedName(qualifiers, unqualified);
  }
}
