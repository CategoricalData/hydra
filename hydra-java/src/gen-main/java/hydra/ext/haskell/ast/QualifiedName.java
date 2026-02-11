// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * A qualified name
 */
public class QualifiedName implements Serializable, Comparable<QualifiedName> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.QualifiedName");
  
  public static final hydra.core.Name FIELD_NAME_QUALIFIERS = new hydra.core.Name("qualifiers");
  
  public static final hydra.core.Name FIELD_NAME_UNQUALIFIED = new hydra.core.Name("unqualified");
  
  /**
   * The qualifier parts
   */
  public final java.util.List<hydra.ext.haskell.ast.NamePart> qualifiers;
  
  /**
   * The unqualified name part
   */
  public final hydra.ext.haskell.ast.NamePart unqualified;
  
  public QualifiedName (java.util.List<hydra.ext.haskell.ast.NamePart> qualifiers, hydra.ext.haskell.ast.NamePart unqualified) {
    this.qualifiers = qualifiers;
    this.unqualified = unqualified;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof QualifiedName)) {
      return false;
    }
    QualifiedName o = (QualifiedName) other;
    return java.util.Objects.equals(
      this.qualifiers,
      o.qualifiers) && java.util.Objects.equals(
      this.unqualified,
      o.unqualified);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(qualifiers) + 3 * java.util.Objects.hashCode(unqualified);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(QualifiedName other) {
    int cmp = 0;
    cmp = Integer.compare(
      qualifiers.hashCode(),
      other.qualifiers.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) unqualified).compareTo(other.unqualified);
  }
  
  public QualifiedName withQualifiers(java.util.List<hydra.ext.haskell.ast.NamePart> qualifiers) {
    return new QualifiedName(qualifiers, unqualified);
  }
  
  public QualifiedName withUnqualified(hydra.ext.haskell.ast.NamePart unqualified) {
    return new QualifiedName(qualifiers, unqualified);
  }
}
