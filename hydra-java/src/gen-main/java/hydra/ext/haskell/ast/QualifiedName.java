package hydra.ext.haskell.ast;

public class QualifiedName {
  public final java.util.List<hydra.ext.haskell.ast.NamePart> qualifiers;
  
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
    QualifiedName o = (QualifiedName) (other);
    return qualifiers.equals(o.qualifiers) && unqualified.equals(o.unqualified);
  }
  
  @Override
  public int hashCode() {
    return 2 * qualifiers.hashCode() + 3 * unqualified.hashCode();
  }
  
  public QualifiedName withQualifiers(java.util.List<hydra.ext.haskell.ast.NamePart> qualifiers) {
    return new QualifiedName(qualifiers, unqualified);
  }
  
  public QualifiedName withUnqualified(hydra.ext.haskell.ast.NamePart unqualified) {
    return new QualifiedName(qualifiers, unqualified);
  }
}