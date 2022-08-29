package hydra.ext.java.syntax;

public class NormalAnnotation {
  public final hydra.ext.java.syntax.TypeName typeName;
  
  public final java.util.List<hydra.ext.java.syntax.ElementValuePair> pairs;
  
  public NormalAnnotation (hydra.ext.java.syntax.TypeName typeName, java.util.List<hydra.ext.java.syntax.ElementValuePair> pairs) {
    this.typeName = typeName;
    this.pairs = pairs;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NormalAnnotation)) {
      return false;
    }
    NormalAnnotation o = (NormalAnnotation) (other);
    return typeName.equals(o.typeName) && pairs.equals(o.pairs);
  }
  
  @Override
  public int hashCode() {
    return 2 * typeName.hashCode() + 3 * pairs.hashCode();
  }
  
  public NormalAnnotation withTypeName(hydra.ext.java.syntax.TypeName typeName) {
    return new NormalAnnotation(typeName, pairs);
  }
  
  public NormalAnnotation withPairs(java.util.List<hydra.ext.java.syntax.ElementValuePair> pairs) {
    return new NormalAnnotation(typeName, pairs);
  }
}