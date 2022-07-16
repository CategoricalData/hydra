package hydra.ext.java.syntax;

public class NormalAnnotation {
  public final TypeName typeName;
  
  public final java.util.List<ElementValuePair> pairs;
  
  public NormalAnnotation (TypeName typeName, java.util.List<ElementValuePair> pairs) {
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
  
  public NormalAnnotation withTypeName(TypeName typeName) {
    return new NormalAnnotation(typeName, pairs);
  }
  
  public NormalAnnotation withPairs(java.util.List<ElementValuePair> pairs) {
    return new NormalAnnotation(typeName, pairs);
  }
}