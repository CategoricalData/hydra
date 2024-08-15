// Note: this is an automatically generated file. Do not edit.

package hydra.langs.java.syntax;

import java.io.Serializable;

public class NormalAnnotation implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/java/syntax.NormalAnnotation");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_NAME = new hydra.core.Name("typeName");
  
  public static final hydra.core.Name FIELD_NAME_PAIRS = new hydra.core.Name("pairs");
  
  public final hydra.langs.java.syntax.TypeName typeName;
  
  public final java.util.List<hydra.langs.java.syntax.ElementValuePair> pairs;
  
  public NormalAnnotation (hydra.langs.java.syntax.TypeName typeName, java.util.List<hydra.langs.java.syntax.ElementValuePair> pairs) {
    java.util.Objects.requireNonNull((typeName));
    java.util.Objects.requireNonNull((pairs));
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
  
  public NormalAnnotation withTypeName(hydra.langs.java.syntax.TypeName typeName) {
    java.util.Objects.requireNonNull((typeName));
    return new NormalAnnotation(typeName, pairs);
  }
  
  public NormalAnnotation withPairs(java.util.List<hydra.langs.java.syntax.ElementValuePair> pairs) {
    java.util.Objects.requireNonNull((pairs));
    return new NormalAnnotation(typeName, pairs);
  }
}