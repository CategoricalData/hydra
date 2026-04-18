// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public class NormalAnnotation implements Serializable, Comparable<NormalAnnotation> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.NormalAnnotation");

  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("typeName");

  public static final hydra.core.Name PAIRS = new hydra.core.Name("pairs");

  public final hydra.java.syntax.TypeName typeName;

  public final java.util.List<hydra.java.syntax.ElementValuePair> pairs;

  public NormalAnnotation (hydra.java.syntax.TypeName typeName, java.util.List<hydra.java.syntax.ElementValuePair> pairs) {
    this.typeName = typeName;
    this.pairs = pairs;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NormalAnnotation)) {
      return false;
    }
    NormalAnnotation o = (NormalAnnotation) other;
    return java.util.Objects.equals(
      this.typeName,
      o.typeName) && java.util.Objects.equals(
      this.pairs,
      o.pairs);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(typeName) + 3 * java.util.Objects.hashCode(pairs);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(NormalAnnotation other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      typeName,
      other.typeName);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      pairs,
      other.pairs);
  }

  public NormalAnnotation withTypeName(hydra.java.syntax.TypeName typeName) {
    return new NormalAnnotation(typeName, pairs);
  }

  public NormalAnnotation withPairs(java.util.List<hydra.java.syntax.ElementValuePair> pairs) {
    return new NormalAnnotation(typeName, pairs);
  }
}
