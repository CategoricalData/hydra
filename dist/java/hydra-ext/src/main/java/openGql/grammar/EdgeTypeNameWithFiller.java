// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class EdgeTypeNameWithFiller implements Serializable, Comparable<EdgeTypeNameWithFiller> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.EdgeTypeNameWithFiller");

  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("typeName");

  public static final hydra.core.Name FILLER = new hydra.core.Name("filler");

  public final String typeName;

  public final hydra.util.Maybe<openGql.grammar.EdgeTypeFiller> filler;

  public EdgeTypeNameWithFiller (String typeName, hydra.util.Maybe<openGql.grammar.EdgeTypeFiller> filler) {
    this.typeName = typeName;
    this.filler = filler;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof EdgeTypeNameWithFiller)) {
      return false;
    }
    EdgeTypeNameWithFiller o = (EdgeTypeNameWithFiller) other;
    return java.util.Objects.equals(
      this.typeName,
      o.typeName) && java.util.Objects.equals(
      this.filler,
      o.filler);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(typeName) + 3 * java.util.Objects.hashCode(filler);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(EdgeTypeNameWithFiller other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      typeName,
      other.typeName);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      filler,
      other.filler);
  }

  public EdgeTypeNameWithFiller withTypeName(String typeName) {
    return new EdgeTypeNameWithFiller(typeName, filler);
  }

  public EdgeTypeNameWithFiller withFiller(hydra.util.Maybe<openGql.grammar.EdgeTypeFiller> filler) {
    return new EdgeTypeNameWithFiller(typeName, filler);
  }
}
