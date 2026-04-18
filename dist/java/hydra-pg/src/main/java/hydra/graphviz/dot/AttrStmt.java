// Note: this is an automatically generated file. Do not edit.

package hydra.graphviz.dot;

import java.io.Serializable;

public class AttrStmt implements Serializable, Comparable<AttrStmt> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.graphviz.dot.AttrStmt");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name ATTRIBUTES = new hydra.core.Name("attributes");

  public final hydra.graphviz.dot.AttrType type;

  public final hydra.graphviz.dot.AttrList attributes;

  public AttrStmt (hydra.graphviz.dot.AttrType type, hydra.graphviz.dot.AttrList attributes) {
    this.type = type;
    this.attributes = attributes;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AttrStmt)) {
      return false;
    }
    AttrStmt o = (AttrStmt) other;
    return java.util.Objects.equals(
      this.type,
      o.type) && java.util.Objects.equals(
      this.attributes,
      o.attributes);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(type) + 3 * java.util.Objects.hashCode(attributes);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(AttrStmt other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      type,
      other.type);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      attributes,
      other.attributes);
  }

  public AttrStmt withType(hydra.graphviz.dot.AttrType type) {
    return new AttrStmt(type, attributes);
  }

  public AttrStmt withAttributes(hydra.graphviz.dot.AttrList attributes) {
    return new AttrStmt(type, attributes);
  }
}
