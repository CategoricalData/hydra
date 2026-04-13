// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.gremlin;

import java.io.Serializable;

public class StructureVertex implements Serializable, Comparable<StructureVertex> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.gremlin.StructureVertex");

  public static final hydra.core.Name NEW = new hydra.core.Name("new");

  public static final hydra.core.Name ID = new hydra.core.Name("id");

  public static final hydra.core.Name LABEL = new hydra.core.Name("label");

  public final Boolean new_;

  public final hydra.tinkerpop.gremlin.GenericLiteralArgument id;

  public final hydra.tinkerpop.gremlin.StringArgument label;

  public StructureVertex (Boolean new_, hydra.tinkerpop.gremlin.GenericLiteralArgument id, hydra.tinkerpop.gremlin.StringArgument label) {
    this.new_ = new_;
    this.id = id;
    this.label = label;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StructureVertex)) {
      return false;
    }
    StructureVertex o = (StructureVertex) other;
    return java.util.Objects.equals(
      this.new_,
      o.new_) && java.util.Objects.equals(
      this.id,
      o.id) && java.util.Objects.equals(
      this.label,
      o.label);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(new_) + 3 * java.util.Objects.hashCode(id) + 5 * java.util.Objects.hashCode(label);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(StructureVertex other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      new_,
      other.new_);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      id,
      other.id);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      label,
      other.label);
  }

  public StructureVertex withNew(Boolean new_) {
    return new StructureVertex(new_, id, label);
  }

  public StructureVertex withId(hydra.tinkerpop.gremlin.GenericLiteralArgument id) {
    return new StructureVertex(new_, id, label);
  }

  public StructureVertex withLabel(hydra.tinkerpop.gremlin.StringArgument label) {
    return new StructureVertex(new_, id, label);
  }
}
