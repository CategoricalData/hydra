// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class NodeTypePhraseFiller implements Serializable, Comparable<NodeTypePhraseFiller> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.NodeTypePhraseFiller");

  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("typeName");

  public static final hydra.core.Name FILLER_ONLY = new hydra.core.Name("fillerOnly");

  private NodeTypePhraseFiller () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(TypeName instance) ;

    R visit(FillerOnly instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NodeTypePhraseFiller instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(TypeName instance) {
      return otherwise(instance);
    }

    default R visit(FillerOnly instance) {
      return otherwise(instance);
    }
  }

  public static final class TypeName extends openGql.grammar.NodeTypePhraseFiller implements Serializable {
    public final openGql.grammar.NodeTypeNameWithFiller value;

    public TypeName (openGql.grammar.NodeTypeNameWithFiller value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeName)) {
        return false;
      }
      TypeName o = (TypeName) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }

    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(NodeTypePhraseFiller other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TypeName o = (TypeName) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class FillerOnly extends openGql.grammar.NodeTypePhraseFiller implements Serializable {
    public final openGql.grammar.NodeTypeFiller value;

    public FillerOnly (openGql.grammar.NodeTypeFiller value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FillerOnly)) {
        return false;
      }
      FillerOnly o = (FillerOnly) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }

    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(NodeTypePhraseFiller other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      FillerOnly o = (FillerOnly) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
