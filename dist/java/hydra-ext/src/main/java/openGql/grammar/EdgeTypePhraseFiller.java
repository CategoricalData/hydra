// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class EdgeTypePhraseFiller implements Serializable, Comparable<EdgeTypePhraseFiller> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.EdgeTypePhraseFiller");

  public static final hydra.core.Name TYPE_NAME_WITH_FILLER = new hydra.core.Name("typeNameWithFiller");

  public static final hydra.core.Name FILLER_ONLY = new hydra.core.Name("fillerOnly");

  private EdgeTypePhraseFiller () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(TypeNameWithFiller instance) ;

    R visit(FillerOnly instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(EdgeTypePhraseFiller instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(TypeNameWithFiller instance) {
      return otherwise(instance);
    }

    default R visit(FillerOnly instance) {
      return otherwise(instance);
    }
  }

  public static final class TypeNameWithFiller extends openGql.grammar.EdgeTypePhraseFiller implements Serializable {
    public final openGql.grammar.EdgeTypeNameWithFiller value;

    public TypeNameWithFiller (openGql.grammar.EdgeTypeNameWithFiller value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeNameWithFiller)) {
        return false;
      }
      TypeNameWithFiller o = (TypeNameWithFiller) other;
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
    public int compareTo(EdgeTypePhraseFiller other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TypeNameWithFiller o = (TypeNameWithFiller) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class FillerOnly extends openGql.grammar.EdgeTypePhraseFiller implements Serializable {
    public final openGql.grammar.EdgeTypeFiller value;

    public FillerOnly (openGql.grammar.EdgeTypeFiller value) {
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
    public int compareTo(EdgeTypePhraseFiller other) {
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
