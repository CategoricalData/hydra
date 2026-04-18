// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class EdgeTypeFiller implements Serializable, Comparable<EdgeTypeFiller> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.EdgeTypeFiller");

  public static final hydra.core.Name KEY_LABEL_SET_WITH_CONTENT = new hydra.core.Name("keyLabelSetWithContent");

  public static final hydra.core.Name IMPLIED_CONTENT = new hydra.core.Name("impliedContent");

  private EdgeTypeFiller () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(KeyLabelSetWithContent instance) ;

    R visit(ImpliedContent instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(EdgeTypeFiller instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(KeyLabelSetWithContent instance) {
      return otherwise(instance);
    }

    default R visit(ImpliedContent instance) {
      return otherwise(instance);
    }
  }

  public static final class KeyLabelSetWithContent extends openGql.grammar.EdgeTypeFiller implements Serializable {
    public final openGql.grammar.EdgeKeyLabelSetWithContent value;

    public KeyLabelSetWithContent (openGql.grammar.EdgeKeyLabelSetWithContent value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof KeyLabelSetWithContent)) {
        return false;
      }
      KeyLabelSetWithContent o = (KeyLabelSetWithContent) other;
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
    public int compareTo(EdgeTypeFiller other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      KeyLabelSetWithContent o = (KeyLabelSetWithContent) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ImpliedContent extends openGql.grammar.EdgeTypeFiller implements Serializable {
    public final openGql.grammar.EdgeTypeImpliedContent value;

    public ImpliedContent (openGql.grammar.EdgeTypeImpliedContent value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ImpliedContent)) {
        return false;
      }
      ImpliedContent o = (ImpliedContent) other;
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
    public int compareTo(EdgeTypeFiller other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ImpliedContent o = (ImpliedContent) other;
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
