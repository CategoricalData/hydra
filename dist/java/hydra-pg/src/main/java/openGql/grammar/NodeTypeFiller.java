// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class NodeTypeFiller implements Serializable, Comparable<NodeTypeFiller> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.NodeTypeFiller");

  public static final hydra.core.Name KEY_LABEL_SET = new hydra.core.Name("keyLabelSet");

  public static final hydra.core.Name IMPLIED_CONTENT = new hydra.core.Name("impliedContent");

  private NodeTypeFiller () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(KeyLabelSet instance) ;

    R visit(ImpliedContent instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NodeTypeFiller instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(KeyLabelSet instance) {
      return otherwise(instance);
    }

    default R visit(ImpliedContent instance) {
      return otherwise(instance);
    }
  }

  public static final class KeyLabelSet extends openGql.grammar.NodeTypeFiller implements Serializable {
    public final openGql.grammar.NodeKeyLabelSetWithContent value;

    public KeyLabelSet (openGql.grammar.NodeKeyLabelSetWithContent value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof KeyLabelSet)) {
        return false;
      }
      KeyLabelSet o = (KeyLabelSet) other;
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
    public int compareTo(NodeTypeFiller other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      KeyLabelSet o = (KeyLabelSet) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ImpliedContent extends openGql.grammar.NodeTypeFiller implements Serializable {
    public final openGql.grammar.NodeTypeImpliedContent value;

    public ImpliedContent (openGql.grammar.NodeTypeImpliedContent value) {
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
    public int compareTo(NodeTypeFiller other) {
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
