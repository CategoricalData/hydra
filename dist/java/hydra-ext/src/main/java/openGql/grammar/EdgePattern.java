// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class EdgePattern implements Serializable, Comparable<EdgePattern> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.EdgePattern");

  public static final hydra.core.Name FULL_EDGE = new hydra.core.Name("fullEdge");

  public static final hydra.core.Name ABBREVIATED_EDGE = new hydra.core.Name("abbreviatedEdge");

  private EdgePattern () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(FullEdge instance) ;

    R visit(AbbreviatedEdge instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(EdgePattern instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(FullEdge instance) {
      return otherwise(instance);
    }

    default R visit(AbbreviatedEdge instance) {
      return otherwise(instance);
    }
  }

  public static final class FullEdge extends openGql.grammar.EdgePattern implements Serializable {
    public final openGql.grammar.FullEdgePattern value;

    public FullEdge (openGql.grammar.FullEdgePattern value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FullEdge)) {
        return false;
      }
      FullEdge o = (FullEdge) other;
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
    public int compareTo(EdgePattern other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      FullEdge o = (FullEdge) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class AbbreviatedEdge extends openGql.grammar.EdgePattern implements Serializable {
    public final openGql.grammar.AbbreviatedEdgePattern value;

    public AbbreviatedEdge (openGql.grammar.AbbreviatedEdgePattern value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AbbreviatedEdge)) {
        return false;
      }
      AbbreviatedEdge o = (AbbreviatedEdge) other;
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
    public int compareTo(EdgePattern other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      AbbreviatedEdge o = (AbbreviatedEdge) other;
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
