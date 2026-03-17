// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class TripleExprLabel implements Serializable, Comparable<TripleExprLabel> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.TripleExprLabel");

  public static final hydra.core.Name IRI = new hydra.core.Name("Iri");

  public static final hydra.core.Name BLANK_NODE = new hydra.core.Name("BlankNode");

  private TripleExprLabel () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Iri instance) ;

    R visit(BlankNode instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TripleExprLabel instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Iri instance) {
      return otherwise(instance);
    }

    default R visit(BlankNode instance) {
      return otherwise(instance);
    }
  }

  public static final class Iri extends hydra.ext.io.shex.syntax.TripleExprLabel implements Serializable {
    public final hydra.ext.io.shex.syntax.Iri value;

    public Iri (hydra.ext.io.shex.syntax.Iri value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Iri)) {
        return false;
      }
      Iri o = (Iri) other;
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
    public int compareTo(TripleExprLabel other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Iri o = (Iri) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class BlankNode extends hydra.ext.io.shex.syntax.TripleExprLabel implements Serializable {
    public final hydra.ext.io.shex.syntax.BlankNode value;

    public BlankNode (hydra.ext.io.shex.syntax.BlankNode value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BlankNode)) {
        return false;
      }
      BlankNode o = (BlankNode) other;
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
    public int compareTo(TripleExprLabel other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      BlankNode o = (BlankNode) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
