// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class UnaryTripleExpr_Sequence_Alts implements Serializable, Comparable<UnaryTripleExpr_Sequence_Alts> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.UnaryTripleExpr_Sequence_Alts");

  public static final hydra.core.Name TRIPLE_CONSTRAINT = new hydra.core.Name("TripleConstraint");

  public static final hydra.core.Name BRACKETED_TRIPLE_EXPR = new hydra.core.Name("BracketedTripleExpr");

  private UnaryTripleExpr_Sequence_Alts () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(TripleConstraint instance) ;

    R visit(BracketedTripleExpr instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(UnaryTripleExpr_Sequence_Alts instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(TripleConstraint instance) {
      return otherwise(instance);
    }

    default R visit(BracketedTripleExpr instance) {
      return otherwise(instance);
    }
  }

  public static final class TripleConstraint extends hydra.ext.io.shex.syntax.UnaryTripleExpr_Sequence_Alts implements Serializable {
    public final hydra.ext.io.shex.syntax.TripleConstraint value;

    public TripleConstraint (hydra.ext.io.shex.syntax.TripleConstraint value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TripleConstraint)) {
        return false;
      }
      TripleConstraint o = (TripleConstraint) other;
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
    public int compareTo(UnaryTripleExpr_Sequence_Alts other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TripleConstraint o = (TripleConstraint) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class BracketedTripleExpr extends hydra.ext.io.shex.syntax.UnaryTripleExpr_Sequence_Alts implements Serializable {
    public final hydra.ext.io.shex.syntax.BracketedTripleExpr value;

    public BracketedTripleExpr (hydra.ext.io.shex.syntax.BracketedTripleExpr value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BracketedTripleExpr)) {
        return false;
      }
      BracketedTripleExpr o = (BracketedTripleExpr) other;
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
    public int compareTo(UnaryTripleExpr_Sequence_Alts other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      BracketedTripleExpr o = (BracketedTripleExpr) other;
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
