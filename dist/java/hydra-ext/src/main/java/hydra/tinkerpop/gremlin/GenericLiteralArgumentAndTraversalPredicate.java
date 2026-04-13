// Note: this is an automatically generated file. Do not edit.

package hydra.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class GenericLiteralArgumentAndTraversalPredicate implements Serializable, Comparable<GenericLiteralArgumentAndTraversalPredicate> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalPredicate");

  public static final hydra.core.Name LITERAL = new hydra.core.Name("literal");

  public static final hydra.core.Name PREDICATE = new hydra.core.Name("predicate");

  private GenericLiteralArgumentAndTraversalPredicate () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Literal instance) ;

    R visit(Predicate instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(GenericLiteralArgumentAndTraversalPredicate instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Literal instance) {
      return otherwise(instance);
    }

    default R visit(Predicate instance) {
      return otherwise(instance);
    }
  }

  public static final class Literal extends hydra.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalPredicate implements Serializable {
    public final hydra.tinkerpop.gremlin.GenericLiteralArgument value;

    public Literal (hydra.tinkerpop.gremlin.GenericLiteralArgument value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Literal)) {
        return false;
      }
      Literal o = (Literal) other;
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
    public int compareTo(GenericLiteralArgumentAndTraversalPredicate other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Literal o = (Literal) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Predicate extends hydra.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalPredicate implements Serializable {
    public final hydra.tinkerpop.gremlin.TraversalPredicate value;

    public Predicate (hydra.tinkerpop.gremlin.TraversalPredicate value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Predicate)) {
        return false;
      }
      Predicate o = (Predicate) other;
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
    public int compareTo(GenericLiteralArgumentAndTraversalPredicate other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Predicate o = (Predicate) other;
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
