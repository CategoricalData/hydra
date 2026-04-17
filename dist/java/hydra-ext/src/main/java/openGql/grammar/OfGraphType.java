// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class OfGraphType implements Serializable, Comparable<OfGraphType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.OfGraphType");

  public static final hydra.core.Name LIKE_GRAPH = new hydra.core.Name("likeGraph");

  public static final hydra.core.Name REFERENCE = new hydra.core.Name("reference");

  public static final hydra.core.Name NESTED = new hydra.core.Name("nested");

  private OfGraphType () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(LikeGraph instance) ;

    R visit(Reference instance) ;

    R visit(Nested instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(OfGraphType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(LikeGraph instance) {
      return otherwise(instance);
    }

    default R visit(Reference instance) {
      return otherwise(instance);
    }

    default R visit(Nested instance) {
      return otherwise(instance);
    }
  }

  public static final class LikeGraph extends openGql.grammar.OfGraphType implements Serializable {
    public final openGql.grammar.GraphExpression value;

    public LikeGraph (openGql.grammar.GraphExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LikeGraph)) {
        return false;
      }
      LikeGraph o = (LikeGraph) other;
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
    public int compareTo(OfGraphType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      LikeGraph o = (LikeGraph) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Reference extends openGql.grammar.OfGraphType implements Serializable {
    public final openGql.grammar.TypedGraphTypeReference value;

    public Reference (openGql.grammar.TypedGraphTypeReference value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Reference)) {
        return false;
      }
      Reference o = (Reference) other;
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
    public int compareTo(OfGraphType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Reference o = (Reference) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Nested extends openGql.grammar.OfGraphType implements Serializable {
    public final openGql.grammar.TypedNestedGraphTypeSpecification value;

    public Nested (openGql.grammar.TypedNestedGraphTypeSpecification value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Nested)) {
        return false;
      }
      Nested o = (Nested) other;
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
    public int compareTo(OfGraphType other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Nested o = (Nested) other;
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
