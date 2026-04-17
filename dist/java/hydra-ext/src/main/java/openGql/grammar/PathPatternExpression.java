// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class PathPatternExpression implements Serializable, Comparable<PathPatternExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.PathPatternExpression");

  public static final hydra.core.Name TERM = new hydra.core.Name("term");

  public static final hydra.core.Name MULTISET_ALTERNATION = new hydra.core.Name("multisetAlternation");

  public static final hydra.core.Name PATTERN_UNION = new hydra.core.Name("patternUnion");

  private PathPatternExpression () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Term instance) ;

    R visit(MultisetAlternation instance) ;

    R visit(PatternUnion instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PathPatternExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Term instance) {
      return otherwise(instance);
    }

    default R visit(MultisetAlternation instance) {
      return otherwise(instance);
    }

    default R visit(PatternUnion instance) {
      return otherwise(instance);
    }
  }

  public static final class Term extends openGql.grammar.PathPatternExpression implements Serializable {
    public final java.util.List<openGql.grammar.PathFactor> value;

    public Term (java.util.List<openGql.grammar.PathFactor> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Term)) {
        return false;
      }
      Term o = (Term) other;
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
    public int compareTo(PathPatternExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Term o = (Term) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class MultisetAlternation extends openGql.grammar.PathPatternExpression implements Serializable {
    public final java.util.List<java.util.List<openGql.grammar.PathFactor>> value;

    public MultisetAlternation (java.util.List<java.util.List<openGql.grammar.PathFactor>> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MultisetAlternation)) {
        return false;
      }
      MultisetAlternation o = (MultisetAlternation) other;
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
    public int compareTo(PathPatternExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      MultisetAlternation o = (MultisetAlternation) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class PatternUnion extends openGql.grammar.PathPatternExpression implements Serializable {
    public final java.util.List<java.util.List<openGql.grammar.PathFactor>> value;

    public PatternUnion (java.util.List<java.util.List<openGql.grammar.PathFactor>> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PatternUnion)) {
        return false;
      }
      PatternUnion o = (PatternUnion) other;
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
    public int compareTo(PathPatternExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PatternUnion o = (PatternUnion) other;
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
