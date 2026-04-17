// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class SimplifiedContents implements Serializable, Comparable<SimplifiedContents> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SimplifiedContents");

  public static final hydra.core.Name TERM = new hydra.core.Name("term");

  public static final hydra.core.Name PATH_UNION = new hydra.core.Name("pathUnion");

  public static final hydra.core.Name MULTISET_ALTERNATION = new hydra.core.Name("multisetAlternation");

  private SimplifiedContents () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Term instance) ;

    R visit(PathUnion instance) ;

    R visit(MultisetAlternation instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SimplifiedContents instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Term instance) {
      return otherwise(instance);
    }

    default R visit(PathUnion instance) {
      return otherwise(instance);
    }

    default R visit(MultisetAlternation instance) {
      return otherwise(instance);
    }
  }

  public static final class Term extends openGql.grammar.SimplifiedContents implements Serializable {
    public final openGql.grammar.SimplifiedTerm value;

    public Term (openGql.grammar.SimplifiedTerm value) {
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
    public int compareTo(SimplifiedContents other) {
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

  public static final class PathUnion extends openGql.grammar.SimplifiedContents implements Serializable {
    public final java.util.List<openGql.grammar.SimplifiedTerm> value;

    public PathUnion (java.util.List<openGql.grammar.SimplifiedTerm> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PathUnion)) {
        return false;
      }
      PathUnion o = (PathUnion) other;
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
    public int compareTo(SimplifiedContents other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PathUnion o = (PathUnion) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class MultisetAlternation extends openGql.grammar.SimplifiedContents implements Serializable {
    public final java.util.List<openGql.grammar.SimplifiedTerm> value;

    public MultisetAlternation (java.util.List<openGql.grammar.SimplifiedTerm> value) {
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
    public int compareTo(SimplifiedContents other) {
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
}
