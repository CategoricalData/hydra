// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class PathFactor implements Serializable, Comparable<PathFactor> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.PathFactor");

  public static final hydra.core.Name PRIMARY = new hydra.core.Name("primary");

  public static final hydra.core.Name QUANTIFIED_PRIMARY = new hydra.core.Name("quantifiedPrimary");

  public static final hydra.core.Name QUESTIONED_PRIMARY = new hydra.core.Name("questionedPrimary");

  private PathFactor () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Primary instance) ;

    R visit(QuantifiedPrimary instance) ;

    R visit(QuestionedPrimary instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PathFactor instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Primary instance) {
      return otherwise(instance);
    }

    default R visit(QuantifiedPrimary instance) {
      return otherwise(instance);
    }

    default R visit(QuestionedPrimary instance) {
      return otherwise(instance);
    }
  }

  public static final class Primary extends openGql.grammar.PathFactor implements Serializable {
    public final openGql.grammar.PathPrimary value;

    public Primary (openGql.grammar.PathPrimary value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Primary)) {
        return false;
      }
      Primary o = (Primary) other;
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
    public int compareTo(PathFactor other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Primary o = (Primary) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class QuantifiedPrimary extends openGql.grammar.PathFactor implements Serializable {
    public final openGql.grammar.QuantifiedPathPrimary value;

    public QuantifiedPrimary (openGql.grammar.QuantifiedPathPrimary value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof QuantifiedPrimary)) {
        return false;
      }
      QuantifiedPrimary o = (QuantifiedPrimary) other;
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
    public int compareTo(PathFactor other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      QuantifiedPrimary o = (QuantifiedPrimary) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class QuestionedPrimary extends openGql.grammar.PathFactor implements Serializable {
    public final openGql.grammar.PathPrimary value;

    public QuestionedPrimary (openGql.grammar.PathPrimary value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof QuestionedPrimary)) {
        return false;
      }
      QuestionedPrimary o = (QuestionedPrimary) other;
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
    public int compareTo(PathFactor other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      QuestionedPrimary o = (QuestionedPrimary) other;
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
