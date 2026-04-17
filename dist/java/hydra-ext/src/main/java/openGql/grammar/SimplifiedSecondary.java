// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class SimplifiedSecondary implements Serializable, Comparable<SimplifiedSecondary> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SimplifiedSecondary");

  public static final hydra.core.Name PRIMARY = new hydra.core.Name("primary");

  public static final hydra.core.Name NEGATION = new hydra.core.Name("negation");

  private SimplifiedSecondary () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Primary instance) ;

    R visit(Negation instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SimplifiedSecondary instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Primary instance) {
      return otherwise(instance);
    }

    default R visit(Negation instance) {
      return otherwise(instance);
    }
  }

  public static final class Primary extends openGql.grammar.SimplifiedSecondary implements Serializable {
    public final openGql.grammar.SimplifiedPrimary value;

    public Primary (openGql.grammar.SimplifiedPrimary value) {
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
    public int compareTo(SimplifiedSecondary other) {
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

  public static final class Negation extends openGql.grammar.SimplifiedSecondary implements Serializable {
    public final openGql.grammar.SimplifiedPrimary value;

    public Negation (openGql.grammar.SimplifiedPrimary value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Negation)) {
        return false;
      }
      Negation o = (Negation) other;
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
    public int compareTo(SimplifiedSecondary other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Negation o = (Negation) other;
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
