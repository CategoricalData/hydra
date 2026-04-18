// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class ListValueFunction implements Serializable, Comparable<ListValueFunction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ListValueFunction");

  public static final hydra.core.Name TRIM = new hydra.core.Name("trim");

  public static final hydra.core.Name ELEMENTS = new hydra.core.Name("elements");

  private ListValueFunction () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Trim instance) ;

    R visit(Elements_ instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ListValueFunction instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Trim instance) {
      return otherwise(instance);
    }

    default R visit(Elements_ instance) {
      return otherwise(instance);
    }
  }

  public static final class Trim extends openGql.grammar.ListValueFunction implements Serializable {
    public final openGql.grammar.TrimListFunction value;

    public Trim (openGql.grammar.TrimListFunction value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Trim)) {
        return false;
      }
      Trim o = (Trim) other;
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
    public int compareTo(ListValueFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Trim o = (Trim) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Elements_ extends openGql.grammar.ListValueFunction implements Serializable {
    public final openGql.grammar.ValueExpression value;

    public Elements_ (openGql.grammar.ValueExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Elements_)) {
        return false;
      }
      Elements_ o = (Elements_) other;
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
    public int compareTo(ListValueFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Elements_ o = (Elements_) other;
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
