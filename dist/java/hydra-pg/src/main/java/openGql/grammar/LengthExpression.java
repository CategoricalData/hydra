// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class LengthExpression implements Serializable, Comparable<LengthExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.LengthExpression");

  public static final hydra.core.Name CHAR = new hydra.core.Name("char");

  public static final hydra.core.Name BYTE = new hydra.core.Name("byte");

  public static final hydra.core.Name PATH = new hydra.core.Name("path");

  private LengthExpression () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Char instance) ;

    R visit(Byte_ instance) ;

    R visit(Path instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(LengthExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Char instance) {
      return otherwise(instance);
    }

    default R visit(Byte_ instance) {
      return otherwise(instance);
    }

    default R visit(Path instance) {
      return otherwise(instance);
    }
  }

  public static final class Char extends openGql.grammar.LengthExpression implements Serializable {
    public final openGql.grammar.ValueExpression value;

    public Char (openGql.grammar.ValueExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Char)) {
        return false;
      }
      Char o = (Char) other;
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
    public int compareTo(LengthExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Char o = (Char) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Byte_ extends openGql.grammar.LengthExpression implements Serializable {
    public final openGql.grammar.ValueExpression value;

    public Byte_ (openGql.grammar.ValueExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Byte_)) {
        return false;
      }
      Byte_ o = (Byte_) other;
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
    public int compareTo(LengthExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Byte_ o = (Byte_) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Path extends openGql.grammar.LengthExpression implements Serializable {
    public final openGql.grammar.ValueExpression value;

    public Path (openGql.grammar.ValueExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Path)) {
        return false;
      }
      Path o = (Path) other;
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
    public int compareTo(LengthExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Path o = (Path) other;
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
