// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class Result implements Serializable, Comparable<Result> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.Result");

  public static final hydra.core.Name SIMPLE = new hydra.core.Name("simple");

  public static final hydra.core.Name NULL_LITERAL = new hydra.core.Name("nullLiteral");

  private Result () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Simple instance) ;

    R visit(NullLiteral instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Result instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Simple instance) {
      return otherwise(instance);
    }

    default R visit(NullLiteral instance) {
      return otherwise(instance);
    }
  }

  public static final class Simple extends openGql.grammar.Result implements Serializable {
    public final openGql.grammar.ValueExpression value;

    public Simple (openGql.grammar.ValueExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Simple)) {
        return false;
      }
      Simple o = (Simple) other;
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
    public int compareTo(Result other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Simple o = (Simple) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class NullLiteral extends openGql.grammar.Result implements Serializable {
    public NullLiteral () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NullLiteral)) {
        return false;
      }
      NullLiteral o = (NullLiteral) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Result other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
