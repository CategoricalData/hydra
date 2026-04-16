// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class QueryConjunction implements Serializable, Comparable<QueryConjunction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.QueryConjunction");

  public static final hydra.core.Name SET_OPERATOR = new hydra.core.Name("setOperator");

  public static final hydra.core.Name OTHERWISE = new hydra.core.Name("otherwise");

  private QueryConjunction () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(SetOperator instance) ;

    R visit(Otherwise instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(QueryConjunction instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(SetOperator instance) {
      return otherwise(instance);
    }

    default R visit(Otherwise instance) {
      return otherwise(instance);
    }
  }

  public static final class SetOperator extends openGql.grammar.QueryConjunction implements Serializable {
    public final openGql.grammar.SetOperator value;

    public SetOperator (openGql.grammar.SetOperator value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SetOperator)) {
        return false;
      }
      SetOperator o = (SetOperator) other;
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
    public int compareTo(QueryConjunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SetOperator o = (SetOperator) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Otherwise extends openGql.grammar.QueryConjunction implements Serializable {
    public Otherwise () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Otherwise)) {
        return false;
      }
      Otherwise o = (Otherwise) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(QueryConjunction other) {
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
