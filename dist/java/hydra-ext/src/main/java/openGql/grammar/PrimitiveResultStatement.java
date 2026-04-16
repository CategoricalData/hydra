// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class PrimitiveResultStatement implements Serializable, Comparable<PrimitiveResultStatement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.PrimitiveResultStatement");

  public static final hydra.core.Name RETURN_AND_OPTIONAL_ORDER_BY = new hydra.core.Name("returnAndOptionalOrderBy");

  public static final hydra.core.Name FINISH = new hydra.core.Name("finish");

  private PrimitiveResultStatement () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(ReturnAndOptionalOrderBy instance) ;

    R visit(Finish instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PrimitiveResultStatement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(ReturnAndOptionalOrderBy instance) {
      return otherwise(instance);
    }

    default R visit(Finish instance) {
      return otherwise(instance);
    }
  }

  public static final class ReturnAndOptionalOrderBy extends openGql.grammar.PrimitiveResultStatement implements Serializable {
    public final openGql.grammar.ReturnAndOptionalOrderByAndPage value;

    public ReturnAndOptionalOrderBy (openGql.grammar.ReturnAndOptionalOrderByAndPage value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ReturnAndOptionalOrderBy)) {
        return false;
      }
      ReturnAndOptionalOrderBy o = (ReturnAndOptionalOrderBy) other;
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
    public int compareTo(PrimitiveResultStatement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ReturnAndOptionalOrderBy o = (ReturnAndOptionalOrderBy) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Finish extends openGql.grammar.PrimitiveResultStatement implements Serializable {
    public Finish () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Finish)) {
        return false;
      }
      Finish o = (Finish) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(PrimitiveResultStatement other) {
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
