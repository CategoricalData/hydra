// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class BinarySetFunctionType implements Serializable, Comparable<BinarySetFunctionType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.BinarySetFunctionType");

  public static final hydra.core.Name PERCENTILE_CONT = new hydra.core.Name("percentileCont");

  public static final hydra.core.Name PERCENTILE_DISC = new hydra.core.Name("percentileDisc");

  private BinarySetFunctionType () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(PercentileCont instance) ;

    R visit(PercentileDisc instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(BinarySetFunctionType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(PercentileCont instance) {
      return otherwise(instance);
    }

    default R visit(PercentileDisc instance) {
      return otherwise(instance);
    }
  }

  public static final class PercentileCont extends openGql.grammar.BinarySetFunctionType implements Serializable {
    public PercentileCont () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PercentileCont)) {
        return false;
      }
      PercentileCont o = (PercentileCont) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(BinarySetFunctionType other) {
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

  public static final class PercentileDisc extends openGql.grammar.BinarySetFunctionType implements Serializable {
    public PercentileDisc () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PercentileDisc)) {
        return false;
      }
      PercentileDisc o = (PercentileDisc) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(BinarySetFunctionType other) {
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
