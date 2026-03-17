// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class PnPrefix_Sequence_Option_Alts implements Serializable, Comparable<PnPrefix_Sequence_Option_Alts> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.PnPrefix_Sequence_Option_Alts");

  public static final hydra.core.Name PN_CHARS = new hydra.core.Name("PnChars");

  public static final hydra.core.Name PERIOD = new hydra.core.Name("Period");

  private PnPrefix_Sequence_Option_Alts () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(PnChars instance) ;

    R visit(Period instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PnPrefix_Sequence_Option_Alts instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(PnChars instance) {
      return otherwise(instance);
    }

    default R visit(Period instance) {
      return otherwise(instance);
    }
  }

  public static final class PnChars extends hydra.ext.io.shex.syntax.PnPrefix_Sequence_Option_Alts implements Serializable {
    public final hydra.ext.io.shex.syntax.PnChars value;

    public PnChars (hydra.ext.io.shex.syntax.PnChars value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PnChars)) {
        return false;
      }
      PnChars o = (PnChars) other;
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
    public int compareTo(PnPrefix_Sequence_Option_Alts other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PnChars o = (PnChars) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Period extends hydra.ext.io.shex.syntax.PnPrefix_Sequence_Option_Alts implements Serializable {
    public Period () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Period)) {
        return false;
      }
      Period o = (Period) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(PnPrefix_Sequence_Option_Alts other) {
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
