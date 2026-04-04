// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class PnCharsU implements Serializable, Comparable<PnCharsU> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.PnCharsU");

  public static final hydra.core.Name PN_CHARS_BASE = new hydra.core.Name("PnCharsBase");

  public static final hydra.core.Name LOWBAR = new hydra.core.Name("Lowbar");

  private PnCharsU () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(PnCharsBase instance) ;

    R visit(Lowbar instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PnCharsU instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(PnCharsBase instance) {
      return otherwise(instance);
    }

    default R visit(Lowbar instance) {
      return otherwise(instance);
    }
  }

  public static final class PnCharsBase extends hydra.ext.io.shex.syntax.PnCharsU implements Serializable {
    public final hydra.ext.io.shex.syntax.PnCharsBase value;

    public PnCharsBase (hydra.ext.io.shex.syntax.PnCharsBase value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PnCharsBase)) {
        return false;
      }
      PnCharsBase o = (PnCharsBase) other;
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
    public int compareTo(PnCharsU other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PnCharsBase o = (PnCharsBase) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Lowbar extends hydra.ext.io.shex.syntax.PnCharsU implements Serializable {
    public Lowbar () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Lowbar)) {
        return false;
      }
      Lowbar o = (Lowbar) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(PnCharsU other) {
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
