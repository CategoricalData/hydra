// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class Plx implements Serializable, Comparable<Plx> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.Plx");

  public static final hydra.core.Name PERCENT = new hydra.core.Name("Percent");

  public static final hydra.core.Name PN_LOCAL_ESC = new hydra.core.Name("PnLocalEsc");

  private Plx () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Percent instance) ;

    R visit(PnLocalEsc instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Plx instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Percent instance) {
      return otherwise(instance);
    }

    default R visit(PnLocalEsc instance) {
      return otherwise(instance);
    }
  }

  public static final class Percent extends hydra.ext.io.shex.syntax.Plx implements Serializable {
    public final hydra.ext.io.shex.syntax.Percent value;

    public Percent (hydra.ext.io.shex.syntax.Percent value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Percent)) {
        return false;
      }
      Percent o = (Percent) other;
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
    public int compareTo(Plx other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Percent o = (Percent) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class PnLocalEsc extends hydra.ext.io.shex.syntax.Plx implements Serializable {
    public final hydra.ext.io.shex.syntax.PnLocalEsc value;

    public PnLocalEsc (hydra.ext.io.shex.syntax.PnLocalEsc value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PnLocalEsc)) {
        return false;
      }
      PnLocalEsc o = (PnLocalEsc) other;
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
    public int compareTo(Plx other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PnLocalEsc o = (PnLocalEsc) other;
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
