// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class BlankNodeLabel_Alts implements Serializable, Comparable<BlankNodeLabel_Alts> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.BlankNodeLabel_Alts");

  public static final hydra.core.Name PN_CHARS_U = new hydra.core.Name("PnCharsU");

  public static final hydra.core.Name REGEX = new hydra.core.Name("regex");

  private BlankNodeLabel_Alts () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(PnCharsU instance) ;

    R visit(Regex instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(BlankNodeLabel_Alts instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(PnCharsU instance) {
      return otherwise(instance);
    }

    default R visit(Regex instance) {
      return otherwise(instance);
    }
  }

  public static final class PnCharsU extends hydra.ext.io.shex.syntax.BlankNodeLabel_Alts implements Serializable {
    public final hydra.ext.io.shex.syntax.PnCharsU value;

    public PnCharsU (hydra.ext.io.shex.syntax.PnCharsU value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PnCharsU)) {
        return false;
      }
      PnCharsU o = (PnCharsU) other;
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
    public int compareTo(BlankNodeLabel_Alts other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PnCharsU o = (PnCharsU) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Regex extends hydra.ext.io.shex.syntax.BlankNodeLabel_Alts implements Serializable {
    public final String value;

    public Regex (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Regex)) {
        return false;
      }
      Regex o = (Regex) other;
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
    public int compareTo(BlankNodeLabel_Alts other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Regex o = (Regex) other;
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
