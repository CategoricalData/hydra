// Note: this is an automatically generated file. Do not edit.

package hydra.shex.syntax;

import java.io.Serializable;

public abstract class PnLocal_Sequence_Option_Alts implements Serializable, Comparable<PnLocal_Sequence_Option_Alts> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.shex.syntax.PnLocal_Sequence_Option_Alts");

  public static final hydra.core.Name PN_CHARS = new hydra.core.Name("PnChars");

  public static final hydra.core.Name COLON = new hydra.core.Name("Colon");

  public static final hydra.core.Name PLX = new hydra.core.Name("Plx");

  private PnLocal_Sequence_Option_Alts () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(PnChars instance) ;

    R visit(Colon instance) ;

    R visit(Plx instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PnLocal_Sequence_Option_Alts instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(PnChars instance) {
      return otherwise(instance);
    }

    default R visit(Colon instance) {
      return otherwise(instance);
    }

    default R visit(Plx instance) {
      return otherwise(instance);
    }
  }

  public static final class PnChars extends hydra.shex.syntax.PnLocal_Sequence_Option_Alts implements Serializable {
    public final hydra.shex.syntax.PnChars value;

    public PnChars (hydra.shex.syntax.PnChars value) {
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
    public int compareTo(PnLocal_Sequence_Option_Alts other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PnChars o = (PnChars) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Colon extends hydra.shex.syntax.PnLocal_Sequence_Option_Alts implements Serializable {
    public Colon () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Colon)) {
        return false;
      }
      Colon o = (Colon) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(PnLocal_Sequence_Option_Alts other) {
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

  public static final class Plx extends hydra.shex.syntax.PnLocal_Sequence_Option_Alts implements Serializable {
    public final hydra.shex.syntax.Plx value;

    public Plx (hydra.shex.syntax.Plx value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Plx)) {
        return false;
      }
      Plx o = (Plx) other;
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
    public int compareTo(PnLocal_Sequence_Option_Alts other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Plx o = (Plx) other;
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
