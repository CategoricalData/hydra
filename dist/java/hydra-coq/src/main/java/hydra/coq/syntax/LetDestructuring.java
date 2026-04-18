// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public abstract class LetDestructuring implements Serializable, Comparable<LetDestructuring> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.LetDestructuring");

  public static final hydra.core.Name VARIANT1 = new hydra.core.Name("variant1");

  public static final hydra.core.Name VARIANT2 = new hydra.core.Name("variant2");

  public static final hydra.core.Name VARIANT3 = new hydra.core.Name("variant3");

  private LetDestructuring () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Variant1 instance) ;

    R visit(Variant2 instance) ;

    R visit(Variant3 instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(LetDestructuring instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Variant1 instance) {
      return otherwise(instance);
    }

    default R visit(Variant2 instance) {
      return otherwise(instance);
    }

    default R visit(Variant3 instance) {
      return otherwise(instance);
    }
  }

  public static final class Variant1 extends hydra.coq.syntax.LetDestructuring implements Serializable {
    public final hydra.coq.syntax.LetDestructuring_Variant1 value;

    public Variant1 (hydra.coq.syntax.LetDestructuring_Variant1 value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Variant1)) {
        return false;
      }
      Variant1 o = (Variant1) other;
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
    public int compareTo(LetDestructuring other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Variant1 o = (Variant1) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Variant2 extends hydra.coq.syntax.LetDestructuring implements Serializable {
    public final hydra.coq.syntax.LetDestructuring_Variant2 value;

    public Variant2 (hydra.coq.syntax.LetDestructuring_Variant2 value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Variant2)) {
        return false;
      }
      Variant2 o = (Variant2) other;
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
    public int compareTo(LetDestructuring other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Variant2 o = (Variant2) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Variant3 extends hydra.coq.syntax.LetDestructuring implements Serializable {
    public final hydra.coq.syntax.LetDestructuring_Variant3 value;

    public Variant3 (hydra.coq.syntax.LetDestructuring_Variant3 value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Variant3)) {
        return false;
      }
      Variant3 o = (Variant3) other;
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
    public int compareTo(LetDestructuring other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Variant3 o = (Variant3) other;
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
