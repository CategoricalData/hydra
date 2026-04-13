// Note: this is an automatically generated file. Do not edit.

package hydra.shex.syntax;

import java.io.Serializable;

public abstract class IriRange implements Serializable, Comparable<IriRange> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.shex.syntax.IriRange");

  public static final hydra.core.Name SEQUENCE = new hydra.core.Name("sequence");

  public static final hydra.core.Name SEQUENCE2 = new hydra.core.Name("sequence2");

  private IriRange () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Sequence instance) ;

    R visit(Sequence2 instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(IriRange instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Sequence instance) {
      return otherwise(instance);
    }

    default R visit(Sequence2 instance) {
      return otherwise(instance);
    }
  }

  public static final class Sequence extends hydra.shex.syntax.IriRange implements Serializable {
    public final hydra.shex.syntax.IriRange_Sequence value;

    public Sequence (hydra.shex.syntax.IriRange_Sequence value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequence)) {
        return false;
      }
      Sequence o = (Sequence) other;
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
    public int compareTo(IriRange other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Sequence o = (Sequence) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Sequence2 extends hydra.shex.syntax.IriRange implements Serializable {
    public final java.util.List<hydra.shex.syntax.Exclusion> value;

    public Sequence2 (java.util.List<hydra.shex.syntax.Exclusion> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequence2)) {
        return false;
      }
      Sequence2 o = (Sequence2) other;
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
    public int compareTo(IriRange other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Sequence2 o = (Sequence2) other;
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
