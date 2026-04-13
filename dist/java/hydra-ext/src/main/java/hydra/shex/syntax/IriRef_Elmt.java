// Note: this is an automatically generated file. Do not edit.

package hydra.shex.syntax;

import java.io.Serializable;

public abstract class IriRef_Elmt implements Serializable, Comparable<IriRef_Elmt> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.shex.syntax.IriRef_Elmt");

  public static final hydra.core.Name REGEX = new hydra.core.Name("regex");

  public static final hydra.core.Name UCHAR = new hydra.core.Name("Uchar");

  private IriRef_Elmt () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Regex instance) ;

    R visit(Uchar instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(IriRef_Elmt instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Regex instance) {
      return otherwise(instance);
    }

    default R visit(Uchar instance) {
      return otherwise(instance);
    }
  }

  public static final class Regex extends hydra.shex.syntax.IriRef_Elmt implements Serializable {
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
    public int compareTo(IriRef_Elmt other) {
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

  public static final class Uchar extends hydra.shex.syntax.IriRef_Elmt implements Serializable {
    public final hydra.shex.syntax.Uchar value;

    public Uchar (hydra.shex.syntax.Uchar value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Uchar)) {
        return false;
      }
      Uchar o = (Uchar) other;
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
    public int compareTo(IriRef_Elmt other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Uchar o = (Uchar) other;
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
