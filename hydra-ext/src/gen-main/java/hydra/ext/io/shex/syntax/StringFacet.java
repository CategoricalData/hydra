// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public abstract class StringFacet implements Serializable, Comparable<StringFacet> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.StringFacet");

  public static final hydra.core.Name SEQUENCE = new hydra.core.Name("sequence");

  public static final hydra.core.Name REGEXP = new hydra.core.Name("Regexp");

  private StringFacet () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Sequence instance) ;

    R visit(Regexp instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(StringFacet instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Sequence instance) {
      return otherwise(instance);
    }

    default R visit(Regexp instance) {
      return otherwise(instance);
    }
  }

  public static final class Sequence extends hydra.ext.io.shex.syntax.StringFacet implements Serializable {
    public final hydra.ext.io.shex.syntax.StringFacet_Sequence value;

    public Sequence (hydra.ext.io.shex.syntax.StringFacet_Sequence value) {
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
    public int compareTo(StringFacet other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Sequence o = (Sequence) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Regexp extends hydra.ext.io.shex.syntax.StringFacet implements Serializable {
    public final hydra.ext.io.shex.syntax.Regexp value;

    public Regexp (hydra.ext.io.shex.syntax.Regexp value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Regexp)) {
        return false;
      }
      Regexp o = (Regexp) other;
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
    public int compareTo(StringFacet other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Regexp o = (Regexp) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
