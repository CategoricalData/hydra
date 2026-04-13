// Note: this is an automatically generated file. Do not edit.

package hydra.shex.syntax;

import java.io.Serializable;

public abstract class Directive implements Serializable, Comparable<Directive> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.shex.syntax.Directive");

  public static final hydra.core.Name BASE_DECL = new hydra.core.Name("BaseDecl");

  public static final hydra.core.Name PREFIX_DECL = new hydra.core.Name("PrefixDecl");

  private Directive () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(BaseDecl instance) ;

    R visit(PrefixDecl instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Directive instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(BaseDecl instance) {
      return otherwise(instance);
    }

    default R visit(PrefixDecl instance) {
      return otherwise(instance);
    }
  }

  public static final class BaseDecl extends hydra.shex.syntax.Directive implements Serializable {
    public final hydra.shex.syntax.BaseDecl value;

    public BaseDecl (hydra.shex.syntax.BaseDecl value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BaseDecl)) {
        return false;
      }
      BaseDecl o = (BaseDecl) other;
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
    public int compareTo(Directive other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      BaseDecl o = (BaseDecl) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class PrefixDecl extends hydra.shex.syntax.Directive implements Serializable {
    public final hydra.shex.syntax.PrefixDecl value;

    public PrefixDecl (hydra.shex.syntax.PrefixDecl value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PrefixDecl)) {
        return false;
      }
      PrefixDecl o = (PrefixDecl) other;
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
    public int compareTo(Directive other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PrefixDecl o = (PrefixDecl) other;
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
