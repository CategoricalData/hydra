// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public abstract class Fix implements Serializable, Comparable<Fix> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Fix");

  public static final hydra.core.Name DECL = new hydra.core.Name("decl");

  public static final hydra.core.Name QUAL = new hydra.core.Name("qual");

  private Fix () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Decl instance) ;

    R visit(Qual instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Fix instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Decl instance) {
      return otherwise(instance);
    }

    default R visit(Qual instance) {
      return otherwise(instance);
    }
  }

  public static final class Decl extends hydra.coq.syntax.Fix implements Serializable {
    public final hydra.coq.syntax.Fix_Decl value;

    public Decl (hydra.coq.syntax.Fix_Decl value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Decl)) {
        return false;
      }
      Decl o = (Decl) other;
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
    public int compareTo(Fix other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Decl o = (Decl) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Qual extends hydra.coq.syntax.Fix implements Serializable {
    public final hydra.util.Maybe<hydra.coq.syntax.Fix_Qual> value;

    public Qual (hydra.util.Maybe<hydra.coq.syntax.Fix_Qual> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Qual)) {
        return false;
      }
      Qual o = (Qual) other;
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
    public int compareTo(Fix other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Qual o = (Qual) other;
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
