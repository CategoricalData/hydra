// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public abstract class Fix_Qual implements Serializable, Comparable<Fix_Qual> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Fix_Qual");

  public static final hydra.core.Name IN = new hydra.core.Name("in");

  public static final hydra.core.Name WITH = new hydra.core.Name("with");

  private Fix_Qual () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(In instance) ;

    R visit(With instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Fix_Qual instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(In instance) {
      return otherwise(instance);
    }

    default R visit(With instance) {
      return otherwise(instance);
    }
  }

  public static final class In extends hydra.coq.syntax.Fix_Qual implements Serializable {
    public final hydra.coq.syntax.Term value;

    public In (hydra.coq.syntax.Term value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof In)) {
        return false;
      }
      In o = (In) other;
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
    public int compareTo(Fix_Qual other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      In o = (In) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class With extends hydra.coq.syntax.Fix_Qual implements Serializable {
    public final hydra.coq.syntax.FixWith value;

    public With (hydra.coq.syntax.FixWith value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof With)) {
        return false;
      }
      With o = (With) other;
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
    public int compareTo(Fix_Qual other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      With o = (With) other;
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
