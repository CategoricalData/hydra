// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public abstract class Application implements Serializable, Comparable<Application> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Application");

  public static final hydra.core.Name NORMAL = new hydra.core.Name("normal");

  public static final hydra.core.Name ANNOTATED = new hydra.core.Name("annotated");

  private Application () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Normal instance) ;

    R visit(Annotated instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Application instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Normal instance) {
      return otherwise(instance);
    }

    default R visit(Annotated instance) {
      return otherwise(instance);
    }
  }

  public static final class Normal extends hydra.coq.syntax.Application implements Serializable {
    public final hydra.coq.syntax.NormalApplication value;

    public Normal (hydra.coq.syntax.NormalApplication value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Normal)) {
        return false;
      }
      Normal o = (Normal) other;
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
    public int compareTo(Application other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Normal o = (Normal) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Annotated extends hydra.coq.syntax.Application implements Serializable {
    public final hydra.coq.syntax.AnnotatedApplication value;

    public Annotated (hydra.coq.syntax.AnnotatedApplication value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Annotated)) {
        return false;
      }
      Annotated o = (Annotated) other;
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
    public int compareTo(Application other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Annotated o = (Annotated) other;
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
