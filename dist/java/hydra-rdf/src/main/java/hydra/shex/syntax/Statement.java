// Note: this is an automatically generated file. Do not edit.

package hydra.shex.syntax;

import java.io.Serializable;

public abstract class Statement implements Serializable, Comparable<Statement> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.shex.syntax.Statement");

  public static final hydra.core.Name DIRECTIVE = new hydra.core.Name("Directive");

  public static final hydra.core.Name NOT_START_ACTION = new hydra.core.Name("NotStartAction");

  private Statement () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Directive instance) ;

    R visit(NotStartAction instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Statement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Directive instance) {
      return otherwise(instance);
    }

    default R visit(NotStartAction instance) {
      return otherwise(instance);
    }
  }

  public static final class Directive extends hydra.shex.syntax.Statement implements Serializable {
    public final hydra.shex.syntax.Directive value;

    public Directive (hydra.shex.syntax.Directive value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Directive)) {
        return false;
      }
      Directive o = (Directive) other;
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
    public int compareTo(Statement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Directive o = (Directive) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class NotStartAction extends hydra.shex.syntax.Statement implements Serializable {
    public final hydra.shex.syntax.NotStartAction value;

    public NotStartAction (hydra.shex.syntax.NotStartAction value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NotStartAction)) {
        return false;
      }
      NotStartAction o = (NotStartAction) other;
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
    public int compareTo(Statement other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NotStartAction o = (NotStartAction) other;
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
