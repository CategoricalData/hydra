// Note: this is an automatically generated file. Do not edit.

package hydra.packaging;

import java.io.Serializable;

/**
 * A definition, which may be either a term or type definition
 */
public abstract class Definition implements Serializable, Comparable<Definition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.packaging.Definition");

  public static final hydra.core.Name TERM = new hydra.core.Name("term");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  private Definition () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Term instance) ;

    R visit(Type instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Definition instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Term instance) {
      return otherwise(instance);
    }

    default R visit(Type instance) {
      return otherwise(instance);
    }
  }

  /**
   * A term definition
   */
  public static final class Term extends hydra.packaging.Definition implements Serializable {
    public final hydra.packaging.TermDefinition value;

    public Term (hydra.packaging.TermDefinition value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Term)) {
        return false;
      }
      Term o = (Term) other;
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
    public int compareTo(Definition other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Term o = (Term) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A type definition
   */
  public static final class Type extends hydra.packaging.Definition implements Serializable {
    public final hydra.packaging.TypeDefinition value;

    public Type (hydra.packaging.TypeDefinition value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Type)) {
        return false;
      }
      Type o = (Type) other;
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
    public int compareTo(Definition other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Type o = (Type) other;
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
