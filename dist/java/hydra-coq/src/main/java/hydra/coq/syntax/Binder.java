// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

public abstract class Binder implements Serializable, Comparable<Binder> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.Binder");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name TERM = new hydra.core.Name("term");

  public static final hydra.core.Name IMPLICIT = new hydra.core.Name("implicit");

  public static final hydra.core.Name GENERALIZING = new hydra.core.Name("generalizing");

  public static final hydra.core.Name PATTERN = new hydra.core.Name("pattern");

  private Binder () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Name instance) ;

    R visit(Type instance) ;

    R visit(Term instance) ;

    R visit(Implicit instance) ;

    R visit(Generalizing instance) ;

    R visit(Pattern instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Binder instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Name instance) {
      return otherwise(instance);
    }

    default R visit(Type instance) {
      return otherwise(instance);
    }

    default R visit(Term instance) {
      return otherwise(instance);
    }

    default R visit(Implicit instance) {
      return otherwise(instance);
    }

    default R visit(Generalizing instance) {
      return otherwise(instance);
    }

    default R visit(Pattern instance) {
      return otherwise(instance);
    }
  }

  public static final class Name extends hydra.coq.syntax.Binder implements Serializable {
    public final hydra.coq.syntax.Name value;

    public Name (hydra.coq.syntax.Name value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Name)) {
        return false;
      }
      Name o = (Name) other;
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
    public int compareTo(Binder other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Name o = (Name) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Type extends hydra.coq.syntax.Binder implements Serializable {
    public final hydra.coq.syntax.TypeBinders value;

    public Type (hydra.coq.syntax.TypeBinders value) {
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
    public int compareTo(Binder other) {
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

  public static final class Term extends hydra.coq.syntax.Binder implements Serializable {
    public final hydra.coq.syntax.LetBinder value;

    public Term (hydra.coq.syntax.LetBinder value) {
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
    public int compareTo(Binder other) {
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

  public static final class Implicit extends hydra.coq.syntax.Binder implements Serializable {
    public final hydra.coq.syntax.ImplicitBinders value;

    public Implicit (hydra.coq.syntax.ImplicitBinders value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Implicit)) {
        return false;
      }
      Implicit o = (Implicit) other;
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
    public int compareTo(Binder other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Implicit o = (Implicit) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Generalizing extends hydra.coq.syntax.Binder implements Serializable {
    public final hydra.coq.syntax.GeneralizingBinder value;

    public Generalizing (hydra.coq.syntax.GeneralizingBinder value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Generalizing)) {
        return false;
      }
      Generalizing o = (Generalizing) other;
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
    public int compareTo(Binder other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Generalizing o = (Generalizing) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Pattern extends hydra.coq.syntax.Binder implements Serializable {
    public final hydra.coq.syntax.Pattern0 value;

    public Pattern (hydra.coq.syntax.Pattern0 value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Pattern)) {
        return false;
      }
      Pattern o = (Pattern) other;
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
    public int compareTo(Binder other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Pattern o = (Pattern) other;
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
