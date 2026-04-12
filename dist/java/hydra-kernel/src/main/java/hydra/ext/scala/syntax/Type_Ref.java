// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public abstract class Type_Ref implements Serializable, Comparable<Type_Ref> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Type_Ref");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name SELECT = new hydra.core.Name("select");

  public static final hydra.core.Name PROJECT = new hydra.core.Name("project");

  public static final hydra.core.Name SINGLETON = new hydra.core.Name("singleton");

  private Type_Ref () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Name instance) ;

    R visit(Select instance) ;

    R visit(Project instance) ;

    R visit(Singleton instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Type_Ref instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Name instance) {
      return otherwise(instance);
    }

    default R visit(Select instance) {
      return otherwise(instance);
    }

    default R visit(Project instance) {
      return otherwise(instance);
    }

    default R visit(Singleton instance) {
      return otherwise(instance);
    }
  }

  public static final class Name extends hydra.ext.scala.syntax.Type_Ref implements Serializable {
    public final hydra.ext.scala.syntax.Type_Name value;

    public Name (hydra.ext.scala.syntax.Type_Name value) {
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
    public int compareTo(Type_Ref other) {
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

  public static final class Select extends hydra.ext.scala.syntax.Type_Ref implements Serializable {
    public final hydra.ext.scala.syntax.Type_Select value;

    public Select (hydra.ext.scala.syntax.Type_Select value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Select)) {
        return false;
      }
      Select o = (Select) other;
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
    public int compareTo(Type_Ref other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Select o = (Select) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Project extends hydra.ext.scala.syntax.Type_Ref implements Serializable {
    public final hydra.ext.scala.syntax.Type_Project value;

    public Project (hydra.ext.scala.syntax.Type_Project value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Project)) {
        return false;
      }
      Project o = (Project) other;
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
    public int compareTo(Type_Ref other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Project o = (Project) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Singleton extends hydra.ext.scala.syntax.Type_Ref implements Serializable {
    public final hydra.ext.scala.syntax.Type_Singleton value;

    public Singleton (hydra.ext.scala.syntax.Type_Singleton value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Singleton)) {
        return false;
      }
      Singleton o = (Singleton) other;
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
    public int compareTo(Type_Ref other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Singleton o = (Singleton) other;
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
