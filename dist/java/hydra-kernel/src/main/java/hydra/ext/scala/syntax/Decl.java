// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public abstract class Decl implements Serializable, Comparable<Decl> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Decl");

  public static final hydra.core.Name VAL = new hydra.core.Name("val");

  public static final hydra.core.Name VAR = new hydra.core.Name("var");

  public static final hydra.core.Name DEF = new hydra.core.Name("def");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name GIVEN = new hydra.core.Name("given");

  private Decl () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Val instance) ;

    R visit(Var instance) ;

    R visit(Def instance) ;

    R visit(Type instance) ;

    R visit(Given instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Decl instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Val instance) {
      return otherwise(instance);
    }

    default R visit(Var instance) {
      return otherwise(instance);
    }

    default R visit(Def instance) {
      return otherwise(instance);
    }

    default R visit(Type instance) {
      return otherwise(instance);
    }

    default R visit(Given instance) {
      return otherwise(instance);
    }
  }

  public static final class Val extends hydra.ext.scala.syntax.Decl implements Serializable {
    public final hydra.ext.scala.syntax.Decl_Val value;

    public Val (hydra.ext.scala.syntax.Decl_Val value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Val)) {
        return false;
      }
      Val o = (Val) other;
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
    public int compareTo(Decl other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Val o = (Val) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Var extends hydra.ext.scala.syntax.Decl implements Serializable {
    public final hydra.ext.scala.syntax.Decl_Var value;

    public Var (hydra.ext.scala.syntax.Decl_Var value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Var)) {
        return false;
      }
      Var o = (Var) other;
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
    public int compareTo(Decl other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Var o = (Var) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Def extends hydra.ext.scala.syntax.Decl implements Serializable {
    public final hydra.ext.scala.syntax.Decl_Def value;

    public Def (hydra.ext.scala.syntax.Decl_Def value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Def)) {
        return false;
      }
      Def o = (Def) other;
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
    public int compareTo(Decl other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Def o = (Def) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Type extends hydra.ext.scala.syntax.Decl implements Serializable {
    public final hydra.ext.scala.syntax.Decl_Type value;

    public Type (hydra.ext.scala.syntax.Decl_Type value) {
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
    public int compareTo(Decl other) {
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

  public static final class Given extends hydra.ext.scala.syntax.Decl implements Serializable {
    public final hydra.ext.scala.syntax.Decl_Given value;

    public Given (hydra.ext.scala.syntax.Decl_Given value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Given)) {
        return false;
      }
      Given o = (Given) other;
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
    public int compareTo(Decl other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Given o = (Given) other;
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
