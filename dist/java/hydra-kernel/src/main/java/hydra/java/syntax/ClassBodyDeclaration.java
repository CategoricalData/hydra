// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public abstract class ClassBodyDeclaration implements Serializable, Comparable<ClassBodyDeclaration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.ClassBodyDeclaration");

  public static final hydra.core.Name CLASS_MEMBER = new hydra.core.Name("classMember");

  public static final hydra.core.Name INSTANCE_INITIALIZER = new hydra.core.Name("instanceInitializer");

  public static final hydra.core.Name STATIC_INITIALIZER = new hydra.core.Name("staticInitializer");

  public static final hydra.core.Name CONSTRUCTOR_DECLARATION = new hydra.core.Name("constructorDeclaration");

  private ClassBodyDeclaration () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(ClassMember instance) ;

    R visit(InstanceInitializer instance) ;

    R visit(StaticInitializer instance) ;

    R visit(ConstructorDeclaration instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ClassBodyDeclaration instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(ClassMember instance) {
      return otherwise(instance);
    }

    default R visit(InstanceInitializer instance) {
      return otherwise(instance);
    }

    default R visit(StaticInitializer instance) {
      return otherwise(instance);
    }

    default R visit(ConstructorDeclaration instance) {
      return otherwise(instance);
    }
  }

  public static final class ClassMember extends hydra.java.syntax.ClassBodyDeclaration implements Serializable {
    public final hydra.java.syntax.ClassMemberDeclaration value;

    public ClassMember (hydra.java.syntax.ClassMemberDeclaration value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ClassMember)) {
        return false;
      }
      ClassMember o = (ClassMember) other;
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
    public int compareTo(ClassBodyDeclaration other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ClassMember o = (ClassMember) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class InstanceInitializer extends hydra.java.syntax.ClassBodyDeclaration implements Serializable {
    public final hydra.java.syntax.InstanceInitializer value;

    public InstanceInitializer (hydra.java.syntax.InstanceInitializer value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InstanceInitializer)) {
        return false;
      }
      InstanceInitializer o = (InstanceInitializer) other;
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
    public int compareTo(ClassBodyDeclaration other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      InstanceInitializer o = (InstanceInitializer) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class StaticInitializer extends hydra.java.syntax.ClassBodyDeclaration implements Serializable {
    public final hydra.java.syntax.StaticInitializer value;

    public StaticInitializer (hydra.java.syntax.StaticInitializer value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StaticInitializer)) {
        return false;
      }
      StaticInitializer o = (StaticInitializer) other;
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
    public int compareTo(ClassBodyDeclaration other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      StaticInitializer o = (StaticInitializer) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ConstructorDeclaration extends hydra.java.syntax.ClassBodyDeclaration implements Serializable {
    public final hydra.java.syntax.ConstructorDeclaration value;

    public ConstructorDeclaration (hydra.java.syntax.ConstructorDeclaration value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ConstructorDeclaration)) {
        return false;
      }
      ConstructorDeclaration o = (ConstructorDeclaration) other;
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
    public int compareTo(ClassBodyDeclaration other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ConstructorDeclaration o = (ConstructorDeclaration) other;
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
