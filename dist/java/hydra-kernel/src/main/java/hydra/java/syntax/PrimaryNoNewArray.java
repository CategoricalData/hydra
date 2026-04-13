// Note: this is an automatically generated file. Do not edit.

package hydra.java.syntax;

import java.io.Serializable;

public abstract class PrimaryNoNewArray implements Serializable, Comparable<PrimaryNoNewArray> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.java.syntax.PrimaryNoNewArray");

  public static final hydra.core.Name LITERAL = new hydra.core.Name("literal");

  public static final hydra.core.Name CLASS_LITERAL = new hydra.core.Name("classLiteral");

  public static final hydra.core.Name THIS = new hydra.core.Name("this");

  public static final hydra.core.Name DOT_THIS = new hydra.core.Name("dotThis");

  public static final hydra.core.Name PARENS = new hydra.core.Name("parens");

  public static final hydra.core.Name CLASS_INSTANCE = new hydra.core.Name("classInstance");

  public static final hydra.core.Name FIELD_ACCESS = new hydra.core.Name("fieldAccess");

  public static final hydra.core.Name ARRAY_ACCESS = new hydra.core.Name("arrayAccess");

  public static final hydra.core.Name METHOD_INVOCATION = new hydra.core.Name("methodInvocation");

  public static final hydra.core.Name METHOD_REFERENCE = new hydra.core.Name("methodReference");

  private PrimaryNoNewArray () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Literal instance) ;

    R visit(ClassLiteral instance) ;

    R visit(This instance) ;

    R visit(DotThis instance) ;

    R visit(Parens instance) ;

    R visit(ClassInstance instance) ;

    R visit(FieldAccess instance) ;

    R visit(ArrayAccess instance) ;

    R visit(MethodInvocation instance) ;

    R visit(MethodReference instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PrimaryNoNewArray instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Literal instance) {
      return otherwise(instance);
    }

    default R visit(ClassLiteral instance) {
      return otherwise(instance);
    }

    default R visit(This instance) {
      return otherwise(instance);
    }

    default R visit(DotThis instance) {
      return otherwise(instance);
    }

    default R visit(Parens instance) {
      return otherwise(instance);
    }

    default R visit(ClassInstance instance) {
      return otherwise(instance);
    }

    default R visit(FieldAccess instance) {
      return otherwise(instance);
    }

    default R visit(ArrayAccess instance) {
      return otherwise(instance);
    }

    default R visit(MethodInvocation instance) {
      return otherwise(instance);
    }

    default R visit(MethodReference instance) {
      return otherwise(instance);
    }
  }

  public static final class Literal extends hydra.java.syntax.PrimaryNoNewArray implements Serializable {
    public final hydra.java.syntax.Literal value;

    public Literal (hydra.java.syntax.Literal value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Literal)) {
        return false;
      }
      Literal o = (Literal) other;
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
    public int compareTo(PrimaryNoNewArray other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Literal o = (Literal) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ClassLiteral extends hydra.java.syntax.PrimaryNoNewArray implements Serializable {
    public final hydra.java.syntax.ClassLiteral value;

    public ClassLiteral (hydra.java.syntax.ClassLiteral value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ClassLiteral)) {
        return false;
      }
      ClassLiteral o = (ClassLiteral) other;
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
    public int compareTo(PrimaryNoNewArray other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ClassLiteral o = (ClassLiteral) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class This extends hydra.java.syntax.PrimaryNoNewArray implements Serializable {
    public This () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof This)) {
        return false;
      }
      This o = (This) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(PrimaryNoNewArray other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class DotThis extends hydra.java.syntax.PrimaryNoNewArray implements Serializable {
    public final hydra.java.syntax.TypeName value;

    public DotThis (hydra.java.syntax.TypeName value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DotThis)) {
        return false;
      }
      DotThis o = (DotThis) other;
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
    public int compareTo(PrimaryNoNewArray other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DotThis o = (DotThis) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Parens extends hydra.java.syntax.PrimaryNoNewArray implements Serializable {
    public final hydra.java.syntax.Expression value;

    public Parens (hydra.java.syntax.Expression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parens)) {
        return false;
      }
      Parens o = (Parens) other;
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
    public int compareTo(PrimaryNoNewArray other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Parens o = (Parens) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ClassInstance extends hydra.java.syntax.PrimaryNoNewArray implements Serializable {
    public final hydra.java.syntax.ClassInstanceCreationExpression value;

    public ClassInstance (hydra.java.syntax.ClassInstanceCreationExpression value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ClassInstance)) {
        return false;
      }
      ClassInstance o = (ClassInstance) other;
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
    public int compareTo(PrimaryNoNewArray other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ClassInstance o = (ClassInstance) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class FieldAccess extends hydra.java.syntax.PrimaryNoNewArray implements Serializable {
    public final hydra.java.syntax.FieldAccess value;

    public FieldAccess (hydra.java.syntax.FieldAccess value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FieldAccess)) {
        return false;
      }
      FieldAccess o = (FieldAccess) other;
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
    public int compareTo(PrimaryNoNewArray other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      FieldAccess o = (FieldAccess) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ArrayAccess extends hydra.java.syntax.PrimaryNoNewArray implements Serializable {
    public final hydra.java.syntax.ArrayAccess value;

    public ArrayAccess (hydra.java.syntax.ArrayAccess value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ArrayAccess)) {
        return false;
      }
      ArrayAccess o = (ArrayAccess) other;
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
    public int compareTo(PrimaryNoNewArray other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ArrayAccess o = (ArrayAccess) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class MethodInvocation extends hydra.java.syntax.PrimaryNoNewArray implements Serializable {
    public final hydra.java.syntax.MethodInvocation value;

    public MethodInvocation (hydra.java.syntax.MethodInvocation value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MethodInvocation)) {
        return false;
      }
      MethodInvocation o = (MethodInvocation) other;
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
    public int compareTo(PrimaryNoNewArray other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      MethodInvocation o = (MethodInvocation) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class MethodReference extends hydra.java.syntax.PrimaryNoNewArray implements Serializable {
    public final hydra.java.syntax.MethodReference value;

    public MethodReference (hydra.java.syntax.MethodReference value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MethodReference)) {
        return false;
      }
      MethodReference o = (MethodReference) other;
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
    public int compareTo(PrimaryNoNewArray other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      MethodReference o = (MethodReference) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
