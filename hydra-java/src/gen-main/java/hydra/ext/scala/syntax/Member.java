// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public abstract class Member implements Serializable, Comparable<Member> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Member");

  public static final hydra.core.Name TERM = new hydra.core.Name("term");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name TERM_PARAM = new hydra.core.Name("termParam");

  public static final hydra.core.Name TYPE_PARAM = new hydra.core.Name("typeParam");

  public static final hydra.core.Name SELF = new hydra.core.Name("self");

  private Member () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Term instance) ;

    R visit(Type instance) ;

    R visit(TermParam instance) ;

    R visit(TypeParam instance) ;

    R visit(Self instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Member instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Term instance) {
      return otherwise(instance);
    }

    default R visit(Type instance) {
      return otherwise(instance);
    }

    default R visit(TermParam instance) {
      return otherwise(instance);
    }

    default R visit(TypeParam instance) {
      return otherwise(instance);
    }

    default R visit(Self instance) {
      return otherwise(instance);
    }
  }

  public static final class Term extends hydra.ext.scala.syntax.Member implements Serializable {
    public final hydra.ext.scala.syntax.Member_Data value;

    public Term (hydra.ext.scala.syntax.Member_Data value) {
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
    public int compareTo(Member other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Term o = (Term) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Type extends hydra.ext.scala.syntax.Member implements Serializable {
    public final hydra.ext.scala.syntax.Member_Type value;

    public Type (hydra.ext.scala.syntax.Member_Type value) {
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
    public int compareTo(Member other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Type o = (Type) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class TermParam extends hydra.ext.scala.syntax.Member implements Serializable {
    public final hydra.ext.scala.syntax.Data_Param value;

    public TermParam (hydra.ext.scala.syntax.Data_Param value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TermParam)) {
        return false;
      }
      TermParam o = (TermParam) other;
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
    public int compareTo(Member other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TermParam o = (TermParam) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class TypeParam extends hydra.ext.scala.syntax.Member implements Serializable {
    public final hydra.ext.scala.syntax.Type_Param value;

    public TypeParam (hydra.ext.scala.syntax.Type_Param value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeParam)) {
        return false;
      }
      TypeParam o = (TypeParam) other;
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
    public int compareTo(Member other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TypeParam o = (TypeParam) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Self extends hydra.ext.scala.syntax.Member implements Serializable {
    public final hydra.ext.scala.syntax.Self value;

    public Self (hydra.ext.scala.syntax.Self value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Self)) {
        return false;
      }
      Self o = (Self) other;
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
    public int compareTo(Member other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Self o = (Self) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
