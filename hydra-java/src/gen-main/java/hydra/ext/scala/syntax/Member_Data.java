// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.syntax;

import java.io.Serializable;

public abstract class Member_Data implements Serializable, Comparable<Member_Data> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.syntax.Member_Data");

  public static final hydra.core.Name PKG = new hydra.core.Name("pkg");

  public static final hydra.core.Name OBJECT = new hydra.core.Name("object");

  private Member_Data () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Pkg instance) ;

    R visit(Object_ instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Member_Data instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Pkg instance) {
      return otherwise(instance);
    }

    default R visit(Object_ instance) {
      return otherwise(instance);
    }
  }

  public static final class Pkg extends hydra.ext.scala.syntax.Member_Data implements Serializable {
    public final hydra.ext.scala.syntax.Pkg value;

    public Pkg (hydra.ext.scala.syntax.Pkg value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Pkg)) {
        return false;
      }
      Pkg o = (Pkg) other;
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
    public int compareTo(Member_Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Pkg o = (Pkg) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Object_ extends hydra.ext.scala.syntax.Member_Data implements Serializable {
    public final hydra.ext.scala.syntax.Pkg_Object value;

    public Object_ (hydra.ext.scala.syntax.Pkg_Object value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Object_)) {
        return false;
      }
      Object_ o = (Object_) other;
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
    public int compareTo(Member_Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Object_ o = (Object_) other;
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
