// Note: this is an automatically generated file. Do not edit.

package hydra.owl.syntax;

import java.io.Serializable;

public abstract class ObjectPropertyExpression implements Serializable, Comparable<ObjectPropertyExpression> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.owl.syntax.ObjectPropertyExpression");

  public static final hydra.core.Name OBJECT = new hydra.core.Name("object");

  public static final hydra.core.Name INVERSE_OBJECT = new hydra.core.Name("inverseObject");

  private ObjectPropertyExpression () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Object_ instance) ;

    R visit(InverseObject instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ObjectPropertyExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Object_ instance) {
      return otherwise(instance);
    }

    default R visit(InverseObject instance) {
      return otherwise(instance);
    }
  }

  public static final class Object_ extends hydra.owl.syntax.ObjectPropertyExpression implements Serializable {
    public final hydra.owl.syntax.ObjectProperty value;

    public Object_ (hydra.owl.syntax.ObjectProperty value) {
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
    public int compareTo(ObjectPropertyExpression other) {
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

  public static final class InverseObject extends hydra.owl.syntax.ObjectPropertyExpression implements Serializable {
    public final hydra.owl.syntax.InverseObjectProperty value;

    public InverseObject (hydra.owl.syntax.InverseObjectProperty value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InverseObject)) {
        return false;
      }
      InverseObject o = (InverseObject) other;
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
    public int compareTo(ObjectPropertyExpression other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      InverseObject o = (InverseObject) other;
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
