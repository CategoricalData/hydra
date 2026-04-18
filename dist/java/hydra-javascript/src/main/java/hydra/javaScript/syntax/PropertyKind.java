// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * The kind of an object property
 */
public abstract class PropertyKind implements Serializable, Comparable<PropertyKind> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.PropertyKind");

  public static final hydra.core.Name INIT = new hydra.core.Name("init");

  public static final hydra.core.Name GET = new hydra.core.Name("get");

  public static final hydra.core.Name SET = new hydra.core.Name("set");

  private PropertyKind () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Init instance) ;

    R visit(Get instance) ;

    R visit(Set instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PropertyKind instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Init instance) {
      return otherwise(instance);
    }

    default R visit(Get instance) {
      return otherwise(instance);
    }

    default R visit(Set instance) {
      return otherwise(instance);
    }
  }

  /**
   * A normal property initialization
   */
  public static final class Init extends hydra.javaScript.syntax.PropertyKind implements Serializable {
    public Init () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Init)) {
        return false;
      }
      Init o = (Init) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(PropertyKind other) {
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

  /**
   * A getter
   */
  public static final class Get extends hydra.javaScript.syntax.PropertyKind implements Serializable {
    public Get () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Get)) {
        return false;
      }
      Get o = (Get) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(PropertyKind other) {
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

  /**
   * A setter
   */
  public static final class Set extends hydra.javaScript.syntax.PropertyKind implements Serializable {
    public Set () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Set)) {
        return false;
      }
      Set o = (Set) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(PropertyKind other) {
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
}
