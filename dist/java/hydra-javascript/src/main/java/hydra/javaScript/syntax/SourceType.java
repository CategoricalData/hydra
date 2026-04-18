// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * Whether the program is a module or script
 */
public abstract class SourceType implements Serializable, Comparable<SourceType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.SourceType");

  public static final hydra.core.Name MODULE = new hydra.core.Name("module");

  public static final hydra.core.Name SCRIPT = new hydra.core.Name("script");

  private SourceType () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Module instance) ;

    R visit(Script instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SourceType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Module instance) {
      return otherwise(instance);
    }

    default R visit(Script instance) {
      return otherwise(instance);
    }
  }

  public static final class Module extends hydra.javaScript.syntax.SourceType implements Serializable {
    public Module () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Module)) {
        return false;
      }
      Module o = (Module) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SourceType other) {
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

  public static final class Script extends hydra.javaScript.syntax.SourceType implements Serializable {
    public Script () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Script)) {
        return false;
      }
      Script o = (Script) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SourceType other) {
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
