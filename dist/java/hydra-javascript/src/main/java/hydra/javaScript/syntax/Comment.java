// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A JavaScript comment
 */
public abstract class Comment implements Serializable, Comparable<Comment> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.Comment");

  public static final hydra.core.Name LINE = new hydra.core.Name("line");

  public static final hydra.core.Name BLOCK = new hydra.core.Name("block");

  public static final hydra.core.Name DOCUMENTATION = new hydra.core.Name("documentation");

  private Comment () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Line instance) ;

    R visit(Block instance) ;

    R visit(Documentation instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Comment instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Line instance) {
      return otherwise(instance);
    }

    default R visit(Block instance) {
      return otherwise(instance);
    }

    default R visit(Documentation instance) {
      return otherwise(instance);
    }
  }

  /**
   * A single-line comment (// ...)
   */
  public static final class Line extends hydra.javaScript.syntax.Comment implements Serializable {
    public final String value;

    public Line (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Line)) {
        return false;
      }
      Line o = (Line) other;
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
    public int compareTo(Comment other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Line o = (Line) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A block comment (/* ... */)
   */
  public static final class Block extends hydra.javaScript.syntax.Comment implements Serializable {
    public final String value;

    public Block (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Block)) {
        return false;
      }
      Block o = (Block) other;
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
    public int compareTo(Comment other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Block o = (Block) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  /**
   * A documentation comment (/** ... */, i.e. JSDoc)
   */
  public static final class Documentation extends hydra.javaScript.syntax.Comment implements Serializable {
    public final hydra.javaScript.syntax.DocumentationComment value;

    public Documentation (hydra.javaScript.syntax.DocumentationComment value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Documentation)) {
        return false;
      }
      Documentation o = (Documentation) other;
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
    public int compareTo(Comment other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Documentation o = (Documentation) other;
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
