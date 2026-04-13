// Note: this is an automatically generated file. Do not edit.

package hydra.lisp.syntax;

import java.io.Serializable;

/**
 * The style of a comment
 */
public abstract class CommentStyle implements Serializable, Comparable<CommentStyle> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.lisp.syntax.CommentStyle");

  public static final hydra.core.Name LINE = new hydra.core.Name("line");

  public static final hydra.core.Name BLOCK = new hydra.core.Name("block");

  public static final hydra.core.Name DATUM = new hydra.core.Name("datum");

  private CommentStyle () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Line instance) ;

    R visit(Block instance) ;

    R visit(Datum instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CommentStyle instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Line instance) {
      return otherwise(instance);
    }

    default R visit(Block instance) {
      return otherwise(instance);
    }

    default R visit(Datum instance) {
      return otherwise(instance);
    }
  }

  public static final class Line extends hydra.lisp.syntax.CommentStyle implements Serializable {
    public Line () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Line)) {
        return false;
      }
      Line o = (Line) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CommentStyle other) {
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

  public static final class Block extends hydra.lisp.syntax.CommentStyle implements Serializable {
    public Block () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Block)) {
        return false;
      }
      Block o = (Block) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CommentStyle other) {
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

  public static final class Datum extends hydra.lisp.syntax.CommentStyle implements Serializable {
    public Datum () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Datum)) {
        return false;
      }
      Datum o = (Datum) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CommentStyle other) {
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
