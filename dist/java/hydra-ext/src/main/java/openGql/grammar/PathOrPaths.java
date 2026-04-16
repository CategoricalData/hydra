// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class PathOrPaths implements Serializable, Comparable<PathOrPaths> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.PathOrPaths");

  public static final hydra.core.Name PATH = new hydra.core.Name("path");

  public static final hydra.core.Name PATHS = new hydra.core.Name("paths");

  private PathOrPaths () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Path instance) ;

    R visit(Paths instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(PathOrPaths instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Path instance) {
      return otherwise(instance);
    }

    default R visit(Paths instance) {
      return otherwise(instance);
    }
  }

  public static final class Path extends openGql.grammar.PathOrPaths implements Serializable {
    public Path () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Path)) {
        return false;
      }
      Path o = (Path) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(PathOrPaths other) {
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

  public static final class Paths extends openGql.grammar.PathOrPaths implements Serializable {
    public Paths () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Paths)) {
        return false;
      }
      Paths o = (Paths) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(PathOrPaths other) {
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
