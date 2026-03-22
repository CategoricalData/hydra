// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public abstract class SortOrder implements Serializable, Comparable<SortOrder> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.SortOrder");

  public static final hydra.core.Name ASCENDING = new hydra.core.Name("ascending");

  public static final hydra.core.Name DESCENDING = new hydra.core.Name("descending");

  private SortOrder () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Ascending instance) ;

    R visit(Descending instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SortOrder instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Ascending instance) {
      return otherwise(instance);
    }

    default R visit(Descending instance) {
      return otherwise(instance);
    }
  }

  public static final class Ascending extends hydra.ext.cypher.openCypher.SortOrder implements Serializable {
    public Ascending () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ascending)) {
        return false;
      }
      Ascending o = (Ascending) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SortOrder other) {
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

  public static final class Descending extends hydra.ext.cypher.openCypher.SortOrder implements Serializable {
    public Descending () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Descending)) {
        return false;
      }
      Descending o = (Descending) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SortOrder other) {
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
