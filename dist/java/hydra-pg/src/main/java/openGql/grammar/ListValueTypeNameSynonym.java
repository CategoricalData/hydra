// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class ListValueTypeNameSynonym implements Serializable, Comparable<ListValueTypeNameSynonym> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ListValueTypeNameSynonym");

  public static final hydra.core.Name LIST = new hydra.core.Name("list");

  public static final hydra.core.Name ARRAY = new hydra.core.Name("array");

  private ListValueTypeNameSynonym () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(List instance) ;

    R visit(Array instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ListValueTypeNameSynonym instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(List instance) {
      return otherwise(instance);
    }

    default R visit(Array instance) {
      return otherwise(instance);
    }
  }

  public static final class List extends openGql.grammar.ListValueTypeNameSynonym implements Serializable {
    public List () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof List)) {
        return false;
      }
      List o = (List) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ListValueTypeNameSynonym other) {
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

  public static final class Array extends openGql.grammar.ListValueTypeNameSynonym implements Serializable {
    public Array () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Array)) {
        return false;
      }
      Array o = (Array) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ListValueTypeNameSynonym other) {
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
