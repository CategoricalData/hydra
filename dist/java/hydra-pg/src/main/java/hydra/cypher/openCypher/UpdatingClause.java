// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.openCypher;

import java.io.Serializable;

public abstract class UpdatingClause implements Serializable, Comparable<UpdatingClause> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.openCypher.UpdatingClause");

  public static final hydra.core.Name CREATE = new hydra.core.Name("create");

  public static final hydra.core.Name MERGE = new hydra.core.Name("merge");

  public static final hydra.core.Name DELETE = new hydra.core.Name("delete");

  public static final hydra.core.Name SET = new hydra.core.Name("set");

  public static final hydra.core.Name REMOVE = new hydra.core.Name("remove");

  private UpdatingClause () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Create instance) ;

    R visit(Merge instance) ;

    R visit(Delete instance) ;

    R visit(Set instance) ;

    R visit(Remove instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(UpdatingClause instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Create instance) {
      return otherwise(instance);
    }

    default R visit(Merge instance) {
      return otherwise(instance);
    }

    default R visit(Delete instance) {
      return otherwise(instance);
    }

    default R visit(Set instance) {
      return otherwise(instance);
    }

    default R visit(Remove instance) {
      return otherwise(instance);
    }
  }

  public static final class Create extends hydra.cypher.openCypher.UpdatingClause implements Serializable {
    public final hydra.cypher.openCypher.Create value;

    public Create (hydra.cypher.openCypher.Create value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Create)) {
        return false;
      }
      Create o = (Create) other;
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
    public int compareTo(UpdatingClause other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Create o = (Create) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Merge extends hydra.cypher.openCypher.UpdatingClause implements Serializable {
    public final hydra.cypher.openCypher.Merge value;

    public Merge (hydra.cypher.openCypher.Merge value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Merge)) {
        return false;
      }
      Merge o = (Merge) other;
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
    public int compareTo(UpdatingClause other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Merge o = (Merge) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Delete extends hydra.cypher.openCypher.UpdatingClause implements Serializable {
    public final hydra.cypher.openCypher.Delete value;

    public Delete (hydra.cypher.openCypher.Delete value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Delete)) {
        return false;
      }
      Delete o = (Delete) other;
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
    public int compareTo(UpdatingClause other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Delete o = (Delete) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Set extends hydra.cypher.openCypher.UpdatingClause implements Serializable {
    public final hydra.cypher.openCypher.Set value;

    public Set (hydra.cypher.openCypher.Set value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Set)) {
        return false;
      }
      Set o = (Set) other;
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
    public int compareTo(UpdatingClause other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Set o = (Set) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Remove extends hydra.cypher.openCypher.UpdatingClause implements Serializable {
    public final hydra.cypher.openCypher.Remove value;

    public Remove (hydra.cypher.openCypher.Remove value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Remove)) {
        return false;
      }
      Remove o = (Remove) other;
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
    public int compareTo(UpdatingClause other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Remove o = (Remove) other;
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
