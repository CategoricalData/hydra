// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class TraversalToken implements Serializable, Comparable<TraversalToken> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalToken");

  public static final hydra.core.Name ID = new hydra.core.Name("id");

  public static final hydra.core.Name LABEL = new hydra.core.Name("label");

  public static final hydra.core.Name KEY = new hydra.core.Name("key");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  private TraversalToken () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Id instance) ;

    R visit(Label instance) ;

    R visit(Key instance) ;

    R visit(Value instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TraversalToken instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Id instance) {
      return otherwise(instance);
    }

    default R visit(Label instance) {
      return otherwise(instance);
    }

    default R visit(Key instance) {
      return otherwise(instance);
    }

    default R visit(Value instance) {
      return otherwise(instance);
    }
  }

  public static final class Id extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalToken implements Serializable {
    public Id () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Id)) {
        return false;
      }
      Id o = (Id) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalToken other) {
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

  public static final class Label extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalToken implements Serializable {
    public Label () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Label)) {
        return false;
      }
      Label o = (Label) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalToken other) {
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

  public static final class Key extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalToken implements Serializable {
    public Key () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Key)) {
        return false;
      }
      Key o = (Key) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalToken other) {
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

  public static final class Value extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalToken implements Serializable {
    public Value () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Value)) {
        return false;
      }
      Value o = (Value) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalToken other) {
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
