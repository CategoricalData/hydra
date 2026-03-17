// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class TraversalFunction implements Serializable, Comparable<TraversalFunction> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalFunction");

  public static final hydra.core.Name TOKEN = new hydra.core.Name("token");

  public static final hydra.core.Name COLUMN = new hydra.core.Name("column");

  private TraversalFunction () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Token instance) ;

    R visit(Column instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TraversalFunction instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Token instance) {
      return otherwise(instance);
    }

    default R visit(Column instance) {
      return otherwise(instance);
    }
  }

  public static final class Token extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalFunction implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalToken value;

    public Token (hydra.ext.org.apache.tinkerpop.gremlin.TraversalToken value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Token)) {
        return false;
      }
      Token o = (Token) other;
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
    public int compareTo(TraversalFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Token o = (Token) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Column extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalFunction implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalColumn value;

    public Column (hydra.ext.org.apache.tinkerpop.gremlin.TraversalColumn value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Column)) {
        return false;
      }
      Column o = (Column) other;
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
    public int compareTo(TraversalFunction other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Column o = (Column) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
