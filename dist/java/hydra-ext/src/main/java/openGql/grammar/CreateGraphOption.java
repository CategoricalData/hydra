// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class CreateGraphOption implements Serializable, Comparable<CreateGraphOption> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.CreateGraphOption");

  public static final hydra.core.Name GRAPH_IF_NOT_EXISTS = new hydra.core.Name("graphIfNotExists");

  public static final hydra.core.Name OR_REPLACE = new hydra.core.Name("orReplace");

  private CreateGraphOption () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(GraphIfNotExists instance) ;

    R visit(OrReplace instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CreateGraphOption instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(GraphIfNotExists instance) {
      return otherwise(instance);
    }

    default R visit(OrReplace instance) {
      return otherwise(instance);
    }
  }

  public static final class GraphIfNotExists extends openGql.grammar.CreateGraphOption implements Serializable {
    public final Boolean value;

    public GraphIfNotExists (Boolean value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GraphIfNotExists)) {
        return false;
      }
      GraphIfNotExists o = (GraphIfNotExists) other;
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
    public int compareTo(CreateGraphOption other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      GraphIfNotExists o = (GraphIfNotExists) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class OrReplace extends openGql.grammar.CreateGraphOption implements Serializable {
    public OrReplace () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OrReplace)) {
        return false;
      }
      OrReplace o = (OrReplace) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CreateGraphOption other) {
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
