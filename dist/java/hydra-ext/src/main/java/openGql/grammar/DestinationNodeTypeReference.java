// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class DestinationNodeTypeReference implements Serializable, Comparable<DestinationNodeTypeReference> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.DestinationNodeTypeReference");

  public static final hydra.core.Name ALIAS = new hydra.core.Name("alias");

  public static final hydra.core.Name FILLER = new hydra.core.Name("filler");

  private DestinationNodeTypeReference () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Alias instance) ;

    R visit(Filler instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DestinationNodeTypeReference instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Alias instance) {
      return otherwise(instance);
    }

    default R visit(Filler instance) {
      return otherwise(instance);
    }
  }

  public static final class Alias extends openGql.grammar.DestinationNodeTypeReference implements Serializable {
    public final String value;

    public Alias (String value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Alias)) {
        return false;
      }
      Alias o = (Alias) other;
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
    public int compareTo(DestinationNodeTypeReference other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Alias o = (Alias) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Filler extends openGql.grammar.DestinationNodeTypeReference implements Serializable {
    public final hydra.util.Maybe<openGql.grammar.NodeTypeFiller> value;

    public Filler (hydra.util.Maybe<openGql.grammar.NodeTypeFiller> value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Filler)) {
        return false;
      }
      Filler o = (Filler) other;
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
    public int compareTo(DestinationNodeTypeReference other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Filler o = (Filler) other;
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
