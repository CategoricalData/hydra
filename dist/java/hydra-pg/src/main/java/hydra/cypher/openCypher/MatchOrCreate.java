// Note: this is an automatically generated file. Do not edit.

package hydra.cypher.openCypher;

import java.io.Serializable;

public abstract class MatchOrCreate implements Serializable, Comparable<MatchOrCreate> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.cypher.openCypher.MatchOrCreate");

  public static final hydra.core.Name MATCH = new hydra.core.Name("match");

  public static final hydra.core.Name CREATE = new hydra.core.Name("create");

  private MatchOrCreate () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Match instance) ;

    R visit(Create instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(MatchOrCreate instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Match instance) {
      return otherwise(instance);
    }

    default R visit(Create instance) {
      return otherwise(instance);
    }
  }

  public static final class Match extends hydra.cypher.openCypher.MatchOrCreate implements Serializable {
    public Match () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Match)) {
        return false;
      }
      Match o = (Match) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(MatchOrCreate other) {
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

  public static final class Create extends hydra.cypher.openCypher.MatchOrCreate implements Serializable {
    public Create () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Create)) {
        return false;
      }
      Create o = (Create) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(MatchOrCreate other) {
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
