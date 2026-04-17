// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class HomeGraph implements Serializable, Comparable<HomeGraph> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.HomeGraph");

  public static final hydra.core.Name HOME_PROPERTY_GRAPH = new hydra.core.Name("homePropertyGraph");

  public static final hydra.core.Name HOME_GRAPH = new hydra.core.Name("homeGraph");

  private HomeGraph () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(HomePropertyGraph instance) ;

    R visit(HomeGraph_ instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(HomeGraph instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(HomePropertyGraph instance) {
      return otherwise(instance);
    }

    default R visit(HomeGraph_ instance) {
      return otherwise(instance);
    }
  }

  public static final class HomePropertyGraph extends openGql.grammar.HomeGraph implements Serializable {
    public HomePropertyGraph () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof HomePropertyGraph)) {
        return false;
      }
      HomePropertyGraph o = (HomePropertyGraph) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(HomeGraph other) {
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

  public static final class HomeGraph_ extends openGql.grammar.HomeGraph implements Serializable {
    public HomeGraph_ () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof HomeGraph_)) {
        return false;
      }
      HomeGraph_ o = (HomeGraph_) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(HomeGraph other) {
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
