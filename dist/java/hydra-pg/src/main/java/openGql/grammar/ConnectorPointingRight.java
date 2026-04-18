// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class ConnectorPointingRight implements Serializable, Comparable<ConnectorPointingRight> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.ConnectorPointingRight");

  public static final hydra.core.Name TO = new hydra.core.Name("to");

  public static final hydra.core.Name RIGHT_ARROW = new hydra.core.Name("rightArrow");

  private ConnectorPointingRight () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(To instance) ;

    R visit(RightArrow instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ConnectorPointingRight instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(To instance) {
      return otherwise(instance);
    }

    default R visit(RightArrow instance) {
      return otherwise(instance);
    }
  }

  public static final class To extends openGql.grammar.ConnectorPointingRight implements Serializable {
    public To () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof To)) {
        return false;
      }
      To o = (To) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ConnectorPointingRight other) {
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

  public static final class RightArrow extends openGql.grammar.ConnectorPointingRight implements Serializable {
    public RightArrow () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RightArrow)) {
        return false;
      }
      RightArrow o = (RightArrow) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ConnectorPointingRight other) {
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
