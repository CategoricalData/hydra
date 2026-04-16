// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class CompOp implements Serializable, Comparable<CompOp> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.CompOp");

  public static final hydra.core.Name EQUALS = new hydra.core.Name("equals");

  public static final hydra.core.Name NOT_EQUALS = new hydra.core.Name("notEquals");

  public static final hydra.core.Name LESS_THAN = new hydra.core.Name("lessThan");

  public static final hydra.core.Name GREATER_THAN = new hydra.core.Name("greaterThan");

  public static final hydra.core.Name LESS_THAN_OR_EQUALS = new hydra.core.Name("lessThanOrEquals");

  public static final hydra.core.Name GREATER_THAN_OR_EQUALS = new hydra.core.Name("greaterThanOrEquals");

  private CompOp () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Equals instance) ;

    R visit(NotEquals instance) ;

    R visit(LessThan instance) ;

    R visit(GreaterThan instance) ;

    R visit(LessThanOrEquals instance) ;

    R visit(GreaterThanOrEquals instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CompOp instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Equals instance) {
      return otherwise(instance);
    }

    default R visit(NotEquals instance) {
      return otherwise(instance);
    }

    default R visit(LessThan instance) {
      return otherwise(instance);
    }

    default R visit(GreaterThan instance) {
      return otherwise(instance);
    }

    default R visit(LessThanOrEquals instance) {
      return otherwise(instance);
    }

    default R visit(GreaterThanOrEquals instance) {
      return otherwise(instance);
    }
  }

  public static final class Equals extends openGql.grammar.CompOp implements Serializable {
    public Equals () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Equals)) {
        return false;
      }
      Equals o = (Equals) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CompOp other) {
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

  public static final class NotEquals extends openGql.grammar.CompOp implements Serializable {
    public NotEquals () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NotEquals)) {
        return false;
      }
      NotEquals o = (NotEquals) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CompOp other) {
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

  public static final class LessThan extends openGql.grammar.CompOp implements Serializable {
    public LessThan () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LessThan)) {
        return false;
      }
      LessThan o = (LessThan) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CompOp other) {
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

  public static final class GreaterThan extends openGql.grammar.CompOp implements Serializable {
    public GreaterThan () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GreaterThan)) {
        return false;
      }
      GreaterThan o = (GreaterThan) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CompOp other) {
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

  public static final class LessThanOrEquals extends openGql.grammar.CompOp implements Serializable {
    public LessThanOrEquals () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LessThanOrEquals)) {
        return false;
      }
      LessThanOrEquals o = (LessThanOrEquals) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CompOp other) {
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

  public static final class GreaterThanOrEquals extends openGql.grammar.CompOp implements Serializable {
    public GreaterThanOrEquals () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GreaterThanOrEquals)) {
        return false;
      }
      GreaterThanOrEquals o = (GreaterThanOrEquals) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(CompOp other) {
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
