// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class GeneralSetFunctionType implements Serializable, Comparable<GeneralSetFunctionType> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.GeneralSetFunctionType");

  public static final hydra.core.Name AVG = new hydra.core.Name("avg");

  public static final hydra.core.Name COUNT = new hydra.core.Name("count");

  public static final hydra.core.Name MAX = new hydra.core.Name("max");

  public static final hydra.core.Name MIN = new hydra.core.Name("min");

  public static final hydra.core.Name SUM = new hydra.core.Name("sum");

  public static final hydra.core.Name COLLECT_LIST = new hydra.core.Name("collectList");

  public static final hydra.core.Name STDDEV_SAMP = new hydra.core.Name("stddevSamp");

  public static final hydra.core.Name STDDEV_POP = new hydra.core.Name("stddevPop");

  private GeneralSetFunctionType () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Avg instance) ;

    R visit(Count instance) ;

    R visit(Max instance) ;

    R visit(Min instance) ;

    R visit(Sum instance) ;

    R visit(CollectList instance) ;

    R visit(StddevSamp instance) ;

    R visit(StddevPop instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(GeneralSetFunctionType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Avg instance) {
      return otherwise(instance);
    }

    default R visit(Count instance) {
      return otherwise(instance);
    }

    default R visit(Max instance) {
      return otherwise(instance);
    }

    default R visit(Min instance) {
      return otherwise(instance);
    }

    default R visit(Sum instance) {
      return otherwise(instance);
    }

    default R visit(CollectList instance) {
      return otherwise(instance);
    }

    default R visit(StddevSamp instance) {
      return otherwise(instance);
    }

    default R visit(StddevPop instance) {
      return otherwise(instance);
    }
  }

  public static final class Avg extends openGql.grammar.GeneralSetFunctionType implements Serializable {
    public Avg () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Avg)) {
        return false;
      }
      Avg o = (Avg) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(GeneralSetFunctionType other) {
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

  public static final class Count extends openGql.grammar.GeneralSetFunctionType implements Serializable {
    public Count () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Count)) {
        return false;
      }
      Count o = (Count) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(GeneralSetFunctionType other) {
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

  public static final class Max extends openGql.grammar.GeneralSetFunctionType implements Serializable {
    public Max () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Max)) {
        return false;
      }
      Max o = (Max) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(GeneralSetFunctionType other) {
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

  public static final class Min extends openGql.grammar.GeneralSetFunctionType implements Serializable {
    public Min () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Min)) {
        return false;
      }
      Min o = (Min) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(GeneralSetFunctionType other) {
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

  public static final class Sum extends openGql.grammar.GeneralSetFunctionType implements Serializable {
    public Sum () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sum)) {
        return false;
      }
      Sum o = (Sum) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(GeneralSetFunctionType other) {
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

  public static final class CollectList extends openGql.grammar.GeneralSetFunctionType implements Serializable {
    public CollectList () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CollectList)) {
        return false;
      }
      CollectList o = (CollectList) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(GeneralSetFunctionType other) {
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

  public static final class StddevSamp extends openGql.grammar.GeneralSetFunctionType implements Serializable {
    public StddevSamp () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StddevSamp)) {
        return false;
      }
      StddevSamp o = (StddevSamp) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(GeneralSetFunctionType other) {
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

  public static final class StddevPop extends openGql.grammar.GeneralSetFunctionType implements Serializable {
    public StddevPop () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StddevPop)) {
        return false;
      }
      StddevPop o = (StddevPop) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(GeneralSetFunctionType other) {
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
