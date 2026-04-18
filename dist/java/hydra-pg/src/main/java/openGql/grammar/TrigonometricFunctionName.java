// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public abstract class TrigonometricFunctionName implements Serializable, Comparable<TrigonometricFunctionName> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.TrigonometricFunctionName");

  public static final hydra.core.Name SIN = new hydra.core.Name("sin");

  public static final hydra.core.Name COS = new hydra.core.Name("cos");

  public static final hydra.core.Name TAN = new hydra.core.Name("tan");

  public static final hydra.core.Name COT = new hydra.core.Name("cot");

  public static final hydra.core.Name SINH = new hydra.core.Name("sinh");

  public static final hydra.core.Name COSH = new hydra.core.Name("cosh");

  public static final hydra.core.Name TANH = new hydra.core.Name("tanh");

  public static final hydra.core.Name ASIN = new hydra.core.Name("asin");

  public static final hydra.core.Name ACOS = new hydra.core.Name("acos");

  public static final hydra.core.Name ATAN = new hydra.core.Name("atan");

  public static final hydra.core.Name DEGREES = new hydra.core.Name("degrees");

  public static final hydra.core.Name RADIANS = new hydra.core.Name("radians");

  private TrigonometricFunctionName () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Sin instance) ;

    R visit(Cos instance) ;

    R visit(Tan instance) ;

    R visit(Cot instance) ;

    R visit(Sinh instance) ;

    R visit(Cosh instance) ;

    R visit(Tanh instance) ;

    R visit(Asin instance) ;

    R visit(Acos instance) ;

    R visit(Atan instance) ;

    R visit(Degrees instance) ;

    R visit(Radians instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TrigonometricFunctionName instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Sin instance) {
      return otherwise(instance);
    }

    default R visit(Cos instance) {
      return otherwise(instance);
    }

    default R visit(Tan instance) {
      return otherwise(instance);
    }

    default R visit(Cot instance) {
      return otherwise(instance);
    }

    default R visit(Sinh instance) {
      return otherwise(instance);
    }

    default R visit(Cosh instance) {
      return otherwise(instance);
    }

    default R visit(Tanh instance) {
      return otherwise(instance);
    }

    default R visit(Asin instance) {
      return otherwise(instance);
    }

    default R visit(Acos instance) {
      return otherwise(instance);
    }

    default R visit(Atan instance) {
      return otherwise(instance);
    }

    default R visit(Degrees instance) {
      return otherwise(instance);
    }

    default R visit(Radians instance) {
      return otherwise(instance);
    }
  }

  public static final class Sin extends openGql.grammar.TrigonometricFunctionName implements Serializable {
    public Sin () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sin)) {
        return false;
      }
      Sin o = (Sin) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TrigonometricFunctionName other) {
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

  public static final class Cos extends openGql.grammar.TrigonometricFunctionName implements Serializable {
    public Cos () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Cos)) {
        return false;
      }
      Cos o = (Cos) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TrigonometricFunctionName other) {
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

  public static final class Tan extends openGql.grammar.TrigonometricFunctionName implements Serializable {
    public Tan () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Tan)) {
        return false;
      }
      Tan o = (Tan) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TrigonometricFunctionName other) {
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

  public static final class Cot extends openGql.grammar.TrigonometricFunctionName implements Serializable {
    public Cot () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Cot)) {
        return false;
      }
      Cot o = (Cot) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TrigonometricFunctionName other) {
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

  public static final class Sinh extends openGql.grammar.TrigonometricFunctionName implements Serializable {
    public Sinh () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sinh)) {
        return false;
      }
      Sinh o = (Sinh) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TrigonometricFunctionName other) {
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

  public static final class Cosh extends openGql.grammar.TrigonometricFunctionName implements Serializable {
    public Cosh () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Cosh)) {
        return false;
      }
      Cosh o = (Cosh) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TrigonometricFunctionName other) {
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

  public static final class Tanh extends openGql.grammar.TrigonometricFunctionName implements Serializable {
    public Tanh () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Tanh)) {
        return false;
      }
      Tanh o = (Tanh) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TrigonometricFunctionName other) {
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

  public static final class Asin extends openGql.grammar.TrigonometricFunctionName implements Serializable {
    public Asin () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Asin)) {
        return false;
      }
      Asin o = (Asin) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TrigonometricFunctionName other) {
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

  public static final class Acos extends openGql.grammar.TrigonometricFunctionName implements Serializable {
    public Acos () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Acos)) {
        return false;
      }
      Acos o = (Acos) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TrigonometricFunctionName other) {
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

  public static final class Atan extends openGql.grammar.TrigonometricFunctionName implements Serializable {
    public Atan () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Atan)) {
        return false;
      }
      Atan o = (Atan) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TrigonometricFunctionName other) {
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

  public static final class Degrees extends openGql.grammar.TrigonometricFunctionName implements Serializable {
    public Degrees () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Degrees)) {
        return false;
      }
      Degrees o = (Degrees) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TrigonometricFunctionName other) {
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

  public static final class Radians extends openGql.grammar.TrigonometricFunctionName implements Serializable {
    public Radians () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Radians)) {
        return false;
      }
      Radians o = (Radians) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TrigonometricFunctionName other) {
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
