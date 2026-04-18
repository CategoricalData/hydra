// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * An assignment operator
 */
public abstract class AssignmentOperator implements Serializable, Comparable<AssignmentOperator> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.AssignmentOperator");

  public static final hydra.core.Name ASSIGN = new hydra.core.Name("assign");

  public static final hydra.core.Name ADD_ASSIGN = new hydra.core.Name("addAssign");

  public static final hydra.core.Name SUBTRACT_ASSIGN = new hydra.core.Name("subtractAssign");

  public static final hydra.core.Name MULTIPLY_ASSIGN = new hydra.core.Name("multiplyAssign");

  public static final hydra.core.Name DIVIDE_ASSIGN = new hydra.core.Name("divideAssign");

  public static final hydra.core.Name MODULO_ASSIGN = new hydra.core.Name("moduloAssign");

  public static final hydra.core.Name EXPONENTIATE_ASSIGN = new hydra.core.Name("exponentiateAssign");

  public static final hydra.core.Name LEFT_SHIFT_ASSIGN = new hydra.core.Name("leftShiftAssign");

  public static final hydra.core.Name RIGHT_SHIFT_ASSIGN = new hydra.core.Name("rightShiftAssign");

  public static final hydra.core.Name UNSIGNED_RIGHT_SHIFT_ASSIGN = new hydra.core.Name("unsignedRightShiftAssign");

  public static final hydra.core.Name BITWISE_AND_ASSIGN = new hydra.core.Name("bitwiseAndAssign");

  public static final hydra.core.Name BITWISE_OR_ASSIGN = new hydra.core.Name("bitwiseOrAssign");

  public static final hydra.core.Name BITWISE_XOR_ASSIGN = new hydra.core.Name("bitwiseXorAssign");

  public static final hydra.core.Name AND_ASSIGN = new hydra.core.Name("andAssign");

  public static final hydra.core.Name OR_ASSIGN = new hydra.core.Name("orAssign");

  public static final hydra.core.Name NULLISH_ASSIGN = new hydra.core.Name("nullishAssign");

  private AssignmentOperator () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Assign instance) ;

    R visit(AddAssign instance) ;

    R visit(SubtractAssign instance) ;

    R visit(MultiplyAssign instance) ;

    R visit(DivideAssign instance) ;

    R visit(ModuloAssign instance) ;

    R visit(ExponentiateAssign instance) ;

    R visit(LeftShiftAssign instance) ;

    R visit(RightShiftAssign instance) ;

    R visit(UnsignedRightShiftAssign instance) ;

    R visit(BitwiseAndAssign instance) ;

    R visit(BitwiseOrAssign instance) ;

    R visit(BitwiseXorAssign instance) ;

    R visit(AndAssign instance) ;

    R visit(OrAssign instance) ;

    R visit(NullishAssign instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(AssignmentOperator instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Assign instance) {
      return otherwise(instance);
    }

    default R visit(AddAssign instance) {
      return otherwise(instance);
    }

    default R visit(SubtractAssign instance) {
      return otherwise(instance);
    }

    default R visit(MultiplyAssign instance) {
      return otherwise(instance);
    }

    default R visit(DivideAssign instance) {
      return otherwise(instance);
    }

    default R visit(ModuloAssign instance) {
      return otherwise(instance);
    }

    default R visit(ExponentiateAssign instance) {
      return otherwise(instance);
    }

    default R visit(LeftShiftAssign instance) {
      return otherwise(instance);
    }

    default R visit(RightShiftAssign instance) {
      return otherwise(instance);
    }

    default R visit(UnsignedRightShiftAssign instance) {
      return otherwise(instance);
    }

    default R visit(BitwiseAndAssign instance) {
      return otherwise(instance);
    }

    default R visit(BitwiseOrAssign instance) {
      return otherwise(instance);
    }

    default R visit(BitwiseXorAssign instance) {
      return otherwise(instance);
    }

    default R visit(AndAssign instance) {
      return otherwise(instance);
    }

    default R visit(OrAssign instance) {
      return otherwise(instance);
    }

    default R visit(NullishAssign instance) {
      return otherwise(instance);
    }
  }

  /**
   * =
   */
  public static final class Assign extends hydra.javaScript.syntax.AssignmentOperator implements Serializable {
    public Assign () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Assign)) {
        return false;
      }
      Assign o = (Assign) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AssignmentOperator other) {
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

  /**
   * +=
   */
  public static final class AddAssign extends hydra.javaScript.syntax.AssignmentOperator implements Serializable {
    public AddAssign () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AddAssign)) {
        return false;
      }
      AddAssign o = (AddAssign) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AssignmentOperator other) {
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

  /**
   * -=
   */
  public static final class SubtractAssign extends hydra.javaScript.syntax.AssignmentOperator implements Serializable {
    public SubtractAssign () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SubtractAssign)) {
        return false;
      }
      SubtractAssign o = (SubtractAssign) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AssignmentOperator other) {
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

  /**
   * *=
   */
  public static final class MultiplyAssign extends hydra.javaScript.syntax.AssignmentOperator implements Serializable {
    public MultiplyAssign () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MultiplyAssign)) {
        return false;
      }
      MultiplyAssign o = (MultiplyAssign) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AssignmentOperator other) {
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

  /**
   * /=
   */
  public static final class DivideAssign extends hydra.javaScript.syntax.AssignmentOperator implements Serializable {
    public DivideAssign () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DivideAssign)) {
        return false;
      }
      DivideAssign o = (DivideAssign) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AssignmentOperator other) {
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

  /**
   * %=
   */
  public static final class ModuloAssign extends hydra.javaScript.syntax.AssignmentOperator implements Serializable {
    public ModuloAssign () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ModuloAssign)) {
        return false;
      }
      ModuloAssign o = (ModuloAssign) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AssignmentOperator other) {
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

  /**
   * **=
   */
  public static final class ExponentiateAssign extends hydra.javaScript.syntax.AssignmentOperator implements Serializable {
    public ExponentiateAssign () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ExponentiateAssign)) {
        return false;
      }
      ExponentiateAssign o = (ExponentiateAssign) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AssignmentOperator other) {
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

  /**
   * &lt;&lt;=
   */
  public static final class LeftShiftAssign extends hydra.javaScript.syntax.AssignmentOperator implements Serializable {
    public LeftShiftAssign () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LeftShiftAssign)) {
        return false;
      }
      LeftShiftAssign o = (LeftShiftAssign) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AssignmentOperator other) {
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

  /**
   * &gt;&gt;=
   */
  public static final class RightShiftAssign extends hydra.javaScript.syntax.AssignmentOperator implements Serializable {
    public RightShiftAssign () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RightShiftAssign)) {
        return false;
      }
      RightShiftAssign o = (RightShiftAssign) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AssignmentOperator other) {
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

  /**
   * &gt;&gt;&gt;=
   */
  public static final class UnsignedRightShiftAssign extends hydra.javaScript.syntax.AssignmentOperator implements Serializable {
    public UnsignedRightShiftAssign () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UnsignedRightShiftAssign)) {
        return false;
      }
      UnsignedRightShiftAssign o = (UnsignedRightShiftAssign) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AssignmentOperator other) {
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

  /**
   * &=
   */
  public static final class BitwiseAndAssign extends hydra.javaScript.syntax.AssignmentOperator implements Serializable {
    public BitwiseAndAssign () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BitwiseAndAssign)) {
        return false;
      }
      BitwiseAndAssign o = (BitwiseAndAssign) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AssignmentOperator other) {
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

  /**
   * |=
   */
  public static final class BitwiseOrAssign extends hydra.javaScript.syntax.AssignmentOperator implements Serializable {
    public BitwiseOrAssign () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BitwiseOrAssign)) {
        return false;
      }
      BitwiseOrAssign o = (BitwiseOrAssign) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AssignmentOperator other) {
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

  /**
   * ^=
   */
  public static final class BitwiseXorAssign extends hydra.javaScript.syntax.AssignmentOperator implements Serializable {
    public BitwiseXorAssign () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BitwiseXorAssign)) {
        return false;
      }
      BitwiseXorAssign o = (BitwiseXorAssign) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AssignmentOperator other) {
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

  /**
   * &&=
   */
  public static final class AndAssign extends hydra.javaScript.syntax.AssignmentOperator implements Serializable {
    public AndAssign () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AndAssign)) {
        return false;
      }
      AndAssign o = (AndAssign) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AssignmentOperator other) {
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

  /**
   * ||=
   */
  public static final class OrAssign extends hydra.javaScript.syntax.AssignmentOperator implements Serializable {
    public OrAssign () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OrAssign)) {
        return false;
      }
      OrAssign o = (OrAssign) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AssignmentOperator other) {
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

  /**
   * ??=
   */
  public static final class NullishAssign extends hydra.javaScript.syntax.AssignmentOperator implements Serializable {
    public NullishAssign () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NullishAssign)) {
        return false;
      }
      NullishAssign o = (NullishAssign) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(AssignmentOperator other) {
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
