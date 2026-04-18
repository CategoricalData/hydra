// Note: this is an automatically generated file. Do not edit.

package hydra.javaScript.syntax;

import java.io.Serializable;

/**
 * A binary operator
 */
public abstract class BinaryOperator implements Serializable, Comparable<BinaryOperator> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.javaScript.syntax.BinaryOperator");

  public static final hydra.core.Name ADD = new hydra.core.Name("add");

  public static final hydra.core.Name SUBTRACT = new hydra.core.Name("subtract");

  public static final hydra.core.Name MULTIPLY = new hydra.core.Name("multiply");

  public static final hydra.core.Name DIVIDE = new hydra.core.Name("divide");

  public static final hydra.core.Name MODULO = new hydra.core.Name("modulo");

  public static final hydra.core.Name EXPONENTIATE = new hydra.core.Name("exponentiate");

  public static final hydra.core.Name EQUAL = new hydra.core.Name("equal");

  public static final hydra.core.Name NOT_EQUAL = new hydra.core.Name("notEqual");

  public static final hydra.core.Name STRICT_EQUAL = new hydra.core.Name("strictEqual");

  public static final hydra.core.Name STRICT_NOT_EQUAL = new hydra.core.Name("strictNotEqual");

  public static final hydra.core.Name LESS_THAN = new hydra.core.Name("lessThan");

  public static final hydra.core.Name LESS_THAN_OR_EQUAL = new hydra.core.Name("lessThanOrEqual");

  public static final hydra.core.Name GREATER_THAN = new hydra.core.Name("greaterThan");

  public static final hydra.core.Name GREATER_THAN_OR_EQUAL = new hydra.core.Name("greaterThanOrEqual");

  public static final hydra.core.Name AND = new hydra.core.Name("and");

  public static final hydra.core.Name OR = new hydra.core.Name("or");

  public static final hydra.core.Name NULLISH_COALESCING = new hydra.core.Name("nullishCoalescing");

  public static final hydra.core.Name BITWISE_AND = new hydra.core.Name("bitwiseAnd");

  public static final hydra.core.Name BITWISE_OR = new hydra.core.Name("bitwiseOr");

  public static final hydra.core.Name BITWISE_XOR = new hydra.core.Name("bitwiseXor");

  public static final hydra.core.Name LEFT_SHIFT = new hydra.core.Name("leftShift");

  public static final hydra.core.Name RIGHT_SHIFT = new hydra.core.Name("rightShift");

  public static final hydra.core.Name UNSIGNED_RIGHT_SHIFT = new hydra.core.Name("unsignedRightShift");

  public static final hydra.core.Name IN = new hydra.core.Name("in");

  public static final hydra.core.Name INSTANCEOF = new hydra.core.Name("instanceof");

  private BinaryOperator () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Add instance) ;

    R visit(Subtract instance) ;

    R visit(Multiply instance) ;

    R visit(Divide instance) ;

    R visit(Modulo instance) ;

    R visit(Exponentiate instance) ;

    R visit(Equal instance) ;

    R visit(NotEqual instance) ;

    R visit(StrictEqual instance) ;

    R visit(StrictNotEqual instance) ;

    R visit(LessThan instance) ;

    R visit(LessThanOrEqual instance) ;

    R visit(GreaterThan instance) ;

    R visit(GreaterThanOrEqual instance) ;

    R visit(And instance) ;

    R visit(Or instance) ;

    R visit(NullishCoalescing instance) ;

    R visit(BitwiseAnd instance) ;

    R visit(BitwiseOr instance) ;

    R visit(BitwiseXor instance) ;

    R visit(LeftShift instance) ;

    R visit(RightShift instance) ;

    R visit(UnsignedRightShift instance) ;

    R visit(In instance) ;

    R visit(Instanceof instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(BinaryOperator instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Add instance) {
      return otherwise(instance);
    }

    default R visit(Subtract instance) {
      return otherwise(instance);
    }

    default R visit(Multiply instance) {
      return otherwise(instance);
    }

    default R visit(Divide instance) {
      return otherwise(instance);
    }

    default R visit(Modulo instance) {
      return otherwise(instance);
    }

    default R visit(Exponentiate instance) {
      return otherwise(instance);
    }

    default R visit(Equal instance) {
      return otherwise(instance);
    }

    default R visit(NotEqual instance) {
      return otherwise(instance);
    }

    default R visit(StrictEqual instance) {
      return otherwise(instance);
    }

    default R visit(StrictNotEqual instance) {
      return otherwise(instance);
    }

    default R visit(LessThan instance) {
      return otherwise(instance);
    }

    default R visit(LessThanOrEqual instance) {
      return otherwise(instance);
    }

    default R visit(GreaterThan instance) {
      return otherwise(instance);
    }

    default R visit(GreaterThanOrEqual instance) {
      return otherwise(instance);
    }

    default R visit(And instance) {
      return otherwise(instance);
    }

    default R visit(Or instance) {
      return otherwise(instance);
    }

    default R visit(NullishCoalescing instance) {
      return otherwise(instance);
    }

    default R visit(BitwiseAnd instance) {
      return otherwise(instance);
    }

    default R visit(BitwiseOr instance) {
      return otherwise(instance);
    }

    default R visit(BitwiseXor instance) {
      return otherwise(instance);
    }

    default R visit(LeftShift instance) {
      return otherwise(instance);
    }

    default R visit(RightShift instance) {
      return otherwise(instance);
    }

    default R visit(UnsignedRightShift instance) {
      return otherwise(instance);
    }

    default R visit(In instance) {
      return otherwise(instance);
    }

    default R visit(Instanceof instance) {
      return otherwise(instance);
    }
  }

  /**
   * +
   */
  public static final class Add extends hydra.javaScript.syntax.BinaryOperator implements Serializable {
    public Add () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Add)) {
        return false;
      }
      Add o = (Add) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(BinaryOperator other) {
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
   * -
   */
  public static final class Subtract extends hydra.javaScript.syntax.BinaryOperator implements Serializable {
    public Subtract () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Subtract)) {
        return false;
      }
      Subtract o = (Subtract) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(BinaryOperator other) {
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
   * *
   */
  public static final class Multiply extends hydra.javaScript.syntax.BinaryOperator implements Serializable {
    public Multiply () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Multiply)) {
        return false;
      }
      Multiply o = (Multiply) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(BinaryOperator other) {
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
   * /
   */
  public static final class Divide extends hydra.javaScript.syntax.BinaryOperator implements Serializable {
    public Divide () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Divide)) {
        return false;
      }
      Divide o = (Divide) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(BinaryOperator other) {
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
   * %
   */
  public static final class Modulo extends hydra.javaScript.syntax.BinaryOperator implements Serializable {
    public Modulo () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Modulo)) {
        return false;
      }
      Modulo o = (Modulo) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(BinaryOperator other) {
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
   * **
   */
  public static final class Exponentiate extends hydra.javaScript.syntax.BinaryOperator implements Serializable {
    public Exponentiate () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Exponentiate)) {
        return false;
      }
      Exponentiate o = (Exponentiate) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(BinaryOperator other) {
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
   * ==
   */
  public static final class Equal extends hydra.javaScript.syntax.BinaryOperator implements Serializable {
    public Equal () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Equal)) {
        return false;
      }
      Equal o = (Equal) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(BinaryOperator other) {
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
   * !=
   */
  public static final class NotEqual extends hydra.javaScript.syntax.BinaryOperator implements Serializable {
    public NotEqual () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NotEqual)) {
        return false;
      }
      NotEqual o = (NotEqual) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(BinaryOperator other) {
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
   * ===
   */
  public static final class StrictEqual extends hydra.javaScript.syntax.BinaryOperator implements Serializable {
    public StrictEqual () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StrictEqual)) {
        return false;
      }
      StrictEqual o = (StrictEqual) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(BinaryOperator other) {
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
   * !==
   */
  public static final class StrictNotEqual extends hydra.javaScript.syntax.BinaryOperator implements Serializable {
    public StrictNotEqual () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StrictNotEqual)) {
        return false;
      }
      StrictNotEqual o = (StrictNotEqual) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(BinaryOperator other) {
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
   * &lt;
   */
  public static final class LessThan extends hydra.javaScript.syntax.BinaryOperator implements Serializable {
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
    public int compareTo(BinaryOperator other) {
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
   * &lt;=
   */
  public static final class LessThanOrEqual extends hydra.javaScript.syntax.BinaryOperator implements Serializable {
    public LessThanOrEqual () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LessThanOrEqual)) {
        return false;
      }
      LessThanOrEqual o = (LessThanOrEqual) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(BinaryOperator other) {
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
   * &gt;
   */
  public static final class GreaterThan extends hydra.javaScript.syntax.BinaryOperator implements Serializable {
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
    public int compareTo(BinaryOperator other) {
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
   * &gt;=
   */
  public static final class GreaterThanOrEqual extends hydra.javaScript.syntax.BinaryOperator implements Serializable {
    public GreaterThanOrEqual () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GreaterThanOrEqual)) {
        return false;
      }
      GreaterThanOrEqual o = (GreaterThanOrEqual) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(BinaryOperator other) {
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
   * &&
   */
  public static final class And extends hydra.javaScript.syntax.BinaryOperator implements Serializable {
    public And () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof And)) {
        return false;
      }
      And o = (And) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(BinaryOperator other) {
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
   * ||
   */
  public static final class Or extends hydra.javaScript.syntax.BinaryOperator implements Serializable {
    public Or () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Or)) {
        return false;
      }
      Or o = (Or) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(BinaryOperator other) {
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
   * ??
   */
  public static final class NullishCoalescing extends hydra.javaScript.syntax.BinaryOperator implements Serializable {
    public NullishCoalescing () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NullishCoalescing)) {
        return false;
      }
      NullishCoalescing o = (NullishCoalescing) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(BinaryOperator other) {
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
   * &
   */
  public static final class BitwiseAnd extends hydra.javaScript.syntax.BinaryOperator implements Serializable {
    public BitwiseAnd () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BitwiseAnd)) {
        return false;
      }
      BitwiseAnd o = (BitwiseAnd) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(BinaryOperator other) {
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
   * |
   */
  public static final class BitwiseOr extends hydra.javaScript.syntax.BinaryOperator implements Serializable {
    public BitwiseOr () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BitwiseOr)) {
        return false;
      }
      BitwiseOr o = (BitwiseOr) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(BinaryOperator other) {
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
   * ^
   */
  public static final class BitwiseXor extends hydra.javaScript.syntax.BinaryOperator implements Serializable {
    public BitwiseXor () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BitwiseXor)) {
        return false;
      }
      BitwiseXor o = (BitwiseXor) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(BinaryOperator other) {
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
   * &lt;&lt;
   */
  public static final class LeftShift extends hydra.javaScript.syntax.BinaryOperator implements Serializable {
    public LeftShift () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LeftShift)) {
        return false;
      }
      LeftShift o = (LeftShift) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(BinaryOperator other) {
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
   * &gt;&gt;
   */
  public static final class RightShift extends hydra.javaScript.syntax.BinaryOperator implements Serializable {
    public RightShift () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RightShift)) {
        return false;
      }
      RightShift o = (RightShift) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(BinaryOperator other) {
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
   * &gt;&gt;&gt;
   */
  public static final class UnsignedRightShift extends hydra.javaScript.syntax.BinaryOperator implements Serializable {
    public UnsignedRightShift () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UnsignedRightShift)) {
        return false;
      }
      UnsignedRightShift o = (UnsignedRightShift) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(BinaryOperator other) {
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
   * in
   */
  public static final class In extends hydra.javaScript.syntax.BinaryOperator implements Serializable {
    public In () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof In)) {
        return false;
      }
      In o = (In) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(BinaryOperator other) {
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
   * instanceof
   */
  public static final class Instanceof extends hydra.javaScript.syntax.BinaryOperator implements Serializable {
    public Instanceof () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Instanceof)) {
        return false;
      }
      Instanceof o = (Instanceof) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(BinaryOperator other) {
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
