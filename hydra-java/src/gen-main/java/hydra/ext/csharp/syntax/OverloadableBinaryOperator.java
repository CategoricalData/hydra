// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class OverloadableBinaryOperator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.OverloadableBinaryOperator");
  
  public static final hydra.core.Name FIELD_NAME_ADD = new hydra.core.Name("add");
  
  public static final hydra.core.Name FIELD_NAME_SUBTRACT = new hydra.core.Name("subtract");
  
  public static final hydra.core.Name FIELD_NAME_MULTIPLY = new hydra.core.Name("multiply");
  
  public static final hydra.core.Name FIELD_NAME_DIVIDE = new hydra.core.Name("divide");
  
  public static final hydra.core.Name FIELD_NAME_MODULUS = new hydra.core.Name("modulus");
  
  public static final hydra.core.Name FIELD_NAME_AND = new hydra.core.Name("and");
  
  public static final hydra.core.Name FIELD_NAME_OR = new hydra.core.Name("or");
  
  public static final hydra.core.Name FIELD_NAME_XOR = new hydra.core.Name("xor");
  
  public static final hydra.core.Name FIELD_NAME_LEFT_SHIFT = new hydra.core.Name("leftShift");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT_SHIFT = new hydra.core.Name("rightShift");
  
  public static final hydra.core.Name FIELD_NAME_EQUAL = new hydra.core.Name("equal");
  
  public static final hydra.core.Name FIELD_NAME_NOT_EQUAL = new hydra.core.Name("notEqual");
  
  public static final hydra.core.Name FIELD_NAME_GREATER_THAN = new hydra.core.Name("greaterThan");
  
  public static final hydra.core.Name FIELD_NAME_LESS_THAN = new hydra.core.Name("lessThan");
  
  public static final hydra.core.Name FIELD_NAME_GREATER_THAN_OR_EQUAL = new hydra.core.Name("greaterThanOrEqual");
  
  public static final hydra.core.Name FIELD_NAME_LESS_THAN_OR_EQUAL = new hydra.core.Name("lessThanOrEqual");
  
  private OverloadableBinaryOperator () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Add instance) ;
    
    R visit(Subtract instance) ;
    
    R visit(Multiply instance) ;
    
    R visit(Divide instance) ;
    
    R visit(Modulus instance) ;
    
    R visit(And instance) ;
    
    R visit(Or instance) ;
    
    R visit(Xor instance) ;
    
    R visit(LeftShift instance) ;
    
    R visit(RightShift instance) ;
    
    R visit(Equal instance) ;
    
    R visit(NotEqual instance) ;
    
    R visit(GreaterThan instance) ;
    
    R visit(LessThan instance) ;
    
    R visit(GreaterThanOrEqual instance) ;
    
    R visit(LessThanOrEqual instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(OverloadableBinaryOperator instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Add instance) {
      return otherwise((instance));
    }
    
    default R visit(Subtract instance) {
      return otherwise((instance));
    }
    
    default R visit(Multiply instance) {
      return otherwise((instance));
    }
    
    default R visit(Divide instance) {
      return otherwise((instance));
    }
    
    default R visit(Modulus instance) {
      return otherwise((instance));
    }
    
    default R visit(And instance) {
      return otherwise((instance));
    }
    
    default R visit(Or instance) {
      return otherwise((instance));
    }
    
    default R visit(Xor instance) {
      return otherwise((instance));
    }
    
    default R visit(LeftShift instance) {
      return otherwise((instance));
    }
    
    default R visit(RightShift instance) {
      return otherwise((instance));
    }
    
    default R visit(Equal instance) {
      return otherwise((instance));
    }
    
    default R visit(NotEqual instance) {
      return otherwise((instance));
    }
    
    default R visit(GreaterThan instance) {
      return otherwise((instance));
    }
    
    default R visit(LessThan instance) {
      return otherwise((instance));
    }
    
    default R visit(GreaterThanOrEqual instance) {
      return otherwise((instance));
    }
    
    default R visit(LessThanOrEqual instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Add extends hydra.ext.csharp.syntax.OverloadableBinaryOperator implements Serializable {
    public Add () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Add)) {
        return false;
      }
      Add o = (Add) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Subtract extends hydra.ext.csharp.syntax.OverloadableBinaryOperator implements Serializable {
    public Subtract () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Subtract)) {
        return false;
      }
      Subtract o = (Subtract) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Multiply extends hydra.ext.csharp.syntax.OverloadableBinaryOperator implements Serializable {
    public Multiply () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Multiply)) {
        return false;
      }
      Multiply o = (Multiply) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Divide extends hydra.ext.csharp.syntax.OverloadableBinaryOperator implements Serializable {
    public Divide () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Divide)) {
        return false;
      }
      Divide o = (Divide) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Modulus extends hydra.ext.csharp.syntax.OverloadableBinaryOperator implements Serializable {
    public Modulus () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Modulus)) {
        return false;
      }
      Modulus o = (Modulus) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class And extends hydra.ext.csharp.syntax.OverloadableBinaryOperator implements Serializable {
    public And () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof And)) {
        return false;
      }
      And o = (And) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Or extends hydra.ext.csharp.syntax.OverloadableBinaryOperator implements Serializable {
    public Or () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Or)) {
        return false;
      }
      Or o = (Or) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Xor extends hydra.ext.csharp.syntax.OverloadableBinaryOperator implements Serializable {
    public Xor () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Xor)) {
        return false;
      }
      Xor o = (Xor) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class LeftShift extends hydra.ext.csharp.syntax.OverloadableBinaryOperator implements Serializable {
    public LeftShift () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LeftShift)) {
        return false;
      }
      LeftShift o = (LeftShift) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class RightShift extends hydra.ext.csharp.syntax.OverloadableBinaryOperator implements Serializable {
    public RightShift () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RightShift)) {
        return false;
      }
      RightShift o = (RightShift) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Equal extends hydra.ext.csharp.syntax.OverloadableBinaryOperator implements Serializable {
    public Equal () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Equal)) {
        return false;
      }
      Equal o = (Equal) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class NotEqual extends hydra.ext.csharp.syntax.OverloadableBinaryOperator implements Serializable {
    public NotEqual () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NotEqual)) {
        return false;
      }
      NotEqual o = (NotEqual) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class GreaterThan extends hydra.ext.csharp.syntax.OverloadableBinaryOperator implements Serializable {
    public GreaterThan () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GreaterThan)) {
        return false;
      }
      GreaterThan o = (GreaterThan) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class LessThan extends hydra.ext.csharp.syntax.OverloadableBinaryOperator implements Serializable {
    public LessThan () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LessThan)) {
        return false;
      }
      LessThan o = (LessThan) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class GreaterThanOrEqual extends hydra.ext.csharp.syntax.OverloadableBinaryOperator implements Serializable {
    public GreaterThanOrEqual () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GreaterThanOrEqual)) {
        return false;
      }
      GreaterThanOrEqual o = (GreaterThanOrEqual) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class LessThanOrEqual extends hydra.ext.csharp.syntax.OverloadableBinaryOperator implements Serializable {
    public LessThanOrEqual () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LessThanOrEqual)) {
        return false;
      }
      LessThanOrEqual o = (LessThanOrEqual) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}