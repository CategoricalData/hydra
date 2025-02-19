// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class AssignmentOperator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.AssignmentOperator");
  
  public static final hydra.core.Name FIELD_NAME_SIMPLE = new hydra.core.Name("simple");
  
  public static final hydra.core.Name FIELD_NAME_PLUS_EQUALS = new hydra.core.Name("plusEquals");
  
  public static final hydra.core.Name FIELD_NAME_MINUS_EQUALS = new hydra.core.Name("minusEquals");
  
  public static final hydra.core.Name FIELD_NAME_TIMES_EQUALS = new hydra.core.Name("timesEquals");
  
  public static final hydra.core.Name FIELD_NAME_DIVIDE_EQUALS = new hydra.core.Name("divideEquals");
  
  public static final hydra.core.Name FIELD_NAME_MOD_EQUALS = new hydra.core.Name("modEquals");
  
  public static final hydra.core.Name FIELD_NAME_AND_EQUALS = new hydra.core.Name("andEquals");
  
  public static final hydra.core.Name FIELD_NAME_OR_EQUALS = new hydra.core.Name("orEquals");
  
  public static final hydra.core.Name FIELD_NAME_XOR_EQUALS = new hydra.core.Name("xorEquals");
  
  public static final hydra.core.Name FIELD_NAME_LEFT_SHIFT_EQUALS = new hydra.core.Name("leftShiftEquals");
  
  public static final hydra.core.Name FIELD_NAME_RIGHT_SHIFT_EQUALS = new hydra.core.Name("rightShiftEquals");
  
  private AssignmentOperator () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Simple instance) ;
    
    R visit(PlusEquals instance) ;
    
    R visit(MinusEquals instance) ;
    
    R visit(TimesEquals instance) ;
    
    R visit(DivideEquals instance) ;
    
    R visit(ModEquals instance) ;
    
    R visit(AndEquals instance) ;
    
    R visit(OrEquals instance) ;
    
    R visit(XorEquals instance) ;
    
    R visit(LeftShiftEquals instance) ;
    
    R visit(RightShiftEquals instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(AssignmentOperator instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Simple instance) {
      return otherwise((instance));
    }
    
    default R visit(PlusEquals instance) {
      return otherwise((instance));
    }
    
    default R visit(MinusEquals instance) {
      return otherwise((instance));
    }
    
    default R visit(TimesEquals instance) {
      return otherwise((instance));
    }
    
    default R visit(DivideEquals instance) {
      return otherwise((instance));
    }
    
    default R visit(ModEquals instance) {
      return otherwise((instance));
    }
    
    default R visit(AndEquals instance) {
      return otherwise((instance));
    }
    
    default R visit(OrEquals instance) {
      return otherwise((instance));
    }
    
    default R visit(XorEquals instance) {
      return otherwise((instance));
    }
    
    default R visit(LeftShiftEquals instance) {
      return otherwise((instance));
    }
    
    default R visit(RightShiftEquals instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Simple extends hydra.ext.csharp.syntax.AssignmentOperator implements Serializable {
    public final Boolean value;
    
    public Simple (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Simple)) {
        return false;
      }
      Simple o = (Simple) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class PlusEquals extends hydra.ext.csharp.syntax.AssignmentOperator implements Serializable {
    public PlusEquals () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PlusEquals)) {
        return false;
      }
      PlusEquals o = (PlusEquals) (other);
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
  
  public static final class MinusEquals extends hydra.ext.csharp.syntax.AssignmentOperator implements Serializable {
    public MinusEquals () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MinusEquals)) {
        return false;
      }
      MinusEquals o = (MinusEquals) (other);
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
  
  public static final class TimesEquals extends hydra.ext.csharp.syntax.AssignmentOperator implements Serializable {
    public TimesEquals () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TimesEquals)) {
        return false;
      }
      TimesEquals o = (TimesEquals) (other);
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
  
  public static final class DivideEquals extends hydra.ext.csharp.syntax.AssignmentOperator implements Serializable {
    public DivideEquals () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DivideEquals)) {
        return false;
      }
      DivideEquals o = (DivideEquals) (other);
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
  
  public static final class ModEquals extends hydra.ext.csharp.syntax.AssignmentOperator implements Serializable {
    public ModEquals () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ModEquals)) {
        return false;
      }
      ModEquals o = (ModEquals) (other);
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
  
  public static final class AndEquals extends hydra.ext.csharp.syntax.AssignmentOperator implements Serializable {
    public AndEquals () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AndEquals)) {
        return false;
      }
      AndEquals o = (AndEquals) (other);
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
  
  public static final class OrEquals extends hydra.ext.csharp.syntax.AssignmentOperator implements Serializable {
    public OrEquals () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OrEquals)) {
        return false;
      }
      OrEquals o = (OrEquals) (other);
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
  
  public static final class XorEquals extends hydra.ext.csharp.syntax.AssignmentOperator implements Serializable {
    public XorEquals () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof XorEquals)) {
        return false;
      }
      XorEquals o = (XorEquals) (other);
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
  
  public static final class LeftShiftEquals extends hydra.ext.csharp.syntax.AssignmentOperator implements Serializable {
    public LeftShiftEquals () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LeftShiftEquals)) {
        return false;
      }
      LeftShiftEquals o = (LeftShiftEquals) (other);
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
  
  public static final class RightShiftEquals extends hydra.ext.csharp.syntax.AssignmentOperator implements Serializable {
    public RightShiftEquals () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RightShiftEquals)) {
        return false;
      }
      RightShiftEquals o = (RightShiftEquals) (other);
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