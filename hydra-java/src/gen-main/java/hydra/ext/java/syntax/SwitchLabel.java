// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class SwitchLabel implements Serializable, Comparable<SwitchLabel> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.SwitchLabel");
  
  public static final hydra.core.Name FIELD_NAME_CONSTANT = new hydra.core.Name("constant");
  
  public static final hydra.core.Name FIELD_NAME_ENUM_CONSTANT = new hydra.core.Name("enumConstant");
  
  public static final hydra.core.Name FIELD_NAME_DEFAULT = new hydra.core.Name("default");
  
  private SwitchLabel () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Constant instance) ;
    
    R visit(EnumConstant instance) ;
    
    R visit(Default instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SwitchLabel instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Constant instance) {
      return otherwise(instance);
    }
    
    default R visit(EnumConstant instance) {
      return otherwise(instance);
    }
    
    default R visit(Default instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Constant extends hydra.ext.java.syntax.SwitchLabel implements Serializable {
    public final hydra.ext.java.syntax.ConstantExpression value;
    
    public Constant (hydra.ext.java.syntax.ConstantExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Constant)) {
        return false;
      }
      Constant o = (Constant) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SwitchLabel other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Constant o = (Constant) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class EnumConstant extends hydra.ext.java.syntax.SwitchLabel implements Serializable {
    public final hydra.ext.java.syntax.EnumConstantName value;
    
    public EnumConstant (hydra.ext.java.syntax.EnumConstantName value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EnumConstant)) {
        return false;
      }
      EnumConstant o = (EnumConstant) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SwitchLabel other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      EnumConstant o = (EnumConstant) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Default extends hydra.ext.java.syntax.SwitchLabel implements Serializable {
    public Default () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Default)) {
        return false;
      }
      Default o = (Default) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(SwitchLabel other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
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
