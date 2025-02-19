// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public abstract class Data_Ref implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.scala.meta.Data_Ref");
  
  public static final hydra.core.Name FIELD_NAME_THIS = new hydra.core.Name("this");
  
  public static final hydra.core.Name FIELD_NAME_SUPER = new hydra.core.Name("super");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_ANONYMOUS = new hydra.core.Name("anonymous");
  
  public static final hydra.core.Name FIELD_NAME_SELECT = new hydra.core.Name("select");
  
  public static final hydra.core.Name FIELD_NAME_APPLY_UNARY = new hydra.core.Name("applyUnary");
  
  private Data_Ref () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(This instance) ;
    
    R visit(Super instance) ;
    
    R visit(Name instance) ;
    
    R visit(Anonymous instance) ;
    
    R visit(Select instance) ;
    
    R visit(ApplyUnary instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Data_Ref instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(This instance) {
      return otherwise((instance));
    }
    
    default R visit(Super instance) {
      return otherwise((instance));
    }
    
    default R visit(Name instance) {
      return otherwise((instance));
    }
    
    default R visit(Anonymous instance) {
      return otherwise((instance));
    }
    
    default R visit(Select instance) {
      return otherwise((instance));
    }
    
    default R visit(ApplyUnary instance) {
      return otherwise((instance));
    }
  }
  
  public static final class This extends hydra.ext.scala.meta.Data_Ref implements Serializable {
    public final hydra.ext.scala.meta.Data_This value;
    
    public This (hydra.ext.scala.meta.Data_This value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof This)) {
        return false;
      }
      This o = (This) (other);
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
  
  public static final class Super extends hydra.ext.scala.meta.Data_Ref implements Serializable {
    public final hydra.ext.scala.meta.Data_Super value;
    
    public Super (hydra.ext.scala.meta.Data_Super value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Super)) {
        return false;
      }
      Super o = (Super) (other);
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
  
  public static final class Name extends hydra.ext.scala.meta.Data_Ref implements Serializable {
    public final hydra.ext.scala.meta.Data_Name value;
    
    public Name (hydra.ext.scala.meta.Data_Name value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Name)) {
        return false;
      }
      Name o = (Name) (other);
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
  
  public static final class Anonymous extends hydra.ext.scala.meta.Data_Ref implements Serializable {
    public final hydra.ext.scala.meta.Data_Anonymous value;
    
    public Anonymous (hydra.ext.scala.meta.Data_Anonymous value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Anonymous)) {
        return false;
      }
      Anonymous o = (Anonymous) (other);
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
  
  public static final class Select extends hydra.ext.scala.meta.Data_Ref implements Serializable {
    public final hydra.ext.scala.meta.Data_Select value;
    
    public Select (hydra.ext.scala.meta.Data_Select value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Select)) {
        return false;
      }
      Select o = (Select) (other);
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
  
  public static final class ApplyUnary extends hydra.ext.scala.meta.Data_Ref implements Serializable {
    public final hydra.ext.scala.meta.Data_ApplyUnary value;
    
    public ApplyUnary (hydra.ext.scala.meta.Data_ApplyUnary value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ApplyUnary)) {
        return false;
      }
      ApplyUnary o = (ApplyUnary) (other);
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
}