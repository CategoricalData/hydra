// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class NamedEntityTarget implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.NamedEntityTarget");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_THIS = new hydra.core.Name("this");
  
  public static final hydra.core.Name FIELD_NAME_BASE = new hydra.core.Name("base");
  
  public static final hydra.core.Name FIELD_NAME_PREDEFINED_TYPE = new hydra.core.Name("predefinedType");
  
  public static final hydra.core.Name FIELD_NAME_QUALIFIED_ALIAS_MEMBER = new hydra.core.Name("qualifiedAliasMember");
  
  private NamedEntityTarget () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Name instance) ;
    
    R visit(This instance) ;
    
    R visit(Base instance) ;
    
    R visit(PredefinedType instance) ;
    
    R visit(QualifiedAliasMember instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(NamedEntityTarget instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Name instance) {
      return otherwise((instance));
    }
    
    default R visit(This instance) {
      return otherwise((instance));
    }
    
    default R visit(Base instance) {
      return otherwise((instance));
    }
    
    default R visit(PredefinedType instance) {
      return otherwise((instance));
    }
    
    default R visit(QualifiedAliasMember instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Name extends hydra.ext.csharp.syntax.NamedEntityTarget implements Serializable {
    public final hydra.ext.csharp.syntax.SimpleName value;
    
    public Name (hydra.ext.csharp.syntax.SimpleName value) {
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
  
  public static final class This extends hydra.ext.csharp.syntax.NamedEntityTarget implements Serializable {
    public This () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof This)) {
        return false;
      }
      This o = (This) (other);
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
  
  public static final class Base extends hydra.ext.csharp.syntax.NamedEntityTarget implements Serializable {
    public Base () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Base)) {
        return false;
      }
      Base o = (Base) (other);
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
  
  public static final class PredefinedType extends hydra.ext.csharp.syntax.NamedEntityTarget implements Serializable {
    public final hydra.ext.csharp.syntax.PredefinedType value;
    
    public PredefinedType (hydra.ext.csharp.syntax.PredefinedType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PredefinedType)) {
        return false;
      }
      PredefinedType o = (PredefinedType) (other);
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
  
  public static final class QualifiedAliasMember extends hydra.ext.csharp.syntax.NamedEntityTarget implements Serializable {
    public final hydra.ext.csharp.syntax.QualifiedAliasMember value;
    
    public QualifiedAliasMember (hydra.ext.csharp.syntax.QualifiedAliasMember value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof QualifiedAliasMember)) {
        return false;
      }
      QualifiedAliasMember o = (QualifiedAliasMember) (other);
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