// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class ClassBodyDeclaration implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ClassBodyDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_CLASS_MEMBER = new hydra.core.Name("classMember");
  
  public static final hydra.core.Name FIELD_NAME_INSTANCE_INITIALIZER = new hydra.core.Name("instanceInitializer");
  
  public static final hydra.core.Name FIELD_NAME_STATIC_INITIALIZER = new hydra.core.Name("staticInitializer");
  
  public static final hydra.core.Name FIELD_NAME_CONSTRUCTOR_DECLARATION = new hydra.core.Name("constructorDeclaration");
  
  private ClassBodyDeclaration () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(ClassMember instance) ;
    
    R visit(InstanceInitializer instance) ;
    
    R visit(StaticInitializer instance) ;
    
    R visit(ConstructorDeclaration instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ClassBodyDeclaration instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(ClassMember instance) {
      return otherwise((instance));
    }
    
    default R visit(InstanceInitializer instance) {
      return otherwise((instance));
    }
    
    default R visit(StaticInitializer instance) {
      return otherwise((instance));
    }
    
    default R visit(ConstructorDeclaration instance) {
      return otherwise((instance));
    }
  }
  
  public static final class ClassMember extends hydra.ext.java.syntax.ClassBodyDeclaration implements Serializable {
    public final hydra.ext.java.syntax.ClassMemberDeclaration value;
    
    public ClassMember (hydra.ext.java.syntax.ClassMemberDeclaration value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ClassMember)) {
        return false;
      }
      ClassMember o = (ClassMember) (other);
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
  
  public static final class InstanceInitializer extends hydra.ext.java.syntax.ClassBodyDeclaration implements Serializable {
    public final hydra.ext.java.syntax.InstanceInitializer value;
    
    public InstanceInitializer (hydra.ext.java.syntax.InstanceInitializer value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InstanceInitializer)) {
        return false;
      }
      InstanceInitializer o = (InstanceInitializer) (other);
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
  
  public static final class StaticInitializer extends hydra.ext.java.syntax.ClassBodyDeclaration implements Serializable {
    public final hydra.ext.java.syntax.StaticInitializer value;
    
    public StaticInitializer (hydra.ext.java.syntax.StaticInitializer value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StaticInitializer)) {
        return false;
      }
      StaticInitializer o = (StaticInitializer) (other);
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
  
  public static final class ConstructorDeclaration extends hydra.ext.java.syntax.ClassBodyDeclaration implements Serializable {
    public final hydra.ext.java.syntax.ConstructorDeclaration value;
    
    public ConstructorDeclaration (hydra.ext.java.syntax.ConstructorDeclaration value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ConstructorDeclaration)) {
        return false;
      }
      ConstructorDeclaration o = (ConstructorDeclaration) (other);
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