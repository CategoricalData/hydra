// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class ClassBodyDeclaration implements Serializable, Comparable<ClassBodyDeclaration> {
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
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(ClassMember instance) {
      return otherwise(instance);
    }
    
    default R visit(InstanceInitializer instance) {
      return otherwise(instance);
    }
    
    default R visit(StaticInitializer instance) {
      return otherwise(instance);
    }
    
    default R visit(ConstructorDeclaration instance) {
      return otherwise(instance);
    }
  }
  
  public static final class ClassMember extends hydra.ext.java.syntax.ClassBodyDeclaration implements Serializable {
    public final hydra.ext.java.syntax.ClassMemberDeclaration value;
    
    public ClassMember (hydra.ext.java.syntax.ClassMemberDeclaration value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ClassMember)) {
        return false;
      }
      ClassMember o = (ClassMember) other;
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
    public int compareTo(ClassBodyDeclaration other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ClassMember o = (ClassMember) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class InstanceInitializer extends hydra.ext.java.syntax.ClassBodyDeclaration implements Serializable {
    public final hydra.ext.java.syntax.InstanceInitializer value;
    
    public InstanceInitializer (hydra.ext.java.syntax.InstanceInitializer value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InstanceInitializer)) {
        return false;
      }
      InstanceInitializer o = (InstanceInitializer) other;
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
    public int compareTo(ClassBodyDeclaration other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      InstanceInitializer o = (InstanceInitializer) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class StaticInitializer extends hydra.ext.java.syntax.ClassBodyDeclaration implements Serializable {
    public final hydra.ext.java.syntax.StaticInitializer value;
    
    public StaticInitializer (hydra.ext.java.syntax.StaticInitializer value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StaticInitializer)) {
        return false;
      }
      StaticInitializer o = (StaticInitializer) other;
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
    public int compareTo(ClassBodyDeclaration other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      StaticInitializer o = (StaticInitializer) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ConstructorDeclaration extends hydra.ext.java.syntax.ClassBodyDeclaration implements Serializable {
    public final hydra.ext.java.syntax.ConstructorDeclaration value;
    
    public ConstructorDeclaration (hydra.ext.java.syntax.ConstructorDeclaration value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ConstructorDeclaration)) {
        return false;
      }
      ConstructorDeclaration o = (ConstructorDeclaration) other;
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
    public int compareTo(ClassBodyDeclaration other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ConstructorDeclaration o = (ConstructorDeclaration) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
