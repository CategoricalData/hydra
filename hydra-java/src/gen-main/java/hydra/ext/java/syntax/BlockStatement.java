// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class BlockStatement implements Serializable, Comparable<BlockStatement> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.BlockStatement");
  
  public static final hydra.core.Name FIELD_NAME_LOCAL_VARIABLE_DECLARATION = new hydra.core.Name("localVariableDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_CLASS = new hydra.core.Name("class");
  
  public static final hydra.core.Name FIELD_NAME_STATEMENT = new hydra.core.Name("statement");
  
  private BlockStatement () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(LocalVariableDeclaration instance) ;
    
    R visit(Class_ instance) ;
    
    R visit(Statement instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(BlockStatement instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(LocalVariableDeclaration instance) {
      return otherwise(instance);
    }
    
    default R visit(Class_ instance) {
      return otherwise(instance);
    }
    
    default R visit(Statement instance) {
      return otherwise(instance);
    }
  }
  
  public static final class LocalVariableDeclaration extends hydra.ext.java.syntax.BlockStatement implements Serializable {
    public final hydra.ext.java.syntax.LocalVariableDeclarationStatement value;
    
    public LocalVariableDeclaration (hydra.ext.java.syntax.LocalVariableDeclarationStatement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LocalVariableDeclaration)) {
        return false;
      }
      LocalVariableDeclaration o = (LocalVariableDeclaration) other;
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
    public int compareTo(BlockStatement other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      LocalVariableDeclaration o = (LocalVariableDeclaration) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Class_ extends hydra.ext.java.syntax.BlockStatement implements Serializable {
    public final hydra.ext.java.syntax.ClassDeclaration value;
    
    public Class_ (hydra.ext.java.syntax.ClassDeclaration value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Class_)) {
        return false;
      }
      Class_ o = (Class_) other;
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
    public int compareTo(BlockStatement other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Class_ o = (Class_) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Statement extends hydra.ext.java.syntax.BlockStatement implements Serializable {
    public final hydra.ext.java.syntax.Statement value;
    
    public Statement (hydra.ext.java.syntax.Statement value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Statement)) {
        return false;
      }
      Statement o = (Statement) other;
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
    public int compareTo(BlockStatement other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Statement o = (Statement) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
