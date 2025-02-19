// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class RefMethodBody implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.RefMethodBody");
  
  public static final hydra.core.Name FIELD_NAME_BLOCK = new hydra.core.Name("block");
  
  public static final hydra.core.Name FIELD_NAME_REF = new hydra.core.Name("ref");
  
  public static final hydra.core.Name FIELD_NAME_EMPTY = new hydra.core.Name("empty");
  
  private RefMethodBody () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Block instance) ;
    
    R visit(Ref instance) ;
    
    R visit(Empty instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(RefMethodBody instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Block instance) {
      return otherwise((instance));
    }
    
    default R visit(Ref instance) {
      return otherwise((instance));
    }
    
    default R visit(Empty instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Block extends hydra.ext.csharp.syntax.RefMethodBody implements Serializable {
    public final hydra.ext.csharp.syntax.Block value;
    
    public Block (hydra.ext.csharp.syntax.Block value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Block)) {
        return false;
      }
      Block o = (Block) (other);
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
  
  public static final class Ref extends hydra.ext.csharp.syntax.RefMethodBody implements Serializable {
    public final hydra.ext.csharp.syntax.VariableReference value;
    
    public Ref (hydra.ext.csharp.syntax.VariableReference value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ref)) {
        return false;
      }
      Ref o = (Ref) (other);
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
  
  public static final class Empty extends hydra.ext.csharp.syntax.RefMethodBody implements Serializable {
    public Empty () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Empty)) {
        return false;
      }
      Empty o = (Empty) (other);
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