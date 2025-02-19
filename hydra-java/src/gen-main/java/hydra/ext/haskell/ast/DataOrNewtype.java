// Note: this is an automatically generated file. Do not edit.

package hydra.ext.haskell.ast;

import java.io.Serializable;

/**
 * The 'data' versus 'newtype keyword
 */
public abstract class DataOrNewtype implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.haskell.ast.DataOrNewtype");
  
  public static final hydra.core.Name FIELD_NAME_DATA = new hydra.core.Name("data");
  
  public static final hydra.core.Name FIELD_NAME_NEWTYPE = new hydra.core.Name("newtype");
  
  private DataOrNewtype () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Data instance) ;
    
    R visit(Newtype instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(DataOrNewtype instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Data instance) {
      return otherwise((instance));
    }
    
    default R visit(Newtype instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Data extends hydra.ext.haskell.ast.DataOrNewtype implements Serializable {
    public Data () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Data)) {
        return false;
      }
      Data o = (Data) (other);
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
  
  public static final class Newtype extends hydra.ext.haskell.ast.DataOrNewtype implements Serializable {
    public Newtype () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Newtype)) {
        return false;
      }
      Newtype o = (Newtype) (other);
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