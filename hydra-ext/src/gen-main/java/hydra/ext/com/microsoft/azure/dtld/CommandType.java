// Note: this is an automatically generated file. Do not edit.

package hydra.ext.com.microsoft.azure.dtld;

import java.io.Serializable;

/**
 * CommandType is deprecated. Either value, synchronous or asynchronous, has the same meaning: a command that starts execution within a configurable time and that completes execution within a configurable time.
 */
public abstract class CommandType implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/com/microsoft/azure/dtld.CommandType");
  
  public static final hydra.core.Name FIELD_NAME_SYNCHRONOUS = new hydra.core.Name("synchronous");
  
  public static final hydra.core.Name FIELD_NAME_ASYNCHRONOUS = new hydra.core.Name("asynchronous");
  
  private CommandType () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Synchronous instance) ;
    
    R visit(Asynchronous instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CommandType instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Synchronous instance) {
      return otherwise((instance));
    }
    
    default R visit(Asynchronous instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Synchronous extends hydra.ext.com.microsoft.azure.dtld.CommandType implements Serializable {
    public Synchronous () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Synchronous)) {
        return false;
      }
      Synchronous o = (Synchronous) (other);
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
  
  public static final class Asynchronous extends hydra.ext.com.microsoft.azure.dtld.CommandType implements Serializable {
    public Asynchronous () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Asynchronous)) {
        return false;
      }
      Asynchronous o = (Asynchronous) (other);
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