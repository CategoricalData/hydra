// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public abstract class ProcedureInvocation implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.ProcedureInvocation");
  
  private ProcedureInvocation () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Explicit instance) ;
    
    R visit(Implicit instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ProcedureInvocation instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Explicit instance) {
      return otherwise((instance));
    }
    
    default R visit(Implicit instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Explicit extends hydra.langs.cypher.openCypher.ProcedureInvocation implements Serializable {
    public final hydra.langs.cypher.openCypher.ExplicitProcedureInvocation value;
    
    public Explicit (hydra.langs.cypher.openCypher.ExplicitProcedureInvocation value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Explicit)) {
        return false;
      }
      Explicit o = (Explicit) (other);
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
  
  public static final class Implicit extends hydra.langs.cypher.openCypher.ProcedureInvocation implements Serializable {
    public final hydra.langs.cypher.openCypher.ImplicitProcedureInvocation value;
    
    public Implicit (hydra.langs.cypher.openCypher.ImplicitProcedureInvocation value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Implicit)) {
        return false;
      }
      Implicit o = (Implicit) (other);
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