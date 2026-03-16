// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public abstract class ProcedureInvocation implements Serializable, Comparable<ProcedureInvocation> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.ProcedureInvocation");
  
  public static final hydra.core.Name EXPLICIT = new hydra.core.Name("explicit");
  
  public static final hydra.core.Name IMPLICIT = new hydra.core.Name("implicit");
  
  private ProcedureInvocation () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Explicit instance) ;
    
    R visit(Implicit instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ProcedureInvocation instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Explicit instance) {
      return otherwise(instance);
    }
    
    default R visit(Implicit instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Explicit extends hydra.ext.cypher.openCypher.ProcedureInvocation implements Serializable {
    public final hydra.ext.cypher.openCypher.ExplicitProcedureInvocation value;
    
    public Explicit (hydra.ext.cypher.openCypher.ExplicitProcedureInvocation value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Explicit)) {
        return false;
      }
      Explicit o = (Explicit) other;
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
    public int compareTo(ProcedureInvocation other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Explicit o = (Explicit) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Implicit extends hydra.ext.cypher.openCypher.ProcedureInvocation implements Serializable {
    public final hydra.ext.cypher.openCypher.ImplicitProcedureInvocation value;
    
    public Implicit (hydra.ext.cypher.openCypher.ImplicitProcedureInvocation value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Implicit)) {
        return false;
      }
      Implicit o = (Implicit) other;
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
    public int compareTo(ProcedureInvocation other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Implicit o = (Implicit) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
