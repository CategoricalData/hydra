// Note: this is an automatically generated file. Do not edit.

package hydra.ext.csharp.syntax;

import java.io.Serializable;

public abstract class CatchClauses implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.csharp.syntax.CatchClauses");
  
  public static final hydra.core.Name FIELD_NAME_SPECIFIC = new hydra.core.Name("specific");
  
  public static final hydra.core.Name FIELD_NAME_GENERAL = new hydra.core.Name("general");
  
  private CatchClauses () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Specific instance) ;
    
    R visit(General instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CatchClauses instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Specific instance) {
      return otherwise((instance));
    }
    
    default R visit(General instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Specific extends hydra.ext.csharp.syntax.CatchClauses implements Serializable {
    public final java.util.List<hydra.ext.csharp.syntax.SpecificCatchClause> value;
    
    public Specific (java.util.List<hydra.ext.csharp.syntax.SpecificCatchClause> value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Specific)) {
        return false;
      }
      Specific o = (Specific) (other);
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
  
  public static final class General extends hydra.ext.csharp.syntax.CatchClauses implements Serializable {
    public final hydra.ext.csharp.syntax.Block value;
    
    public General (hydra.ext.csharp.syntax.Block value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof General)) {
        return false;
      }
      General o = (General) (other);
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