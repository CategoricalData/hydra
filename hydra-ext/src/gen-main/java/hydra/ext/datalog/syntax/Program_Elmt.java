// Note: this is an automatically generated file. Do not edit.

package hydra.ext.datalog.syntax;

import java.io.Serializable;

public abstract class Program_Elmt implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/datalog/syntax.Program.Elmt");
  
  public static final hydra.core.Name FIELD_NAME_FACT = new hydra.core.Name("fact");
  
  public static final hydra.core.Name FIELD_NAME_RULE = new hydra.core.Name("rule");
  
  private Program_Elmt () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Fact instance) ;
    
    R visit(Rule instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Program_Elmt instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Fact instance) {
      return otherwise((instance));
    }
    
    default R visit(Rule instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Fact extends hydra.ext.datalog.syntax.Program_Elmt implements Serializable {
    public final hydra.ext.datalog.syntax.Fact value;
    
    public Fact (hydra.ext.datalog.syntax.Fact value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Fact)) {
        return false;
      }
      Fact o = (Fact) (other);
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
  
  public static final class Rule extends hydra.ext.datalog.syntax.Program_Elmt implements Serializable {
    public final hydra.ext.datalog.syntax.Rule value;
    
    public Rule (hydra.ext.datalog.syntax.Rule value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Rule)) {
        return false;
      }
      Rule o = (Rule) (other);
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