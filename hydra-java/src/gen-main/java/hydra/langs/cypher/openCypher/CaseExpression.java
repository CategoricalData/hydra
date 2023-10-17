package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public abstract class CaseExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.CaseExpression");
  
  private CaseExpression () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Cases instance) ;
    
    R visit(Else instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(CaseExpression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Cases instance) {
      return otherwise((instance));
    }
    
    default R visit(Else instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Cases extends hydra.langs.cypher.openCypher.CaseExpression implements Serializable {
    public final java.util.List<hydra.langs.cypher.openCypher.Case> value;
    
    public Cases (java.util.List<hydra.langs.cypher.openCypher.Case> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Cases)) {
        return false;
      }
      Cases o = (Cases) (other);
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
  
  public static final class Else extends hydra.langs.cypher.openCypher.CaseExpression implements Serializable {
    public final java.util.Optional<hydra.langs.cypher.openCypher.Expression> value;
    
    public Else (java.util.Optional<hydra.langs.cypher.openCypher.Expression> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Else)) {
        return false;
      }
      Else o = (Else) (other);
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