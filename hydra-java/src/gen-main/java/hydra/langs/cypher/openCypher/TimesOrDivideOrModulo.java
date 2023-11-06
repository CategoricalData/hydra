package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public abstract class TimesOrDivideOrModulo implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.TimesOrDivideOrModulo");
  
  private TimesOrDivideOrModulo () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Times instance) ;
    
    R visit(Divide instance) ;
    
    R visit(Modulo instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TimesOrDivideOrModulo instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Times instance) {
      return otherwise((instance));
    }
    
    default R visit(Divide instance) {
      return otherwise((instance));
    }
    
    default R visit(Modulo instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Times extends hydra.langs.cypher.openCypher.TimesOrDivideOrModulo implements Serializable {
    public Times () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Times)) {
        return false;
      }
      Times o = (Times) (other);
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
  
  public static final class Divide extends hydra.langs.cypher.openCypher.TimesOrDivideOrModulo implements Serializable {
    public Divide () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Divide)) {
        return false;
      }
      Divide o = (Divide) (other);
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
  
  public static final class Modulo extends hydra.langs.cypher.openCypher.TimesOrDivideOrModulo implements Serializable {
    public Modulo () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Modulo)) {
        return false;
      }
      Modulo o = (Modulo) (other);
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