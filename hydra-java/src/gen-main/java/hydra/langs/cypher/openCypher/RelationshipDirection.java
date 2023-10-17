package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public abstract class RelationshipDirection implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.RelationshipDirection");
  
  private RelationshipDirection () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(LeftToRight instance) ;
    
    R visit(RightToLeft instance) ;
    
    R visit(Bidirectional instance) ;
    
    R visit(Undirected instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(RelationshipDirection instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(LeftToRight instance) {
      return otherwise((instance));
    }
    
    default R visit(RightToLeft instance) {
      return otherwise((instance));
    }
    
    default R visit(Bidirectional instance) {
      return otherwise((instance));
    }
    
    default R visit(Undirected instance) {
      return otherwise((instance));
    }
  }
  
  public static final class LeftToRight extends hydra.langs.cypher.openCypher.RelationshipDirection implements Serializable {
    public LeftToRight () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LeftToRight)) {
        return false;
      }
      LeftToRight o = (LeftToRight) (other);
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
  
  public static final class RightToLeft extends hydra.langs.cypher.openCypher.RelationshipDirection implements Serializable {
    public RightToLeft () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RightToLeft)) {
        return false;
      }
      RightToLeft o = (RightToLeft) (other);
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
  
  public static final class Bidirectional extends hydra.langs.cypher.openCypher.RelationshipDirection implements Serializable {
    public Bidirectional () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Bidirectional)) {
        return false;
      }
      Bidirectional o = (Bidirectional) (other);
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
  
  public static final class Undirected extends hydra.langs.cypher.openCypher.RelationshipDirection implements Serializable {
    public Undirected () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Undirected)) {
        return false;
      }
      Undirected o = (Undirected) (other);
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