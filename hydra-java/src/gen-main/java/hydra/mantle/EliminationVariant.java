package hydra.mantle;

/**
 * The identifier of an elimination constructor
 */
public abstract class EliminationVariant {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/mantle.EliminationVariant");
  
  private EliminationVariant () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Element instance) ;
    
    R visit(List instance) ;
    
    R visit(Wrapped instance) ;
    
    R visit(Optional instance) ;
    
    R visit(Record instance) ;
    
    R visit(Union instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(EliminationVariant instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Element instance) {
      return otherwise((instance));
    }
    
    default R visit(List instance) {
      return otherwise((instance));
    }
    
    default R visit(Wrapped instance) {
      return otherwise((instance));
    }
    
    default R visit(Optional instance) {
      return otherwise((instance));
    }
    
    default R visit(Record instance) {
      return otherwise((instance));
    }
    
    default R visit(Union instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Element extends hydra.mantle.EliminationVariant {
    public Element () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Element)) {
        return false;
      }
      Element o = (Element) (other);
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
  
  public static final class List extends hydra.mantle.EliminationVariant {
    public List () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof List)) {
        return false;
      }
      List o = (List) (other);
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
  
  public static final class Wrapped extends hydra.mantle.EliminationVariant {
    public Wrapped () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Wrapped)) {
        return false;
      }
      Wrapped o = (Wrapped) (other);
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
  
  public static final class Optional extends hydra.mantle.EliminationVariant {
    public Optional () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Optional)) {
        return false;
      }
      Optional o = (Optional) (other);
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
  
  public static final class Record extends hydra.mantle.EliminationVariant {
    public Record () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Record)) {
        return false;
      }
      Record o = (Record) (other);
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
  
  public static final class Union extends hydra.mantle.EliminationVariant {
    public Union () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Union)) {
        return false;
      }
      Union o = (Union) (other);
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