package hydra.ext.coq.syntax;

public abstract class FixAnnot {
  private FixAnnot () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Struct instance) ;
    
    R visit(Wf instance) ;
    
    R visit(Measure instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(FixAnnot instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Struct instance) {
      return otherwise((instance));
    }
    
    default R visit(Wf instance) {
      return otherwise((instance));
    }
    
    default R visit(Measure instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Struct extends FixAnnot {
    public final Ident value;
    
    public Struct (Ident value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Struct)) {
        return false;
      }
      Struct o = (Struct) (other);
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
  
  public static final class Wf extends FixAnnot {
    public final FixAnnot_Wf value;
    
    public Wf (FixAnnot_Wf value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Wf)) {
        return false;
      }
      Wf o = (Wf) (other);
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
  
  public static final class Measure extends FixAnnot {
    public final FixAnnot_Measure value;
    
    public Measure (FixAnnot_Measure value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Measure)) {
        return false;
      }
      Measure o = (Measure) (other);
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