package hydra.ext.coq.syntax;

public abstract class FixAnnot {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.FixAnnot");
  
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
  
  public static final class Struct extends hydra.ext.coq.syntax.FixAnnot {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.Struct");
    
    public final hydra.ext.coq.syntax.Ident value;
    
    public Struct (hydra.ext.coq.syntax.Ident value) {
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
  
  public static final class Wf extends hydra.ext.coq.syntax.FixAnnot {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.Wf");
    
    public final hydra.ext.coq.syntax.FixAnnot_Wf value;
    
    public Wf (hydra.ext.coq.syntax.FixAnnot_Wf value) {
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
  
  public static final class Measure extends hydra.ext.coq.syntax.FixAnnot {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/coq/syntax.Measure");
    
    public final hydra.ext.coq.syntax.FixAnnot_Measure value;
    
    public Measure (hydra.ext.coq.syntax.FixAnnot_Measure value) {
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