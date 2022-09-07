package hydra.ext.java.syntax;

public abstract class ModuleDirective {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.ModuleDirective");
  
  private ModuleDirective () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Requires instance) ;
    
    R visit(Exports instance) ;
    
    R visit(Opens instance) ;
    
    R visit(Uses instance) ;
    
    R visit(Provides instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ModuleDirective instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Requires instance) {
      return otherwise((instance));
    }
    
    default R visit(Exports instance) {
      return otherwise((instance));
    }
    
    default R visit(Opens instance) {
      return otherwise((instance));
    }
    
    default R visit(Uses instance) {
      return otherwise((instance));
    }
    
    default R visit(Provides instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Requires extends hydra.ext.java.syntax.ModuleDirective {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Requires");
    
    public final hydra.ext.java.syntax.ModuleDirective_Requires value;
    
    public Requires (hydra.ext.java.syntax.ModuleDirective_Requires value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Requires)) {
        return false;
      }
      Requires o = (Requires) (other);
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
  
  public static final class Exports extends hydra.ext.java.syntax.ModuleDirective {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Exports");
    
    public final hydra.ext.java.syntax.ModuleDirective_ExportsOrOpens value;
    
    public Exports (hydra.ext.java.syntax.ModuleDirective_ExportsOrOpens value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Exports)) {
        return false;
      }
      Exports o = (Exports) (other);
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
  
  public static final class Opens extends hydra.ext.java.syntax.ModuleDirective {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Opens");
    
    public final hydra.ext.java.syntax.ModuleDirective_ExportsOrOpens value;
    
    public Opens (hydra.ext.java.syntax.ModuleDirective_ExportsOrOpens value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Opens)) {
        return false;
      }
      Opens o = (Opens) (other);
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
  
  public static final class Uses extends hydra.ext.java.syntax.ModuleDirective {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Uses");
    
    public final hydra.ext.java.syntax.TypeName value;
    
    public Uses (hydra.ext.java.syntax.TypeName value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Uses)) {
        return false;
      }
      Uses o = (Uses) (other);
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
  
  public static final class Provides extends hydra.ext.java.syntax.ModuleDirective {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.Provides");
    
    public final hydra.ext.java.syntax.ModuleDirective_Provides value;
    
    public Provides (hydra.ext.java.syntax.ModuleDirective_Provides value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Provides)) {
        return false;
      }
      Provides o = (Provides) (other);
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