package hydra.ext.java.syntax;

public abstract class ImportDeclaration {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.ImportDeclaration");
  
  private ImportDeclaration () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(SingleType instance) ;
    
    R visit(TypeImportOnDemand instance) ;
    
    R visit(SingleStaticImport instance) ;
    
    R visit(StaticImportOnDemand instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ImportDeclaration instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(SingleType instance) {
      return otherwise((instance));
    }
    
    default R visit(TypeImportOnDemand instance) {
      return otherwise((instance));
    }
    
    default R visit(SingleStaticImport instance) {
      return otherwise((instance));
    }
    
    default R visit(StaticImportOnDemand instance) {
      return otherwise((instance));
    }
  }
  
  public static final class SingleType extends hydra.ext.java.syntax.ImportDeclaration {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.SingleType");
    
    public final hydra.ext.java.syntax.SingleTypeImportDeclaration value;
    
    public SingleType (hydra.ext.java.syntax.SingleTypeImportDeclaration value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SingleType)) {
        return false;
      }
      SingleType o = (SingleType) (other);
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
  
  public static final class TypeImportOnDemand extends hydra.ext.java.syntax.ImportDeclaration {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.TypeImportOnDemand");
    
    public final hydra.ext.java.syntax.TypeImportOnDemandDeclaration value;
    
    public TypeImportOnDemand (hydra.ext.java.syntax.TypeImportOnDemandDeclaration value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeImportOnDemand)) {
        return false;
      }
      TypeImportOnDemand o = (TypeImportOnDemand) (other);
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
  
  public static final class SingleStaticImport extends hydra.ext.java.syntax.ImportDeclaration {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.SingleStaticImport");
    
    public final hydra.ext.java.syntax.SingleStaticImportDeclaration value;
    
    public SingleStaticImport (hydra.ext.java.syntax.SingleStaticImportDeclaration value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SingleStaticImport)) {
        return false;
      }
      SingleStaticImport o = (SingleStaticImport) (other);
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
  
  public static final class StaticImportOnDemand extends hydra.ext.java.syntax.ImportDeclaration {
    public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/java/syntax.StaticImportOnDemand");
    
    public final hydra.ext.java.syntax.StaticImportOnDemandDeclaration value;
    
    public StaticImportOnDemand (hydra.ext.java.syntax.StaticImportOnDemandDeclaration value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StaticImportOnDemand)) {
        return false;
      }
      StaticImportOnDemand o = (StaticImportOnDemand) (other);
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