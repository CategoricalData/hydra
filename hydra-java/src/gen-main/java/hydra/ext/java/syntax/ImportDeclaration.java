package hydra.ext.java.syntax;

public abstract class ImportDeclaration {
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
  
  public static final class SingleType extends ImportDeclaration {
    public final SingleTypeImportDeclaration value;
    
    public SingleType (SingleTypeImportDeclaration value) {
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
  
  public static final class TypeImportOnDemand extends ImportDeclaration {
    public final TypeImportOnDemandDeclaration value;
    
    public TypeImportOnDemand (TypeImportOnDemandDeclaration value) {
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
  
  public static final class SingleStaticImport extends ImportDeclaration {
    public final SingleStaticImportDeclaration value;
    
    public SingleStaticImport (SingleStaticImportDeclaration value) {
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
  
  public static final class StaticImportOnDemand extends ImportDeclaration {
    public final StaticImportOnDemandDeclaration value;
    
    public StaticImportOnDemand (StaticImportOnDemandDeclaration value) {
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