// Note: this is an automatically generated file. Do not edit.

package hydra.ext.java.syntax;

import java.io.Serializable;

public abstract class ImportDeclaration implements Serializable, Comparable<ImportDeclaration> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.java.syntax.ImportDeclaration");
  
  public static final hydra.core.Name FIELD_NAME_SINGLE_TYPE = new hydra.core.Name("singleType");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_IMPORT_ON_DEMAND = new hydra.core.Name("typeImportOnDemand");
  
  public static final hydra.core.Name FIELD_NAME_SINGLE_STATIC_IMPORT = new hydra.core.Name("singleStaticImport");
  
  public static final hydra.core.Name FIELD_NAME_STATIC_IMPORT_ON_DEMAND = new hydra.core.Name("staticImportOnDemand");
  
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
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(SingleType instance) {
      return otherwise(instance);
    }
    
    default R visit(TypeImportOnDemand instance) {
      return otherwise(instance);
    }
    
    default R visit(SingleStaticImport instance) {
      return otherwise(instance);
    }
    
    default R visit(StaticImportOnDemand instance) {
      return otherwise(instance);
    }
  }
  
  public static final class SingleType extends hydra.ext.java.syntax.ImportDeclaration implements Serializable {
    public final hydra.ext.java.syntax.SingleTypeImportDeclaration value;
    
    public SingleType (hydra.ext.java.syntax.SingleTypeImportDeclaration value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SingleType)) {
        return false;
      }
      SingleType o = (SingleType) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ImportDeclaration other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SingleType o = (SingleType) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class TypeImportOnDemand extends hydra.ext.java.syntax.ImportDeclaration implements Serializable {
    public final hydra.ext.java.syntax.TypeImportOnDemandDeclaration value;
    
    public TypeImportOnDemand (hydra.ext.java.syntax.TypeImportOnDemandDeclaration value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeImportOnDemand)) {
        return false;
      }
      TypeImportOnDemand o = (TypeImportOnDemand) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ImportDeclaration other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TypeImportOnDemand o = (TypeImportOnDemand) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class SingleStaticImport extends hydra.ext.java.syntax.ImportDeclaration implements Serializable {
    public final hydra.ext.java.syntax.SingleStaticImportDeclaration value;
    
    public SingleStaticImport (hydra.ext.java.syntax.SingleStaticImportDeclaration value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SingleStaticImport)) {
        return false;
      }
      SingleStaticImport o = (SingleStaticImport) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ImportDeclaration other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SingleStaticImport o = (SingleStaticImport) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class StaticImportOnDemand extends hydra.ext.java.syntax.ImportDeclaration implements Serializable {
    public final hydra.ext.java.syntax.StaticImportOnDemandDeclaration value;
    
    public StaticImportOnDemand (hydra.ext.java.syntax.StaticImportOnDemandDeclaration value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StaticImportOnDemand)) {
        return false;
      }
      StaticImportOnDemand o = (StaticImportOnDemand) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(ImportDeclaration other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      StaticImportOnDemand o = (StaticImportOnDemand) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
