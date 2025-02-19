// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.ansi.sql.syntax;

import java.io.Serializable;

public abstract class ColumnDefinition_TypeOrDomain_Option implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.ansi.sql.syntax.ColumnDefinition_TypeOrDomain_Option");
  
  public static final hydra.core.Name FIELD_NAME_DATA_TYPE = new hydra.core.Name("dataType");
  
  public static final hydra.core.Name FIELD_NAME_DOMAIN_NAME = new hydra.core.Name("domainName");
  
  private ColumnDefinition_TypeOrDomain_Option () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(DataType instance) ;
    
    R visit(DomainName instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ColumnDefinition_TypeOrDomain_Option instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(DataType instance) {
      return otherwise((instance));
    }
    
    default R visit(DomainName instance) {
      return otherwise((instance));
    }
  }
  
  public static final class DataType extends hydra.ext.org.ansi.sql.syntax.ColumnDefinition_TypeOrDomain_Option implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.DataType value;
    
    public DataType (hydra.ext.org.ansi.sql.syntax.DataType value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DataType)) {
        return false;
      }
      DataType o = (DataType) (other);
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
  
  public static final class DomainName extends hydra.ext.org.ansi.sql.syntax.ColumnDefinition_TypeOrDomain_Option implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.DomainName value;
    
    public DomainName (hydra.ext.org.ansi.sql.syntax.DomainName value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DomainName)) {
        return false;
      }
      DomainName o = (DomainName) (other);
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