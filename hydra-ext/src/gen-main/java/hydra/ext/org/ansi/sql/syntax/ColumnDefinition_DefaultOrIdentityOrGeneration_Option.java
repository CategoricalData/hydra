// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.ansi.sql.syntax;

import java.io.Serializable;

public abstract class ColumnDefinition_DefaultOrIdentityOrGeneration_Option implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.ansi.sql.syntax.ColumnDefinition_DefaultOrIdentityOrGeneration_Option");
  
  public static final hydra.core.Name FIELD_NAME_DEFAULT_CLAUSE = new hydra.core.Name("defaultClause");
  
  public static final hydra.core.Name FIELD_NAME_IDENTITY_COLUMN_SPECIFICATION = new hydra.core.Name("identityColumnSpecification");
  
  public static final hydra.core.Name FIELD_NAME_GENERATION_CLAUSE = new hydra.core.Name("generationClause");
  
  private ColumnDefinition_DefaultOrIdentityOrGeneration_Option () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(DefaultClause instance) ;
    
    R visit(IdentityColumnSpecification instance) ;
    
    R visit(GenerationClause instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ColumnDefinition_DefaultOrIdentityOrGeneration_Option instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(DefaultClause instance) {
      return otherwise((instance));
    }
    
    default R visit(IdentityColumnSpecification instance) {
      return otherwise((instance));
    }
    
    default R visit(GenerationClause instance) {
      return otherwise((instance));
    }
  }
  
  public static final class DefaultClause extends hydra.ext.org.ansi.sql.syntax.ColumnDefinition_DefaultOrIdentityOrGeneration_Option implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.DefaultClause value;
    
    public DefaultClause (hydra.ext.org.ansi.sql.syntax.DefaultClause value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DefaultClause)) {
        return false;
      }
      DefaultClause o = (DefaultClause) (other);
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
  
  public static final class IdentityColumnSpecification extends hydra.ext.org.ansi.sql.syntax.ColumnDefinition_DefaultOrIdentityOrGeneration_Option implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.IdentityColumnSpecification value;
    
    public IdentityColumnSpecification (hydra.ext.org.ansi.sql.syntax.IdentityColumnSpecification value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof IdentityColumnSpecification)) {
        return false;
      }
      IdentityColumnSpecification o = (IdentityColumnSpecification) (other);
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
  
  public static final class GenerationClause extends hydra.ext.org.ansi.sql.syntax.ColumnDefinition_DefaultOrIdentityOrGeneration_Option implements Serializable {
    public final hydra.ext.org.ansi.sql.syntax.GenerationClause value;
    
    public GenerationClause (hydra.ext.org.ansi.sql.syntax.GenerationClause value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GenerationClause)) {
        return false;
      }
      GenerationClause o = (GenerationClause) (other);
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