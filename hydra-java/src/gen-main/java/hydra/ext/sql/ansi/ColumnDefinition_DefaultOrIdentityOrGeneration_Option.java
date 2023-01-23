package hydra.ext.sql.ansi;

public abstract class ColumnDefinition_DefaultOrIdentityOrGeneration_Option {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/sql/ansi.ColumnDefinition.DefaultOrIdentityOrGeneration.Option");
  
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
  
  public static final class DefaultClause extends hydra.ext.sql.ansi.ColumnDefinition_DefaultOrIdentityOrGeneration_Option {
    public final hydra.ext.sql.ansi.DefaultClause value;
    
    public DefaultClause (hydra.ext.sql.ansi.DefaultClause value) {
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
  
  public static final class IdentityColumnSpecification extends hydra.ext.sql.ansi.ColumnDefinition_DefaultOrIdentityOrGeneration_Option {
    public final hydra.ext.sql.ansi.IdentityColumnSpecification value;
    
    public IdentityColumnSpecification (hydra.ext.sql.ansi.IdentityColumnSpecification value) {
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
  
  public static final class GenerationClause extends hydra.ext.sql.ansi.ColumnDefinition_DefaultOrIdentityOrGeneration_Option {
    public final hydra.ext.sql.ansi.GenerationClause value;
    
    public GenerationClause (hydra.ext.sql.ansi.GenerationClause value) {
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