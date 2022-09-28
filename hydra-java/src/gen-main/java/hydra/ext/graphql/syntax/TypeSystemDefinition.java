package hydra.ext.graphql.syntax;

public abstract class TypeSystemDefinition {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/ext/graphql/syntax.TypeSystemDefinition");
  
  private TypeSystemDefinition () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Schema instance) ;
    
    R visit(Type instance) ;
    
    R visit(Directive instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TypeSystemDefinition instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Schema instance) {
      return otherwise((instance));
    }
    
    default R visit(Type instance) {
      return otherwise((instance));
    }
    
    default R visit(Directive instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Schema extends hydra.ext.graphql.syntax.TypeSystemDefinition {
    public final hydra.ext.graphql.syntax.SchemaDefinition value;
    
    public Schema (hydra.ext.graphql.syntax.SchemaDefinition value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Schema)) {
        return false;
      }
      Schema o = (Schema) (other);
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
  
  public static final class Type extends hydra.ext.graphql.syntax.TypeSystemDefinition {
    public final hydra.ext.graphql.syntax.TypeDefinition value;
    
    public Type (hydra.ext.graphql.syntax.TypeDefinition value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Type)) {
        return false;
      }
      Type o = (Type) (other);
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
  
  public static final class Directive extends hydra.ext.graphql.syntax.TypeSystemDefinition {
    public final hydra.ext.graphql.syntax.DirectiveDefinition value;
    
    public Directive (hydra.ext.graphql.syntax.DirectiveDefinition value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Directive)) {
        return false;
      }
      Directive o = (Directive) (other);
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