package hydra.langs.tinkerpop.mappings;

/**
 * A mapping specification producing values (usually literal values) whose type is understood in context
 */
public abstract class ValueSpec {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/mappings.ValueSpec");
  
  private ValueSpec () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Pattern instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(ValueSpec instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Pattern instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * A compact path representing the function, e.g. ${}/engineInfo/model/name
   */
  public static final class Pattern extends hydra.langs.tinkerpop.mappings.ValueSpec {
    /**
     * A compact path representing the function, e.g. ${}/engineInfo/model/name
     */
    public final String value;
    
    public Pattern (String value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Pattern)) {
        return false;
      }
      Pattern o = (Pattern) (other);
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