package hydra.core;

/**
 * A term constant; an instance of a literal type
 */
public abstract class Literal {
  private Literal() {}
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  /**
   * An interface for applying a function to a Literal according to its variant (subclass)
   */
  public interface Visitor<R> {
    R visit(Binary instance) ;
    
    R visit(BooleanEsc instance) ;
    
    R visit(FloatEsc instance) ;
    
    R visit(IntegerEsc instance) ;
    
    R visit(StringEsc instance) ;
  }
  
  /**
   * An interface for applying a function to a Literal according to its variant (subclass). If a visit() method for a
   * particular variant is not implemented, a default method is used instead.
   */
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Literal instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    @Override
    default R visit(Binary instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(BooleanEsc instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(FloatEsc instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(IntegerEsc instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(StringEsc instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Binary extends Literal {
    public final String binary;
    
    /**
     * Constructs an immutable Binary object
     */
    public Binary(String binary) {
      this.binary = binary;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Binary)) {
          return false;
      }
      Binary o = (Binary) other;
      return binary.equals(o.binary);
    }
    
    @Override
    public int hashCode() {
      return 2 * binary.hashCode();
    }
  }
  
  public static final class BooleanEsc extends Literal {
    public final hydra.core.BooleanValue booleanEsc;
    
    /**
     * Constructs an immutable BooleanEsc object
     */
    public BooleanEsc(hydra.core.BooleanValue booleanEsc) {
      this.booleanEsc = booleanEsc;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BooleanEsc)) {
          return false;
      }
      BooleanEsc o = (BooleanEsc) other;
      return booleanEsc.equals(o.booleanEsc);
    }
    
    @Override
    public int hashCode() {
      return 2 * booleanEsc.hashCode();
    }
  }
  
  public static final class FloatEsc extends Literal {
    public final hydra.core.FloatValue floatEsc;
    
    /**
     * Constructs an immutable FloatEsc object
     */
    public FloatEsc(hydra.core.FloatValue floatEsc) {
      this.floatEsc = floatEsc;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FloatEsc)) {
          return false;
      }
      FloatEsc o = (FloatEsc) other;
      return floatEsc.equals(o.floatEsc);
    }
    
    @Override
    public int hashCode() {
      return 2 * floatEsc.hashCode();
    }
  }
  
  public static final class IntegerEsc extends Literal {
    public final hydra.core.IntegerValue integer;
    
    /**
     * Constructs an immutable IntegerEsc object
     */
    public IntegerEsc(hydra.core.IntegerValue integer) {
      this.integer = integer;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof IntegerEsc)) {
          return false;
      }
      IntegerEsc o = (IntegerEsc) other;
      return integer.equals(o.integer);
    }
    
    @Override
    public int hashCode() {
      return 2 * integer.hashCode();
    }
  }
  
  public static final class StringEsc extends Literal {
    public final String string;
    
    /**
     * Constructs an immutable StringEsc object
     */
    public StringEsc(String string) {
      this.string = string;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StringEsc)) {
          return false;
      }
      StringEsc o = (StringEsc) other;
      return string.equals(o.string);
    }
    
    @Override
    public int hashCode() {
      return 2 * string.hashCode();
    }
  }
}
