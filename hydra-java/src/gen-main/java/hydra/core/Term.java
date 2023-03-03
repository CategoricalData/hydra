package hydra.core;

/**
 * A data term
 */
public abstract class Term<A> {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/core.Term");
  
  private Term () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Annotated instance) ;
    
    R visit(Application instance) ;
    
    R visit(Element instance) ;
    
    R visit(Function instance) ;
    
    R visit(Let instance) ;
    
    R visit(List instance) ;
    
    R visit(Literal instance) ;
    
    R visit(Map instance) ;
    
    R visit(Optional instance) ;
    
    R visit(Product instance) ;
    
    R visit(Record instance) ;
    
    R visit(Set instance) ;
    
    R visit(Stream instance) ;
    
    R visit(Sum instance) ;
    
    R visit(Union instance) ;
    
    R visit(Variable instance) ;
    
    R visit(Wrap instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Term instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Annotated instance) {
      return otherwise((instance));
    }
    
    default R visit(Application instance) {
      return otherwise((instance));
    }
    
    default R visit(Element instance) {
      return otherwise((instance));
    }
    
    default R visit(Function instance) {
      return otherwise((instance));
    }
    
    default R visit(Let instance) {
      return otherwise((instance));
    }
    
    default R visit(List instance) {
      return otherwise((instance));
    }
    
    default R visit(Literal instance) {
      return otherwise((instance));
    }
    
    default R visit(Map instance) {
      return otherwise((instance));
    }
    
    default R visit(Optional instance) {
      return otherwise((instance));
    }
    
    default R visit(Product instance) {
      return otherwise((instance));
    }
    
    default R visit(Record instance) {
      return otherwise((instance));
    }
    
    default R visit(Set instance) {
      return otherwise((instance));
    }
    
    default R visit(Stream instance) {
      return otherwise((instance));
    }
    
    default R visit(Sum instance) {
      return otherwise((instance));
    }
    
    default R visit(Union instance) {
      return otherwise((instance));
    }
    
    default R visit(Variable instance) {
      return otherwise((instance));
    }
    
    default R visit(Wrap instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * A term annotated with metadata
   */
  public static final class Annotated<A> extends hydra.core.Term<A> {
    /**
     * A term annotated with metadata
     */
    public final hydra.core.Annotated<hydra.core.Term<A>, A> value;
    
    public Annotated (hydra.core.Annotated<hydra.core.Term<A>, A> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Annotated)) {
        return false;
      }
      Annotated o = (Annotated) (other);
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
  
  /**
   * A function application
   */
  public static final class Application<A> extends hydra.core.Term<A> {
    /**
     * A function application
     */
    public final hydra.core.Application<A> value;
    
    public Application (hydra.core.Application<A> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Application)) {
        return false;
      }
      Application o = (Application) (other);
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
  
  /**
   * An element reference
   */
  public static final class Element<A> extends hydra.core.Term<A> {
    /**
     * An element reference
     */
    public final hydra.core.Name value;
    
    public Element (hydra.core.Name value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Element)) {
        return false;
      }
      Element o = (Element) (other);
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
  
  /**
   * A function term
   */
  public static final class Function<A> extends hydra.core.Term<A> {
    /**
     * A function term
     */
    public final hydra.core.Function<A> value;
    
    public Function (hydra.core.Function<A> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Function)) {
        return false;
      }
      Function o = (Function) (other);
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
  
  public static final class Let<A> extends hydra.core.Term<A> {
    public final hydra.core.Let<A> value;
    
    public Let (hydra.core.Let<A> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Let)) {
        return false;
      }
      Let o = (Let) (other);
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
  
  /**
   * A list
   */
  public static final class List<A> extends hydra.core.Term<A> {
    /**
     * A list
     */
    public final java.util.List<hydra.core.Term<A>> value;
    
    public List (java.util.List<hydra.core.Term<A>> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof List)) {
        return false;
      }
      List o = (List) (other);
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
  
  /**
   * A literal value
   */
  public static final class Literal<A> extends hydra.core.Term<A> {
    /**
     * A literal value
     */
    public final hydra.core.Literal value;
    
    public Literal (hydra.core.Literal value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Literal)) {
        return false;
      }
      Literal o = (Literal) (other);
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
  
  /**
   * A map of keys to values
   */
  public static final class Map<A> extends hydra.core.Term<A> {
    /**
     * A map of keys to values
     */
    public final java.util.Map<hydra.core.Term<A>, hydra.core.Term<A>> value;
    
    public Map (java.util.Map<hydra.core.Term<A>, hydra.core.Term<A>> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Map)) {
        return false;
      }
      Map o = (Map) (other);
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
  
  /**
   * An optional value
   */
  public static final class Optional<A> extends hydra.core.Term<A> {
    /**
     * An optional value
     */
    public final java.util.Optional<hydra.core.Term<A>> value;
    
    public Optional (java.util.Optional<hydra.core.Term<A>> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Optional)) {
        return false;
      }
      Optional o = (Optional) (other);
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
  
  /**
   * A tuple
   */
  public static final class Product<A> extends hydra.core.Term<A> {
    /**
     * A tuple
     */
    public final java.util.List<hydra.core.Term<A>> value;
    
    public Product (java.util.List<hydra.core.Term<A>> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Product)) {
        return false;
      }
      Product o = (Product) (other);
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
  
  /**
   * A record term
   */
  public static final class Record<A> extends hydra.core.Term<A> {
    /**
     * A record term
     */
    public final hydra.core.Record<A> value;
    
    public Record (hydra.core.Record<A> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Record)) {
        return false;
      }
      Record o = (Record) (other);
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
  
  /**
   * A set of values
   */
  public static final class Set<A> extends hydra.core.Term<A> {
    /**
     * A set of values
     */
    public final java.util.Set<hydra.core.Term<A>> value;
    
    public Set (java.util.Set<hydra.core.Term<A>> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Set)) {
        return false;
      }
      Set o = (Set) (other);
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
  
  /**
   * An infinite stream of terms
   */
  public static final class Stream<A> extends hydra.core.Term<A> {
    /**
     * An infinite stream of terms
     */
    public final hydra.core.Stream<A> value;
    
    public Stream (hydra.core.Stream<A> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Stream)) {
        return false;
      }
      Stream o = (Stream) (other);
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
  
  /**
   * A variant tuple
   */
  public static final class Sum<A> extends hydra.core.Term<A> {
    /**
     * A variant tuple
     */
    public final hydra.core.Sum<A> value;
    
    public Sum (hydra.core.Sum<A> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sum)) {
        return false;
      }
      Sum o = (Sum) (other);
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
  
  /**
   * An injection; an instance of a union type
   */
  public static final class Union<A> extends hydra.core.Term<A> {
    /**
     * An injection; an instance of a union type
     */
    public final hydra.core.Injection<A> value;
    
    public Union (hydra.core.Injection<A> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Union)) {
        return false;
      }
      Union o = (Union) (other);
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
  
  /**
   * A variable reference
   */
  public static final class Variable<A> extends hydra.core.Term<A> {
    /**
     * A variable reference
     */
    public final hydra.core.Name value;
    
    public Variable (hydra.core.Name value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Variable)) {
        return false;
      }
      Variable o = (Variable) (other);
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
  
  public static final class Wrap<A> extends hydra.core.Term<A> {
    public final hydra.core.Nominal<hydra.core.Term<A>> value;
    
    public Wrap (hydra.core.Nominal<hydra.core.Term<A>> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Wrap)) {
        return false;
      }
      Wrap o = (Wrap) (other);
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