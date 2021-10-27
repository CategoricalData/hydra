package hydra.core;

public abstract class Type {
  private Type() {}
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  /**
   * An interface for applying a function to a Type according to its variant (subclass)
   */
  public interface Visitor<R> {
    R visit(Atomic instance) ;
    
    R visit(Element instance) ;
    
    R visit(Function instance) ;
    
    R visit(List instance) ;
    
    R visit(Map instance) ;
    
    R visit(Nominal instance) ;
    
    R visit(Optional instance) ;
    
    R visit(Record instance) ;
    
    R visit(Set instance) ;
    
    R visit(Union instance) ;
  }
  
  /**
   * An interface for applying a function to a Type according to its variant (subclass). If a visit() method for a
   * particular variant is not implemented, a default method is used instead.
   */
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Type instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    @Override
    default R visit(Atomic instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(Element instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(Function instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(List instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(Map instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(Nominal instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(Optional instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(Record instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(Set instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(Union instance) {
      return otherwise(instance);
    }
  }
  
  /**
   * @type hydra/core.AtomicType
   */
  public static final class Atomic extends Type {
    public final AtomicType atomic;
    
    /**
     * Constructs an immutable Atomic object
     */
    public Atomic(AtomicType atomic) {
      this.atomic = atomic;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Atomic)) {
          return false;
      }
      Atomic o = (Atomic) other;
      return atomic.equals(o.atomic);
    }
    
    @Override
    public int hashCode() {
      return 2 * atomic.hashCode();
    }
  }
  
  /**
   * @type hydra/core.Type
   */
  public static final class Element extends Type {
    public final Type element;
    
    /**
     * Constructs an immutable Element object
     */
    public Element(Type element) {
      this.element = element;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Element)) {
          return false;
      }
      Element o = (Element) other;
      return element.equals(o.element);
    }
    
    @Override
    public int hashCode() {
      return 2 * element.hashCode();
    }
  }
  
  /**
   * @type hydra/core.FunctionType
   */
  public static final class Function extends Type {
    public final FunctionType function;
    
    /**
     * Constructs an immutable Function object
     */
    public Function(FunctionType function) {
      this.function = function;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Function)) {
          return false;
      }
      Function o = (Function) other;
      return function.equals(o.function);
    }
    
    @Override
    public int hashCode() {
      return 2 * function.hashCode();
    }
  }
  
  /**
   * @type hydra/core.Type
   */
  public static final class List extends Type {
    public final Type list;
    
    /**
     * Constructs an immutable List object
     */
    public List(Type list) {
      this.list = list;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof List)) {
          return false;
      }
      List o = (List) other;
      return list.equals(o.list);
    }
    
    @Override
    public int hashCode() {
      return 2 * list.hashCode();
    }
  }
  
  /**
   * @type hydra/core.MapType
   */
  public static final class Map extends Type {
    public final MapType map;
    
    /**
     * Constructs an immutable Map object
     */
    public Map(MapType map) {
      this.map = map;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Map)) {
          return false;
      }
      Map o = (Map) other;
      return map.equals(o.map);
    }
    
    @Override
    public int hashCode() {
      return 2 * map.hashCode();
    }
  }
  
  /**
   * @type hydra/core.Name
   */
  public static final class Nominal extends Type {
    public final Name nominal;
    
    /**
     * Constructs an immutable Nominal object
     */
    public Nominal(Name nominal) {
      this.nominal = nominal;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Nominal)) {
          return false;
      }
      Nominal o = (Nominal) other;
      return nominal.equals(o.nominal);
    }
    
    @Override
    public int hashCode() {
      return 2 * nominal.hashCode();
    }
  }
  
  /**
   * @type hydra/core.Type
   */
  public static final class Optional extends Type {
    public final Type optional;
    
    /**
     * Constructs an immutable Optional object
     */
    public Optional(Type optional) {
      this.optional = optional;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Optional)) {
          return false;
      }
      Optional o = (Optional) other;
      return optional.equals(o.optional);
    }
    
    @Override
    public int hashCode() {
      return 2 * optional.hashCode();
    }
  }
  
  /**
   * @type list: hydra/core.FieldType
   */
  public static final class Record extends Type {
    public final java.util.List<FieldType> recordEsc;
    
    /**
     * Constructs an immutable Record object
     */
    public Record(java.util.List<FieldType> recordEsc) {
      this.recordEsc = recordEsc;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Record)) {
          return false;
      }
      Record o = (Record) other;
      return recordEsc.equals(o.recordEsc);
    }
    
    @Override
    public int hashCode() {
      return 2 * recordEsc.hashCode();
    }
  }
  
  /**
   * @type hydra/core.Type
   */
  public static final class Set extends Type {
    public final Type set;
    
    /**
     * Constructs an immutable Set object
     */
    public Set(Type set) {
      this.set = set;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Set)) {
          return false;
      }
      Set o = (Set) other;
      return set.equals(o.set);
    }
    
    @Override
    public int hashCode() {
      return 2 * set.hashCode();
    }
  }
  
  /**
   * @type list: hydra/core.FieldType
   */
  public static final class Union extends Type {
    public final java.util.List<FieldType> union;
    
    /**
     * Constructs an immutable Union object
     */
    public Union(java.util.List<FieldType> union) {
      this.union = union;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Union)) {
          return false;
      }
      Union o = (Union) other;
      return union.equals(o.union);
    }
    
    @Override
    public int hashCode() {
      return 2 * union.hashCode();
    }
  }
}
