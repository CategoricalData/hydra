package hydra.core;

public abstract class Term {
  private Term() {}
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  /**
   * An interface for applying a function to a Term according to its variant (subclass)
   */
  public interface Visitor<R> {
    R visit(Application instance) ;
    
    R visit(Atomic instance) ;
    
    R visit(Element instance) ;
    
    R visit(Function instance) ;
    
    R visit(List instance) ;
    
    R visit(Map instance) ;
    
    R visit(Optional instance) ;
    
    R visit(Record instance) ;
    
    R visit(Set instance) ;
    
    R visit(Union instance) ;
    
    R visit(Variable instance) ;
  }
  
  /**
   * An interface for applying a function to a Term according to its variant (subclass). If a visit() method for a
   * particular variant is not implemented, a default method is used instead.
   */
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Term instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    @Override
    default R visit(Application instance) {
      return otherwise(instance);
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
    
    @Override
    default R visit(Variable instance) {
      return otherwise(instance);
    }
  }
  
  /**
   * A function application
   */
  public static final class Application extends Term {
    public final hydra.core.Application application;
    
    /**
     * Constructs an immutable Application object
     */
    public Application(hydra.core.Application application) {
      this.application = application;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Application)) {
          return false;
      }
      Application o = (Application) other;
      return application.equals(o.application);
    }
    
    @Override
    public int hashCode() {
      return 2 * application.hashCode();
    }
  }
  
  /**
   * An atomic value
   */
  public static final class Atomic extends Term {
    public final hydra.core.AtomicValue atomic;
    
    /**
     * Constructs an immutable Atomic object
     */
    public Atomic(hydra.core.AtomicValue atomic) {
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
   * An element reference
   */
  public static final class Element extends Term {
    public final hydra.core.Name element;
    
    /**
     * Constructs an immutable Element object
     */
    public Element(hydra.core.Name element) {
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
   * A function term
   */
  public static final class Function extends Term {
    public final hydra.core.Function function;
    
    /**
     * Constructs an immutable Function object
     */
    public Function(hydra.core.Function function) {
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
   * A list
   */
  public static final class List extends Term {
    public final java.util.List<hydra.core.Term> list;
    
    /**
     * Constructs an immutable List object
     */
    public List(java.util.List<hydra.core.Term> list) {
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
   * A map of key terms to value terms
   */
  public static final class Map extends Term {
    public final java.util.Map<hydra.core.Term, hydra.core.Term> map;
    
    /**
     * Constructs an immutable Map object
     */
    public Map(java.util.Map<hydra.core.Term, hydra.core.Term> map) {
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
   * An optional value
   */
  public static final class Optional extends Term {
    public final java.util.Optional<hydra.core.Term> optional;
    
    /**
     * Constructs an immutable Optional object
     */
    public Optional(java.util.Optional<hydra.core.Term> optional) {
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
   * A record, or labeled tuple
   */
  public static final class Record extends Term {
    public final java.util.List<hydra.core.Field> recordEsc;
    
    /**
     * Constructs an immutable Record object
     */
    public Record(java.util.List<hydra.core.Field> recordEsc) {
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
   * A set of terms
   */
  public static final class Set extends Term {
    public final java.util.Set<hydra.core.Term> set;
    
    /**
     * Constructs an immutable Set object
     */
    public Set(java.util.Set<hydra.core.Term> set) {
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
   * A union term, i.e. a generalization of inl() or inr()
   */
  public static final class Union extends Term {
    public final hydra.core.Field union;
    
    /**
     * Constructs an immutable Union object
     */
    public Union(hydra.core.Field union) {
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
  
  /**
   * A variable reference
   */
  public static final class Variable extends Term {
    public final hydra.core.Variable variable;
    
    /**
     * Constructs an immutable Variable object
     */
    public Variable(hydra.core.Variable variable) {
      this.variable = variable;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Variable)) {
          return false;
      }
      Variable o = (Variable) other;
      return variable.equals(o.variable);
    }
    
    @Override
    public int hashCode() {
      return 2 * variable.hashCode();
    }
  }
}
