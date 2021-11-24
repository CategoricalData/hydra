package hydra.core;

public abstract class Expression<A> {
  private Expression() {}
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  /**
   * An interface for applying a function to a Expression according to its variant (subclass)
   */
  public interface Visitor<R> {
    R visit(Application instance) ;
    
    R visit(Element instance) ;
    
    R visit(Function instance) ;
    
    R visit(Let instance) ;
    
    R visit(List instance) ;
    
    R visit(Literal instance) ;
    
    R visit(Map instance) ;
    
    R visit(Optional instance) ;
    
    R visit(Record instance) ;
    
    R visit(Set instance) ;
    
    R visit(TypeAbstraction instance) ;
    
    R visit(TypeApplication instance) ;
    
    R visit(Union instance) ;
    
    R visit(Variable instance) ;
  }
  
  /**
   * An interface for applying a function to a Expression according to its variant (subclass). If a visit() method for a
   * particular variant is not implemented, a default method is used instead.
   */
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Expression instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    @Override
    default R visit(Application instance) {
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
    default R visit(Let instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(List instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(Literal instance) {
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
    default R visit(TypeAbstraction instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(TypeApplication instance) {
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
  public static final class Application<A> extends Expression<A> {
    public final hydra.core.Application<A> application;
    
    /**
     * Constructs an immutable Application object
     */
    public Application(hydra.core.Application<A> application) {
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
   * An element reference
   */
  public static final class Element<A> extends Expression<A> {
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
  public static final class Function<A> extends Expression<A> {
    public final hydra.core.Function<A> function;
    
    /**
     * Constructs an immutable Function object
     */
    public Function(hydra.core.Function<A> function) {
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
  
  public static final class Let<A> extends Expression<A> {
    public final hydra.core.Let<A> let;
    
    /**
     * Constructs an immutable Let object
     */
    public Let(hydra.core.Let<A> let) {
      this.let = let;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Let)) {
          return false;
      }
      Let o = (Let) other;
      return let.equals(o.let);
    }
    
    @Override
    public int hashCode() {
      return 2 * let.hashCode();
    }
  }
  
  /**
   * A list
   */
  public static final class List<A> extends Expression<A> {
    public final java.util.List<hydra.core.Term<A>> list;
    
    /**
     * Constructs an immutable List object
     */
    public List(java.util.List<hydra.core.Term<A>> list) {
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
   * A literal value
   */
  public static final class Literal<A> extends Expression<A> {
    public final hydra.core.Literal literal;
    
    /**
     * Constructs an immutable Literal object
     */
    public Literal(hydra.core.Literal literal) {
      this.literal = literal;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Literal)) {
          return false;
      }
      Literal o = (Literal) other;
      return literal.equals(o.literal);
    }
    
    @Override
    public int hashCode() {
      return 2 * literal.hashCode();
    }
  }
  
  /**
   * A map of key terms to value terms
   */
  public static final class Map<A> extends Expression<A> {
    public final java.util.Map<hydra.core.Term<A>, hydra.core.Term<A>> map;
    
    /**
     * Constructs an immutable Map object
     */
    public Map(java.util.Map<hydra.core.Term<A>, hydra.core.Term<A>> map) {
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
  public static final class Optional<A> extends Expression<A> {
    public final java.util.Optional<hydra.core.Term<A>> optional;
    
    /**
     * Constructs an immutable Optional object
     */
    public Optional(java.util.Optional<hydra.core.Term<A>> optional) {
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
  public static final class Record<A> extends Expression<A> {
    public final java.util.List<hydra.core.Field<A>> recordEsc;
    
    /**
     * Constructs an immutable Record object
     */
    public Record(java.util.List<hydra.core.Field<A>> recordEsc) {
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
  public static final class Set<A> extends Expression<A> {
    public final java.util.Set<hydra.core.Term<A>> set;
    
    /**
     * Constructs an immutable Set object
     */
    public Set(java.util.Set<hydra.core.Term<A>> set) {
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
   * A type abstraction (generalization), which binds a type variable to a term
   */
  public static final class TypeAbstraction<A> extends Expression<A> {
    public final hydra.core.TypeAbstraction<A> typeAbstraction;
    
    /**
     * Constructs an immutable TypeAbstraction object
     */
    public TypeAbstraction(hydra.core.TypeAbstraction<A> typeAbstraction) {
      this.typeAbstraction = typeAbstraction;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeAbstraction)) {
          return false;
      }
      TypeAbstraction o = (TypeAbstraction) other;
      return typeAbstraction.equals(o.typeAbstraction);
    }
    
    @Override
    public int hashCode() {
      return 2 * typeAbstraction.hashCode();
    }
  }
  
  /**
   * A type application (instantiation), which applies a term to a type
   */
  public static final class TypeApplication<A> extends Expression<A> {
    public final hydra.core.TypeApplication<A> typeApplication;
    
    /**
     * Constructs an immutable TypeApplication object
     */
    public TypeApplication(hydra.core.TypeApplication<A> typeApplication) {
      this.typeApplication = typeApplication;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeApplication)) {
          return false;
      }
      TypeApplication o = (TypeApplication) other;
      return typeApplication.equals(o.typeApplication);
    }
    
    @Override
    public int hashCode() {
      return 2 * typeApplication.hashCode();
    }
  }
  
  /**
   * A union term, i.e. a generalization of inl() or inr()
   */
  public static final class Union<A> extends Expression<A> {
    public final hydra.core.Field<A> union;
    
    /**
     * Constructs an immutable Union object
     */
    public Union(hydra.core.Field<A> union) {
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
  public static final class Variable<A> extends Expression<A> {
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
