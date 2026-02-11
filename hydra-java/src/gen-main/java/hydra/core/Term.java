// Note: this is an automatically generated file. Do not edit.

package hydra.core;

import java.io.Serializable;

/**
 * A data term
 */
public abstract class Term implements Serializable, Comparable<Term> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.core.Term");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATED = new hydra.core.Name("annotated");
  
  public static final hydra.core.Name FIELD_NAME_APPLICATION = new hydra.core.Name("application");
  
  public static final hydra.core.Name FIELD_NAME_EITHER = new hydra.core.Name("either");
  
  public static final hydra.core.Name FIELD_NAME_FUNCTION = new hydra.core.Name("function");
  
  public static final hydra.core.Name FIELD_NAME_LET = new hydra.core.Name("let");
  
  public static final hydra.core.Name FIELD_NAME_LIST = new hydra.core.Name("list");
  
  public static final hydra.core.Name FIELD_NAME_LITERAL = new hydra.core.Name("literal");
  
  public static final hydra.core.Name FIELD_NAME_MAP = new hydra.core.Name("map");
  
  public static final hydra.core.Name FIELD_NAME_MAYBE = new hydra.core.Name("maybe");
  
  public static final hydra.core.Name FIELD_NAME_PAIR = new hydra.core.Name("pair");
  
  public static final hydra.core.Name FIELD_NAME_RECORD = new hydra.core.Name("record");
  
  public static final hydra.core.Name FIELD_NAME_SET = new hydra.core.Name("set");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_APPLICATION = new hydra.core.Name("typeApplication");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_LAMBDA = new hydra.core.Name("typeLambda");
  
  public static final hydra.core.Name FIELD_NAME_UNION = new hydra.core.Name("union");
  
  public static final hydra.core.Name FIELD_NAME_UNIT = new hydra.core.Name("unit");
  
  public static final hydra.core.Name FIELD_NAME_VARIABLE = new hydra.core.Name("variable");
  
  public static final hydra.core.Name FIELD_NAME_WRAP = new hydra.core.Name("wrap");
  
  private Term () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Annotated instance) ;
    
    R visit(Application instance) ;
    
    R visit(Either instance) ;
    
    R visit(Function instance) ;
    
    R visit(Let instance) ;
    
    R visit(List instance) ;
    
    R visit(Literal instance) ;
    
    R visit(Map instance) ;
    
    R visit(Maybe instance) ;
    
    R visit(Pair instance) ;
    
    R visit(Record instance) ;
    
    R visit(Set instance) ;
    
    R visit(TypeApplication instance) ;
    
    R visit(TypeLambda instance) ;
    
    R visit(Union instance) ;
    
    R visit(Unit instance) ;
    
    R visit(Variable instance) ;
    
    R visit(Wrap instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Term instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Annotated instance) {
      return otherwise(instance);
    }
    
    default R visit(Application instance) {
      return otherwise(instance);
    }
    
    default R visit(Either instance) {
      return otherwise(instance);
    }
    
    default R visit(Function instance) {
      return otherwise(instance);
    }
    
    default R visit(Let instance) {
      return otherwise(instance);
    }
    
    default R visit(List instance) {
      return otherwise(instance);
    }
    
    default R visit(Literal instance) {
      return otherwise(instance);
    }
    
    default R visit(Map instance) {
      return otherwise(instance);
    }
    
    default R visit(Maybe instance) {
      return otherwise(instance);
    }
    
    default R visit(Pair instance) {
      return otherwise(instance);
    }
    
    default R visit(Record instance) {
      return otherwise(instance);
    }
    
    default R visit(Set instance) {
      return otherwise(instance);
    }
    
    default R visit(TypeApplication instance) {
      return otherwise(instance);
    }
    
    default R visit(TypeLambda instance) {
      return otherwise(instance);
    }
    
    default R visit(Union instance) {
      return otherwise(instance);
    }
    
    default R visit(Unit instance) {
      return otherwise(instance);
    }
    
    default R visit(Variable instance) {
      return otherwise(instance);
    }
    
    default R visit(Wrap instance) {
      return otherwise(instance);
    }
  }
  
  /**
   * A term annotated with metadata
   */
  public static final class Annotated extends hydra.core.Term implements Serializable {
    public final hydra.core.AnnotatedTerm value;
    
    public Annotated (hydra.core.AnnotatedTerm value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Annotated)) {
        return false;
      }
      Annotated o = (Annotated) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Term other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Annotated o = (Annotated) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A function application
   */
  public static final class Application extends hydra.core.Term implements Serializable {
    public final hydra.core.Application value;
    
    public Application (hydra.core.Application value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Application)) {
        return false;
      }
      Application o = (Application) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Term other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Application o = (Application) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * An either value
   */
  public static final class Either extends hydra.core.Term implements Serializable {
    public final hydra.util.Either<hydra.core.Term, hydra.core.Term> value;
    
    public Either (hydra.util.Either<hydra.core.Term, hydra.core.Term> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Either)) {
        return false;
      }
      Either o = (Either) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Term other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Either o = (Either) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A function term
   */
  public static final class Function extends hydra.core.Term implements Serializable {
    public final hydra.core.Function value;
    
    public Function (hydra.core.Function value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Function)) {
        return false;
      }
      Function o = (Function) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Term other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Function o = (Function) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A 'let' term, which binds variables to terms
   */
  public static final class Let extends hydra.core.Term implements Serializable {
    public final hydra.core.Let value;
    
    public Let (hydra.core.Let value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Let)) {
        return false;
      }
      Let o = (Let) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Term other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Let o = (Let) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A list
   */
  public static final class List extends hydra.core.Term implements Serializable {
    public final java.util.List<hydra.core.Term> value;
    
    public List (java.util.List<hydra.core.Term> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof List)) {
        return false;
      }
      List o = (List) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Term other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      List o = (List) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A literal value
   */
  public static final class Literal extends hydra.core.Term implements Serializable {
    public final hydra.core.Literal value;
    
    public Literal (hydra.core.Literal value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Literal)) {
        return false;
      }
      Literal o = (Literal) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Term other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Literal o = (Literal) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A map of keys to values
   */
  public static final class Map extends hydra.core.Term implements Serializable {
    public final java.util.Map<hydra.core.Term, hydra.core.Term> value;
    
    public Map (java.util.Map<hydra.core.Term, hydra.core.Term> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Map)) {
        return false;
      }
      Map o = (Map) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Term other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Map o = (Map) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * An optional value
   */
  public static final class Maybe extends hydra.core.Term implements Serializable {
    public final hydra.util.Maybe<hydra.core.Term> value;
    
    public Maybe (hydra.util.Maybe<hydra.core.Term> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Maybe)) {
        return false;
      }
      Maybe o = (Maybe) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Term other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Maybe o = (Maybe) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A pair (2-tuple)
   */
  public static final class Pair extends hydra.core.Term implements Serializable {
    public final hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term> value;
    
    public Pair (hydra.util.Tuple.Tuple2<hydra.core.Term, hydra.core.Term> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Pair)) {
        return false;
      }
      Pair o = (Pair) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Term other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Pair o = (Pair) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A record term
   */
  public static final class Record extends hydra.core.Term implements Serializable {
    public final hydra.core.Record value;
    
    public Record (hydra.core.Record value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Record)) {
        return false;
      }
      Record o = (Record) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Term other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Record o = (Record) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A set of values
   */
  public static final class Set extends hydra.core.Term implements Serializable {
    public final java.util.Set<hydra.core.Term> value;
    
    public Set (java.util.Set<hydra.core.Term> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Set)) {
        return false;
      }
      Set o = (Set) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Term other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Set o = (Set) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A System F type application term
   */
  public static final class TypeApplication extends hydra.core.Term implements Serializable {
    public final hydra.core.TypeApplicationTerm value;
    
    public TypeApplication (hydra.core.TypeApplicationTerm value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeApplication)) {
        return false;
      }
      TypeApplication o = (TypeApplication) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Term other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TypeApplication o = (TypeApplication) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A System F type abstraction term
   */
  public static final class TypeLambda extends hydra.core.Term implements Serializable {
    public final hydra.core.TypeLambda value;
    
    public TypeLambda (hydra.core.TypeLambda value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TypeLambda)) {
        return false;
      }
      TypeLambda o = (TypeLambda) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Term other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TypeLambda o = (TypeLambda) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * An injection; an instance of a union type
   */
  public static final class Union extends hydra.core.Term implements Serializable {
    public final hydra.core.Injection value;
    
    public Union (hydra.core.Injection value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Union)) {
        return false;
      }
      Union o = (Union) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Term other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Union o = (Union) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A unit value; a term with no value
   */
  public static final class Unit extends hydra.core.Term implements Serializable {
    public Unit () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Unit)) {
        return false;
      }
      Unit o = (Unit) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Term other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A variable reference
   */
  public static final class Variable extends hydra.core.Term implements Serializable {
    public final hydra.core.Name value;
    
    public Variable (hydra.core.Name value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Variable)) {
        return false;
      }
      Variable o = (Variable) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Term other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Variable o = (Variable) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * A wrapped term; an instance of a wrapper type (newtype)
   */
  public static final class Wrap extends hydra.core.Term implements Serializable {
    public final hydra.core.WrappedTerm value;
    
    public Wrap (hydra.core.WrappedTerm value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Wrap)) {
        return false;
      }
      Wrap o = (Wrap) other;
      return java.util.Objects.equals(
        this.value,
        o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * java.util.Objects.hashCode(value);
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Term other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Wrap o = (Wrap) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
