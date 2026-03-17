// Note: this is an automatically generated file. Do not edit.

package hydra.variants;

import java.io.Serializable;

/**
 * The identifier of a type constructor
 */
public abstract class TypeVariant implements Serializable, Comparable<TypeVariant> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.variants.TypeVariant");

  public static final hydra.core.Name ANNOTATED = new hydra.core.Name("annotated");

  public static final hydra.core.Name APPLICATION = new hydra.core.Name("application");

  public static final hydra.core.Name EITHER = new hydra.core.Name("either");

  public static final hydra.core.Name FORALL = new hydra.core.Name("forall");

  public static final hydra.core.Name FUNCTION = new hydra.core.Name("function");

  public static final hydra.core.Name LIST = new hydra.core.Name("list");

  public static final hydra.core.Name LITERAL = new hydra.core.Name("literal");

  public static final hydra.core.Name MAP = new hydra.core.Name("map");

  public static final hydra.core.Name MAYBE = new hydra.core.Name("maybe");

  public static final hydra.core.Name PAIR = new hydra.core.Name("pair");

  public static final hydra.core.Name RECORD = new hydra.core.Name("record");

  public static final hydra.core.Name SET = new hydra.core.Name("set");

  public static final hydra.core.Name UNION = new hydra.core.Name("union");

  public static final hydra.core.Name UNIT = new hydra.core.Name("unit");

  public static final hydra.core.Name VARIABLE = new hydra.core.Name("variable");

  public static final hydra.core.Name WRAP = new hydra.core.Name("wrap");

  private TypeVariant () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Annotated instance) ;

    R visit(Application instance) ;

    R visit(Either instance) ;

    R visit(Forall instance) ;

    R visit(Function instance) ;

    R visit(List instance) ;

    R visit(Literal instance) ;

    R visit(Map instance) ;

    R visit(Maybe instance) ;

    R visit(Pair instance) ;

    R visit(Record instance) ;

    R visit(Set instance) ;

    R visit(Union instance) ;

    R visit(Unit instance) ;

    R visit(Variable instance) ;

    R visit(Wrap instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TypeVariant instance) {
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

    default R visit(Forall instance) {
      return otherwise(instance);
    }

    default R visit(Function instance) {
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

  public static final class Annotated extends hydra.variants.TypeVariant implements Serializable {
    public Annotated () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Annotated)) {
        return false;
      }
      Annotated o = (Annotated) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TypeVariant other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
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

  public static final class Application extends hydra.variants.TypeVariant implements Serializable {
    public Application () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Application)) {
        return false;
      }
      Application o = (Application) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TypeVariant other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
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

  public static final class Either extends hydra.variants.TypeVariant implements Serializable {
    public Either () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Either)) {
        return false;
      }
      Either o = (Either) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TypeVariant other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
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

  public static final class Forall extends hydra.variants.TypeVariant implements Serializable {
    public Forall () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Forall)) {
        return false;
      }
      Forall o = (Forall) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TypeVariant other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
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

  public static final class Function extends hydra.variants.TypeVariant implements Serializable {
    public Function () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Function)) {
        return false;
      }
      Function o = (Function) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TypeVariant other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
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

  public static final class List extends hydra.variants.TypeVariant implements Serializable {
    public List () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof List)) {
        return false;
      }
      List o = (List) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TypeVariant other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
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

  public static final class Literal extends hydra.variants.TypeVariant implements Serializable {
    public Literal () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Literal)) {
        return false;
      }
      Literal o = (Literal) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TypeVariant other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
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

  public static final class Map extends hydra.variants.TypeVariant implements Serializable {
    public Map () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Map)) {
        return false;
      }
      Map o = (Map) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TypeVariant other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
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

  public static final class Maybe extends hydra.variants.TypeVariant implements Serializable {
    public Maybe () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Maybe)) {
        return false;
      }
      Maybe o = (Maybe) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TypeVariant other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
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

  public static final class Pair extends hydra.variants.TypeVariant implements Serializable {
    public Pair () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Pair)) {
        return false;
      }
      Pair o = (Pair) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TypeVariant other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
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

  public static final class Record extends hydra.variants.TypeVariant implements Serializable {
    public Record () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Record)) {
        return false;
      }
      Record o = (Record) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TypeVariant other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
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

  public static final class Set extends hydra.variants.TypeVariant implements Serializable {
    public Set () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Set)) {
        return false;
      }
      Set o = (Set) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TypeVariant other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
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

  public static final class Union extends hydra.variants.TypeVariant implements Serializable {
    public Union () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Union)) {
        return false;
      }
      Union o = (Union) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TypeVariant other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
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

  public static final class Unit extends hydra.variants.TypeVariant implements Serializable {
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
    public int compareTo(TypeVariant other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
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

  public static final class Variable extends hydra.variants.TypeVariant implements Serializable {
    public Variable () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Variable)) {
        return false;
      }
      Variable o = (Variable) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TypeVariant other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
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

  public static final class Wrap extends hydra.variants.TypeVariant implements Serializable {
    public Wrap () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Wrap)) {
        return false;
      }
      Wrap o = (Wrap) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TypeVariant other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
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
}
