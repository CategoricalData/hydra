// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public abstract class Atom implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.Atom");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TRUE = new hydra.core.Name("true");
  
  public static final hydra.core.Name FIELD_NAME_FALSE = new hydra.core.Name("false");
  
  public static final hydra.core.Name FIELD_NAME_NONE = new hydra.core.Name("none");
  
  public static final hydra.core.Name FIELD_NAME_STRING = new hydra.core.Name("string");
  
  public static final hydra.core.Name FIELD_NAME_NUMBER = new hydra.core.Name("number");
  
  public static final hydra.core.Name FIELD_NAME_TUPLE = new hydra.core.Name("tuple");
  
  public static final hydra.core.Name FIELD_NAME_GROUP = new hydra.core.Name("group");
  
  public static final hydra.core.Name FIELD_NAME_GENEXP = new hydra.core.Name("genexp");
  
  public static final hydra.core.Name FIELD_NAME_LIST = new hydra.core.Name("list");
  
  public static final hydra.core.Name FIELD_NAME_LISTCOMP = new hydra.core.Name("listcomp");
  
  public static final hydra.core.Name FIELD_NAME_DICT = new hydra.core.Name("dict");
  
  public static final hydra.core.Name FIELD_NAME_SET = new hydra.core.Name("set");
  
  public static final hydra.core.Name FIELD_NAME_DICTCOMP = new hydra.core.Name("dictcomp");
  
  public static final hydra.core.Name FIELD_NAME_SETCOMP = new hydra.core.Name("setcomp");
  
  public static final hydra.core.Name FIELD_NAME_ELLIPSIS = new hydra.core.Name("ellipsis");
  
  private Atom () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Name instance) ;
    
    R visit(True instance) ;
    
    R visit(False instance) ;
    
    R visit(None instance) ;
    
    R visit(String_ instance) ;
    
    R visit(Number_ instance) ;
    
    R visit(Tuple instance) ;
    
    R visit(Group instance) ;
    
    R visit(Genexp instance) ;
    
    R visit(List instance) ;
    
    R visit(Listcomp instance) ;
    
    R visit(Dict instance) ;
    
    R visit(Set instance) ;
    
    R visit(Dictcomp instance) ;
    
    R visit(Setcomp instance) ;
    
    R visit(Ellipsis instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Atom instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Name instance) {
      return otherwise((instance));
    }
    
    default R visit(True instance) {
      return otherwise((instance));
    }
    
    default R visit(False instance) {
      return otherwise((instance));
    }
    
    default R visit(None instance) {
      return otherwise((instance));
    }
    
    default R visit(String_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Number_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Tuple instance) {
      return otherwise((instance));
    }
    
    default R visit(Group instance) {
      return otherwise((instance));
    }
    
    default R visit(Genexp instance) {
      return otherwise((instance));
    }
    
    default R visit(List instance) {
      return otherwise((instance));
    }
    
    default R visit(Listcomp instance) {
      return otherwise((instance));
    }
    
    default R visit(Dict instance) {
      return otherwise((instance));
    }
    
    default R visit(Set instance) {
      return otherwise((instance));
    }
    
    default R visit(Dictcomp instance) {
      return otherwise((instance));
    }
    
    default R visit(Setcomp instance) {
      return otherwise((instance));
    }
    
    default R visit(Ellipsis instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Name extends hydra.ext.python.syntax.Atom implements Serializable {
    public final hydra.ext.python.syntax.Name value;
    
    public Name (hydra.ext.python.syntax.Name value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Name)) {
        return false;
      }
      Name o = (Name) (other);
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
  
  public static final class True extends hydra.ext.python.syntax.Atom implements Serializable {
    public True () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof True)) {
        return false;
      }
      True o = (True) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class False extends hydra.ext.python.syntax.Atom implements Serializable {
    public False () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof False)) {
        return false;
      }
      False o = (False) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class None extends hydra.ext.python.syntax.Atom implements Serializable {
    public None () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof None)) {
        return false;
      }
      None o = (None) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class String_ extends hydra.ext.python.syntax.Atom implements Serializable {
    public final hydra.ext.python.syntax.String_ value;
    
    public String_ (hydra.ext.python.syntax.String_ value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof String_)) {
        return false;
      }
      String_ o = (String_) (other);
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
  
  public static final class Number_ extends hydra.ext.python.syntax.Atom implements Serializable {
    public final hydra.ext.python.syntax.Number_ value;
    
    public Number_ (hydra.ext.python.syntax.Number_ value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Number_)) {
        return false;
      }
      Number_ o = (Number_) (other);
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
  
  public static final class Tuple extends hydra.ext.python.syntax.Atom implements Serializable {
    public final hydra.ext.python.syntax.Tuple value;
    
    public Tuple (hydra.ext.python.syntax.Tuple value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Tuple)) {
        return false;
      }
      Tuple o = (Tuple) (other);
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
  
  public static final class Group extends hydra.ext.python.syntax.Atom implements Serializable {
    public final hydra.ext.python.syntax.Group value;
    
    public Group (hydra.ext.python.syntax.Group value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Group)) {
        return false;
      }
      Group o = (Group) (other);
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
  
  public static final class Genexp extends hydra.ext.python.syntax.Atom implements Serializable {
    public final hydra.ext.python.syntax.Genexp value;
    
    public Genexp (hydra.ext.python.syntax.Genexp value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Genexp)) {
        return false;
      }
      Genexp o = (Genexp) (other);
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
  
  public static final class List extends hydra.ext.python.syntax.Atom implements Serializable {
    public final hydra.ext.python.syntax.List value;
    
    public List (hydra.ext.python.syntax.List value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Listcomp extends hydra.ext.python.syntax.Atom implements Serializable {
    public final hydra.ext.python.syntax.Listcomp value;
    
    public Listcomp (hydra.ext.python.syntax.Listcomp value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Listcomp)) {
        return false;
      }
      Listcomp o = (Listcomp) (other);
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
  
  public static final class Dict extends hydra.ext.python.syntax.Atom implements Serializable {
    public final hydra.ext.python.syntax.Dict value;
    
    public Dict (hydra.ext.python.syntax.Dict value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Dict)) {
        return false;
      }
      Dict o = (Dict) (other);
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
  
  public static final class Set extends hydra.ext.python.syntax.Atom implements Serializable {
    public final hydra.ext.python.syntax.Set value;
    
    public Set (hydra.ext.python.syntax.Set value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Dictcomp extends hydra.ext.python.syntax.Atom implements Serializable {
    public final hydra.ext.python.syntax.Dictcomp value;
    
    public Dictcomp (hydra.ext.python.syntax.Dictcomp value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Dictcomp)) {
        return false;
      }
      Dictcomp o = (Dictcomp) (other);
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
  
  public static final class Setcomp extends hydra.ext.python.syntax.Atom implements Serializable {
    public final hydra.ext.python.syntax.Setcomp value;
    
    public Setcomp (hydra.ext.python.syntax.Setcomp value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Setcomp)) {
        return false;
      }
      Setcomp o = (Setcomp) (other);
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
  
  public static final class Ellipsis extends hydra.ext.python.syntax.Atom implements Serializable {
    public Ellipsis () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ellipsis)) {
        return false;
      }
      Ellipsis o = (Ellipsis) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}