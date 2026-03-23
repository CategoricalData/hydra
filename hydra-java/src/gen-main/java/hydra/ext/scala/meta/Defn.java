// Note: this is an automatically generated file. Do not edit.

package hydra.ext.scala.meta;

import java.io.Serializable;

public abstract class Defn implements Serializable, Comparable<Defn> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.scala.meta.Defn");

  public static final hydra.core.Name VAL = new hydra.core.Name("val");

  public static final hydra.core.Name VAR = new hydra.core.Name("var");

  public static final hydra.core.Name GIVEN = new hydra.core.Name("given");

  public static final hydra.core.Name ENUM = new hydra.core.Name("enum");

  public static final hydra.core.Name ENUM_CASE = new hydra.core.Name("enumCase");

  public static final hydra.core.Name REPEATED_ENUM_CASE = new hydra.core.Name("repeatedEnumCase");

  public static final hydra.core.Name GIVEN_ALIAS = new hydra.core.Name("givenAlias");

  public static final hydra.core.Name EXTENSION_GROUP = new hydra.core.Name("extensionGroup");

  public static final hydra.core.Name DEF = new hydra.core.Name("def");

  public static final hydra.core.Name MACRO = new hydra.core.Name("macro");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name CLASS = new hydra.core.Name("class");

  public static final hydra.core.Name TRAIT = new hydra.core.Name("trait");

  public static final hydra.core.Name OBJECT = new hydra.core.Name("object");

  private Defn () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Val instance) ;

    R visit(Var instance) ;

    R visit(Given instance) ;

    R visit(Enum_ instance) ;

    R visit(EnumCase instance) ;

    R visit(RepeatedEnumCase instance) ;

    R visit(GivenAlias instance) ;

    R visit(ExtensionGroup instance) ;

    R visit(Def instance) ;

    R visit(Macro instance) ;

    R visit(Type instance) ;

    R visit(Class_ instance) ;

    R visit(Trait instance) ;

    R visit(Object_ instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Defn instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Val instance) {
      return otherwise(instance);
    }

    default R visit(Var instance) {
      return otherwise(instance);
    }

    default R visit(Given instance) {
      return otherwise(instance);
    }

    default R visit(Enum_ instance) {
      return otherwise(instance);
    }

    default R visit(EnumCase instance) {
      return otherwise(instance);
    }

    default R visit(RepeatedEnumCase instance) {
      return otherwise(instance);
    }

    default R visit(GivenAlias instance) {
      return otherwise(instance);
    }

    default R visit(ExtensionGroup instance) {
      return otherwise(instance);
    }

    default R visit(Def instance) {
      return otherwise(instance);
    }

    default R visit(Macro instance) {
      return otherwise(instance);
    }

    default R visit(Type instance) {
      return otherwise(instance);
    }

    default R visit(Class_ instance) {
      return otherwise(instance);
    }

    default R visit(Trait instance) {
      return otherwise(instance);
    }

    default R visit(Object_ instance) {
      return otherwise(instance);
    }
  }

  public static final class Val extends hydra.ext.scala.meta.Defn implements Serializable {
    public final hydra.ext.scala.meta.Defn_Val value;

    public Val (hydra.ext.scala.meta.Defn_Val value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Val)) {
        return false;
      }
      Val o = (Val) other;
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
    public int compareTo(Defn other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Val o = (Val) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Var extends hydra.ext.scala.meta.Defn implements Serializable {
    public final hydra.ext.scala.meta.Defn_Var value;

    public Var (hydra.ext.scala.meta.Defn_Var value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Var)) {
        return false;
      }
      Var o = (Var) other;
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
    public int compareTo(Defn other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Var o = (Var) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Given extends hydra.ext.scala.meta.Defn implements Serializable {
    public final hydra.ext.scala.meta.Defn_Given value;

    public Given (hydra.ext.scala.meta.Defn_Given value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Given)) {
        return false;
      }
      Given o = (Given) other;
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
    public int compareTo(Defn other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Given o = (Given) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Enum_ extends hydra.ext.scala.meta.Defn implements Serializable {
    public final hydra.ext.scala.meta.Defn_Enum value;

    public Enum_ (hydra.ext.scala.meta.Defn_Enum value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Enum_)) {
        return false;
      }
      Enum_ o = (Enum_) other;
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
    public int compareTo(Defn other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Enum_ o = (Enum_) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class EnumCase extends hydra.ext.scala.meta.Defn implements Serializable {
    public final hydra.ext.scala.meta.Defn_EnumCase value;

    public EnumCase (hydra.ext.scala.meta.Defn_EnumCase value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EnumCase)) {
        return false;
      }
      EnumCase o = (EnumCase) other;
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
    public int compareTo(Defn other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      EnumCase o = (EnumCase) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class RepeatedEnumCase extends hydra.ext.scala.meta.Defn implements Serializable {
    public final hydra.ext.scala.meta.Defn_RepeatedEnumCase value;

    public RepeatedEnumCase (hydra.ext.scala.meta.Defn_RepeatedEnumCase value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RepeatedEnumCase)) {
        return false;
      }
      RepeatedEnumCase o = (RepeatedEnumCase) other;
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
    public int compareTo(Defn other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      RepeatedEnumCase o = (RepeatedEnumCase) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class GivenAlias extends hydra.ext.scala.meta.Defn implements Serializable {
    public final hydra.ext.scala.meta.Defn_GivenAlias value;

    public GivenAlias (hydra.ext.scala.meta.Defn_GivenAlias value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GivenAlias)) {
        return false;
      }
      GivenAlias o = (GivenAlias) other;
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
    public int compareTo(Defn other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      GivenAlias o = (GivenAlias) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ExtensionGroup extends hydra.ext.scala.meta.Defn implements Serializable {
    public final hydra.ext.scala.meta.Defn_ExtensionGroup value;

    public ExtensionGroup (hydra.ext.scala.meta.Defn_ExtensionGroup value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ExtensionGroup)) {
        return false;
      }
      ExtensionGroup o = (ExtensionGroup) other;
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
    public int compareTo(Defn other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ExtensionGroup o = (ExtensionGroup) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Def extends hydra.ext.scala.meta.Defn implements Serializable {
    public final hydra.ext.scala.meta.Defn_Def value;

    public Def (hydra.ext.scala.meta.Defn_Def value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Def)) {
        return false;
      }
      Def o = (Def) other;
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
    public int compareTo(Defn other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Def o = (Def) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Macro extends hydra.ext.scala.meta.Defn implements Serializable {
    public final hydra.ext.scala.meta.Defn_Macro value;

    public Macro (hydra.ext.scala.meta.Defn_Macro value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Macro)) {
        return false;
      }
      Macro o = (Macro) other;
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
    public int compareTo(Defn other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Macro o = (Macro) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Type extends hydra.ext.scala.meta.Defn implements Serializable {
    public final hydra.ext.scala.meta.Defn_Type value;

    public Type (hydra.ext.scala.meta.Defn_Type value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Type)) {
        return false;
      }
      Type o = (Type) other;
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
    public int compareTo(Defn other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Type o = (Type) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Class_ extends hydra.ext.scala.meta.Defn implements Serializable {
    public final hydra.ext.scala.meta.Defn_Class value;

    public Class_ (hydra.ext.scala.meta.Defn_Class value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Class_)) {
        return false;
      }
      Class_ o = (Class_) other;
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
    public int compareTo(Defn other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Class_ o = (Class_) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Trait extends hydra.ext.scala.meta.Defn implements Serializable {
    public final hydra.ext.scala.meta.Defn_Trait value;

    public Trait (hydra.ext.scala.meta.Defn_Trait value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Trait)) {
        return false;
      }
      Trait o = (Trait) other;
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
    public int compareTo(Defn other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Trait o = (Trait) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Object_ extends hydra.ext.scala.meta.Defn implements Serializable {
    public final hydra.ext.scala.meta.Defn_Object value;

    public Object_ (hydra.ext.scala.meta.Defn_Object value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Object_)) {
        return false;
      }
      Object_ o = (Object_) other;
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
    public int compareTo(Defn other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Object_ o = (Object_) other;
      return ((Comparable) value).compareTo(o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
