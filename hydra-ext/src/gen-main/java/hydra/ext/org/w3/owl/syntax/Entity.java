// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public abstract class Entity implements Serializable, Comparable<Entity> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.Entity");

  public static final hydra.core.Name ANNOTATION_PROPERTY = new hydra.core.Name("annotationProperty");

  public static final hydra.core.Name CLASS = new hydra.core.Name("class");

  public static final hydra.core.Name DATA_PROPERTY = new hydra.core.Name("dataProperty");

  public static final hydra.core.Name DATATYPE = new hydra.core.Name("datatype");

  public static final hydra.core.Name NAMED_INDIVIDUAL = new hydra.core.Name("namedIndividual");

  public static final hydra.core.Name OBJECT_PROPERTY = new hydra.core.Name("objectProperty");

  private Entity () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(AnnotationProperty instance) ;

    R visit(Class_ instance) ;

    R visit(DataProperty instance) ;

    R visit(Datatype instance) ;

    R visit(NamedIndividual instance) ;

    R visit(ObjectProperty instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Entity instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(AnnotationProperty instance) {
      return otherwise(instance);
    }

    default R visit(Class_ instance) {
      return otherwise(instance);
    }

    default R visit(DataProperty instance) {
      return otherwise(instance);
    }

    default R visit(Datatype instance) {
      return otherwise(instance);
    }

    default R visit(NamedIndividual instance) {
      return otherwise(instance);
    }

    default R visit(ObjectProperty instance) {
      return otherwise(instance);
    }
  }

  public static final class AnnotationProperty extends hydra.ext.org.w3.owl.syntax.Entity implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.AnnotationProperty value;

    public AnnotationProperty (hydra.ext.org.w3.owl.syntax.AnnotationProperty value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AnnotationProperty)) {
        return false;
      }
      AnnotationProperty o = (AnnotationProperty) other;
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
    public int compareTo(Entity other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      AnnotationProperty o = (AnnotationProperty) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Class_ extends hydra.ext.org.w3.owl.syntax.Entity implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.Class_ value;

    public Class_ (hydra.ext.org.w3.owl.syntax.Class_ value) {
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
    public int compareTo(Entity other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Class_ o = (Class_) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class DataProperty extends hydra.ext.org.w3.owl.syntax.Entity implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.DataProperty value;

    public DataProperty (hydra.ext.org.w3.owl.syntax.DataProperty value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DataProperty)) {
        return false;
      }
      DataProperty o = (DataProperty) other;
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
    public int compareTo(Entity other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DataProperty o = (DataProperty) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Datatype extends hydra.ext.org.w3.owl.syntax.Entity implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.Datatype value;

    public Datatype (hydra.ext.org.w3.owl.syntax.Datatype value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Datatype)) {
        return false;
      }
      Datatype o = (Datatype) other;
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
    public int compareTo(Entity other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Datatype o = (Datatype) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class NamedIndividual extends hydra.ext.org.w3.owl.syntax.Entity implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.NamedIndividual value;

    public NamedIndividual (hydra.ext.org.w3.owl.syntax.NamedIndividual value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NamedIndividual)) {
        return false;
      }
      NamedIndividual o = (NamedIndividual) other;
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
    public int compareTo(Entity other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NamedIndividual o = (NamedIndividual) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ObjectProperty extends hydra.ext.org.w3.owl.syntax.Entity implements Serializable {
    public final hydra.ext.org.w3.owl.syntax.ObjectProperty value;

    public ObjectProperty (hydra.ext.org.w3.owl.syntax.ObjectProperty value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ObjectProperty)) {
        return false;
      }
      ObjectProperty o = (ObjectProperty) other;
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
    public int compareTo(Entity other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ObjectProperty o = (ObjectProperty) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
