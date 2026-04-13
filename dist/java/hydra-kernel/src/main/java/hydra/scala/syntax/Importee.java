// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public abstract class Importee implements Serializable, Comparable<Importee> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Importee");

  public static final hydra.core.Name WILDCARD = new hydra.core.Name("wildcard");

  public static final hydra.core.Name GIVEN = new hydra.core.Name("given");

  public static final hydra.core.Name GIVEN_ALL = new hydra.core.Name("givenAll");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name RENAME = new hydra.core.Name("rename");

  public static final hydra.core.Name UNIMPORT = new hydra.core.Name("unimport");

  private Importee () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Wildcard instance) ;

    R visit(Given instance) ;

    R visit(GivenAll instance) ;

    R visit(Name instance) ;

    R visit(Rename instance) ;

    R visit(Unimport instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Importee instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Wildcard instance) {
      return otherwise(instance);
    }

    default R visit(Given instance) {
      return otherwise(instance);
    }

    default R visit(GivenAll instance) {
      return otherwise(instance);
    }

    default R visit(Name instance) {
      return otherwise(instance);
    }

    default R visit(Rename instance) {
      return otherwise(instance);
    }

    default R visit(Unimport instance) {
      return otherwise(instance);
    }
  }

  public static final class Wildcard extends hydra.scala.syntax.Importee implements Serializable {
    public Wildcard () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Wildcard)) {
        return false;
      }
      Wildcard o = (Wildcard) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Importee other) {
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

  public static final class Given extends hydra.scala.syntax.Importee implements Serializable {
    public final hydra.scala.syntax.Importee_Given value;

    public Given (hydra.scala.syntax.Importee_Given value) {
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
    public int compareTo(Importee other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Given o = (Given) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class GivenAll extends hydra.scala.syntax.Importee implements Serializable {
    public GivenAll () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GivenAll)) {
        return false;
      }
      GivenAll o = (GivenAll) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Importee other) {
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

  public static final class Name extends hydra.scala.syntax.Importee implements Serializable {
    public final hydra.scala.syntax.Importee_Name value;

    public Name (hydra.scala.syntax.Importee_Name value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Name)) {
        return false;
      }
      Name o = (Name) other;
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
    public int compareTo(Importee other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Name o = (Name) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Rename extends hydra.scala.syntax.Importee implements Serializable {
    public final hydra.scala.syntax.Importee_Rename value;

    public Rename (hydra.scala.syntax.Importee_Rename value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Rename)) {
        return false;
      }
      Rename o = (Rename) other;
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
    public int compareTo(Importee other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Rename o = (Rename) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Unimport extends hydra.scala.syntax.Importee implements Serializable {
    public final hydra.scala.syntax.Importee_Unimport value;

    public Unimport (hydra.scala.syntax.Importee_Unimport value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Unimport)) {
        return false;
      }
      Unimport o = (Unimport) other;
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
    public int compareTo(Importee other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Unimport o = (Unimport) other;
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
