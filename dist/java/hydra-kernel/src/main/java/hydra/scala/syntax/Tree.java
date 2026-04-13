// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public abstract class Tree implements Serializable, Comparable<Tree> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Tree");

  public static final hydra.core.Name REF = new hydra.core.Name("ref");

  public static final hydra.core.Name STAT = new hydra.core.Name("stat");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public static final hydra.core.Name BOUNDS = new hydra.core.Name("bounds");

  public static final hydra.core.Name PAT = new hydra.core.Name("pat");

  public static final hydra.core.Name MEMBER = new hydra.core.Name("member");

  public static final hydra.core.Name CTOR = new hydra.core.Name("ctor");

  public static final hydra.core.Name TEMPLATE = new hydra.core.Name("template");

  public static final hydra.core.Name MOD = new hydra.core.Name("mod");

  public static final hydra.core.Name ENUMERATOR = new hydra.core.Name("enumerator");

  public static final hydra.core.Name IMPORTER = new hydra.core.Name("importer");

  public static final hydra.core.Name IMPORTEE = new hydra.core.Name("importee");

  public static final hydra.core.Name CASE_TREE = new hydra.core.Name("caseTree");

  public static final hydra.core.Name SOURCE = new hydra.core.Name("source");

  public static final hydra.core.Name QUASI = new hydra.core.Name("quasi");

  private Tree () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Ref instance) ;

    R visit(Stat instance) ;

    R visit(Type instance) ;

    R visit(Bounds instance) ;

    R visit(Pat instance) ;

    R visit(Member instance) ;

    R visit(Ctor instance) ;

    R visit(Template instance) ;

    R visit(Mod instance) ;

    R visit(Enumerator instance) ;

    R visit(Importer instance) ;

    R visit(Importee instance) ;

    R visit(CaseTree instance) ;

    R visit(Source instance) ;

    R visit(Quasi instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Tree instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Ref instance) {
      return otherwise(instance);
    }

    default R visit(Stat instance) {
      return otherwise(instance);
    }

    default R visit(Type instance) {
      return otherwise(instance);
    }

    default R visit(Bounds instance) {
      return otherwise(instance);
    }

    default R visit(Pat instance) {
      return otherwise(instance);
    }

    default R visit(Member instance) {
      return otherwise(instance);
    }

    default R visit(Ctor instance) {
      return otherwise(instance);
    }

    default R visit(Template instance) {
      return otherwise(instance);
    }

    default R visit(Mod instance) {
      return otherwise(instance);
    }

    default R visit(Enumerator instance) {
      return otherwise(instance);
    }

    default R visit(Importer instance) {
      return otherwise(instance);
    }

    default R visit(Importee instance) {
      return otherwise(instance);
    }

    default R visit(CaseTree instance) {
      return otherwise(instance);
    }

    default R visit(Source instance) {
      return otherwise(instance);
    }

    default R visit(Quasi instance) {
      return otherwise(instance);
    }
  }

  public static final class Ref extends hydra.scala.syntax.Tree implements Serializable {
    public final hydra.scala.syntax.Ref value;

    public Ref (hydra.scala.syntax.Ref value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ref)) {
        return false;
      }
      Ref o = (Ref) other;
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
    public int compareTo(Tree other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Ref o = (Ref) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Stat extends hydra.scala.syntax.Tree implements Serializable {
    public final hydra.scala.syntax.Stat value;

    public Stat (hydra.scala.syntax.Stat value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Stat)) {
        return false;
      }
      Stat o = (Stat) other;
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
    public int compareTo(Tree other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Stat o = (Stat) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Type extends hydra.scala.syntax.Tree implements Serializable {
    public final hydra.scala.syntax.Type value;

    public Type (hydra.scala.syntax.Type value) {
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
    public int compareTo(Tree other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Type o = (Type) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Bounds extends hydra.scala.syntax.Tree implements Serializable {
    public final hydra.scala.syntax.TypeBounds value;

    public Bounds (hydra.scala.syntax.TypeBounds value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Bounds)) {
        return false;
      }
      Bounds o = (Bounds) other;
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
    public int compareTo(Tree other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Bounds o = (Bounds) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Pat extends hydra.scala.syntax.Tree implements Serializable {
    public final hydra.scala.syntax.Pat value;

    public Pat (hydra.scala.syntax.Pat value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Pat)) {
        return false;
      }
      Pat o = (Pat) other;
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
    public int compareTo(Tree other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Pat o = (Pat) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Member extends hydra.scala.syntax.Tree implements Serializable {
    public final hydra.scala.syntax.Member value;

    public Member (hydra.scala.syntax.Member value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Member)) {
        return false;
      }
      Member o = (Member) other;
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
    public int compareTo(Tree other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Member o = (Member) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Ctor extends hydra.scala.syntax.Tree implements Serializable {
    public final hydra.scala.syntax.Ctor value;

    public Ctor (hydra.scala.syntax.Ctor value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ctor)) {
        return false;
      }
      Ctor o = (Ctor) other;
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
    public int compareTo(Tree other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Ctor o = (Ctor) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Template extends hydra.scala.syntax.Tree implements Serializable {
    public final hydra.scala.syntax.Template value;

    public Template (hydra.scala.syntax.Template value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Template)) {
        return false;
      }
      Template o = (Template) other;
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
    public int compareTo(Tree other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Template o = (Template) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Mod extends hydra.scala.syntax.Tree implements Serializable {
    public final hydra.scala.syntax.Mod value;

    public Mod (hydra.scala.syntax.Mod value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Mod)) {
        return false;
      }
      Mod o = (Mod) other;
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
    public int compareTo(Tree other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Mod o = (Mod) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Enumerator extends hydra.scala.syntax.Tree implements Serializable {
    public final hydra.scala.syntax.Enumerator value;

    public Enumerator (hydra.scala.syntax.Enumerator value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Enumerator)) {
        return false;
      }
      Enumerator o = (Enumerator) other;
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
    public int compareTo(Tree other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Enumerator o = (Enumerator) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Importer extends hydra.scala.syntax.Tree implements Serializable {
    public final hydra.scala.syntax.Importer value;

    public Importer (hydra.scala.syntax.Importer value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Importer)) {
        return false;
      }
      Importer o = (Importer) other;
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
    public int compareTo(Tree other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Importer o = (Importer) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Importee extends hydra.scala.syntax.Tree implements Serializable {
    public final hydra.scala.syntax.Importee value;

    public Importee (hydra.scala.syntax.Importee value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Importee)) {
        return false;
      }
      Importee o = (Importee) other;
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
    public int compareTo(Tree other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Importee o = (Importee) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class CaseTree extends hydra.scala.syntax.Tree implements Serializable {
    public final hydra.scala.syntax.CaseTree value;

    public CaseTree (hydra.scala.syntax.CaseTree value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CaseTree)) {
        return false;
      }
      CaseTree o = (CaseTree) other;
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
    public int compareTo(Tree other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      CaseTree o = (CaseTree) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Source extends hydra.scala.syntax.Tree implements Serializable {
    public final hydra.scala.syntax.Source value;

    public Source (hydra.scala.syntax.Source value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Source)) {
        return false;
      }
      Source o = (Source) other;
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
    public int compareTo(Tree other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Source o = (Source) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Quasi extends hydra.scala.syntax.Tree implements Serializable {
    public final hydra.scala.syntax.Quasi value;

    public Quasi (hydra.scala.syntax.Quasi value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Quasi)) {
        return false;
      }
      Quasi o = (Quasi) other;
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
    public int compareTo(Tree other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Quasi o = (Quasi) other;
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
