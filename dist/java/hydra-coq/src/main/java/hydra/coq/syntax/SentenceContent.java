// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

/**
 * The content of a top-level sentence
 */
public abstract class SentenceContent implements Serializable, Comparable<SentenceContent> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.SentenceContent");

  public static final hydra.core.Name AXIOM = new hydra.core.Name("axiom");

  public static final hydra.core.Name DEFINITION = new hydra.core.Name("definition");

  public static final hydra.core.Name FIXPOINT = new hydra.core.Name("fixpoint");

  public static final hydra.core.Name INDUCTIVE = new hydra.core.Name("inductive");

  public static final hydra.core.Name MODULE = new hydra.core.Name("module");

  public static final hydra.core.Name NOTATION = new hydra.core.Name("notation");

  public static final hydra.core.Name RECORD = new hydra.core.Name("record");

  public static final hydra.core.Name REQUIRE_IMPORT = new hydra.core.Name("requireImport");

  public static final hydra.core.Name SECTION = new hydra.core.Name("section");

  public static final hydra.core.Name THEOREM = new hydra.core.Name("theorem");

  private SentenceContent () {

  }

  public abstract <R> R accept(Visitor<R> visitor) ;

  public interface Visitor<R> {
    R visit(Axiom instance) ;

    R visit(Definition instance) ;

    R visit(Fixpoint instance) ;

    R visit(Inductive instance) ;

    R visit(Module instance) ;

    R visit(Notation instance) ;

    R visit(Record instance) ;

    R visit(RequireImport instance) ;

    R visit(Section instance) ;

    R visit(Theorem instance) ;
  }

  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(SentenceContent instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Axiom instance) {
      return otherwise(instance);
    }

    default R visit(Definition instance) {
      return otherwise(instance);
    }

    default R visit(Fixpoint instance) {
      return otherwise(instance);
    }

    default R visit(Inductive instance) {
      return otherwise(instance);
    }

    default R visit(Module instance) {
      return otherwise(instance);
    }

    default R visit(Notation instance) {
      return otherwise(instance);
    }

    default R visit(Record instance) {
      return otherwise(instance);
    }

    default R visit(RequireImport instance) {
      return otherwise(instance);
    }

    default R visit(Section instance) {
      return otherwise(instance);
    }

    default R visit(Theorem instance) {
      return otherwise(instance);
    }
  }

  public static final class Axiom extends hydra.coq.syntax.SentenceContent implements Serializable {
    public final hydra.coq.syntax.AxiomDeclaration value;

    public Axiom (hydra.coq.syntax.AxiomDeclaration value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Axiom)) {
        return false;
      }
      Axiom o = (Axiom) other;
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
    public int compareTo(SentenceContent other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Axiom o = (Axiom) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Definition extends hydra.coq.syntax.SentenceContent implements Serializable {
    public final hydra.coq.syntax.Definition value;

    public Definition (hydra.coq.syntax.Definition value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Definition)) {
        return false;
      }
      Definition o = (Definition) other;
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
    public int compareTo(SentenceContent other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Definition o = (Definition) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Fixpoint extends hydra.coq.syntax.SentenceContent implements Serializable {
    public final hydra.coq.syntax.FixpointDefinition value;

    public Fixpoint (hydra.coq.syntax.FixpointDefinition value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Fixpoint)) {
        return false;
      }
      Fixpoint o = (Fixpoint) other;
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
    public int compareTo(SentenceContent other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Fixpoint o = (Fixpoint) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Inductive extends hydra.coq.syntax.SentenceContent implements Serializable {
    public final hydra.coq.syntax.InductiveDefinition value;

    public Inductive (hydra.coq.syntax.InductiveDefinition value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Inductive)) {
        return false;
      }
      Inductive o = (Inductive) other;
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
    public int compareTo(SentenceContent other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Inductive o = (Inductive) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Module extends hydra.coq.syntax.SentenceContent implements Serializable {
    public final hydra.coq.syntax.ModuleDefinition value;

    public Module (hydra.coq.syntax.ModuleDefinition value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Module)) {
        return false;
      }
      Module o = (Module) other;
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
    public int compareTo(SentenceContent other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Module o = (Module) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Notation extends hydra.coq.syntax.SentenceContent implements Serializable {
    public final hydra.coq.syntax.NotationDeclaration value;

    public Notation (hydra.coq.syntax.NotationDeclaration value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Notation)) {
        return false;
      }
      Notation o = (Notation) other;
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
    public int compareTo(SentenceContent other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Notation o = (Notation) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Record extends hydra.coq.syntax.SentenceContent implements Serializable {
    public final hydra.coq.syntax.RecordDefinition value;

    public Record (hydra.coq.syntax.RecordDefinition value) {
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
    public int compareTo(SentenceContent other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Record o = (Record) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class RequireImport extends hydra.coq.syntax.SentenceContent implements Serializable {
    public final hydra.coq.syntax.RequireImport value;

    public RequireImport (hydra.coq.syntax.RequireImport value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RequireImport)) {
        return false;
      }
      RequireImport o = (RequireImport) other;
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
    public int compareTo(SentenceContent other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      RequireImport o = (RequireImport) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Section extends hydra.coq.syntax.SentenceContent implements Serializable {
    public final hydra.coq.syntax.SectionDefinition value;

    public Section (hydra.coq.syntax.SectionDefinition value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Section)) {
        return false;
      }
      Section o = (Section) other;
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
    public int compareTo(SentenceContent other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Section o = (Section) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Theorem extends hydra.coq.syntax.SentenceContent implements Serializable {
    public final hydra.coq.syntax.TheoremBody value;

    public Theorem (hydra.coq.syntax.TheoremBody value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Theorem)) {
        return false;
      }
      Theorem o = (Theorem) other;
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
    public int compareTo(SentenceContent other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Theorem o = (Theorem) other;
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
