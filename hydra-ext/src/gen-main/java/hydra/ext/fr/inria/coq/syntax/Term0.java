// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public abstract class Term0 implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/fr/inria/coq/syntax.Term0");
  
  public static final hydra.core.Name FIELD_NAME_QUALID_ANNOTATED = new hydra.core.Name("qualidAnnotated");
  
  public static final hydra.core.Name FIELD_NAME_SORT = new hydra.core.Name("sort");
  
  public static final hydra.core.Name FIELD_NAME_PRIMITIVE_NOTATIONS = new hydra.core.Name("primitiveNotations");
  
  public static final hydra.core.Name FIELD_NAME_EVAR = new hydra.core.Name("evar");
  
  public static final hydra.core.Name FIELD_NAME_MATCH = new hydra.core.Name("match");
  
  public static final hydra.core.Name FIELD_NAME_RECORD = new hydra.core.Name("record");
  
  public static final hydra.core.Name FIELD_NAME_GENERALIZING = new hydra.core.Name("generalizing");
  
  public static final hydra.core.Name FIELD_NAME_LTAC = new hydra.core.Name("ltac");
  
  public static final hydra.core.Name FIELD_NAME_PARENS = new hydra.core.Name("parens");
  
  private Term0 () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(QualidAnnotated instance) ;
    
    R visit(Sort instance) ;
    
    R visit(PrimitiveNotations instance) ;
    
    R visit(Evar instance) ;
    
    R visit(Match instance) ;
    
    R visit(Record instance) ;
    
    R visit(Generalizing instance) ;
    
    R visit(Ltac instance) ;
    
    R visit(Parens instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Term0 instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(QualidAnnotated instance) {
      return otherwise((instance));
    }
    
    default R visit(Sort instance) {
      return otherwise((instance));
    }
    
    default R visit(PrimitiveNotations instance) {
      return otherwise((instance));
    }
    
    default R visit(Evar instance) {
      return otherwise((instance));
    }
    
    default R visit(Match instance) {
      return otherwise((instance));
    }
    
    default R visit(Record instance) {
      return otherwise((instance));
    }
    
    default R visit(Generalizing instance) {
      return otherwise((instance));
    }
    
    default R visit(Ltac instance) {
      return otherwise((instance));
    }
    
    default R visit(Parens instance) {
      return otherwise((instance));
    }
  }
  
  public static final class QualidAnnotated extends hydra.ext.fr.inria.coq.syntax.Term0 implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.QualidAnnotated value;
    
    public QualidAnnotated (hydra.ext.fr.inria.coq.syntax.QualidAnnotated value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof QualidAnnotated)) {
        return false;
      }
      QualidAnnotated o = (QualidAnnotated) (other);
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
  
  public static final class Sort extends hydra.ext.fr.inria.coq.syntax.Term0 implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.Sort value;
    
    public Sort (hydra.ext.fr.inria.coq.syntax.Sort value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sort)) {
        return false;
      }
      Sort o = (Sort) (other);
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
  
  public static final class PrimitiveNotations extends hydra.ext.fr.inria.coq.syntax.Term0 implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.PrimitiveNotations value;
    
    public PrimitiveNotations (hydra.ext.fr.inria.coq.syntax.PrimitiveNotations value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PrimitiveNotations)) {
        return false;
      }
      PrimitiveNotations o = (PrimitiveNotations) (other);
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
  
  public static final class Evar extends hydra.ext.fr.inria.coq.syntax.Term0 implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.ExistentialVariable value;
    
    public Evar (hydra.ext.fr.inria.coq.syntax.ExistentialVariable value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Evar)) {
        return false;
      }
      Evar o = (Evar) (other);
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
  
  public static final class Match extends hydra.ext.fr.inria.coq.syntax.Term0 implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.Match value;
    
    public Match (hydra.ext.fr.inria.coq.syntax.Match value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Match)) {
        return false;
      }
      Match o = (Match) (other);
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
  
  public static final class Record extends hydra.ext.fr.inria.coq.syntax.Term0 implements Serializable {
    public Record () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Record)) {
        return false;
      }
      Record o = (Record) (other);
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
  
  public static final class Generalizing extends hydra.ext.fr.inria.coq.syntax.Term0 implements Serializable {
    public Generalizing () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Generalizing)) {
        return false;
      }
      Generalizing o = (Generalizing) (other);
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
  
  public static final class Ltac extends hydra.ext.fr.inria.coq.syntax.Term0 implements Serializable {
    public Ltac () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ltac)) {
        return false;
      }
      Ltac o = (Ltac) (other);
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
  
  public static final class Parens extends hydra.ext.fr.inria.coq.syntax.Term0 implements Serializable {
    public final hydra.ext.fr.inria.coq.syntax.Term value;
    
    public Parens (hydra.ext.fr.inria.coq.syntax.Term value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parens)) {
        return false;
      }
      Parens o = (Parens) (other);
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
}