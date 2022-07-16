package hydra.ext.coq.syntax;

public abstract class Term0 {
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
  
  public static final class QualidAnnotated extends Term0 {
    public final QualidAnnotated value;
    
    public QualidAnnotated (QualidAnnotated value) {
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
  
  public static final class Sort extends Term0 {
    public final Sort value;
    
    public Sort (Sort value) {
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
  
  public static final class PrimitiveNotations extends Term0 {
    public final PrimitiveNotations value;
    
    public PrimitiveNotations (PrimitiveNotations value) {
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
  
  public static final class Evar extends Term0 {
    public final ExistentialVariable value;
    
    public Evar (ExistentialVariable value) {
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
  
  public static final class Match extends Term0 {
    public final Match value;
    
    public Match (Match value) {
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
  
  public static final class Record extends Term0 {
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
  
  public static final class Generalizing extends Term0 {
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
  
  public static final class Ltac extends Term0 {
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
  
  public static final class Parens extends Term0 {
    public final Term value;
    
    public Parens (Term value) {
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