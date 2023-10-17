package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public abstract class Atom implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.Atom");
  
  private Atom () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Parameter instance) ;
    
    R visit(Case instance) ;
    
    R visit(Count instance) ;
    
    R visit(List instance) ;
    
    R visit(Pattern instance) ;
    
    R visit(Quantifier instance) ;
    
    R visit(Predicate instance) ;
    
    R visit(Parens instance) ;
    
    R visit(Function instance) ;
    
    R visit(Existence instance) ;
    
    R visit(Variable instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Atom instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Parameter instance) {
      return otherwise((instance));
    }
    
    default R visit(Case instance) {
      return otherwise((instance));
    }
    
    default R visit(Count instance) {
      return otherwise((instance));
    }
    
    default R visit(List instance) {
      return otherwise((instance));
    }
    
    default R visit(Pattern instance) {
      return otherwise((instance));
    }
    
    default R visit(Quantifier instance) {
      return otherwise((instance));
    }
    
    default R visit(Predicate instance) {
      return otherwise((instance));
    }
    
    default R visit(Parens instance) {
      return otherwise((instance));
    }
    
    default R visit(Function instance) {
      return otherwise((instance));
    }
    
    default R visit(Existence instance) {
      return otherwise((instance));
    }
    
    default R visit(Variable instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Parameter extends hydra.langs.cypher.openCypher.Atom implements Serializable {
    public final hydra.langs.cypher.openCypher.Parameter value;
    
    public Parameter (hydra.langs.cypher.openCypher.Parameter value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parameter)) {
        return false;
      }
      Parameter o = (Parameter) (other);
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
  
  public static final class Case extends hydra.langs.cypher.openCypher.Atom implements Serializable {
    public final hydra.langs.cypher.openCypher.CaseExpression value;
    
    public Case (hydra.langs.cypher.openCypher.CaseExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Case)) {
        return false;
      }
      Case o = (Case) (other);
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
  
  public static final class Count extends hydra.langs.cypher.openCypher.Atom implements Serializable {
    public Count () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Count)) {
        return false;
      }
      Count o = (Count) (other);
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
  
  public static final class List extends hydra.langs.cypher.openCypher.Atom implements Serializable {
    public final hydra.langs.cypher.openCypher.ListComprehension value;
    
    public List (hydra.langs.cypher.openCypher.ListComprehension value) {
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
  
  public static final class Pattern extends hydra.langs.cypher.openCypher.Atom implements Serializable {
    public final hydra.langs.cypher.openCypher.PatternComprehension value;
    
    public Pattern (hydra.langs.cypher.openCypher.PatternComprehension value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Pattern)) {
        return false;
      }
      Pattern o = (Pattern) (other);
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
  
  public static final class Quantifier extends hydra.langs.cypher.openCypher.Atom implements Serializable {
    public final hydra.langs.cypher.openCypher.Quantifier value;
    
    public Quantifier (hydra.langs.cypher.openCypher.Quantifier value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Quantifier)) {
        return false;
      }
      Quantifier o = (Quantifier) (other);
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
  
  public static final class Predicate extends hydra.langs.cypher.openCypher.Atom implements Serializable {
    public final hydra.langs.cypher.openCypher.RelationshipsPattern value;
    
    public Predicate (hydra.langs.cypher.openCypher.RelationshipsPattern value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Predicate)) {
        return false;
      }
      Predicate o = (Predicate) (other);
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
  
  public static final class Parens extends hydra.langs.cypher.openCypher.Atom implements Serializable {
    public final hydra.langs.cypher.openCypher.Expression value;
    
    public Parens (hydra.langs.cypher.openCypher.Expression value) {
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
  
  public static final class Function extends hydra.langs.cypher.openCypher.Atom implements Serializable {
    public final hydra.langs.cypher.openCypher.FunctionInvocation value;
    
    public Function (hydra.langs.cypher.openCypher.FunctionInvocation value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Function)) {
        return false;
      }
      Function o = (Function) (other);
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
  
  public static final class Existence extends hydra.langs.cypher.openCypher.Atom implements Serializable {
    public final hydra.langs.cypher.openCypher.ExistentialSubquery value;
    
    public Existence (hydra.langs.cypher.openCypher.ExistentialSubquery value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Existence)) {
        return false;
      }
      Existence o = (Existence) (other);
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
  
  public static final class Variable extends hydra.langs.cypher.openCypher.Atom implements Serializable {
    public final hydra.langs.cypher.openCypher.Variable value;
    
    public Variable (hydra.langs.cypher.openCypher.Variable value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Variable)) {
        return false;
      }
      Variable o = (Variable) (other);
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