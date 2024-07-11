// Note: this is an automatically generated file. Do not edit.

package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public abstract class Atom implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.Atom");
  
  private Atom () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Literal instance) ;
    
    R visit(Parameter instance) ;
    
    R visit(Case instance) ;
    
    R visit(CountStar instance) ;
    
    R visit(ListComprehension instance) ;
    
    R visit(PatternComprehension instance) ;
    
    R visit(Quantifier instance) ;
    
    R visit(PatternPredicate instance) ;
    
    R visit(Parenthesized instance) ;
    
    R visit(FunctionInvocation instance) ;
    
    R visit(ExistentialSubquery instance) ;
    
    R visit(Variable instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Atom instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Literal instance) {
      return otherwise((instance));
    }
    
    default R visit(Parameter instance) {
      return otherwise((instance));
    }
    
    default R visit(Case instance) {
      return otherwise((instance));
    }
    
    default R visit(CountStar instance) {
      return otherwise((instance));
    }
    
    default R visit(ListComprehension instance) {
      return otherwise((instance));
    }
    
    default R visit(PatternComprehension instance) {
      return otherwise((instance));
    }
    
    default R visit(Quantifier instance) {
      return otherwise((instance));
    }
    
    default R visit(PatternPredicate instance) {
      return otherwise((instance));
    }
    
    default R visit(Parenthesized instance) {
      return otherwise((instance));
    }
    
    default R visit(FunctionInvocation instance) {
      return otherwise((instance));
    }
    
    default R visit(ExistentialSubquery instance) {
      return otherwise((instance));
    }
    
    default R visit(Variable instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Literal extends hydra.langs.cypher.openCypher.Atom implements Serializable {
    public final hydra.langs.cypher.openCypher.Literal value;
    
    public Literal (hydra.langs.cypher.openCypher.Literal value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Literal)) {
        return false;
      }
      Literal o = (Literal) (other);
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
  
  public static final class Parameter extends hydra.langs.cypher.openCypher.Atom implements Serializable {
    public final hydra.langs.cypher.openCypher.Parameter value;
    
    public Parameter (hydra.langs.cypher.openCypher.Parameter value) {
      java.util.Objects.requireNonNull((value));
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
      java.util.Objects.requireNonNull((value));
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
  
  public static final class CountStar extends hydra.langs.cypher.openCypher.Atom implements Serializable {
    public CountStar () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CountStar)) {
        return false;
      }
      CountStar o = (CountStar) (other);
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
  
  public static final class ListComprehension extends hydra.langs.cypher.openCypher.Atom implements Serializable {
    public final hydra.langs.cypher.openCypher.ListComprehension value;
    
    public ListComprehension (hydra.langs.cypher.openCypher.ListComprehension value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ListComprehension)) {
        return false;
      }
      ListComprehension o = (ListComprehension) (other);
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
  
  public static final class PatternComprehension extends hydra.langs.cypher.openCypher.Atom implements Serializable {
    public final hydra.langs.cypher.openCypher.PatternComprehension value;
    
    public PatternComprehension (hydra.langs.cypher.openCypher.PatternComprehension value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PatternComprehension)) {
        return false;
      }
      PatternComprehension o = (PatternComprehension) (other);
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
      java.util.Objects.requireNonNull((value));
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
  
  public static final class PatternPredicate extends hydra.langs.cypher.openCypher.Atom implements Serializable {
    public final hydra.langs.cypher.openCypher.PatternPredicate value;
    
    public PatternPredicate (hydra.langs.cypher.openCypher.PatternPredicate value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PatternPredicate)) {
        return false;
      }
      PatternPredicate o = (PatternPredicate) (other);
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
  
  public static final class Parenthesized extends hydra.langs.cypher.openCypher.Atom implements Serializable {
    public final hydra.langs.cypher.openCypher.ParenthesizedExpression value;
    
    public Parenthesized (hydra.langs.cypher.openCypher.ParenthesizedExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parenthesized)) {
        return false;
      }
      Parenthesized o = (Parenthesized) (other);
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
  
  public static final class FunctionInvocation extends hydra.langs.cypher.openCypher.Atom implements Serializable {
    public final hydra.langs.cypher.openCypher.FunctionInvocation value;
    
    public FunctionInvocation (hydra.langs.cypher.openCypher.FunctionInvocation value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FunctionInvocation)) {
        return false;
      }
      FunctionInvocation o = (FunctionInvocation) (other);
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
  
  public static final class ExistentialSubquery extends hydra.langs.cypher.openCypher.Atom implements Serializable {
    public final hydra.langs.cypher.openCypher.ExistentialSubquery value;
    
    public ExistentialSubquery (hydra.langs.cypher.openCypher.ExistentialSubquery value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ExistentialSubquery)) {
        return false;
      }
      ExistentialSubquery o = (ExistentialSubquery) (other);
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
      java.util.Objects.requireNonNull((value));
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