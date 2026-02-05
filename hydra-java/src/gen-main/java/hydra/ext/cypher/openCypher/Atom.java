// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public abstract class Atom implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.cypher.openCypher.Atom");
  
  public static final hydra.core.Name FIELD_NAME_LITERAL = new hydra.core.Name("literal");
  
  public static final hydra.core.Name FIELD_NAME_PARAMETER = new hydra.core.Name("parameter");
  
  public static final hydra.core.Name FIELD_NAME_CASE = new hydra.core.Name("case");
  
  public static final hydra.core.Name FIELD_NAME_COUNT_STAR = new hydra.core.Name("countStar");
  
  public static final hydra.core.Name FIELD_NAME_LIST_COMPREHENSION = new hydra.core.Name("listComprehension");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN_COMPREHENSION = new hydra.core.Name("patternComprehension");
  
  public static final hydra.core.Name FIELD_NAME_QUANTIFIER = new hydra.core.Name("quantifier");
  
  public static final hydra.core.Name FIELD_NAME_PATTERN_PREDICATE = new hydra.core.Name("patternPredicate");
  
  public static final hydra.core.Name FIELD_NAME_PARENTHESIZED = new hydra.core.Name("parenthesized");
  
  public static final hydra.core.Name FIELD_NAME_FUNCTION_INVOCATION = new hydra.core.Name("functionInvocation");
  
  public static final hydra.core.Name FIELD_NAME_EXISTENTIAL_SUBQUERY = new hydra.core.Name("existentialSubquery");
  
  public static final hydra.core.Name FIELD_NAME_VARIABLE = new hydra.core.Name("variable");
  
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
  
  public static final class Literal extends hydra.ext.cypher.openCypher.Atom implements Serializable {
    public final hydra.ext.cypher.openCypher.Literal value;
    
    public Literal (hydra.ext.cypher.openCypher.Literal value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Literal)) {
        return false;
      }
      Literal o = (Literal) (other);
      return other instanceof Atom;
    }
    
    @Override
    public int hashCode() {
      return getClass().hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Parameter extends hydra.ext.cypher.openCypher.Atom implements Serializable {
    public final hydra.ext.cypher.openCypher.Parameter value;
    
    public Parameter (hydra.ext.cypher.openCypher.Parameter value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parameter)) {
        return false;
      }
      Parameter o = (Parameter) (other);
      return other instanceof Atom;
    }
    
    @Override
    public int hashCode() {
      return getClass().hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Case extends hydra.ext.cypher.openCypher.Atom implements Serializable {
    public final hydra.ext.cypher.openCypher.CaseExpression value;
    
    public Case (hydra.ext.cypher.openCypher.CaseExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Case)) {
        return false;
      }
      Case o = (Case) (other);
      return other instanceof Atom;
    }
    
    @Override
    public int hashCode() {
      return getClass().hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class CountStar extends hydra.ext.cypher.openCypher.Atom implements Serializable {
    public final Boolean value;
    
    public CountStar (Boolean value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      return other instanceof CountStar;
    }
    
    @Override
    public int hashCode() {
      return getClass().hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ListComprehension extends hydra.ext.cypher.openCypher.Atom implements Serializable {
    public final hydra.ext.cypher.openCypher.ListComprehension value;
    
    public ListComprehension (hydra.ext.cypher.openCypher.ListComprehension value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ListComprehension)) {
        return false;
      }
      ListComprehension o = (ListComprehension) (other);
      return other instanceof Atom;
    }
    
    @Override
    public int hashCode() {
      return getClass().hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class PatternComprehension extends hydra.ext.cypher.openCypher.Atom implements Serializable {
    public final hydra.ext.cypher.openCypher.PatternComprehension value;
    
    public PatternComprehension (hydra.ext.cypher.openCypher.PatternComprehension value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PatternComprehension)) {
        return false;
      }
      PatternComprehension o = (PatternComprehension) (other);
      return other instanceof Atom;
    }
    
    @Override
    public int hashCode() {
      return getClass().hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Quantifier extends hydra.ext.cypher.openCypher.Atom implements Serializable {
    public final hydra.ext.cypher.openCypher.Quantifier value;
    
    public Quantifier (hydra.ext.cypher.openCypher.Quantifier value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Quantifier)) {
        return false;
      }
      Quantifier o = (Quantifier) (other);
      return other instanceof Atom;
    }
    
    @Override
    public int hashCode() {
      return getClass().hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class PatternPredicate extends hydra.ext.cypher.openCypher.Atom implements Serializable {
    public final hydra.ext.cypher.openCypher.PatternPredicate value;
    
    public PatternPredicate (hydra.ext.cypher.openCypher.PatternPredicate value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PatternPredicate)) {
        return false;
      }
      PatternPredicate o = (PatternPredicate) (other);
      return other instanceof Atom;
    }
    
    @Override
    public int hashCode() {
      return getClass().hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Parenthesized extends hydra.ext.cypher.openCypher.Atom implements Serializable {
    public final hydra.ext.cypher.openCypher.ParenthesizedExpression value;
    
    public Parenthesized (hydra.ext.cypher.openCypher.ParenthesizedExpression value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parenthesized)) {
        return false;
      }
      Parenthesized o = (Parenthesized) (other);
      return other instanceof Atom;
    }
    
    @Override
    public int hashCode() {
      return getClass().hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class FunctionInvocation extends hydra.ext.cypher.openCypher.Atom implements Serializable {
    public final hydra.ext.cypher.openCypher.FunctionInvocation value;
    
    public FunctionInvocation (hydra.ext.cypher.openCypher.FunctionInvocation value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FunctionInvocation)) {
        return false;
      }
      FunctionInvocation o = (FunctionInvocation) (other);
      return other instanceof Atom;
    }
    
    @Override
    public int hashCode() {
      return getClass().hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ExistentialSubquery extends hydra.ext.cypher.openCypher.Atom implements Serializable {
    public final hydra.ext.cypher.openCypher.ExistentialSubquery value;
    
    public ExistentialSubquery (hydra.ext.cypher.openCypher.ExistentialSubquery value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ExistentialSubquery)) {
        return false;
      }
      ExistentialSubquery o = (ExistentialSubquery) (other);
      return other instanceof Atom;
    }
    
    @Override
    public int hashCode() {
      return getClass().hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Variable extends hydra.ext.cypher.openCypher.Atom implements Serializable {
    public final hydra.ext.cypher.openCypher.Variable value;
    
    public Variable (hydra.ext.cypher.openCypher.Variable value) {
      java.util.Objects.requireNonNull((value));
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Variable)) {
        return false;
      }
      Variable o = (Variable) (other);
      return other instanceof Atom;
    }
    
    @Override
    public int hashCode() {
      return getClass().hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
