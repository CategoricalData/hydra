// Note: this is an automatically generated file. Do not edit.

package hydra.ext.cypher.openCypher;

import java.io.Serializable;

public abstract class Atom implements Serializable, Comparable<Atom> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.cypher.openCypher.Atom");
  
  public static final hydra.core.Name LITERAL = new hydra.core.Name("literal");
  
  public static final hydra.core.Name PARAMETER = new hydra.core.Name("parameter");
  
  public static final hydra.core.Name CASE = new hydra.core.Name("case");
  
  public static final hydra.core.Name COUNT_STAR = new hydra.core.Name("countStar");
  
  public static final hydra.core.Name LIST_COMPREHENSION = new hydra.core.Name("listComprehension");
  
  public static final hydra.core.Name PATTERN_COMPREHENSION = new hydra.core.Name("patternComprehension");
  
  public static final hydra.core.Name QUANTIFIER = new hydra.core.Name("quantifier");
  
  public static final hydra.core.Name PATTERN_PREDICATE = new hydra.core.Name("patternPredicate");
  
  public static final hydra.core.Name PARENTHESIZED = new hydra.core.Name("parenthesized");
  
  public static final hydra.core.Name FUNCTION_INVOCATION = new hydra.core.Name("functionInvocation");
  
  public static final hydra.core.Name EXISTENTIAL_SUBQUERY = new hydra.core.Name("existentialSubquery");
  
  public static final hydra.core.Name VARIABLE = new hydra.core.Name("variable");
  
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
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Literal instance) {
      return otherwise(instance);
    }
    
    default R visit(Parameter instance) {
      return otherwise(instance);
    }
    
    default R visit(Case instance) {
      return otherwise(instance);
    }
    
    default R visit(CountStar instance) {
      return otherwise(instance);
    }
    
    default R visit(ListComprehension instance) {
      return otherwise(instance);
    }
    
    default R visit(PatternComprehension instance) {
      return otherwise(instance);
    }
    
    default R visit(Quantifier instance) {
      return otherwise(instance);
    }
    
    default R visit(PatternPredicate instance) {
      return otherwise(instance);
    }
    
    default R visit(Parenthesized instance) {
      return otherwise(instance);
    }
    
    default R visit(FunctionInvocation instance) {
      return otherwise(instance);
    }
    
    default R visit(ExistentialSubquery instance) {
      return otherwise(instance);
    }
    
    default R visit(Variable instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Literal extends hydra.ext.cypher.openCypher.Atom implements Serializable {
    public final hydra.ext.cypher.openCypher.Literal value;
    
    public Literal (hydra.ext.cypher.openCypher.Literal value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Literal)) {
        return false;
      }
      Literal o = (Literal) other;
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
    public int compareTo(Atom other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Literal o = (Literal) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Parameter extends hydra.ext.cypher.openCypher.Atom implements Serializable {
    public final hydra.ext.cypher.openCypher.Parameter value;
    
    public Parameter (hydra.ext.cypher.openCypher.Parameter value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parameter)) {
        return false;
      }
      Parameter o = (Parameter) other;
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
    public int compareTo(Atom other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Parameter o = (Parameter) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Case extends hydra.ext.cypher.openCypher.Atom implements Serializable {
    public final hydra.ext.cypher.openCypher.CaseExpression value;
    
    public Case (hydra.ext.cypher.openCypher.CaseExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Case)) {
        return false;
      }
      Case o = (Case) other;
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
    public int compareTo(Atom other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Case o = (Case) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class CountStar extends hydra.ext.cypher.openCypher.Atom implements Serializable {
    public CountStar () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CountStar)) {
        return false;
      }
      CountStar o = (CountStar) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Atom other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
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
  
  public static final class ListComprehension extends hydra.ext.cypher.openCypher.Atom implements Serializable {
    public final hydra.ext.cypher.openCypher.ListComprehension value;
    
    public ListComprehension (hydra.ext.cypher.openCypher.ListComprehension value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ListComprehension)) {
        return false;
      }
      ListComprehension o = (ListComprehension) other;
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
    public int compareTo(Atom other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ListComprehension o = (ListComprehension) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class PatternComprehension extends hydra.ext.cypher.openCypher.Atom implements Serializable {
    public final hydra.ext.cypher.openCypher.PatternComprehension value;
    
    public PatternComprehension (hydra.ext.cypher.openCypher.PatternComprehension value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PatternComprehension)) {
        return false;
      }
      PatternComprehension o = (PatternComprehension) other;
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
    public int compareTo(Atom other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PatternComprehension o = (PatternComprehension) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Quantifier extends hydra.ext.cypher.openCypher.Atom implements Serializable {
    public final hydra.ext.cypher.openCypher.Quantifier value;
    
    public Quantifier (hydra.ext.cypher.openCypher.Quantifier value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Quantifier)) {
        return false;
      }
      Quantifier o = (Quantifier) other;
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
    public int compareTo(Atom other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Quantifier o = (Quantifier) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class PatternPredicate extends hydra.ext.cypher.openCypher.Atom implements Serializable {
    public final hydra.ext.cypher.openCypher.PatternPredicate value;
    
    public PatternPredicate (hydra.ext.cypher.openCypher.PatternPredicate value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PatternPredicate)) {
        return false;
      }
      PatternPredicate o = (PatternPredicate) other;
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
    public int compareTo(Atom other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PatternPredicate o = (PatternPredicate) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Parenthesized extends hydra.ext.cypher.openCypher.Atom implements Serializable {
    public final hydra.ext.cypher.openCypher.ParenthesizedExpression value;
    
    public Parenthesized (hydra.ext.cypher.openCypher.ParenthesizedExpression value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Parenthesized)) {
        return false;
      }
      Parenthesized o = (Parenthesized) other;
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
    public int compareTo(Atom other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Parenthesized o = (Parenthesized) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class FunctionInvocation extends hydra.ext.cypher.openCypher.Atom implements Serializable {
    public final hydra.ext.cypher.openCypher.FunctionInvocation value;
    
    public FunctionInvocation (hydra.ext.cypher.openCypher.FunctionInvocation value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FunctionInvocation)) {
        return false;
      }
      FunctionInvocation o = (FunctionInvocation) other;
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
    public int compareTo(Atom other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      FunctionInvocation o = (FunctionInvocation) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ExistentialSubquery extends hydra.ext.cypher.openCypher.Atom implements Serializable {
    public final hydra.ext.cypher.openCypher.ExistentialSubquery value;
    
    public ExistentialSubquery (hydra.ext.cypher.openCypher.ExistentialSubquery value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ExistentialSubquery)) {
        return false;
      }
      ExistentialSubquery o = (ExistentialSubquery) other;
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
    public int compareTo(Atom other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ExistentialSubquery o = (ExistentialSubquery) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Variable extends hydra.ext.cypher.openCypher.Atom implements Serializable {
    public final hydra.ext.cypher.openCypher.Variable value;
    
    public Variable (hydra.ext.cypher.openCypher.Variable value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Variable)) {
        return false;
      }
      Variable o = (Variable) other;
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
    public int compareTo(Atom other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Variable o = (Variable) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
