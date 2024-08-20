// Note: this is an automatically generated file. Do not edit.

package hydra.ext.com.microsoft.kusto.kql;

import java.io.Serializable;

public abstract class BinaryOperator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/com/microsoft/kusto/kql.BinaryOperator");
  
  public static final hydra.core.Name FIELD_NAME_CASE_INSENSITIVE_EQUAL = new hydra.core.Name("caseInsensitiveEqual");
  
  public static final hydra.core.Name FIELD_NAME_CONTAINS = new hydra.core.Name("contains");
  
  public static final hydra.core.Name FIELD_NAME_DIVIDE = new hydra.core.Name("divide");
  
  public static final hydra.core.Name FIELD_NAME_ENDS_WITH = new hydra.core.Name("endsWith");
  
  public static final hydra.core.Name FIELD_NAME_EQUAL = new hydra.core.Name("equal");
  
  public static final hydra.core.Name FIELD_NAME_GREATER = new hydra.core.Name("greater");
  
  public static final hydra.core.Name FIELD_NAME_GREATER_OR_EQUAL = new hydra.core.Name("greaterOrEqual");
  
  public static final hydra.core.Name FIELD_NAME_HAS = new hydra.core.Name("has");
  
  public static final hydra.core.Name FIELD_NAME_HAS_PREFIX = new hydra.core.Name("hasPrefix");
  
  public static final hydra.core.Name FIELD_NAME_HAS_SUFFIX = new hydra.core.Name("hasSuffix");
  
  public static final hydra.core.Name FIELD_NAME_LESS = new hydra.core.Name("less");
  
  public static final hydra.core.Name FIELD_NAME_LESS_OR_EQUAL = new hydra.core.Name("lessOrEqual");
  
  public static final hydra.core.Name FIELD_NAME_MATCHES_REGEX = new hydra.core.Name("matchesRegex");
  
  public static final hydra.core.Name FIELD_NAME_MINUS = new hydra.core.Name("minus");
  
  public static final hydra.core.Name FIELD_NAME_NOT_EQUAL = new hydra.core.Name("notEqual");
  
  public static final hydra.core.Name FIELD_NAME_PLUS = new hydra.core.Name("plus");
  
  public static final hydra.core.Name FIELD_NAME_STARTS_WITH = new hydra.core.Name("startsWith");
  
  public static final hydra.core.Name FIELD_NAME_TIMES = new hydra.core.Name("times");
  
  private BinaryOperator () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(CaseInsensitiveEqual instance) ;
    
    R visit(Contains instance) ;
    
    R visit(Divide instance) ;
    
    R visit(EndsWith instance) ;
    
    R visit(Equal instance) ;
    
    R visit(Greater instance) ;
    
    R visit(GreaterOrEqual instance) ;
    
    R visit(Has instance) ;
    
    R visit(HasPrefix instance) ;
    
    R visit(HasSuffix instance) ;
    
    R visit(Less instance) ;
    
    R visit(LessOrEqual instance) ;
    
    R visit(MatchesRegex instance) ;
    
    R visit(Minus instance) ;
    
    R visit(NotEqual instance) ;
    
    R visit(Plus instance) ;
    
    R visit(StartsWith instance) ;
    
    R visit(Times instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(BinaryOperator instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(CaseInsensitiveEqual instance) {
      return otherwise((instance));
    }
    
    default R visit(Contains instance) {
      return otherwise((instance));
    }
    
    default R visit(Divide instance) {
      return otherwise((instance));
    }
    
    default R visit(EndsWith instance) {
      return otherwise((instance));
    }
    
    default R visit(Equal instance) {
      return otherwise((instance));
    }
    
    default R visit(Greater instance) {
      return otherwise((instance));
    }
    
    default R visit(GreaterOrEqual instance) {
      return otherwise((instance));
    }
    
    default R visit(Has instance) {
      return otherwise((instance));
    }
    
    default R visit(HasPrefix instance) {
      return otherwise((instance));
    }
    
    default R visit(HasSuffix instance) {
      return otherwise((instance));
    }
    
    default R visit(Less instance) {
      return otherwise((instance));
    }
    
    default R visit(LessOrEqual instance) {
      return otherwise((instance));
    }
    
    default R visit(MatchesRegex instance) {
      return otherwise((instance));
    }
    
    default R visit(Minus instance) {
      return otherwise((instance));
    }
    
    default R visit(NotEqual instance) {
      return otherwise((instance));
    }
    
    default R visit(Plus instance) {
      return otherwise((instance));
    }
    
    default R visit(StartsWith instance) {
      return otherwise((instance));
    }
    
    default R visit(Times instance) {
      return otherwise((instance));
    }
  }
  
  public static final class CaseInsensitiveEqual extends hydra.ext.com.microsoft.kusto.kql.BinaryOperator implements Serializable {
    public CaseInsensitiveEqual () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CaseInsensitiveEqual)) {
        return false;
      }
      CaseInsensitiveEqual o = (CaseInsensitiveEqual) (other);
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
  
  public static final class Contains extends hydra.ext.com.microsoft.kusto.kql.BinaryOperator implements Serializable {
    public Contains () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Contains)) {
        return false;
      }
      Contains o = (Contains) (other);
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
  
  public static final class Divide extends hydra.ext.com.microsoft.kusto.kql.BinaryOperator implements Serializable {
    public Divide () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Divide)) {
        return false;
      }
      Divide o = (Divide) (other);
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
  
  public static final class EndsWith extends hydra.ext.com.microsoft.kusto.kql.BinaryOperator implements Serializable {
    public EndsWith () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EndsWith)) {
        return false;
      }
      EndsWith o = (EndsWith) (other);
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
  
  public static final class Equal extends hydra.ext.com.microsoft.kusto.kql.BinaryOperator implements Serializable {
    public Equal () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Equal)) {
        return false;
      }
      Equal o = (Equal) (other);
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
  
  public static final class Greater extends hydra.ext.com.microsoft.kusto.kql.BinaryOperator implements Serializable {
    public Greater () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Greater)) {
        return false;
      }
      Greater o = (Greater) (other);
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
  
  public static final class GreaterOrEqual extends hydra.ext.com.microsoft.kusto.kql.BinaryOperator implements Serializable {
    public GreaterOrEqual () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GreaterOrEqual)) {
        return false;
      }
      GreaterOrEqual o = (GreaterOrEqual) (other);
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
  
  public static final class Has extends hydra.ext.com.microsoft.kusto.kql.BinaryOperator implements Serializable {
    public Has () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Has)) {
        return false;
      }
      Has o = (Has) (other);
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
  
  public static final class HasPrefix extends hydra.ext.com.microsoft.kusto.kql.BinaryOperator implements Serializable {
    public HasPrefix () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof HasPrefix)) {
        return false;
      }
      HasPrefix o = (HasPrefix) (other);
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
  
  public static final class HasSuffix extends hydra.ext.com.microsoft.kusto.kql.BinaryOperator implements Serializable {
    public HasSuffix () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof HasSuffix)) {
        return false;
      }
      HasSuffix o = (HasSuffix) (other);
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
  
  public static final class Less extends hydra.ext.com.microsoft.kusto.kql.BinaryOperator implements Serializable {
    public Less () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Less)) {
        return false;
      }
      Less o = (Less) (other);
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
  
  public static final class LessOrEqual extends hydra.ext.com.microsoft.kusto.kql.BinaryOperator implements Serializable {
    public LessOrEqual () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LessOrEqual)) {
        return false;
      }
      LessOrEqual o = (LessOrEqual) (other);
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
  
  public static final class MatchesRegex extends hydra.ext.com.microsoft.kusto.kql.BinaryOperator implements Serializable {
    public MatchesRegex () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MatchesRegex)) {
        return false;
      }
      MatchesRegex o = (MatchesRegex) (other);
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
  
  public static final class Minus extends hydra.ext.com.microsoft.kusto.kql.BinaryOperator implements Serializable {
    public Minus () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Minus)) {
        return false;
      }
      Minus o = (Minus) (other);
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
  
  public static final class NotEqual extends hydra.ext.com.microsoft.kusto.kql.BinaryOperator implements Serializable {
    public NotEqual () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NotEqual)) {
        return false;
      }
      NotEqual o = (NotEqual) (other);
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
  
  public static final class Plus extends hydra.ext.com.microsoft.kusto.kql.BinaryOperator implements Serializable {
    public Plus () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Plus)) {
        return false;
      }
      Plus o = (Plus) (other);
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
  
  public static final class StartsWith extends hydra.ext.com.microsoft.kusto.kql.BinaryOperator implements Serializable {
    public StartsWith () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StartsWith)) {
        return false;
      }
      StartsWith o = (StartsWith) (other);
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
  
  public static final class Times extends hydra.ext.com.microsoft.kusto.kql.BinaryOperator implements Serializable {
    public Times () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Times)) {
        return false;
      }
      Times o = (Times) (other);
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
}