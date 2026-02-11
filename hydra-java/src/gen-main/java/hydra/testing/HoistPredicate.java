// Note: this is an automatically generated file. Do not edit.

package hydra.testing;

import java.io.Serializable;

/**
 * A predefined predicate for testing hoistSubterms. Each predicate determines which subterms should be hoisted into let bindings.
 */
public abstract class HoistPredicate implements Serializable, Comparable<HoistPredicate> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.testing.HoistPredicate");
  
  public static final hydra.core.Name FIELD_NAME_CASE_STATEMENTS = new hydra.core.Name("caseStatements");
  
  public static final hydra.core.Name FIELD_NAME_APPLICATIONS = new hydra.core.Name("applications");
  
  public static final hydra.core.Name FIELD_NAME_LISTS = new hydra.core.Name("lists");
  
  public static final hydra.core.Name FIELD_NAME_NOTHING = new hydra.core.Name("nothing");
  
  private HoistPredicate () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(CaseStatements instance) ;
    
    R visit(Applications instance) ;
    
    R visit(Lists instance) ;
    
    R visit(Nothing instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(HoistPredicate instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(CaseStatements instance) {
      return otherwise(instance);
    }
    
    default R visit(Applications instance) {
      return otherwise(instance);
    }
    
    default R visit(Lists instance) {
      return otherwise(instance);
    }
    
    default R visit(Nothing instance) {
      return otherwise(instance);
    }
  }
  
  /**
   * Hoist case statements (elimination unions) that appear in non-top-level positions
   */
  public static final class CaseStatements extends hydra.testing.HoistPredicate implements Serializable {
    public CaseStatements () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CaseStatements)) {
        return false;
      }
      CaseStatements o = (CaseStatements) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(HoistPredicate other) {
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
  
  /**
   * Hoist function applications that appear in non-top-level positions
   */
  public static final class Applications extends hydra.testing.HoistPredicate implements Serializable {
    public Applications () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Applications)) {
        return false;
      }
      Applications o = (Applications) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(HoistPredicate other) {
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
  
  /**
   * Hoist list terms that appear in non-top-level positions
   */
  public static final class Lists extends hydra.testing.HoistPredicate implements Serializable {
    public Lists () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Lists)) {
        return false;
      }
      Lists o = (Lists) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(HoistPredicate other) {
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
  
  /**
   * Never hoist anything (identity transformation for let terms)
   */
  public static final class Nothing extends hydra.testing.HoistPredicate implements Serializable {
    public Nothing () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Nothing)) {
        return false;
      }
      Nothing o = (Nothing) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(HoistPredicate other) {
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
}
