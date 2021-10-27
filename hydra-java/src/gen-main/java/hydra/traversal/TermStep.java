package hydra.traversal;

import hydra.core.FieldName;

public abstract class TermStep {
  private TermStep() {}
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  /**
   * An interface for applying a function to a TermStep according to its variant (subclass)
   */
  public interface Visitor<R> {
    R visit(ApplicationFunction instance) ;
    
    R visit(ApplicationArgument instance) ;
    
    R visit(Case instance) ;
    
    R visit(CompareTo instance) ;
    
    R visit(LambdaBody instance) ;
    
    R visit(List instance) ;
    
    R visit(MapKey instance) ;
    
    R visit(MapValue instance) ;
    
    R visit(Record instance) ;
    
    R visit(Set instance) ;
    
    R visit(Union instance) ;
  }
  
  /**
   * An interface for applying a function to a TermStep according to its variant (subclass). If a visit() method for a
   * particular variant is not implemented, a default method is used instead.
   */
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TermStep instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    @Override
    default R visit(ApplicationFunction instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(ApplicationArgument instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(Case instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(CompareTo instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(LambdaBody instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(List instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(MapKey instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(MapValue instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(Record instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(Set instance) {
      return otherwise(instance);
    }
    
    @Override
    default R visit(Union instance) {
      return otherwise(instance);
    }
  }
  
  public static final class ApplicationFunction extends TermStep {
    /**
     * Constructs an immutable ApplicationFunction object
     */
    public ApplicationFunction() {}
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ApplicationFunction)) {
          return false;
      }
      ApplicationFunction o = (ApplicationFunction) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
  }
  
  public static final class ApplicationArgument extends TermStep {
    /**
     * Constructs an immutable ApplicationArgument object
     */
    public ApplicationArgument() {}
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ApplicationArgument)) {
          return false;
      }
      ApplicationArgument o = (ApplicationArgument) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
  }
  
  /**
   * @type hydra/core.FieldName
   */
  public static final class Case extends TermStep {
    public final FieldName caseEsc;
    
    /**
     * Constructs an immutable Case object
     */
    public Case(FieldName caseEsc) {
      this.caseEsc = caseEsc;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Case)) {
          return false;
      }
      Case o = (Case) other;
      return caseEsc.equals(o.caseEsc);
    }
    
    @Override
    public int hashCode() {
      return 2 * caseEsc.hashCode();
    }
  }
  
  public static final class CompareTo extends TermStep {
    /**
     * Constructs an immutable CompareTo object
     */
    public CompareTo() {}
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CompareTo)) {
          return false;
      }
      CompareTo o = (CompareTo) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
  }
  
  public static final class LambdaBody extends TermStep {
    /**
     * Constructs an immutable LambdaBody object
     */
    public LambdaBody() {}
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LambdaBody)) {
          return false;
      }
      LambdaBody o = (LambdaBody) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
  }
  
  /**
   * @type integer
   */
  public static final class List extends TermStep {
    public final Integer list;
    
    /**
     * Constructs an immutable List object
     */
    public List(Integer list) {
      this.list = list;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof List)) {
          return false;
      }
      List o = (List) other;
      return list.equals(o.list);
    }
    
    @Override
    public int hashCode() {
      return 2 * list.hashCode();
    }
  }
  
  /**
   * @type integer
   */
  public static final class MapKey extends TermStep {
    public final Integer mapKey;
    
    /**
     * Constructs an immutable MapKey object
     */
    public MapKey(Integer mapKey) {
      this.mapKey = mapKey;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MapKey)) {
          return false;
      }
      MapKey o = (MapKey) other;
      return mapKey.equals(o.mapKey);
    }
    
    @Override
    public int hashCode() {
      return 2 * mapKey.hashCode();
    }
  }
  
  /**
   * @type integer
   */
  public static final class MapValue extends TermStep {
    public final Integer mapValue;
    
    /**
     * Constructs an immutable MapValue object
     */
    public MapValue(Integer mapValue) {
      this.mapValue = mapValue;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MapValue)) {
          return false;
      }
      MapValue o = (MapValue) other;
      return mapValue.equals(o.mapValue);
    }
    
    @Override
    public int hashCode() {
      return 2 * mapValue.hashCode();
    }
  }
  
  /**
   * @type hydra/core.FieldName
   */
  public static final class Record extends TermStep {
    public final FieldName recordEsc;
    
    /**
     * Constructs an immutable Record object
     */
    public Record(FieldName recordEsc) {
      this.recordEsc = recordEsc;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Record)) {
          return false;
      }
      Record o = (Record) other;
      return recordEsc.equals(o.recordEsc);
    }
    
    @Override
    public int hashCode() {
      return 2 * recordEsc.hashCode();
    }
  }
  
  /**
   * @type integer
   */
  public static final class Set extends TermStep {
    public final Integer set;
    
    /**
     * Constructs an immutable Set object
     */
    public Set(Integer set) {
      this.set = set;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Set)) {
          return false;
      }
      Set o = (Set) other;
      return set.equals(o.set);
    }
    
    @Override
    public int hashCode() {
      return 2 * set.hashCode();
    }
  }
  
  public static final class Union extends TermStep {
    /**
     * Constructs an immutable Union object
     */
    public Union() {}
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Union)) {
          return false;
      }
      Union o = (Union) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
  }
}
