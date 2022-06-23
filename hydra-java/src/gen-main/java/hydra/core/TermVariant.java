package hydra.core;

/**
 * The identifier of a term expression constructor
 */
public abstract class TermVariant {
  private TermVariant () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Application instance) ;
    
    R visit(Element instance) ;
    
    R visit(Function instance) ;
    
    R visit(Let instance) ;
    
    R visit(List instance) ;
    
    R visit(Literal instance) ;
    
    R visit(Map instance) ;
    
    R visit(Nominal instance) ;
    
    R visit(Optional instance) ;
    
    R visit(Record instance) ;
    
    R visit(Set instance) ;
    
    R visit(Union instance) ;
    
    R visit(Universal instance) ;
    
    R visit(Variable instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TermVariant instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Application instance) {
      return otherwise((instance));
    }
    
    default R visit(Element instance) {
      return otherwise((instance));
    }
    
    default R visit(Function instance) {
      return otherwise((instance));
    }
    
    default R visit(Let instance) {
      return otherwise((instance));
    }
    
    default R visit(List instance) {
      return otherwise((instance));
    }
    
    default R visit(Literal instance) {
      return otherwise((instance));
    }
    
    default R visit(Map instance) {
      return otherwise((instance));
    }
    
    default R visit(Nominal instance) {
      return otherwise((instance));
    }
    
    default R visit(Optional instance) {
      return otherwise((instance));
    }
    
    default R visit(Record instance) {
      return otherwise((instance));
    }
    
    default R visit(Set instance) {
      return otherwise((instance));
    }
    
    default R visit(Union instance) {
      return otherwise((instance));
    }
    
    default R visit(Universal instance) {
      return otherwise((instance));
    }
    
    default R visit(Variable instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Application extends TermVariant {
    public Application () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Application)) {
        return false;
      }
      Application o = (Application) (other);
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
  
  public static final class Element extends TermVariant {
    public Element () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Element)) {
        return false;
      }
      Element o = (Element) (other);
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
  
  public static final class Function extends TermVariant {
    public Function () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Function)) {
        return false;
      }
      Function o = (Function) (other);
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
  
  public static final class Let extends TermVariant {
    public Let () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Let)) {
        return false;
      }
      Let o = (Let) (other);
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
  
  public static final class List extends TermVariant {
    public List () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof List)) {
        return false;
      }
      List o = (List) (other);
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
  
  public static final class Literal extends TermVariant {
    public Literal () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Literal)) {
        return false;
      }
      Literal o = (Literal) (other);
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
  
  public static final class Map extends TermVariant {
    public Map () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Map)) {
        return false;
      }
      Map o = (Map) (other);
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
  
  public static final class Nominal extends TermVariant {
    public Nominal () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Nominal)) {
        return false;
      }
      Nominal o = (Nominal) (other);
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
  
  public static final class Optional extends TermVariant {
    public Optional () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Optional)) {
        return false;
      }
      Optional o = (Optional) (other);
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
  
  public static final class Record extends TermVariant {
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
  
  public static final class Set extends TermVariant {
    public Set () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Set)) {
        return false;
      }
      Set o = (Set) (other);
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
  
  public static final class Union extends TermVariant {
    public Union () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Union)) {
        return false;
      }
      Union o = (Union) (other);
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
  
  public static final class Universal extends TermVariant {
    public Universal () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Universal)) {
        return false;
      }
      Universal o = (Universal) (other);
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
  
  public static final class Variable extends TermVariant {
    public Variable () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Variable)) {
        return false;
      }
      Variable o = (Variable) (other);
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