package hydra.grammar;

public abstract class Pattern {
  private Pattern () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Nil instance) ;
    
    R visit(Labeled instance) ;
    
    R visit(Constant instance) ;
    
    R visit(Regex instance) ;
    
    R visit(Nonterminal instance) ;
    
    R visit(Sequence instance) ;
    
    R visit(Alternatives instance) ;
    
    R visit(Option instance) ;
    
    R visit(Star instance) ;
    
    R visit(Plus instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Pattern instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Nil instance) {
      return otherwise((instance));
    }
    
    default R visit(Labeled instance) {
      return otherwise((instance));
    }
    
    default R visit(Constant instance) {
      return otherwise((instance));
    }
    
    default R visit(Regex instance) {
      return otherwise((instance));
    }
    
    default R visit(Nonterminal instance) {
      return otherwise((instance));
    }
    
    default R visit(Sequence instance) {
      return otherwise((instance));
    }
    
    default R visit(Alternatives instance) {
      return otherwise((instance));
    }
    
    default R visit(Option instance) {
      return otherwise((instance));
    }
    
    default R visit(Star instance) {
      return otherwise((instance));
    }
    
    default R visit(Plus instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Nil extends Pattern {
    public Nil () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Nil)) {
        return false;
      }
      Nil o = (Nil) (other);
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
  
  public static final class Labeled extends Pattern {
    public final LabeledPattern value;
    
    public Labeled (LabeledPattern value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Labeled)) {
        return false;
      }
      Labeled o = (Labeled) (other);
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
  
  public static final class Constant extends Pattern {
    public final Constant value;
    
    public Constant (Constant value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Constant)) {
        return false;
      }
      Constant o = (Constant) (other);
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
  
  public static final class Regex extends Pattern {
    public final Regex value;
    
    public Regex (Regex value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Regex)) {
        return false;
      }
      Regex o = (Regex) (other);
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
  
  public static final class Nonterminal extends Pattern {
    public final Symbol value;
    
    public Nonterminal (Symbol value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Nonterminal)) {
        return false;
      }
      Nonterminal o = (Nonterminal) (other);
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
  
  public static final class Sequence extends Pattern {
    public final java.util.List<Pattern> value;
    
    public Sequence (java.util.List<Pattern> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sequence)) {
        return false;
      }
      Sequence o = (Sequence) (other);
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
  
  public static final class Alternatives extends Pattern {
    public final java.util.List<Pattern> value;
    
    public Alternatives (java.util.List<Pattern> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Alternatives)) {
        return false;
      }
      Alternatives o = (Alternatives) (other);
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
  
  public static final class Option extends Pattern {
    public final Pattern value;
    
    public Option (Pattern value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Option)) {
        return false;
      }
      Option o = (Option) (other);
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
  
  public static final class Star extends Pattern {
    public final Pattern value;
    
    public Star (Pattern value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Star)) {
        return false;
      }
      Star o = (Star) (other);
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
  
  public static final class Plus extends Pattern {
    public final Pattern value;
    
    public Plus (Pattern value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Plus)) {
        return false;
      }
      Plus o = (Plus) (other);
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