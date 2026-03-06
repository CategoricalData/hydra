// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class TraversalPredicate implements Serializable, Comparable<TraversalPredicate> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate");
  
  public static final hydra.core.Name EQ = new hydra.core.Name("eq");
  
  public static final hydra.core.Name NEQ = new hydra.core.Name("neq");
  
  public static final hydra.core.Name LT = new hydra.core.Name("lt");
  
  public static final hydra.core.Name LTE = new hydra.core.Name("lte");
  
  public static final hydra.core.Name GT = new hydra.core.Name("gt");
  
  public static final hydra.core.Name GTE = new hydra.core.Name("gte");
  
  public static final hydra.core.Name INSIDE = new hydra.core.Name("inside");
  
  public static final hydra.core.Name OUTSIDE = new hydra.core.Name("outside");
  
  public static final hydra.core.Name BETWEEN = new hydra.core.Name("between");
  
  public static final hydra.core.Name WITHIN = new hydra.core.Name("within");
  
  public static final hydra.core.Name WITHOUT = new hydra.core.Name("without");
  
  public static final hydra.core.Name NOT = new hydra.core.Name("not");
  
  public static final hydra.core.Name STARTING_WITH = new hydra.core.Name("startingWith");
  
  public static final hydra.core.Name NOT_STARTING_WITH = new hydra.core.Name("notStartingWith");
  
  public static final hydra.core.Name ENDING_WITH = new hydra.core.Name("endingWith");
  
  public static final hydra.core.Name NOT_ENDING_WITH = new hydra.core.Name("notEndingWith");
  
  public static final hydra.core.Name CONTAINING = new hydra.core.Name("containing");
  
  public static final hydra.core.Name NOT_CONTAINING = new hydra.core.Name("notContaining");
  
  public static final hydra.core.Name REGEX = new hydra.core.Name("regex");
  
  public static final hydra.core.Name NOT_REGEX = new hydra.core.Name("notRegex");
  
  public static final hydra.core.Name AND = new hydra.core.Name("and");
  
  public static final hydra.core.Name OR = new hydra.core.Name("or");
  
  public static final hydra.core.Name NEGATE = new hydra.core.Name("negate");
  
  private TraversalPredicate () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Eq instance) ;
    
    R visit(Neq instance) ;
    
    R visit(Lt instance) ;
    
    R visit(Lte instance) ;
    
    R visit(Gt instance) ;
    
    R visit(Gte instance) ;
    
    R visit(Inside instance) ;
    
    R visit(Outside instance) ;
    
    R visit(Between instance) ;
    
    R visit(Within instance) ;
    
    R visit(Without instance) ;
    
    R visit(Not instance) ;
    
    R visit(StartingWith instance) ;
    
    R visit(NotStartingWith instance) ;
    
    R visit(EndingWith instance) ;
    
    R visit(NotEndingWith instance) ;
    
    R visit(Containing instance) ;
    
    R visit(NotContaining instance) ;
    
    R visit(Regex instance) ;
    
    R visit(NotRegex instance) ;
    
    R visit(And instance) ;
    
    R visit(Or instance) ;
    
    R visit(Negate instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TraversalPredicate instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(Eq instance) {
      return otherwise(instance);
    }
    
    default R visit(Neq instance) {
      return otherwise(instance);
    }
    
    default R visit(Lt instance) {
      return otherwise(instance);
    }
    
    default R visit(Lte instance) {
      return otherwise(instance);
    }
    
    default R visit(Gt instance) {
      return otherwise(instance);
    }
    
    default R visit(Gte instance) {
      return otherwise(instance);
    }
    
    default R visit(Inside instance) {
      return otherwise(instance);
    }
    
    default R visit(Outside instance) {
      return otherwise(instance);
    }
    
    default R visit(Between instance) {
      return otherwise(instance);
    }
    
    default R visit(Within instance) {
      return otherwise(instance);
    }
    
    default R visit(Without instance) {
      return otherwise(instance);
    }
    
    default R visit(Not instance) {
      return otherwise(instance);
    }
    
    default R visit(StartingWith instance) {
      return otherwise(instance);
    }
    
    default R visit(NotStartingWith instance) {
      return otherwise(instance);
    }
    
    default R visit(EndingWith instance) {
      return otherwise(instance);
    }
    
    default R visit(NotEndingWith instance) {
      return otherwise(instance);
    }
    
    default R visit(Containing instance) {
      return otherwise(instance);
    }
    
    default R visit(NotContaining instance) {
      return otherwise(instance);
    }
    
    default R visit(Regex instance) {
      return otherwise(instance);
    }
    
    default R visit(NotRegex instance) {
      return otherwise(instance);
    }
    
    default R visit(And instance) {
      return otherwise(instance);
    }
    
    default R visit(Or instance) {
      return otherwise(instance);
    }
    
    default R visit(Negate instance) {
      return otherwise(instance);
    }
  }
  
  public static final class Eq extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument value;
    
    public Eq (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Eq)) {
        return false;
      }
      Eq o = (Eq) other;
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
    public int compareTo(TraversalPredicate other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Eq o = (Eq) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Neq extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument value;
    
    public Neq (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Neq)) {
        return false;
      }
      Neq o = (Neq) other;
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
    public int compareTo(TraversalPredicate other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Neq o = (Neq) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Lt extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument value;
    
    public Lt (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Lt)) {
        return false;
      }
      Lt o = (Lt) other;
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
    public int compareTo(TraversalPredicate other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Lt o = (Lt) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Lte extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument value;
    
    public Lte (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Lte)) {
        return false;
      }
      Lte o = (Lte) other;
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
    public int compareTo(TraversalPredicate other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Lte o = (Lte) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Gt extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument value;
    
    public Gt (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Gt)) {
        return false;
      }
      Gt o = (Gt) other;
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
    public int compareTo(TraversalPredicate other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Gt o = (Gt) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Gte extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument value;
    
    public Gte (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Gte)) {
        return false;
      }
      Gte o = (Gte) other;
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
    public int compareTo(TraversalPredicate other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Gte o = (Gte) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Inside extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.RangeArgument value;
    
    public Inside (hydra.ext.org.apache.tinkerpop.gremlin.RangeArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Inside)) {
        return false;
      }
      Inside o = (Inside) other;
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
    public int compareTo(TraversalPredicate other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Inside o = (Inside) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Outside extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.RangeArgument value;
    
    public Outside (hydra.ext.org.apache.tinkerpop.gremlin.RangeArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Outside)) {
        return false;
      }
      Outside o = (Outside) other;
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
    public int compareTo(TraversalPredicate other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Outside o = (Outside) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Between extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.RangeArgument value;
    
    public Between (hydra.ext.org.apache.tinkerpop.gremlin.RangeArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Between)) {
        return false;
      }
      Between o = (Between) other;
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
    public int compareTo(TraversalPredicate other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Between o = (Between) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Within extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument> value;
    
    public Within (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Within)) {
        return false;
      }
      Within o = (Within) other;
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
    public int compareTo(TraversalPredicate other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Within o = (Within) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Without extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument> value;
    
    public Without (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Without)) {
        return false;
      }
      Without o = (Without) other;
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
    public int compareTo(TraversalPredicate other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Without o = (Without) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Not extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate value;
    
    public Not (hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Not)) {
        return false;
      }
      Not o = (Not) other;
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
    public int compareTo(TraversalPredicate other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Not o = (Not) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class StartingWith extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value;
    
    public StartingWith (hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof StartingWith)) {
        return false;
      }
      StartingWith o = (StartingWith) other;
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
    public int compareTo(TraversalPredicate other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      StartingWith o = (StartingWith) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class NotStartingWith extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value;
    
    public NotStartingWith (hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NotStartingWith)) {
        return false;
      }
      NotStartingWith o = (NotStartingWith) other;
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
    public int compareTo(TraversalPredicate other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NotStartingWith o = (NotStartingWith) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class EndingWith extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value;
    
    public EndingWith (hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EndingWith)) {
        return false;
      }
      EndingWith o = (EndingWith) other;
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
    public int compareTo(TraversalPredicate other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      EndingWith o = (EndingWith) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class NotEndingWith extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value;
    
    public NotEndingWith (hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NotEndingWith)) {
        return false;
      }
      NotEndingWith o = (NotEndingWith) other;
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
    public int compareTo(TraversalPredicate other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NotEndingWith o = (NotEndingWith) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Containing extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value;
    
    public Containing (hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Containing)) {
        return false;
      }
      Containing o = (Containing) other;
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
    public int compareTo(TraversalPredicate other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Containing o = (Containing) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class NotContaining extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value;
    
    public NotContaining (hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NotContaining)) {
        return false;
      }
      NotContaining o = (NotContaining) other;
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
    public int compareTo(TraversalPredicate other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NotContaining o = (NotContaining) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Regex extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value;
    
    public Regex (hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Regex)) {
        return false;
      }
      Regex o = (Regex) other;
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
    public int compareTo(TraversalPredicate other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Regex o = (Regex) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class NotRegex extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value;
    
    public NotRegex (hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NotRegex)) {
        return false;
      }
      NotRegex o = (NotRegex) other;
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
    public int compareTo(TraversalPredicate other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NotRegex o = (NotRegex) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class And extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TwoTraversalPredicates value;
    
    public And (hydra.ext.org.apache.tinkerpop.gremlin.TwoTraversalPredicates value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof And)) {
        return false;
      }
      And o = (And) other;
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
    public int compareTo(TraversalPredicate other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      And o = (And) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Or extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TwoTraversalPredicates value;
    
    public Or (hydra.ext.org.apache.tinkerpop.gremlin.TwoTraversalPredicates value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Or)) {
        return false;
      }
      Or o = (Or) other;
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
    public int compareTo(TraversalPredicate other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Or o = (Or) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Negate extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate value;
    
    public Negate (hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Negate)) {
        return false;
      }
      Negate o = (Negate) other;
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
    public int compareTo(TraversalPredicate other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Negate o = (Negate) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
