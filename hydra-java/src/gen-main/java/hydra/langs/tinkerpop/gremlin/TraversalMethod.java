// Note: this is an automatically generated file. Do not edit.

package hydra.langs.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class TraversalMethod implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/tinkerpop/gremlin.TraversalMethod");
  
  private TraversalMethod () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(V instance) ;
    
    R visit(E instance) ;
    
    R visit(AddE instance) ;
    
    R visit(AddV instance) ;
    
    R visit(MergeE instance) ;
    
    R visit(MergeV instance) ;
    
    R visit(Aggregate instance) ;
    
    R visit(All instance) ;
    
    R visit(And instance) ;
    
    R visit(Any instance) ;
    
    R visit(As instance) ;
    
    R visit(Barrier instance) ;
    
    R visit(Both instance) ;
    
    R visit(BothE instance) ;
    
    R visit(BothV instance) ;
    
    R visit(Branch instance) ;
    
    R visit(By instance) ;
    
    R visit(Cap instance) ;
    
    R visit(Choose instance) ;
    
    R visit(Coalesce instance) ;
    
    R visit(Coin instance) ;
    
    R visit(Conjoin instance) ;
    
    R visit(ConnectedComponent instance) ;
    
    R visit(Constant instance) ;
    
    R visit(Count instance) ;
    
    R visit(CyclicPath instance) ;
    
    R visit(Dedup instance) ;
    
    R visit(Difference instance) ;
    
    R visit(Disjunct instance) ;
    
    R visit(Drop instance) ;
    
    R visit(ElementMap instance) ;
    
    R visit(Emit instance) ;
    
    R visit(Filter instance) ;
    
    R visit(FlatMap instance) ;
    
    R visit(Fold instance) ;
    
    R visit(From instance) ;
    
    R visit(Group instance) ;
    
    R visit(GroupCount instance) ;
    
    R visit(Has instance) ;
    
    R visit(HasId instance) ;
    
    R visit(HasKey instance) ;
    
    R visit(HasLabel instance) ;
    
    R visit(HasNot instance) ;
    
    R visit(HasValue instance) ;
    
    R visit(Id instance) ;
    
    R visit(Identity instance) ;
    
    R visit(In instance) ;
    
    R visit(InE instance) ;
    
    R visit(Intersect instance) ;
    
    R visit(InV instance) ;
    
    R visit(Index instance) ;
    
    R visit(Inject instance) ;
    
    R visit(Is instance) ;
    
    R visit(Key instance) ;
    
    R visit(Label instance) ;
    
    R visit(Limit instance) ;
    
    R visit(Local instance) ;
    
    R visit(Loops instance) ;
    
    R visit(Map instance) ;
    
    R visit(Match instance) ;
    
    R visit(Math_ instance) ;
    
    R visit(Max instance) ;
    
    R visit(Mean instance) ;
    
    R visit(Min instance) ;
    
    R visit(None instance) ;
    
    R visit(Not instance) ;
    
    R visit(Option instance) ;
    
    R visit(Optional instance) ;
    
    R visit(Or instance) ;
    
    R visit(Order instance) ;
    
    R visit(OtherV instance) ;
    
    R visit(Out instance) ;
    
    R visit(OutE instance) ;
    
    R visit(OutV instance) ;
    
    R visit(PageRank instance) ;
    
    R visit(Path instance) ;
    
    R visit(PeerPressure instance) ;
    
    R visit(Profile instance) ;
    
    R visit(Project instance) ;
    
    R visit(Properties instance) ;
    
    R visit(Property instance) ;
    
    R visit(PropertyMap instance) ;
    
    R visit(Range instance) ;
    
    R visit(Read instance) ;
    
    R visit(Repeat instance) ;
    
    R visit(Sack instance) ;
    
    R visit(Sample instance) ;
    
    R visit(Select instance) ;
    
    R visit(Combine instance) ;
    
    R visit(Product instance) ;
    
    R visit(Merge instance) ;
    
    R visit(ShortestPath instance) ;
    
    R visit(SideEffect instance) ;
    
    R visit(SimplePath instance) ;
    
    R visit(Skip instance) ;
    
    R visit(Store instance) ;
    
    R visit(Subgraph instance) ;
    
    R visit(Sum instance) ;
    
    R visit(Tail instance) ;
    
    R visit(Fail instance) ;
    
    R visit(Times instance) ;
    
    R visit(To instance) ;
    
    R visit(ToE instance) ;
    
    R visit(ToV instance) ;
    
    R visit(Tree instance) ;
    
    R visit(Unfold instance) ;
    
    R visit(Union instance) ;
    
    R visit(Until instance) ;
    
    R visit(Value instance) ;
    
    R visit(ValueMap instance) ;
    
    R visit(Values instance) ;
    
    R visit(Where instance) ;
    
    R visit(With instance) ;
    
    R visit(Write instance) ;
    
    R visit(Element instance) ;
    
    R visit(Call instance) ;
    
    R visit(Concat instance) ;
    
    R visit(AsString instance) ;
    
    R visit(Format instance) ;
    
    R visit(ToUpper instance) ;
    
    R visit(ToLower instance) ;
    
    R visit(Length instance) ;
    
    R visit(Trim instance) ;
    
    R visit(LTrim instance) ;
    
    R visit(RTrim instance) ;
    
    R visit(Reverse instance) ;
    
    R visit(Replace instance) ;
    
    R visit(Split instance) ;
    
    R visit(Substring instance) ;
    
    R visit(AsDate instance) ;
    
    R visit(DateAdd instance) ;
    
    R visit(DateDiff instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TraversalMethod instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(V instance) {
      return otherwise((instance));
    }
    
    default R visit(E instance) {
      return otherwise((instance));
    }
    
    default R visit(AddE instance) {
      return otherwise((instance));
    }
    
    default R visit(AddV instance) {
      return otherwise((instance));
    }
    
    default R visit(MergeE instance) {
      return otherwise((instance));
    }
    
    default R visit(MergeV instance) {
      return otherwise((instance));
    }
    
    default R visit(Aggregate instance) {
      return otherwise((instance));
    }
    
    default R visit(All instance) {
      return otherwise((instance));
    }
    
    default R visit(And instance) {
      return otherwise((instance));
    }
    
    default R visit(Any instance) {
      return otherwise((instance));
    }
    
    default R visit(As instance) {
      return otherwise((instance));
    }
    
    default R visit(Barrier instance) {
      return otherwise((instance));
    }
    
    default R visit(Both instance) {
      return otherwise((instance));
    }
    
    default R visit(BothE instance) {
      return otherwise((instance));
    }
    
    default R visit(BothV instance) {
      return otherwise((instance));
    }
    
    default R visit(Branch instance) {
      return otherwise((instance));
    }
    
    default R visit(By instance) {
      return otherwise((instance));
    }
    
    default R visit(Cap instance) {
      return otherwise((instance));
    }
    
    default R visit(Choose instance) {
      return otherwise((instance));
    }
    
    default R visit(Coalesce instance) {
      return otherwise((instance));
    }
    
    default R visit(Coin instance) {
      return otherwise((instance));
    }
    
    default R visit(Conjoin instance) {
      return otherwise((instance));
    }
    
    default R visit(ConnectedComponent instance) {
      return otherwise((instance));
    }
    
    default R visit(Constant instance) {
      return otherwise((instance));
    }
    
    default R visit(Count instance) {
      return otherwise((instance));
    }
    
    default R visit(CyclicPath instance) {
      return otherwise((instance));
    }
    
    default R visit(Dedup instance) {
      return otherwise((instance));
    }
    
    default R visit(Difference instance) {
      return otherwise((instance));
    }
    
    default R visit(Disjunct instance) {
      return otherwise((instance));
    }
    
    default R visit(Drop instance) {
      return otherwise((instance));
    }
    
    default R visit(ElementMap instance) {
      return otherwise((instance));
    }
    
    default R visit(Emit instance) {
      return otherwise((instance));
    }
    
    default R visit(Filter instance) {
      return otherwise((instance));
    }
    
    default R visit(FlatMap instance) {
      return otherwise((instance));
    }
    
    default R visit(Fold instance) {
      return otherwise((instance));
    }
    
    default R visit(From instance) {
      return otherwise((instance));
    }
    
    default R visit(Group instance) {
      return otherwise((instance));
    }
    
    default R visit(GroupCount instance) {
      return otherwise((instance));
    }
    
    default R visit(Has instance) {
      return otherwise((instance));
    }
    
    default R visit(HasId instance) {
      return otherwise((instance));
    }
    
    default R visit(HasKey instance) {
      return otherwise((instance));
    }
    
    default R visit(HasLabel instance) {
      return otherwise((instance));
    }
    
    default R visit(HasNot instance) {
      return otherwise((instance));
    }
    
    default R visit(HasValue instance) {
      return otherwise((instance));
    }
    
    default R visit(Id instance) {
      return otherwise((instance));
    }
    
    default R visit(Identity instance) {
      return otherwise((instance));
    }
    
    default R visit(In instance) {
      return otherwise((instance));
    }
    
    default R visit(InE instance) {
      return otherwise((instance));
    }
    
    default R visit(Intersect instance) {
      return otherwise((instance));
    }
    
    default R visit(InV instance) {
      return otherwise((instance));
    }
    
    default R visit(Index instance) {
      return otherwise((instance));
    }
    
    default R visit(Inject instance) {
      return otherwise((instance));
    }
    
    default R visit(Is instance) {
      return otherwise((instance));
    }
    
    default R visit(Key instance) {
      return otherwise((instance));
    }
    
    default R visit(Label instance) {
      return otherwise((instance));
    }
    
    default R visit(Limit instance) {
      return otherwise((instance));
    }
    
    default R visit(Local instance) {
      return otherwise((instance));
    }
    
    default R visit(Loops instance) {
      return otherwise((instance));
    }
    
    default R visit(Map instance) {
      return otherwise((instance));
    }
    
    default R visit(Match instance) {
      return otherwise((instance));
    }
    
    default R visit(Math_ instance) {
      return otherwise((instance));
    }
    
    default R visit(Max instance) {
      return otherwise((instance));
    }
    
    default R visit(Mean instance) {
      return otherwise((instance));
    }
    
    default R visit(Min instance) {
      return otherwise((instance));
    }
    
    default R visit(None instance) {
      return otherwise((instance));
    }
    
    default R visit(Not instance) {
      return otherwise((instance));
    }
    
    default R visit(Option instance) {
      return otherwise((instance));
    }
    
    default R visit(Optional instance) {
      return otherwise((instance));
    }
    
    default R visit(Or instance) {
      return otherwise((instance));
    }
    
    default R visit(Order instance) {
      return otherwise((instance));
    }
    
    default R visit(OtherV instance) {
      return otherwise((instance));
    }
    
    default R visit(Out instance) {
      return otherwise((instance));
    }
    
    default R visit(OutE instance) {
      return otherwise((instance));
    }
    
    default R visit(OutV instance) {
      return otherwise((instance));
    }
    
    default R visit(PageRank instance) {
      return otherwise((instance));
    }
    
    default R visit(Path instance) {
      return otherwise((instance));
    }
    
    default R visit(PeerPressure instance) {
      return otherwise((instance));
    }
    
    default R visit(Profile instance) {
      return otherwise((instance));
    }
    
    default R visit(Project instance) {
      return otherwise((instance));
    }
    
    default R visit(Properties instance) {
      return otherwise((instance));
    }
    
    default R visit(Property instance) {
      return otherwise((instance));
    }
    
    default R visit(PropertyMap instance) {
      return otherwise((instance));
    }
    
    default R visit(Range instance) {
      return otherwise((instance));
    }
    
    default R visit(Read instance) {
      return otherwise((instance));
    }
    
    default R visit(Repeat instance) {
      return otherwise((instance));
    }
    
    default R visit(Sack instance) {
      return otherwise((instance));
    }
    
    default R visit(Sample instance) {
      return otherwise((instance));
    }
    
    default R visit(Select instance) {
      return otherwise((instance));
    }
    
    default R visit(Combine instance) {
      return otherwise((instance));
    }
    
    default R visit(Product instance) {
      return otherwise((instance));
    }
    
    default R visit(Merge instance) {
      return otherwise((instance));
    }
    
    default R visit(ShortestPath instance) {
      return otherwise((instance));
    }
    
    default R visit(SideEffect instance) {
      return otherwise((instance));
    }
    
    default R visit(SimplePath instance) {
      return otherwise((instance));
    }
    
    default R visit(Skip instance) {
      return otherwise((instance));
    }
    
    default R visit(Store instance) {
      return otherwise((instance));
    }
    
    default R visit(Subgraph instance) {
      return otherwise((instance));
    }
    
    default R visit(Sum instance) {
      return otherwise((instance));
    }
    
    default R visit(Tail instance) {
      return otherwise((instance));
    }
    
    default R visit(Fail instance) {
      return otherwise((instance));
    }
    
    default R visit(Times instance) {
      return otherwise((instance));
    }
    
    default R visit(To instance) {
      return otherwise((instance));
    }
    
    default R visit(ToE instance) {
      return otherwise((instance));
    }
    
    default R visit(ToV instance) {
      return otherwise((instance));
    }
    
    default R visit(Tree instance) {
      return otherwise((instance));
    }
    
    default R visit(Unfold instance) {
      return otherwise((instance));
    }
    
    default R visit(Union instance) {
      return otherwise((instance));
    }
    
    default R visit(Until instance) {
      return otherwise((instance));
    }
    
    default R visit(Value instance) {
      return otherwise((instance));
    }
    
    default R visit(ValueMap instance) {
      return otherwise((instance));
    }
    
    default R visit(Values instance) {
      return otherwise((instance));
    }
    
    default R visit(Where instance) {
      return otherwise((instance));
    }
    
    default R visit(With instance) {
      return otherwise((instance));
    }
    
    default R visit(Write instance) {
      return otherwise((instance));
    }
    
    default R visit(Element instance) {
      return otherwise((instance));
    }
    
    default R visit(Call instance) {
      return otherwise((instance));
    }
    
    default R visit(Concat instance) {
      return otherwise((instance));
    }
    
    default R visit(AsString instance) {
      return otherwise((instance));
    }
    
    default R visit(Format instance) {
      return otherwise((instance));
    }
    
    default R visit(ToUpper instance) {
      return otherwise((instance));
    }
    
    default R visit(ToLower instance) {
      return otherwise((instance));
    }
    
    default R visit(Length instance) {
      return otherwise((instance));
    }
    
    default R visit(Trim instance) {
      return otherwise((instance));
    }
    
    default R visit(LTrim instance) {
      return otherwise((instance));
    }
    
    default R visit(RTrim instance) {
      return otherwise((instance));
    }
    
    default R visit(Reverse instance) {
      return otherwise((instance));
    }
    
    default R visit(Replace instance) {
      return otherwise((instance));
    }
    
    default R visit(Split instance) {
      return otherwise((instance));
    }
    
    default R visit(Substring instance) {
      return otherwise((instance));
    }
    
    default R visit(AsDate instance) {
      return otherwise((instance));
    }
    
    default R visit(DateAdd instance) {
      return otherwise((instance));
    }
    
    default R visit(DateDiff instance) {
      return otherwise((instance));
    }
  }
  
  public static final class V extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.langs.tinkerpop.gremlin.GenericLiteralArgument> value;
    
    public V (java.util.List<hydra.langs.tinkerpop.gremlin.GenericLiteralArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof V)) {
        return false;
      }
      V o = (V) (other);
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
  
  public static final class E extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.langs.tinkerpop.gremlin.GenericLiteralArgument> value;
    
    public E (java.util.List<hydra.langs.tinkerpop.gremlin.GenericLiteralArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof E)) {
        return false;
      }
      E o = (E) (other);
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
  
  public static final class AddE extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.StringArgumentOrNestedTraversal value;
    
    public AddE (hydra.langs.tinkerpop.gremlin.StringArgumentOrNestedTraversal value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AddE)) {
        return false;
      }
      AddE o = (AddE) (other);
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
  
  public static final class AddV extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.StringArgumentOrNestedTraversal> value;
    
    public AddV (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.StringArgumentOrNestedTraversal> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AddV)) {
        return false;
      }
      AddV o = (AddV) (other);
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
  
  public static final class MergeE extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal> value;
    
    public MergeE (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MergeE)) {
        return false;
      }
      MergeE o = (MergeE) (other);
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
  
  public static final class MergeV extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal> value;
    
    public MergeV (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MergeV)) {
        return false;
      }
      MergeV o = (MergeV) (other);
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
  
  public static final class Aggregate extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndStringArgument value;
    
    public Aggregate (hydra.langs.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndStringArgument value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Aggregate)) {
        return false;
      }
      Aggregate o = (Aggregate) (other);
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
  
  public static final class All extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.TraversalPredicate value;
    
    public All (hydra.langs.tinkerpop.gremlin.TraversalPredicate value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof All)) {
        return false;
      }
      All o = (All) (other);
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
  
  public static final class And extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.langs.tinkerpop.gremlin.NestedTraversal> value;
    
    public And (java.util.List<hydra.langs.tinkerpop.gremlin.NestedTraversal> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof And)) {
        return false;
      }
      And o = (And) (other);
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
  
  public static final class Any extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.TraversalPredicate value;
    
    public Any (hydra.langs.tinkerpop.gremlin.TraversalPredicate value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Any)) {
        return false;
      }
      Any o = (Any) (other);
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
  
  public static final class As extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.StringArgumentAndOptionalStringLiteralVarargs value;
    
    public As (hydra.langs.tinkerpop.gremlin.StringArgumentAndOptionalStringLiteralVarargs value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof As)) {
        return false;
      }
      As o = (As) (other);
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
  
  public static final class Barrier extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalSackMethodArgumentOrIntegerArgument> value;
    
    public Barrier (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalSackMethodArgumentOrIntegerArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Barrier)) {
        return false;
      }
      Barrier o = (Barrier) (other);
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
  
  public static final class Both extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.langs.tinkerpop.gremlin.StringNullableArgument> value;
    
    public Both (java.util.List<hydra.langs.tinkerpop.gremlin.StringNullableArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Both)) {
        return false;
      }
      Both o = (Both) (other);
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
  
  public static final class BothE extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.langs.tinkerpop.gremlin.StringNullableArgument> value;
    
    public BothE (java.util.List<hydra.langs.tinkerpop.gremlin.StringNullableArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BothE)) {
        return false;
      }
      BothE o = (BothE) (other);
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
  
  public static final class BothV extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public BothV () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BothV)) {
        return false;
      }
      BothV o = (BothV) (other);
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
  
  public static final class Branch extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.NestedTraversal value;
    
    public Branch (hydra.langs.tinkerpop.gremlin.NestedTraversal value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Branch)) {
        return false;
      }
      Branch o = (Branch) (other);
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
  
  public static final class By extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.ByArgs value;
    
    public By (hydra.langs.tinkerpop.gremlin.ByArgs value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof By)) {
        return false;
      }
      By o = (By) (other);
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
  
  public static final class Cap extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.StringArgumentAndOptionalStringLiteralVarargs value;
    
    public Cap (hydra.langs.tinkerpop.gremlin.StringArgumentAndOptionalStringLiteralVarargs value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Cap)) {
        return false;
      }
      Cap o = (Cap) (other);
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
  
  public static final class Choose extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.ChooseArgs value;
    
    public Choose (hydra.langs.tinkerpop.gremlin.ChooseArgs value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Choose)) {
        return false;
      }
      Choose o = (Choose) (other);
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
  
  public static final class Coalesce extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.langs.tinkerpop.gremlin.NestedTraversal> value;
    
    public Coalesce (java.util.List<hydra.langs.tinkerpop.gremlin.NestedTraversal> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Coalesce)) {
        return false;
      }
      Coalesce o = (Coalesce) (other);
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
  
  public static final class Coin extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.FloatArgument value;
    
    public Coin (hydra.langs.tinkerpop.gremlin.FloatArgument value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Coin)) {
        return false;
      }
      Coin o = (Coin) (other);
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
  
  public static final class Conjoin extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.StringArgument value;
    
    public Conjoin (hydra.langs.tinkerpop.gremlin.StringArgument value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Conjoin)) {
        return false;
      }
      Conjoin o = (Conjoin) (other);
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
  
  public static final class ConnectedComponent extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public ConnectedComponent () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ConnectedComponent)) {
        return false;
      }
      ConnectedComponent o = (ConnectedComponent) (other);
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
  
  public static final class Constant extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.GenericLiteralArgument value;
    
    public Constant (hydra.langs.tinkerpop.gremlin.GenericLiteralArgument value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
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
  
  public static final class Count extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> value;
    
    public Count (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Count)) {
        return false;
      }
      Count o = (Count) (other);
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
  
  public static final class CyclicPath extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public CyclicPath () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CyclicPath)) {
        return false;
      }
      CyclicPath o = (CyclicPath) (other);
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
  
  public static final class Dedup extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.DedupArgs value;
    
    public Dedup (hydra.langs.tinkerpop.gremlin.DedupArgs value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Dedup)) {
        return false;
      }
      Dedup o = (Dedup) (other);
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
  
  public static final class Difference extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.GenericLiteralArgument value;
    
    public Difference (hydra.langs.tinkerpop.gremlin.GenericLiteralArgument value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Difference)) {
        return false;
      }
      Difference o = (Difference) (other);
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
  
  public static final class Disjunct extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.GenericLiteralArgument value;
    
    public Disjunct (hydra.langs.tinkerpop.gremlin.GenericLiteralArgument value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Disjunct)) {
        return false;
      }
      Disjunct o = (Disjunct) (other);
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
  
  public static final class Drop extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public Drop () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Drop)) {
        return false;
      }
      Drop o = (Drop) (other);
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
  
  public static final class ElementMap extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.langs.tinkerpop.gremlin.StringNullableArgument> value;
    
    public ElementMap (java.util.List<hydra.langs.tinkerpop.gremlin.StringNullableArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ElementMap)) {
        return false;
      }
      ElementMap o = (ElementMap) (other);
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
  
  public static final class Emit extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.PredicateOrTraversal> value;
    
    public Emit (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.PredicateOrTraversal> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Emit)) {
        return false;
      }
      Emit o = (Emit) (other);
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
  
  public static final class Filter extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.PredicateOrTraversal value;
    
    public Filter (hydra.langs.tinkerpop.gremlin.PredicateOrTraversal value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Filter)) {
        return false;
      }
      Filter o = (Filter) (other);
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
  
  public static final class FlatMap extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.NestedTraversal value;
    
    public FlatMap (hydra.langs.tinkerpop.gremlin.NestedTraversal value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FlatMap)) {
        return false;
      }
      FlatMap o = (FlatMap) (other);
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
  
  public static final class Fold extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument> value;
    
    public Fold (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Fold)) {
        return false;
      }
      Fold o = (Fold) (other);
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
  
  public static final class From extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.FromArgs value;
    
    public From (hydra.langs.tinkerpop.gremlin.FromArgs value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof From)) {
        return false;
      }
      From o = (From) (other);
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
  
  public static final class Group extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.StringArgument> value;
    
    public Group (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.StringArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Group)) {
        return false;
      }
      Group o = (Group) (other);
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
  
  public static final class GroupCount extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.StringArgument> value;
    
    public GroupCount (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.StringArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GroupCount)) {
        return false;
      }
      GroupCount o = (GroupCount) (other);
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
  
  public static final class Has extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.HasArgs value;
    
    public Has (hydra.langs.tinkerpop.gremlin.HasArgs value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Has)) {
        return false;
      }
      Has o = (Has) (other);
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
  
  public static final class HasId extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalPredicate value;
    
    public HasId (hydra.langs.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalPredicate value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof HasId)) {
        return false;
      }
      HasId o = (HasId) (other);
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
  
  public static final class HasKey extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.TraversalPredicateOrStringLiteralVarargs value;
    
    public HasKey (hydra.langs.tinkerpop.gremlin.TraversalPredicateOrStringLiteralVarargs value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof HasKey)) {
        return false;
      }
      HasKey o = (HasKey) (other);
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
  
  public static final class HasLabel extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.TraversalPredicateOrStringLiteralVarargs value;
    
    public HasLabel (hydra.langs.tinkerpop.gremlin.TraversalPredicateOrStringLiteralVarargs value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof HasLabel)) {
        return false;
      }
      HasLabel o = (HasLabel) (other);
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
  
  public static final class HasNot extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.StringNullableArgument value;
    
    public HasNot (hydra.langs.tinkerpop.gremlin.StringNullableArgument value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof HasNot)) {
        return false;
      }
      HasNot o = (HasNot) (other);
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
  
  public static final class HasValue extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.TraversalPredicateOrGenericLiteralArgument value;
    
    public HasValue (hydra.langs.tinkerpop.gremlin.TraversalPredicateOrGenericLiteralArgument value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof HasValue)) {
        return false;
      }
      HasValue o = (HasValue) (other);
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
  
  public static final class Id extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public Id () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Id)) {
        return false;
      }
      Id o = (Id) (other);
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
  
  public static final class Identity extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public Identity () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Identity)) {
        return false;
      }
      Identity o = (Identity) (other);
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
  
  public static final class In extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.langs.tinkerpop.gremlin.StringNullableArgument> value;
    
    public In (java.util.List<hydra.langs.tinkerpop.gremlin.StringNullableArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof In)) {
        return false;
      }
      In o = (In) (other);
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
  
  public static final class InE extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.langs.tinkerpop.gremlin.StringNullableArgument> value;
    
    public InE (java.util.List<hydra.langs.tinkerpop.gremlin.StringNullableArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InE)) {
        return false;
      }
      InE o = (InE) (other);
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
  
  public static final class Intersect extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.GenericLiteralArgument value;
    
    public Intersect (hydra.langs.tinkerpop.gremlin.GenericLiteralArgument value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Intersect)) {
        return false;
      }
      Intersect o = (Intersect) (other);
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
  
  public static final class InV extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public InV () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InV)) {
        return false;
      }
      InV o = (InV) (other);
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
  
  public static final class Index extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public Index () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Index)) {
        return false;
      }
      Index o = (Index) (other);
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
  
  public static final class Inject extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.langs.tinkerpop.gremlin.GenericLiteralArgument> value;
    
    public Inject (java.util.List<hydra.langs.tinkerpop.gremlin.GenericLiteralArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Inject)) {
        return false;
      }
      Inject o = (Inject) (other);
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
  
  public static final class Is extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.TraversalPredicateOrGenericLiteralArgument value;
    
    public Is (hydra.langs.tinkerpop.gremlin.TraversalPredicateOrGenericLiteralArgument value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Is)) {
        return false;
      }
      Is o = (Is) (other);
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
  
  public static final class Key extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public Key () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Key)) {
        return false;
      }
      Key o = (Key) (other);
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
  
  public static final class Label extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public Label () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Label)) {
        return false;
      }
      Label o = (Label) (other);
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
  
  public static final class Limit extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndIntegerArgument value;
    
    public Limit (hydra.langs.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndIntegerArgument value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Limit)) {
        return false;
      }
      Limit o = (Limit) (other);
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
  
  public static final class Local extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.NestedTraversal value;
    
    public Local (hydra.langs.tinkerpop.gremlin.NestedTraversal value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Local)) {
        return false;
      }
      Local o = (Local) (other);
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
  
  public static final class Loops extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.StringArgument> value;
    
    public Loops (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.StringArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Loops)) {
        return false;
      }
      Loops o = (Loops) (other);
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
  
  public static final class Map extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.NestedTraversal value;
    
    public Map (hydra.langs.tinkerpop.gremlin.NestedTraversal value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Map)) {
        return false;
      }
      Map o = (Map) (other);
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
  
  public static final class Match extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.langs.tinkerpop.gremlin.NestedTraversal> value;
    
    public Match (java.util.List<hydra.langs.tinkerpop.gremlin.NestedTraversal> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Match)) {
        return false;
      }
      Match o = (Match) (other);
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
  
  public static final class Math_ extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.StringArgument value;
    
    public Math_ (hydra.langs.tinkerpop.gremlin.StringArgument value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Math_)) {
        return false;
      }
      Math_ o = (Math_) (other);
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
  
  public static final class Max extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> value;
    
    public Max (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Max)) {
        return false;
      }
      Max o = (Max) (other);
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
  
  public static final class Mean extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> value;
    
    public Mean (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Mean)) {
        return false;
      }
      Mean o = (Mean) (other);
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
  
  public static final class Min extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> value;
    
    public Min (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Min)) {
        return false;
      }
      Min o = (Min) (other);
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
  
  public static final class None extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.TraversalPredicate value;
    
    public None (hydra.langs.tinkerpop.gremlin.TraversalPredicate value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof None)) {
        return false;
      }
      None o = (None) (other);
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
  
  public static final class Not extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.NestedTraversal value;
    
    public Not (hydra.langs.tinkerpop.gremlin.NestedTraversal value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Not)) {
        return false;
      }
      Not o = (Not) (other);
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
  
  public static final class Option extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.OptionArgs value;
    
    public Option (hydra.langs.tinkerpop.gremlin.OptionArgs value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
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
  
  public static final class Optional extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.NestedTraversal value;
    
    public Optional (hydra.langs.tinkerpop.gremlin.NestedTraversal value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Optional)) {
        return false;
      }
      Optional o = (Optional) (other);
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
  
  public static final class Or extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.langs.tinkerpop.gremlin.NestedTraversal> value;
    
    public Or (java.util.List<hydra.langs.tinkerpop.gremlin.NestedTraversal> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Or)) {
        return false;
      }
      Or o = (Or) (other);
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
  
  public static final class Order extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> value;
    
    public Order (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Order)) {
        return false;
      }
      Order o = (Order) (other);
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
  
  public static final class OtherV extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public OtherV () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OtherV)) {
        return false;
      }
      OtherV o = (OtherV) (other);
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
  
  public static final class Out extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.langs.tinkerpop.gremlin.StringNullableArgument> value;
    
    public Out (java.util.List<hydra.langs.tinkerpop.gremlin.StringNullableArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Out)) {
        return false;
      }
      Out o = (Out) (other);
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
  
  public static final class OutE extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.langs.tinkerpop.gremlin.StringNullableArgument> value;
    
    public OutE (java.util.List<hydra.langs.tinkerpop.gremlin.StringNullableArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OutE)) {
        return false;
      }
      OutE o = (OutE) (other);
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
  
  public static final class OutV extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public OutV () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OutV)) {
        return false;
      }
      OutV o = (OutV) (other);
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
  
  public static final class PageRank extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.FloatArgument> value;
    
    public PageRank (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.FloatArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PageRank)) {
        return false;
      }
      PageRank o = (PageRank) (other);
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
  
  public static final class Path extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public Path () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Path)) {
        return false;
      }
      Path o = (Path) (other);
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
  
  public static final class PeerPressure extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public PeerPressure () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PeerPressure)) {
        return false;
      }
      PeerPressure o = (PeerPressure) (other);
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
  
  public static final class Profile extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.StringArgument> value;
    
    public Profile (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.StringArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Profile)) {
        return false;
      }
      Profile o = (Profile) (other);
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
  
  public static final class Project extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.StringArgumentAndOptionalStringLiteralVarargs value;
    
    public Project (hydra.langs.tinkerpop.gremlin.StringArgumentAndOptionalStringLiteralVarargs value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Project)) {
        return false;
      }
      Project o = (Project) (other);
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
  
  public static final class Properties extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.langs.tinkerpop.gremlin.StringNullableArgument> value;
    
    public Properties (java.util.List<hydra.langs.tinkerpop.gremlin.StringNullableArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Properties)) {
        return false;
      }
      Properties o = (Properties) (other);
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
  
  public static final class Property extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.PropertyArgs value;
    
    public Property (hydra.langs.tinkerpop.gremlin.PropertyArgs value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Property)) {
        return false;
      }
      Property o = (Property) (other);
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
  
  public static final class PropertyMap extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.langs.tinkerpop.gremlin.StringNullableArgument> value;
    
    public PropertyMap (java.util.List<hydra.langs.tinkerpop.gremlin.StringNullableArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PropertyMap)) {
        return false;
      }
      PropertyMap o = (PropertyMap) (other);
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
  
  public static final class Range extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.RangeArgs value;
    
    public Range (hydra.langs.tinkerpop.gremlin.RangeArgs value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Range)) {
        return false;
      }
      Range o = (Range) (other);
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
  
  public static final class Read extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public Read () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Read)) {
        return false;
      }
      Read o = (Read) (other);
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
  
  public static final class Repeat extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.OptionalStringArgumentAndNestedTraversal value;
    
    public Repeat (hydra.langs.tinkerpop.gremlin.OptionalStringArgumentAndNestedTraversal value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Repeat)) {
        return false;
      }
      Repeat o = (Repeat) (other);
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
  
  public static final class Sack extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalBiFunctionArgument> value;
    
    public Sack (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalBiFunctionArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sack)) {
        return false;
      }
      Sack o = (Sack) (other);
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
  
  public static final class Sample extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndIntegerArgument value;
    
    public Sample (hydra.langs.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndIntegerArgument value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sample)) {
        return false;
      }
      Sample o = (Sample) (other);
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
  
  public static final class Select extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.SelectArgs value;
    
    public Select (hydra.langs.tinkerpop.gremlin.SelectArgs value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Select)) {
        return false;
      }
      Select o = (Select) (other);
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
  
  public static final class Combine extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.GenericLiteralArgument value;
    
    public Combine (hydra.langs.tinkerpop.gremlin.GenericLiteralArgument value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Combine)) {
        return false;
      }
      Combine o = (Combine) (other);
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
  
  public static final class Product extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.GenericLiteralArgument value;
    
    public Product (hydra.langs.tinkerpop.gremlin.GenericLiteralArgument value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Product)) {
        return false;
      }
      Product o = (Product) (other);
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
  
  public static final class Merge extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.GenericLiteralArgument value;
    
    public Merge (hydra.langs.tinkerpop.gremlin.GenericLiteralArgument value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Merge)) {
        return false;
      }
      Merge o = (Merge) (other);
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
  
  public static final class ShortestPath extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public ShortestPath () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ShortestPath)) {
        return false;
      }
      ShortestPath o = (ShortestPath) (other);
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
  
  public static final class SideEffect extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.NestedTraversal value;
    
    public SideEffect (hydra.langs.tinkerpop.gremlin.NestedTraversal value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SideEffect)) {
        return false;
      }
      SideEffect o = (SideEffect) (other);
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
  
  public static final class SimplePath extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public SimplePath () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SimplePath)) {
        return false;
      }
      SimplePath o = (SimplePath) (other);
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
  
  public static final class Skip extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndIntegerArgument value;
    
    public Skip (hydra.langs.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndIntegerArgument value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Skip)) {
        return false;
      }
      Skip o = (Skip) (other);
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
  
  public static final class Store extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.StringArgument value;
    
    public Store (hydra.langs.tinkerpop.gremlin.StringArgument value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Store)) {
        return false;
      }
      Store o = (Store) (other);
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
  
  public static final class Subgraph extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.StringArgument value;
    
    public Subgraph (hydra.langs.tinkerpop.gremlin.StringArgument value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Subgraph)) {
        return false;
      }
      Subgraph o = (Subgraph) (other);
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
  
  public static final class Sum extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> value;
    
    public Sum (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sum)) {
        return false;
      }
      Sum o = (Sum) (other);
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
  
  public static final class Tail extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TailArgs> value;
    
    public Tail (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TailArgs> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Tail)) {
        return false;
      }
      Tail o = (Tail) (other);
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
  
  public static final class Fail extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.StringArgument> value;
    
    public Fail (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.StringArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Fail)) {
        return false;
      }
      Fail o = (Fail) (other);
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
  
  public static final class Times extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.IntegerArgument value;
    
    public Times (hydra.langs.tinkerpop.gremlin.IntegerArgument value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Times)) {
        return false;
      }
      Times o = (Times) (other);
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
  
  public static final class To extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.ToArgs value;
    
    public To (hydra.langs.tinkerpop.gremlin.ToArgs value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof To)) {
        return false;
      }
      To o = (To) (other);
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
  
  public static final class ToE extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.DirectionAndVarargs value;
    
    public ToE (hydra.langs.tinkerpop.gremlin.DirectionAndVarargs value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ToE)) {
        return false;
      }
      ToE o = (ToE) (other);
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
  
  public static final class ToV extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.TraversalDirectionArgument value;
    
    public ToV (hydra.langs.tinkerpop.gremlin.TraversalDirectionArgument value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ToV)) {
        return false;
      }
      ToV o = (ToV) (other);
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
  
  public static final class Tree extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.StringArgument> value;
    
    public Tree (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.StringArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Tree)) {
        return false;
      }
      Tree o = (Tree) (other);
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
  
  public static final class Unfold extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public Unfold () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Unfold)) {
        return false;
      }
      Unfold o = (Unfold) (other);
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
  
  public static final class Union extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.langs.tinkerpop.gremlin.NestedTraversal> value;
    
    public Union (java.util.List<hydra.langs.tinkerpop.gremlin.NestedTraversal> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Union)) {
        return false;
      }
      Union o = (Union) (other);
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
  
  public static final class Until extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.PredicateOrTraversal value;
    
    public Until (hydra.langs.tinkerpop.gremlin.PredicateOrTraversal value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Until)) {
        return false;
      }
      Until o = (Until) (other);
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
  
  public static final class Value extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public Value () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Value)) {
        return false;
      }
      Value o = (Value) (other);
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
  
  public static final class ValueMap extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.ValueMapArgs value;
    
    public ValueMap (hydra.langs.tinkerpop.gremlin.ValueMapArgs value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ValueMap)) {
        return false;
      }
      ValueMap o = (ValueMap) (other);
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
  
  public static final class Values extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.langs.tinkerpop.gremlin.StringNullableArgument> value;
    
    public Values (java.util.List<hydra.langs.tinkerpop.gremlin.StringNullableArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Values)) {
        return false;
      }
      Values o = (Values) (other);
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
  
  public static final class Where extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.WhereArgs value;
    
    public Where (hydra.langs.tinkerpop.gremlin.WhereArgs value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Where)) {
        return false;
      }
      Where o = (Where) (other);
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
  
  public static final class With extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.WithArgs value;
    
    public With (hydra.langs.tinkerpop.gremlin.WithArgs value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof With)) {
        return false;
      }
      With o = (With) (other);
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
  
  public static final class Write extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public Write () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Write)) {
        return false;
      }
      Write o = (Write) (other);
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
  
  public static final class Element extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.langs.tinkerpop.gremlin.StringNullableArgument> value;
    
    public Element (java.util.List<hydra.langs.tinkerpop.gremlin.StringNullableArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Element)) {
        return false;
      }
      Element o = (Element) (other);
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
  
  public static final class Call extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.ServiceCall value;
    
    public Call (hydra.langs.tinkerpop.gremlin.ServiceCall value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Call)) {
        return false;
      }
      Call o = (Call) (other);
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
  
  public static final class Concat extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.ConcatArgs value;
    
    public Concat (hydra.langs.tinkerpop.gremlin.ConcatArgs value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Concat)) {
        return false;
      }
      Concat o = (Concat) (other);
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
  
  public static final class AsString extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> value;
    
    public AsString (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AsString)) {
        return false;
      }
      AsString o = (AsString) (other);
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
  
  public static final class Format extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.StringArgument value;
    
    public Format (hydra.langs.tinkerpop.gremlin.StringArgument value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Format)) {
        return false;
      }
      Format o = (Format) (other);
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
  
  public static final class ToUpper extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> value;
    
    public ToUpper (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ToUpper)) {
        return false;
      }
      ToUpper o = (ToUpper) (other);
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
  
  public static final class ToLower extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> value;
    
    public ToLower (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ToLower)) {
        return false;
      }
      ToLower o = (ToLower) (other);
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
  
  public static final class Length extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> value;
    
    public Length (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Length)) {
        return false;
      }
      Length o = (Length) (other);
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
  
  public static final class Trim extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> value;
    
    public Trim (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Trim)) {
        return false;
      }
      Trim o = (Trim) (other);
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
  
  public static final class LTrim extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> value;
    
    public LTrim (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LTrim)) {
        return false;
      }
      LTrim o = (LTrim) (other);
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
  
  public static final class RTrim extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> value;
    
    public RTrim (hydra.util.Opt<hydra.langs.tinkerpop.gremlin.TraversalScopeArgument> value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RTrim)) {
        return false;
      }
      RTrim o = (RTrim) (other);
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
  
  public static final class Reverse extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public Reverse () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Reverse)) {
        return false;
      }
      Reverse o = (Reverse) (other);
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
  
  public static final class Replace extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.ReplaceArgs value;
    
    public Replace (hydra.langs.tinkerpop.gremlin.ReplaceArgs value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Replace)) {
        return false;
      }
      Replace o = (Replace) (other);
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
  
  public static final class Split extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.SplitArgs value;
    
    public Split (hydra.langs.tinkerpop.gremlin.SplitArgs value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Split)) {
        return false;
      }
      Split o = (Split) (other);
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
  
  public static final class Substring extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.SubstringArgs value;
    
    public Substring (hydra.langs.tinkerpop.gremlin.SubstringArgs value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Substring)) {
        return false;
      }
      Substring o = (Substring) (other);
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
  
  public static final class AsDate extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public AsDate () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AsDate)) {
        return false;
      }
      AsDate o = (AsDate) (other);
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
  
  public static final class DateAdd extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.DateAddArgs value;
    
    public DateAdd (hydra.langs.tinkerpop.gremlin.DateAddArgs value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DateAdd)) {
        return false;
      }
      DateAdd o = (DateAdd) (other);
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
  
  public static final class DateDiff extends hydra.langs.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.langs.tinkerpop.gremlin.DateDiffArgs value;
    
    public DateDiff (hydra.langs.tinkerpop.gremlin.DateDiffArgs value) {
      if (value == null) {
        throw new IllegalArgumentException("null value for 'value' argument");
      }
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DateDiff)) {
        return false;
      }
      DateDiff o = (DateDiff) (other);
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