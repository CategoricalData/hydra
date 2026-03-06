// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.apache.tinkerpop.gremlin;

import java.io.Serializable;

public abstract class TraversalMethod implements Serializable, Comparable<TraversalMethod> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod");
  
  public static final hydra.core.Name V = new hydra.core.Name("v");
  
  public static final hydra.core.Name E = new hydra.core.Name("e");
  
  public static final hydra.core.Name ADD_E = new hydra.core.Name("addE");
  
  public static final hydra.core.Name ADD_V = new hydra.core.Name("addV");
  
  public static final hydra.core.Name MERGE_E = new hydra.core.Name("mergeE");
  
  public static final hydra.core.Name MERGE_V = new hydra.core.Name("mergeV");
  
  public static final hydra.core.Name AGGREGATE = new hydra.core.Name("aggregate");
  
  public static final hydra.core.Name ALL = new hydra.core.Name("all");
  
  public static final hydra.core.Name AND = new hydra.core.Name("and");
  
  public static final hydra.core.Name ANY = new hydra.core.Name("any");
  
  public static final hydra.core.Name AS = new hydra.core.Name("as");
  
  public static final hydra.core.Name BARRIER = new hydra.core.Name("barrier");
  
  public static final hydra.core.Name BOTH = new hydra.core.Name("both");
  
  public static final hydra.core.Name BOTH_E = new hydra.core.Name("bothE");
  
  public static final hydra.core.Name BOTH_V = new hydra.core.Name("bothV");
  
  public static final hydra.core.Name BRANCH = new hydra.core.Name("branch");
  
  public static final hydra.core.Name BY = new hydra.core.Name("by");
  
  public static final hydra.core.Name CAP = new hydra.core.Name("cap");
  
  public static final hydra.core.Name CHOOSE = new hydra.core.Name("choose");
  
  public static final hydra.core.Name COALESCE = new hydra.core.Name("coalesce");
  
  public static final hydra.core.Name COIN = new hydra.core.Name("coin");
  
  public static final hydra.core.Name CONJOIN = new hydra.core.Name("conjoin");
  
  public static final hydra.core.Name CONNECTED_COMPONENT = new hydra.core.Name("connectedComponent");
  
  public static final hydra.core.Name CONSTANT = new hydra.core.Name("constant");
  
  public static final hydra.core.Name COUNT = new hydra.core.Name("count");
  
  public static final hydra.core.Name CYCLIC_PATH = new hydra.core.Name("cyclicPath");
  
  public static final hydra.core.Name DEDUP = new hydra.core.Name("dedup");
  
  public static final hydra.core.Name DIFFERENCE = new hydra.core.Name("difference");
  
  public static final hydra.core.Name DISJUNCT = new hydra.core.Name("disjunct");
  
  public static final hydra.core.Name DROP = new hydra.core.Name("drop");
  
  public static final hydra.core.Name ELEMENT_MAP = new hydra.core.Name("elementMap");
  
  public static final hydra.core.Name EMIT = new hydra.core.Name("emit");
  
  public static final hydra.core.Name FILTER = new hydra.core.Name("filter");
  
  public static final hydra.core.Name FLAT_MAP = new hydra.core.Name("flatMap");
  
  public static final hydra.core.Name FOLD = new hydra.core.Name("fold");
  
  public static final hydra.core.Name FROM = new hydra.core.Name("from");
  
  public static final hydra.core.Name GROUP = new hydra.core.Name("group");
  
  public static final hydra.core.Name GROUP_COUNT = new hydra.core.Name("groupCount");
  
  public static final hydra.core.Name HAS = new hydra.core.Name("has");
  
  public static final hydra.core.Name HAS_ID = new hydra.core.Name("hasId");
  
  public static final hydra.core.Name HAS_KEY = new hydra.core.Name("hasKey");
  
  public static final hydra.core.Name HAS_LABEL = new hydra.core.Name("hasLabel");
  
  public static final hydra.core.Name HAS_NOT = new hydra.core.Name("hasNot");
  
  public static final hydra.core.Name HAS_VALUE = new hydra.core.Name("hasValue");
  
  public static final hydra.core.Name ID = new hydra.core.Name("id");
  
  public static final hydra.core.Name IDENTITY = new hydra.core.Name("identity");
  
  public static final hydra.core.Name IN = new hydra.core.Name("in");
  
  public static final hydra.core.Name IN_E = new hydra.core.Name("inE");
  
  public static final hydra.core.Name INTERSECT = new hydra.core.Name("intersect");
  
  public static final hydra.core.Name IN_V = new hydra.core.Name("inV");
  
  public static final hydra.core.Name INDEX = new hydra.core.Name("index");
  
  public static final hydra.core.Name INJECT = new hydra.core.Name("inject");
  
  public static final hydra.core.Name IS = new hydra.core.Name("is");
  
  public static final hydra.core.Name KEY = new hydra.core.Name("key");
  
  public static final hydra.core.Name LABEL = new hydra.core.Name("label");
  
  public static final hydra.core.Name LIMIT = new hydra.core.Name("limit");
  
  public static final hydra.core.Name LOCAL = new hydra.core.Name("local");
  
  public static final hydra.core.Name LOOPS = new hydra.core.Name("loops");
  
  public static final hydra.core.Name MAP = new hydra.core.Name("map");
  
  public static final hydra.core.Name MATCH = new hydra.core.Name("match");
  
  public static final hydra.core.Name MATH = new hydra.core.Name("math");
  
  public static final hydra.core.Name MAX = new hydra.core.Name("max");
  
  public static final hydra.core.Name MEAN = new hydra.core.Name("mean");
  
  public static final hydra.core.Name MIN = new hydra.core.Name("min");
  
  public static final hydra.core.Name NONE = new hydra.core.Name("none");
  
  public static final hydra.core.Name NOT = new hydra.core.Name("not");
  
  public static final hydra.core.Name OPTION = new hydra.core.Name("option");
  
  public static final hydra.core.Name OPTIONAL = new hydra.core.Name("optional");
  
  public static final hydra.core.Name OR = new hydra.core.Name("or");
  
  public static final hydra.core.Name ORDER = new hydra.core.Name("order");
  
  public static final hydra.core.Name OTHER_V = new hydra.core.Name("otherV");
  
  public static final hydra.core.Name OUT = new hydra.core.Name("out");
  
  public static final hydra.core.Name OUT_E = new hydra.core.Name("outE");
  
  public static final hydra.core.Name OUT_V = new hydra.core.Name("outV");
  
  public static final hydra.core.Name PAGE_RANK = new hydra.core.Name("pageRank");
  
  public static final hydra.core.Name PATH = new hydra.core.Name("path");
  
  public static final hydra.core.Name PEER_PRESSURE = new hydra.core.Name("peerPressure");
  
  public static final hydra.core.Name PROFILE = new hydra.core.Name("profile");
  
  public static final hydra.core.Name PROJECT = new hydra.core.Name("project");
  
  public static final hydra.core.Name PROPERTIES = new hydra.core.Name("properties");
  
  public static final hydra.core.Name PROPERTY = new hydra.core.Name("property");
  
  public static final hydra.core.Name PROPERTY_MAP = new hydra.core.Name("propertyMap");
  
  public static final hydra.core.Name RANGE = new hydra.core.Name("range");
  
  public static final hydra.core.Name READ = new hydra.core.Name("read");
  
  public static final hydra.core.Name REPEAT = new hydra.core.Name("repeat");
  
  public static final hydra.core.Name SACK = new hydra.core.Name("sack");
  
  public static final hydra.core.Name SAMPLE = new hydra.core.Name("sample");
  
  public static final hydra.core.Name SELECT = new hydra.core.Name("select");
  
  public static final hydra.core.Name COMBINE = new hydra.core.Name("combine");
  
  public static final hydra.core.Name PRODUCT = new hydra.core.Name("product");
  
  public static final hydra.core.Name MERGE = new hydra.core.Name("merge");
  
  public static final hydra.core.Name SHORTEST_PATH = new hydra.core.Name("shortestPath");
  
  public static final hydra.core.Name SIDE_EFFECT = new hydra.core.Name("sideEffect");
  
  public static final hydra.core.Name SIMPLE_PATH = new hydra.core.Name("simplePath");
  
  public static final hydra.core.Name SKIP = new hydra.core.Name("skip");
  
  public static final hydra.core.Name STORE = new hydra.core.Name("store");
  
  public static final hydra.core.Name SUBGRAPH = new hydra.core.Name("subgraph");
  
  public static final hydra.core.Name SUM = new hydra.core.Name("sum");
  
  public static final hydra.core.Name TAIL = new hydra.core.Name("tail");
  
  public static final hydra.core.Name FAIL = new hydra.core.Name("fail");
  
  public static final hydra.core.Name TIMES = new hydra.core.Name("times");
  
  public static final hydra.core.Name TO = new hydra.core.Name("to");
  
  public static final hydra.core.Name TO_E = new hydra.core.Name("toE");
  
  public static final hydra.core.Name TO_V = new hydra.core.Name("toV");
  
  public static final hydra.core.Name TREE = new hydra.core.Name("tree");
  
  public static final hydra.core.Name UNFOLD = new hydra.core.Name("unfold");
  
  public static final hydra.core.Name UNION = new hydra.core.Name("union");
  
  public static final hydra.core.Name UNTIL = new hydra.core.Name("until");
  
  public static final hydra.core.Name VALUE = new hydra.core.Name("value");
  
  public static final hydra.core.Name VALUE_MAP = new hydra.core.Name("valueMap");
  
  public static final hydra.core.Name VALUES = new hydra.core.Name("values");
  
  public static final hydra.core.Name WHERE = new hydra.core.Name("where");
  
  public static final hydra.core.Name WITH = new hydra.core.Name("with");
  
  public static final hydra.core.Name WRITE = new hydra.core.Name("write");
  
  public static final hydra.core.Name ELEMENT = new hydra.core.Name("element");
  
  public static final hydra.core.Name CALL = new hydra.core.Name("call");
  
  public static final hydra.core.Name CONCAT = new hydra.core.Name("concat");
  
  public static final hydra.core.Name AS_STRING = new hydra.core.Name("asString");
  
  public static final hydra.core.Name FORMAT = new hydra.core.Name("format");
  
  public static final hydra.core.Name TO_UPPER = new hydra.core.Name("toUpper");
  
  public static final hydra.core.Name TO_LOWER = new hydra.core.Name("toLower");
  
  public static final hydra.core.Name LENGTH = new hydra.core.Name("length");
  
  public static final hydra.core.Name TRIM = new hydra.core.Name("trim");
  
  public static final hydra.core.Name L_TRIM = new hydra.core.Name("lTrim");
  
  public static final hydra.core.Name R_TRIM = new hydra.core.Name("rTrim");
  
  public static final hydra.core.Name REVERSE = new hydra.core.Name("reverse");
  
  public static final hydra.core.Name REPLACE = new hydra.core.Name("replace");
  
  public static final hydra.core.Name SPLIT = new hydra.core.Name("split");
  
  public static final hydra.core.Name SUBSTRING = new hydra.core.Name("substring");
  
  public static final hydra.core.Name AS_DATE = new hydra.core.Name("asDate");
  
  public static final hydra.core.Name DATE_ADD = new hydra.core.Name("dateAdd");
  
  public static final hydra.core.Name DATE_DIFF = new hydra.core.Name("dateDiff");
  
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
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }
    
    default R visit(V instance) {
      return otherwise(instance);
    }
    
    default R visit(E instance) {
      return otherwise(instance);
    }
    
    default R visit(AddE instance) {
      return otherwise(instance);
    }
    
    default R visit(AddV instance) {
      return otherwise(instance);
    }
    
    default R visit(MergeE instance) {
      return otherwise(instance);
    }
    
    default R visit(MergeV instance) {
      return otherwise(instance);
    }
    
    default R visit(Aggregate instance) {
      return otherwise(instance);
    }
    
    default R visit(All instance) {
      return otherwise(instance);
    }
    
    default R visit(And instance) {
      return otherwise(instance);
    }
    
    default R visit(Any instance) {
      return otherwise(instance);
    }
    
    default R visit(As instance) {
      return otherwise(instance);
    }
    
    default R visit(Barrier instance) {
      return otherwise(instance);
    }
    
    default R visit(Both instance) {
      return otherwise(instance);
    }
    
    default R visit(BothE instance) {
      return otherwise(instance);
    }
    
    default R visit(BothV instance) {
      return otherwise(instance);
    }
    
    default R visit(Branch instance) {
      return otherwise(instance);
    }
    
    default R visit(By instance) {
      return otherwise(instance);
    }
    
    default R visit(Cap instance) {
      return otherwise(instance);
    }
    
    default R visit(Choose instance) {
      return otherwise(instance);
    }
    
    default R visit(Coalesce instance) {
      return otherwise(instance);
    }
    
    default R visit(Coin instance) {
      return otherwise(instance);
    }
    
    default R visit(Conjoin instance) {
      return otherwise(instance);
    }
    
    default R visit(ConnectedComponent instance) {
      return otherwise(instance);
    }
    
    default R visit(Constant instance) {
      return otherwise(instance);
    }
    
    default R visit(Count instance) {
      return otherwise(instance);
    }
    
    default R visit(CyclicPath instance) {
      return otherwise(instance);
    }
    
    default R visit(Dedup instance) {
      return otherwise(instance);
    }
    
    default R visit(Difference instance) {
      return otherwise(instance);
    }
    
    default R visit(Disjunct instance) {
      return otherwise(instance);
    }
    
    default R visit(Drop instance) {
      return otherwise(instance);
    }
    
    default R visit(ElementMap instance) {
      return otherwise(instance);
    }
    
    default R visit(Emit instance) {
      return otherwise(instance);
    }
    
    default R visit(Filter instance) {
      return otherwise(instance);
    }
    
    default R visit(FlatMap instance) {
      return otherwise(instance);
    }
    
    default R visit(Fold instance) {
      return otherwise(instance);
    }
    
    default R visit(From instance) {
      return otherwise(instance);
    }
    
    default R visit(Group instance) {
      return otherwise(instance);
    }
    
    default R visit(GroupCount instance) {
      return otherwise(instance);
    }
    
    default R visit(Has instance) {
      return otherwise(instance);
    }
    
    default R visit(HasId instance) {
      return otherwise(instance);
    }
    
    default R visit(HasKey instance) {
      return otherwise(instance);
    }
    
    default R visit(HasLabel instance) {
      return otherwise(instance);
    }
    
    default R visit(HasNot instance) {
      return otherwise(instance);
    }
    
    default R visit(HasValue instance) {
      return otherwise(instance);
    }
    
    default R visit(Id instance) {
      return otherwise(instance);
    }
    
    default R visit(Identity instance) {
      return otherwise(instance);
    }
    
    default R visit(In instance) {
      return otherwise(instance);
    }
    
    default R visit(InE instance) {
      return otherwise(instance);
    }
    
    default R visit(Intersect instance) {
      return otherwise(instance);
    }
    
    default R visit(InV instance) {
      return otherwise(instance);
    }
    
    default R visit(Index instance) {
      return otherwise(instance);
    }
    
    default R visit(Inject instance) {
      return otherwise(instance);
    }
    
    default R visit(Is instance) {
      return otherwise(instance);
    }
    
    default R visit(Key instance) {
      return otherwise(instance);
    }
    
    default R visit(Label instance) {
      return otherwise(instance);
    }
    
    default R visit(Limit instance) {
      return otherwise(instance);
    }
    
    default R visit(Local instance) {
      return otherwise(instance);
    }
    
    default R visit(Loops instance) {
      return otherwise(instance);
    }
    
    default R visit(Map instance) {
      return otherwise(instance);
    }
    
    default R visit(Match instance) {
      return otherwise(instance);
    }
    
    default R visit(Math_ instance) {
      return otherwise(instance);
    }
    
    default R visit(Max instance) {
      return otherwise(instance);
    }
    
    default R visit(Mean instance) {
      return otherwise(instance);
    }
    
    default R visit(Min instance) {
      return otherwise(instance);
    }
    
    default R visit(None instance) {
      return otherwise(instance);
    }
    
    default R visit(Not instance) {
      return otherwise(instance);
    }
    
    default R visit(Option instance) {
      return otherwise(instance);
    }
    
    default R visit(Optional instance) {
      return otherwise(instance);
    }
    
    default R visit(Or instance) {
      return otherwise(instance);
    }
    
    default R visit(Order instance) {
      return otherwise(instance);
    }
    
    default R visit(OtherV instance) {
      return otherwise(instance);
    }
    
    default R visit(Out instance) {
      return otherwise(instance);
    }
    
    default R visit(OutE instance) {
      return otherwise(instance);
    }
    
    default R visit(OutV instance) {
      return otherwise(instance);
    }
    
    default R visit(PageRank instance) {
      return otherwise(instance);
    }
    
    default R visit(Path instance) {
      return otherwise(instance);
    }
    
    default R visit(PeerPressure instance) {
      return otherwise(instance);
    }
    
    default R visit(Profile instance) {
      return otherwise(instance);
    }
    
    default R visit(Project instance) {
      return otherwise(instance);
    }
    
    default R visit(Properties instance) {
      return otherwise(instance);
    }
    
    default R visit(Property instance) {
      return otherwise(instance);
    }
    
    default R visit(PropertyMap instance) {
      return otherwise(instance);
    }
    
    default R visit(Range instance) {
      return otherwise(instance);
    }
    
    default R visit(Read instance) {
      return otherwise(instance);
    }
    
    default R visit(Repeat instance) {
      return otherwise(instance);
    }
    
    default R visit(Sack instance) {
      return otherwise(instance);
    }
    
    default R visit(Sample instance) {
      return otherwise(instance);
    }
    
    default R visit(Select instance) {
      return otherwise(instance);
    }
    
    default R visit(Combine instance) {
      return otherwise(instance);
    }
    
    default R visit(Product instance) {
      return otherwise(instance);
    }
    
    default R visit(Merge instance) {
      return otherwise(instance);
    }
    
    default R visit(ShortestPath instance) {
      return otherwise(instance);
    }
    
    default R visit(SideEffect instance) {
      return otherwise(instance);
    }
    
    default R visit(SimplePath instance) {
      return otherwise(instance);
    }
    
    default R visit(Skip instance) {
      return otherwise(instance);
    }
    
    default R visit(Store instance) {
      return otherwise(instance);
    }
    
    default R visit(Subgraph instance) {
      return otherwise(instance);
    }
    
    default R visit(Sum instance) {
      return otherwise(instance);
    }
    
    default R visit(Tail instance) {
      return otherwise(instance);
    }
    
    default R visit(Fail instance) {
      return otherwise(instance);
    }
    
    default R visit(Times instance) {
      return otherwise(instance);
    }
    
    default R visit(To instance) {
      return otherwise(instance);
    }
    
    default R visit(ToE instance) {
      return otherwise(instance);
    }
    
    default R visit(ToV instance) {
      return otherwise(instance);
    }
    
    default R visit(Tree instance) {
      return otherwise(instance);
    }
    
    default R visit(Unfold instance) {
      return otherwise(instance);
    }
    
    default R visit(Union instance) {
      return otherwise(instance);
    }
    
    default R visit(Until instance) {
      return otherwise(instance);
    }
    
    default R visit(Value instance) {
      return otherwise(instance);
    }
    
    default R visit(ValueMap instance) {
      return otherwise(instance);
    }
    
    default R visit(Values instance) {
      return otherwise(instance);
    }
    
    default R visit(Where instance) {
      return otherwise(instance);
    }
    
    default R visit(With instance) {
      return otherwise(instance);
    }
    
    default R visit(Write instance) {
      return otherwise(instance);
    }
    
    default R visit(Element instance) {
      return otherwise(instance);
    }
    
    default R visit(Call instance) {
      return otherwise(instance);
    }
    
    default R visit(Concat instance) {
      return otherwise(instance);
    }
    
    default R visit(AsString instance) {
      return otherwise(instance);
    }
    
    default R visit(Format instance) {
      return otherwise(instance);
    }
    
    default R visit(ToUpper instance) {
      return otherwise(instance);
    }
    
    default R visit(ToLower instance) {
      return otherwise(instance);
    }
    
    default R visit(Length instance) {
      return otherwise(instance);
    }
    
    default R visit(Trim instance) {
      return otherwise(instance);
    }
    
    default R visit(LTrim instance) {
      return otherwise(instance);
    }
    
    default R visit(RTrim instance) {
      return otherwise(instance);
    }
    
    default R visit(Reverse instance) {
      return otherwise(instance);
    }
    
    default R visit(Replace instance) {
      return otherwise(instance);
    }
    
    default R visit(Split instance) {
      return otherwise(instance);
    }
    
    default R visit(Substring instance) {
      return otherwise(instance);
    }
    
    default R visit(AsDate instance) {
      return otherwise(instance);
    }
    
    default R visit(DateAdd instance) {
      return otherwise(instance);
    }
    
    default R visit(DateDiff instance) {
      return otherwise(instance);
    }
  }
  
  public static final class V extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument> value;
    
    public V (java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof V)) {
        return false;
      }
      V o = (V) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      V o = (V) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class E extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument> value;
    
    public E (java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof E)) {
        return false;
      }
      E o = (E) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      E o = (E) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class AddE extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.StringArgumentOrNestedTraversal value;
    
    public AddE (hydra.ext.org.apache.tinkerpop.gremlin.StringArgumentOrNestedTraversal value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AddE)) {
        return false;
      }
      AddE o = (AddE) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      AddE o = (AddE) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class AddV extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.StringArgumentOrNestedTraversal> value;
    
    public AddV (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.StringArgumentOrNestedTraversal> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AddV)) {
        return false;
      }
      AddV o = (AddV) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      AddV o = (AddV) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class MergeE extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal> value;
    
    public MergeE (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MergeE)) {
        return false;
      }
      MergeE o = (MergeE) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      MergeE o = (MergeE) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class MergeV extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal> value;
    
    public MergeV (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralMapNullableArgumentOrNestedTraversal> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof MergeV)) {
        return false;
      }
      MergeV o = (MergeV) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      MergeV o = (MergeV) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Aggregate extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndStringArgument value;
    
    public Aggregate (hydra.ext.org.apache.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndStringArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Aggregate)) {
        return false;
      }
      Aggregate o = (Aggregate) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Aggregate o = (Aggregate) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class All extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate value;
    
    public All (hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof All)) {
        return false;
      }
      All o = (All) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      All o = (All) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class And extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal> value;
    
    public And (java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal> value) {
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      And o = (And) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Any extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate value;
    
    public Any (hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Any)) {
        return false;
      }
      Any o = (Any) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Any o = (Any) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class As extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.StringArgumentAndOptionalStringLiteralVarargs value;
    
    public As (hydra.ext.org.apache.tinkerpop.gremlin.StringArgumentAndOptionalStringLiteralVarargs value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof As)) {
        return false;
      }
      As o = (As) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      As o = (As) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Barrier extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalSackMethodArgumentOrIntegerArgument> value;
    
    public Barrier (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalSackMethodArgumentOrIntegerArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Barrier)) {
        return false;
      }
      Barrier o = (Barrier) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Barrier o = (Barrier) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Both extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> value;
    
    public Both (java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Both)) {
        return false;
      }
      Both o = (Both) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Both o = (Both) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class BothE extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> value;
    
    public BothE (java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BothE)) {
        return false;
      }
      BothE o = (BothE) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      BothE o = (BothE) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class BothV extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public BothV () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof BothV)) {
        return false;
      }
      BothV o = (BothV) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalMethod other) {
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
  
  public static final class Branch extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal value;
    
    public Branch (hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Branch)) {
        return false;
      }
      Branch o = (Branch) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Branch o = (Branch) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class By extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.ByArgs value;
    
    public By (hydra.ext.org.apache.tinkerpop.gremlin.ByArgs value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof By)) {
        return false;
      }
      By o = (By) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      By o = (By) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Cap extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.StringArgumentAndOptionalStringLiteralVarargs value;
    
    public Cap (hydra.ext.org.apache.tinkerpop.gremlin.StringArgumentAndOptionalStringLiteralVarargs value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Cap)) {
        return false;
      }
      Cap o = (Cap) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Cap o = (Cap) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Choose extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.ChooseArgs value;
    
    public Choose (hydra.ext.org.apache.tinkerpop.gremlin.ChooseArgs value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Choose)) {
        return false;
      }
      Choose o = (Choose) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Choose o = (Choose) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Coalesce extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal> value;
    
    public Coalesce (java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Coalesce)) {
        return false;
      }
      Coalesce o = (Coalesce) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Coalesce o = (Coalesce) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Coin extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.FloatArgument value;
    
    public Coin (hydra.ext.org.apache.tinkerpop.gremlin.FloatArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Coin)) {
        return false;
      }
      Coin o = (Coin) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Coin o = (Coin) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Conjoin extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value;
    
    public Conjoin (hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Conjoin)) {
        return false;
      }
      Conjoin o = (Conjoin) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Conjoin o = (Conjoin) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ConnectedComponent extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public ConnectedComponent () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ConnectedComponent)) {
        return false;
      }
      ConnectedComponent o = (ConnectedComponent) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalMethod other) {
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
  
  public static final class Constant extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument value;
    
    public Constant (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Constant)) {
        return false;
      }
      Constant o = (Constant) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Constant o = (Constant) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Count extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> value;
    
    public Count (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Count)) {
        return false;
      }
      Count o = (Count) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Count o = (Count) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class CyclicPath extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public CyclicPath () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof CyclicPath)) {
        return false;
      }
      CyclicPath o = (CyclicPath) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalMethod other) {
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
  
  public static final class Dedup extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.DedupArgs value;
    
    public Dedup (hydra.ext.org.apache.tinkerpop.gremlin.DedupArgs value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Dedup)) {
        return false;
      }
      Dedup o = (Dedup) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Dedup o = (Dedup) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Difference extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument value;
    
    public Difference (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Difference)) {
        return false;
      }
      Difference o = (Difference) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Difference o = (Difference) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Disjunct extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument value;
    
    public Disjunct (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Disjunct)) {
        return false;
      }
      Disjunct o = (Disjunct) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Disjunct o = (Disjunct) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Drop extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public Drop () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Drop)) {
        return false;
      }
      Drop o = (Drop) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalMethod other) {
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
  
  public static final class ElementMap extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> value;
    
    public ElementMap (java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ElementMap)) {
        return false;
      }
      ElementMap o = (ElementMap) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ElementMap o = (ElementMap) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Emit extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.PredicateOrTraversal> value;
    
    public Emit (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.PredicateOrTraversal> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Emit)) {
        return false;
      }
      Emit o = (Emit) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Emit o = (Emit) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Filter extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.PredicateOrTraversal value;
    
    public Filter (hydra.ext.org.apache.tinkerpop.gremlin.PredicateOrTraversal value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Filter)) {
        return false;
      }
      Filter o = (Filter) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Filter o = (Filter) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class FlatMap extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal value;
    
    public FlatMap (hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FlatMap)) {
        return false;
      }
      FlatMap o = (FlatMap) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      FlatMap o = (FlatMap) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Fold extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument> value;
    
    public Fold (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalBiFunctionArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Fold)) {
        return false;
      }
      Fold o = (Fold) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Fold o = (Fold) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class From extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.FromArgs value;
    
    public From (hydra.ext.org.apache.tinkerpop.gremlin.FromArgs value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof From)) {
        return false;
      }
      From o = (From) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      From o = (From) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Group extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.StringArgument> value;
    
    public Group (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.StringArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Group)) {
        return false;
      }
      Group o = (Group) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Group o = (Group) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class GroupCount extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.StringArgument> value;
    
    public GroupCount (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.StringArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof GroupCount)) {
        return false;
      }
      GroupCount o = (GroupCount) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      GroupCount o = (GroupCount) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Has extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.HasArgs value;
    
    public Has (hydra.ext.org.apache.tinkerpop.gremlin.HasArgs value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Has)) {
        return false;
      }
      Has o = (Has) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Has o = (Has) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class HasId extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalPredicate value;
    
    public HasId (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgumentAndTraversalPredicate value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof HasId)) {
        return false;
      }
      HasId o = (HasId) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      HasId o = (HasId) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class HasKey extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicateOrStringLiteralVarargs value;
    
    public HasKey (hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicateOrStringLiteralVarargs value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof HasKey)) {
        return false;
      }
      HasKey o = (HasKey) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      HasKey o = (HasKey) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class HasLabel extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicateOrStringLiteralVarargs value;
    
    public HasLabel (hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicateOrStringLiteralVarargs value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof HasLabel)) {
        return false;
      }
      HasLabel o = (HasLabel) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      HasLabel o = (HasLabel) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class HasNot extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument value;
    
    public HasNot (hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof HasNot)) {
        return false;
      }
      HasNot o = (HasNot) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      HasNot o = (HasNot) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class HasValue extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicateOrGenericLiteralArgument value;
    
    public HasValue (hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicateOrGenericLiteralArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof HasValue)) {
        return false;
      }
      HasValue o = (HasValue) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      HasValue o = (HasValue) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Id extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public Id () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Id)) {
        return false;
      }
      Id o = (Id) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalMethod other) {
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
  
  public static final class Identity extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public Identity () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Identity)) {
        return false;
      }
      Identity o = (Identity) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalMethod other) {
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
  
  public static final class In extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> value;
    
    public In (java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof In)) {
        return false;
      }
      In o = (In) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      In o = (In) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class InE extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> value;
    
    public InE (java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InE)) {
        return false;
      }
      InE o = (InE) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      InE o = (InE) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Intersect extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument value;
    
    public Intersect (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Intersect)) {
        return false;
      }
      Intersect o = (Intersect) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Intersect o = (Intersect) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class InV extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public InV () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof InV)) {
        return false;
      }
      InV o = (InV) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalMethod other) {
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
  
  public static final class Index extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public Index () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Index)) {
        return false;
      }
      Index o = (Index) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalMethod other) {
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
  
  public static final class Inject extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument> value;
    
    public Inject (java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Inject)) {
        return false;
      }
      Inject o = (Inject) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Inject o = (Inject) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Is extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicateOrGenericLiteralArgument value;
    
    public Is (hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicateOrGenericLiteralArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Is)) {
        return false;
      }
      Is o = (Is) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Is o = (Is) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Key extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public Key () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Key)) {
        return false;
      }
      Key o = (Key) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalMethod other) {
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
  
  public static final class Label extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public Label () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Label)) {
        return false;
      }
      Label o = (Label) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalMethod other) {
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
  
  public static final class Limit extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndIntegerArgument value;
    
    public Limit (hydra.ext.org.apache.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndIntegerArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Limit)) {
        return false;
      }
      Limit o = (Limit) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Limit o = (Limit) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Local extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal value;
    
    public Local (hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Local)) {
        return false;
      }
      Local o = (Local) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Local o = (Local) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Loops extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.StringArgument> value;
    
    public Loops (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.StringArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Loops)) {
        return false;
      }
      Loops o = (Loops) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Loops o = (Loops) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Map extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal value;
    
    public Map (hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Map)) {
        return false;
      }
      Map o = (Map) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Map o = (Map) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Match extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal> value;
    
    public Match (java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Match)) {
        return false;
      }
      Match o = (Match) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Match o = (Match) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Math_ extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value;
    
    public Math_ (hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Math_)) {
        return false;
      }
      Math_ o = (Math_) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Math_ o = (Math_) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Max extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> value;
    
    public Max (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Max)) {
        return false;
      }
      Max o = (Max) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Max o = (Max) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Mean extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> value;
    
    public Mean (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Mean)) {
        return false;
      }
      Mean o = (Mean) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Mean o = (Mean) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Min extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> value;
    
    public Min (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Min)) {
        return false;
      }
      Min o = (Min) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Min o = (Min) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class None extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate value;
    
    public None (hydra.ext.org.apache.tinkerpop.gremlin.TraversalPredicate value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof None)) {
        return false;
      }
      None o = (None) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      None o = (None) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Not extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal value;
    
    public Not (hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal value) {
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
    public int compareTo(TraversalMethod other) {
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
  
  public static final class Option extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.OptionArgs value;
    
    public Option (hydra.ext.org.apache.tinkerpop.gremlin.OptionArgs value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Option)) {
        return false;
      }
      Option o = (Option) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Option o = (Option) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Optional extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal value;
    
    public Optional (hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Optional)) {
        return false;
      }
      Optional o = (Optional) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Optional o = (Optional) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Or extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal> value;
    
    public Or (java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal> value) {
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Or o = (Or) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Order extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> value;
    
    public Order (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Order)) {
        return false;
      }
      Order o = (Order) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Order o = (Order) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class OtherV extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public OtherV () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OtherV)) {
        return false;
      }
      OtherV o = (OtherV) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalMethod other) {
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
  
  public static final class Out extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> value;
    
    public Out (java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Out)) {
        return false;
      }
      Out o = (Out) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Out o = (Out) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class OutE extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> value;
    
    public OutE (java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OutE)) {
        return false;
      }
      OutE o = (OutE) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      OutE o = (OutE) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class OutV extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public OutV () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OutV)) {
        return false;
      }
      OutV o = (OutV) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalMethod other) {
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
  
  public static final class PageRank extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.FloatArgument> value;
    
    public PageRank (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.FloatArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PageRank)) {
        return false;
      }
      PageRank o = (PageRank) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PageRank o = (PageRank) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Path extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public Path () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Path)) {
        return false;
      }
      Path o = (Path) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalMethod other) {
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
  
  public static final class PeerPressure extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public PeerPressure () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PeerPressure)) {
        return false;
      }
      PeerPressure o = (PeerPressure) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalMethod other) {
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
  
  public static final class Profile extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.StringArgument> value;
    
    public Profile (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.StringArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Profile)) {
        return false;
      }
      Profile o = (Profile) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Profile o = (Profile) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Project extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.StringArgumentAndOptionalStringLiteralVarargs value;
    
    public Project (hydra.ext.org.apache.tinkerpop.gremlin.StringArgumentAndOptionalStringLiteralVarargs value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Project)) {
        return false;
      }
      Project o = (Project) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Project o = (Project) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Properties extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> value;
    
    public Properties (java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Properties)) {
        return false;
      }
      Properties o = (Properties) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Properties o = (Properties) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Property extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.PropertyArgs value;
    
    public Property (hydra.ext.org.apache.tinkerpop.gremlin.PropertyArgs value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Property)) {
        return false;
      }
      Property o = (Property) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Property o = (Property) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class PropertyMap extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> value;
    
    public PropertyMap (java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PropertyMap)) {
        return false;
      }
      PropertyMap o = (PropertyMap) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PropertyMap o = (PropertyMap) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Range extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.RangeArgs value;
    
    public Range (hydra.ext.org.apache.tinkerpop.gremlin.RangeArgs value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Range)) {
        return false;
      }
      Range o = (Range) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Range o = (Range) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Read extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public Read () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Read)) {
        return false;
      }
      Read o = (Read) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalMethod other) {
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
  
  public static final class Repeat extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.OptionalStringArgumentAndNestedTraversal value;
    
    public Repeat (hydra.ext.org.apache.tinkerpop.gremlin.OptionalStringArgumentAndNestedTraversal value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Repeat)) {
        return false;
      }
      Repeat o = (Repeat) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Repeat o = (Repeat) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Sack extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalBiFunctionArgument> value;
    
    public Sack (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalBiFunctionArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sack)) {
        return false;
      }
      Sack o = (Sack) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Sack o = (Sack) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Sample extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndIntegerArgument value;
    
    public Sample (hydra.ext.org.apache.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndIntegerArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sample)) {
        return false;
      }
      Sample o = (Sample) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Sample o = (Sample) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Select extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.SelectArgs value;
    
    public Select (hydra.ext.org.apache.tinkerpop.gremlin.SelectArgs value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Select)) {
        return false;
      }
      Select o = (Select) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Select o = (Select) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Combine extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument value;
    
    public Combine (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Combine)) {
        return false;
      }
      Combine o = (Combine) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Combine o = (Combine) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Product extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument value;
    
    public Product (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Product)) {
        return false;
      }
      Product o = (Product) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Product o = (Product) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Merge extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument value;
    
    public Merge (hydra.ext.org.apache.tinkerpop.gremlin.GenericLiteralArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Merge)) {
        return false;
      }
      Merge o = (Merge) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Merge o = (Merge) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ShortestPath extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public ShortestPath () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ShortestPath)) {
        return false;
      }
      ShortestPath o = (ShortestPath) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalMethod other) {
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
  
  public static final class SideEffect extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal value;
    
    public SideEffect (hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SideEffect)) {
        return false;
      }
      SideEffect o = (SideEffect) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SideEffect o = (SideEffect) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class SimplePath extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public SimplePath () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SimplePath)) {
        return false;
      }
      SimplePath o = (SimplePath) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalMethod other) {
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
  
  public static final class Skip extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndIntegerArgument value;
    
    public Skip (hydra.ext.org.apache.tinkerpop.gremlin.OptionalTraversalScopeArgumentAndIntegerArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Skip)) {
        return false;
      }
      Skip o = (Skip) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Skip o = (Skip) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Store extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value;
    
    public Store (hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Store)) {
        return false;
      }
      Store o = (Store) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Store o = (Store) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Subgraph extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value;
    
    public Subgraph (hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Subgraph)) {
        return false;
      }
      Subgraph o = (Subgraph) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Subgraph o = (Subgraph) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Sum extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> value;
    
    public Sum (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Sum)) {
        return false;
      }
      Sum o = (Sum) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Sum o = (Sum) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Tail extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TailArgs> value;
    
    public Tail (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TailArgs> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Tail)) {
        return false;
      }
      Tail o = (Tail) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Tail o = (Tail) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Fail extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.StringArgument> value;
    
    public Fail (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.StringArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Fail)) {
        return false;
      }
      Fail o = (Fail) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Fail o = (Fail) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Times extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.IntegerArgument value;
    
    public Times (hydra.ext.org.apache.tinkerpop.gremlin.IntegerArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Times)) {
        return false;
      }
      Times o = (Times) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Times o = (Times) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class To extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.ToArgs value;
    
    public To (hydra.ext.org.apache.tinkerpop.gremlin.ToArgs value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof To)) {
        return false;
      }
      To o = (To) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      To o = (To) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ToE extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.DirectionAndVarargs value;
    
    public ToE (hydra.ext.org.apache.tinkerpop.gremlin.DirectionAndVarargs value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ToE)) {
        return false;
      }
      ToE o = (ToE) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ToE o = (ToE) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ToV extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.TraversalDirectionArgument value;
    
    public ToV (hydra.ext.org.apache.tinkerpop.gremlin.TraversalDirectionArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ToV)) {
        return false;
      }
      ToV o = (ToV) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ToV o = (ToV) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Tree extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.StringArgument> value;
    
    public Tree (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.StringArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Tree)) {
        return false;
      }
      Tree o = (Tree) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Tree o = (Tree) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Unfold extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public Unfold () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Unfold)) {
        return false;
      }
      Unfold o = (Unfold) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalMethod other) {
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
  
  public static final class Union extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal> value;
    
    public Union (java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.NestedTraversal> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Union)) {
        return false;
      }
      Union o = (Union) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Union o = (Union) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Until extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.PredicateOrTraversal value;
    
    public Until (hydra.ext.org.apache.tinkerpop.gremlin.PredicateOrTraversal value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Until)) {
        return false;
      }
      Until o = (Until) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Until o = (Until) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Value extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public Value () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Value)) {
        return false;
      }
      Value o = (Value) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalMethod other) {
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
  
  public static final class ValueMap extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.ValueMapArgs value;
    
    public ValueMap (hydra.ext.org.apache.tinkerpop.gremlin.ValueMapArgs value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ValueMap)) {
        return false;
      }
      ValueMap o = (ValueMap) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ValueMap o = (ValueMap) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Values extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> value;
    
    public Values (java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Values)) {
        return false;
      }
      Values o = (Values) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Values o = (Values) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Where extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.WhereArgs value;
    
    public Where (hydra.ext.org.apache.tinkerpop.gremlin.WhereArgs value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Where)) {
        return false;
      }
      Where o = (Where) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Where o = (Where) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class With extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.WithArgs value;
    
    public With (hydra.ext.org.apache.tinkerpop.gremlin.WithArgs value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof With)) {
        return false;
      }
      With o = (With) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      With o = (With) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Write extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public Write () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Write)) {
        return false;
      }
      Write o = (Write) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalMethod other) {
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
  
  public static final class Element extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> value;
    
    public Element (java.util.List<hydra.ext.org.apache.tinkerpop.gremlin.StringNullableArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Element)) {
        return false;
      }
      Element o = (Element) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Element o = (Element) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Call extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.ServiceCall value;
    
    public Call (hydra.ext.org.apache.tinkerpop.gremlin.ServiceCall value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Call)) {
        return false;
      }
      Call o = (Call) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Call o = (Call) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Concat extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.ConcatArgs value;
    
    public Concat (hydra.ext.org.apache.tinkerpop.gremlin.ConcatArgs value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Concat)) {
        return false;
      }
      Concat o = (Concat) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Concat o = (Concat) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class AsString extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> value;
    
    public AsString (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AsString)) {
        return false;
      }
      AsString o = (AsString) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      AsString o = (AsString) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Format extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value;
    
    public Format (hydra.ext.org.apache.tinkerpop.gremlin.StringArgument value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Format)) {
        return false;
      }
      Format o = (Format) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Format o = (Format) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ToUpper extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> value;
    
    public ToUpper (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ToUpper)) {
        return false;
      }
      ToUpper o = (ToUpper) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ToUpper o = (ToUpper) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class ToLower extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> value;
    
    public ToLower (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ToLower)) {
        return false;
      }
      ToLower o = (ToLower) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ToLower o = (ToLower) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Length extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> value;
    
    public Length (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Length)) {
        return false;
      }
      Length o = (Length) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Length o = (Length) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Trim extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> value;
    
    public Trim (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Trim)) {
        return false;
      }
      Trim o = (Trim) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Trim o = (Trim) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class LTrim extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> value;
    
    public LTrim (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof LTrim)) {
        return false;
      }
      LTrim o = (LTrim) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      LTrim o = (LTrim) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class RTrim extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> value;
    
    public RTrim (hydra.util.Maybe<hydra.ext.org.apache.tinkerpop.gremlin.TraversalScopeArgument> value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof RTrim)) {
        return false;
      }
      RTrim o = (RTrim) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      RTrim o = (RTrim) other;
      return Integer.compare(
        value.hashCode(),
        o.value.hashCode());
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Reverse extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public Reverse () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Reverse)) {
        return false;
      }
      Reverse o = (Reverse) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalMethod other) {
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
  
  public static final class Replace extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.ReplaceArgs value;
    
    public Replace (hydra.ext.org.apache.tinkerpop.gremlin.ReplaceArgs value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Replace)) {
        return false;
      }
      Replace o = (Replace) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Replace o = (Replace) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Split extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.SplitArgs value;
    
    public Split (hydra.ext.org.apache.tinkerpop.gremlin.SplitArgs value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Split)) {
        return false;
      }
      Split o = (Split) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Split o = (Split) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class Substring extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.SubstringArgs value;
    
    public Substring (hydra.ext.org.apache.tinkerpop.gremlin.SubstringArgs value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Substring)) {
        return false;
      }
      Substring o = (Substring) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Substring o = (Substring) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class AsDate extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public AsDate () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof AsDate)) {
        return false;
      }
      AsDate o = (AsDate) other;
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(TraversalMethod other) {
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
  
  public static final class DateAdd extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.DateAddArgs value;
    
    public DateAdd (hydra.ext.org.apache.tinkerpop.gremlin.DateAddArgs value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DateAdd)) {
        return false;
      }
      DateAdd o = (DateAdd) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DateAdd o = (DateAdd) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  public static final class DateDiff extends hydra.ext.org.apache.tinkerpop.gremlin.TraversalMethod implements Serializable {
    public final hydra.ext.org.apache.tinkerpop.gremlin.DateDiffArgs value;
    
    public DateDiff (hydra.ext.org.apache.tinkerpop.gremlin.DateDiffArgs value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof DateDiff)) {
        return false;
      }
      DateDiff o = (DateDiff) other;
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
    public int compareTo(TraversalMethod other) {
      int tagCmp = (this).getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      DateDiff o = (DateDiff) other;
      return ((Comparable) value).compareTo(o.value);
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
