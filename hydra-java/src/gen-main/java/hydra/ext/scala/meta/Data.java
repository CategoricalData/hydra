package hydra.ext.scala.meta;

public abstract class Data {
  private Data () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Lit instance) ;
    
    R visit(Ref instance) ;
    
    R visit(Interpolate instance) ;
    
    R visit(Xml instance) ;
    
    R visit(Apply instance) ;
    
    R visit(ApplyUsing instance) ;
    
    R visit(ApplyType instance) ;
    
    R visit(Assign instance) ;
    
    R visit(Return instance) ;
    
    R visit(Throw instance) ;
    
    R visit(Ascribe instance) ;
    
    R visit(Annotate instance) ;
    
    R visit(Tuple instance) ;
    
    R visit(Block instance) ;
    
    R visit(EndMarker instance) ;
    
    R visit(If instance) ;
    
    R visit(QuotedMacroExpr instance) ;
    
    R visit(QuotedMacroType instance) ;
    
    R visit(SplicedMacroExpr instance) ;
    
    R visit(Match instance) ;
    
    R visit(Try instance) ;
    
    R visit(TryWithHandler instance) ;
    
    R visit(FunctionData instance) ;
    
    R visit(PolyFunction instance) ;
    
    R visit(PartialFunction instance) ;
    
    R visit(While instance) ;
    
    R visit(Do instance) ;
    
    R visit(For instance) ;
    
    R visit(ForYield instance) ;
    
    R visit(New instance) ;
    
    R visit(NewAnonymous instance) ;
    
    R visit(Placeholder instance) ;
    
    R visit(Eta instance) ;
    
    R visit(Repeated instance) ;
    
    R visit(Param instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(Data instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Lit instance) {
      return otherwise((instance));
    }
    
    default R visit(Ref instance) {
      return otherwise((instance));
    }
    
    default R visit(Interpolate instance) {
      return otherwise((instance));
    }
    
    default R visit(Xml instance) {
      return otherwise((instance));
    }
    
    default R visit(Apply instance) {
      return otherwise((instance));
    }
    
    default R visit(ApplyUsing instance) {
      return otherwise((instance));
    }
    
    default R visit(ApplyType instance) {
      return otherwise((instance));
    }
    
    default R visit(Assign instance) {
      return otherwise((instance));
    }
    
    default R visit(Return instance) {
      return otherwise((instance));
    }
    
    default R visit(Throw instance) {
      return otherwise((instance));
    }
    
    default R visit(Ascribe instance) {
      return otherwise((instance));
    }
    
    default R visit(Annotate instance) {
      return otherwise((instance));
    }
    
    default R visit(Tuple instance) {
      return otherwise((instance));
    }
    
    default R visit(Block instance) {
      return otherwise((instance));
    }
    
    default R visit(EndMarker instance) {
      return otherwise((instance));
    }
    
    default R visit(If instance) {
      return otherwise((instance));
    }
    
    default R visit(QuotedMacroExpr instance) {
      return otherwise((instance));
    }
    
    default R visit(QuotedMacroType instance) {
      return otherwise((instance));
    }
    
    default R visit(SplicedMacroExpr instance) {
      return otherwise((instance));
    }
    
    default R visit(Match instance) {
      return otherwise((instance));
    }
    
    default R visit(Try instance) {
      return otherwise((instance));
    }
    
    default R visit(TryWithHandler instance) {
      return otherwise((instance));
    }
    
    default R visit(FunctionData instance) {
      return otherwise((instance));
    }
    
    default R visit(PolyFunction instance) {
      return otherwise((instance));
    }
    
    default R visit(PartialFunction instance) {
      return otherwise((instance));
    }
    
    default R visit(While instance) {
      return otherwise((instance));
    }
    
    default R visit(Do instance) {
      return otherwise((instance));
    }
    
    default R visit(For instance) {
      return otherwise((instance));
    }
    
    default R visit(ForYield instance) {
      return otherwise((instance));
    }
    
    default R visit(New instance) {
      return otherwise((instance));
    }
    
    default R visit(NewAnonymous instance) {
      return otherwise((instance));
    }
    
    default R visit(Placeholder instance) {
      return otherwise((instance));
    }
    
    default R visit(Eta instance) {
      return otherwise((instance));
    }
    
    default R visit(Repeated instance) {
      return otherwise((instance));
    }
    
    default R visit(Param instance) {
      return otherwise((instance));
    }
  }
  
  public static final class Lit extends Data {
    public final Lit value;
    
    public Lit (Lit value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Lit)) {
        return false;
      }
      Lit o = (Lit) (other);
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
  
  public static final class Ref extends Data {
    public final Data_Ref value;
    
    public Ref (Data_Ref value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ref)) {
        return false;
      }
      Ref o = (Ref) (other);
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
  
  public static final class Interpolate extends Data {
    public final Data_Interpolate value;
    
    public Interpolate (Data_Interpolate value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Interpolate)) {
        return false;
      }
      Interpolate o = (Interpolate) (other);
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
  
  public static final class Xml extends Data {
    public final Data_Xml value;
    
    public Xml (Data_Xml value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Xml)) {
        return false;
      }
      Xml o = (Xml) (other);
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
  
  public static final class Apply extends Data {
    public final Data_Apply value;
    
    public Apply (Data_Apply value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Apply)) {
        return false;
      }
      Apply o = (Apply) (other);
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
  
  public static final class ApplyUsing extends Data {
    public final Data_ApplyUsing value;
    
    public ApplyUsing (Data_ApplyUsing value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ApplyUsing)) {
        return false;
      }
      ApplyUsing o = (ApplyUsing) (other);
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
  
  public static final class ApplyType extends Data {
    public final Data_ApplyType value;
    
    public ApplyType (Data_ApplyType value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ApplyType)) {
        return false;
      }
      ApplyType o = (ApplyType) (other);
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
  
  public static final class Assign extends Data {
    public final Data_Assign value;
    
    public Assign (Data_Assign value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Assign)) {
        return false;
      }
      Assign o = (Assign) (other);
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
  
  public static final class Return extends Data {
    public final Data_Return value;
    
    public Return (Data_Return value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Return)) {
        return false;
      }
      Return o = (Return) (other);
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
  
  public static final class Throw extends Data {
    public final Data_Throw value;
    
    public Throw (Data_Throw value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Throw)) {
        return false;
      }
      Throw o = (Throw) (other);
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
  
  public static final class Ascribe extends Data {
    public final Data_Ascribe value;
    
    public Ascribe (Data_Ascribe value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ascribe)) {
        return false;
      }
      Ascribe o = (Ascribe) (other);
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
  
  public static final class Annotate extends Data {
    public final Data_Annotate value;
    
    public Annotate (Data_Annotate value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Annotate)) {
        return false;
      }
      Annotate o = (Annotate) (other);
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
  
  public static final class Tuple extends Data {
    public final Data_Tuple value;
    
    public Tuple (Data_Tuple value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Tuple)) {
        return false;
      }
      Tuple o = (Tuple) (other);
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
  
  public static final class Block extends Data {
    public final Data_Block value;
    
    public Block (Data_Block value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Block)) {
        return false;
      }
      Block o = (Block) (other);
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
  
  public static final class EndMarker extends Data {
    public final Data_EndMarker value;
    
    public EndMarker (Data_EndMarker value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EndMarker)) {
        return false;
      }
      EndMarker o = (EndMarker) (other);
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
  
  public static final class If extends Data {
    public final Data_If value;
    
    public If (Data_If value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof If)) {
        return false;
      }
      If o = (If) (other);
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
  
  public static final class QuotedMacroExpr extends Data {
    public final Data_QuotedMacroExpr value;
    
    public QuotedMacroExpr (Data_QuotedMacroExpr value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof QuotedMacroExpr)) {
        return false;
      }
      QuotedMacroExpr o = (QuotedMacroExpr) (other);
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
  
  public static final class QuotedMacroType extends Data {
    public final Data_QuotedMacroType value;
    
    public QuotedMacroType (Data_QuotedMacroType value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof QuotedMacroType)) {
        return false;
      }
      QuotedMacroType o = (QuotedMacroType) (other);
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
  
  public static final class SplicedMacroExpr extends Data {
    public final Data_SplicedMacroExpr value;
    
    public SplicedMacroExpr (Data_SplicedMacroExpr value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SplicedMacroExpr)) {
        return false;
      }
      SplicedMacroExpr o = (SplicedMacroExpr) (other);
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
  
  public static final class Match extends Data {
    public final Data_Match value;
    
    public Match (Data_Match value) {
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
  
  public static final class Try extends Data {
    public final Data_Try value;
    
    public Try (Data_Try value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Try)) {
        return false;
      }
      Try o = (Try) (other);
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
  
  public static final class TryWithHandler extends Data {
    public final Data_TryWithHandler value;
    
    public TryWithHandler (Data_TryWithHandler value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TryWithHandler)) {
        return false;
      }
      TryWithHandler o = (TryWithHandler) (other);
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
  
  public static final class FunctionData extends Data {
    public final Data_FunctionData value;
    
    public FunctionData (Data_FunctionData value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FunctionData)) {
        return false;
      }
      FunctionData o = (FunctionData) (other);
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
  
  public static final class PolyFunction extends Data {
    public final Data_PolyFunction value;
    
    public PolyFunction (Data_PolyFunction value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PolyFunction)) {
        return false;
      }
      PolyFunction o = (PolyFunction) (other);
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
  
  public static final class PartialFunction extends Data {
    public final Data_PartialFunction value;
    
    public PartialFunction (Data_PartialFunction value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PartialFunction)) {
        return false;
      }
      PartialFunction o = (PartialFunction) (other);
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
  
  public static final class While extends Data {
    public final Data_While value;
    
    public While (Data_While value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof While)) {
        return false;
      }
      While o = (While) (other);
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
  
  public static final class Do extends Data {
    public final Data_Do value;
    
    public Do (Data_Do value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Do)) {
        return false;
      }
      Do o = (Do) (other);
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
  
  public static final class For extends Data {
    public final Data_For value;
    
    public For (Data_For value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof For)) {
        return false;
      }
      For o = (For) (other);
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
  
  public static final class ForYield extends Data {
    public final Data_ForYield value;
    
    public ForYield (Data_ForYield value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ForYield)) {
        return false;
      }
      ForYield o = (ForYield) (other);
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
  
  public static final class New extends Data {
    public final Data_New value;
    
    public New (Data_New value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof New)) {
        return false;
      }
      New o = (New) (other);
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
  
  public static final class NewAnonymous extends Data {
    public final Data_NewAnonymous value;
    
    public NewAnonymous (Data_NewAnonymous value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NewAnonymous)) {
        return false;
      }
      NewAnonymous o = (NewAnonymous) (other);
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
  
  public static final class Placeholder extends Data {
    public final Data_Placeholder value;
    
    public Placeholder (Data_Placeholder value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Placeholder)) {
        return false;
      }
      Placeholder o = (Placeholder) (other);
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
  
  public static final class Eta extends Data {
    public final Data_Eta value;
    
    public Eta (Data_Eta value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Eta)) {
        return false;
      }
      Eta o = (Eta) (other);
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
  
  public static final class Repeated extends Data {
    public final Data_Repeated value;
    
    public Repeated (Data_Repeated value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Repeated)) {
        return false;
      }
      Repeated o = (Repeated) (other);
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
  
  public static final class Param extends Data {
    public final Data_Param value;
    
    public Param (Data_Param value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Param)) {
        return false;
      }
      Param o = (Param) (other);
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