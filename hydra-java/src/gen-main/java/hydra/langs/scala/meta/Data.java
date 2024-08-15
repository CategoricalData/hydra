// Note: this is an automatically generated file. Do not edit.

package hydra.langs.scala.meta;

import java.io.Serializable;

public abstract class Data implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/langs/scala/meta.Data");
  
  public static final hydra.core.Name FIELD_NAME_LIT = new hydra.core.Name("lit");
  
  public static final hydra.core.Name FIELD_NAME_REF = new hydra.core.Name("ref");
  
  public static final hydra.core.Name FIELD_NAME_INTERPOLATE = new hydra.core.Name("interpolate");
  
  public static final hydra.core.Name FIELD_NAME_XML = new hydra.core.Name("xml");
  
  public static final hydra.core.Name FIELD_NAME_APPLY = new hydra.core.Name("apply");
  
  public static final hydra.core.Name FIELD_NAME_APPLY_USING = new hydra.core.Name("applyUsing");
  
  public static final hydra.core.Name FIELD_NAME_APPLY_TYPE = new hydra.core.Name("applyType");
  
  public static final hydra.core.Name FIELD_NAME_ASSIGN = new hydra.core.Name("assign");
  
  public static final hydra.core.Name FIELD_NAME_RETURN = new hydra.core.Name("return");
  
  public static final hydra.core.Name FIELD_NAME_THROW = new hydra.core.Name("throw");
  
  public static final hydra.core.Name FIELD_NAME_ASCRIBE = new hydra.core.Name("ascribe");
  
  public static final hydra.core.Name FIELD_NAME_ANNOTATE = new hydra.core.Name("annotate");
  
  public static final hydra.core.Name FIELD_NAME_TUPLE = new hydra.core.Name("tuple");
  
  public static final hydra.core.Name FIELD_NAME_BLOCK = new hydra.core.Name("block");
  
  public static final hydra.core.Name FIELD_NAME_END_MARKER = new hydra.core.Name("endMarker");
  
  public static final hydra.core.Name FIELD_NAME_IF = new hydra.core.Name("if");
  
  public static final hydra.core.Name FIELD_NAME_QUOTED_MACRO_EXPR = new hydra.core.Name("quotedMacroExpr");
  
  public static final hydra.core.Name FIELD_NAME_QUOTED_MACRO_TYPE = new hydra.core.Name("quotedMacroType");
  
  public static final hydra.core.Name FIELD_NAME_SPLICED_MACRO_EXPR = new hydra.core.Name("splicedMacroExpr");
  
  public static final hydra.core.Name FIELD_NAME_MATCH = new hydra.core.Name("match");
  
  public static final hydra.core.Name FIELD_NAME_TRY = new hydra.core.Name("try");
  
  public static final hydra.core.Name FIELD_NAME_TRY_WITH_HANDLER = new hydra.core.Name("tryWithHandler");
  
  public static final hydra.core.Name FIELD_NAME_FUNCTION_DATA = new hydra.core.Name("functionData");
  
  public static final hydra.core.Name FIELD_NAME_POLY_FUNCTION = new hydra.core.Name("polyFunction");
  
  public static final hydra.core.Name FIELD_NAME_PARTIAL_FUNCTION = new hydra.core.Name("partialFunction");
  
  public static final hydra.core.Name FIELD_NAME_WHILE = new hydra.core.Name("while");
  
  public static final hydra.core.Name FIELD_NAME_DO = new hydra.core.Name("do");
  
  public static final hydra.core.Name FIELD_NAME_FOR = new hydra.core.Name("for");
  
  public static final hydra.core.Name FIELD_NAME_FOR_YIELD = new hydra.core.Name("forYield");
  
  public static final hydra.core.Name FIELD_NAME_NEW = new hydra.core.Name("new");
  
  public static final hydra.core.Name FIELD_NAME_NEW_ANONYMOUS = new hydra.core.Name("newAnonymous");
  
  public static final hydra.core.Name FIELD_NAME_PLACEHOLDER = new hydra.core.Name("placeholder");
  
  public static final hydra.core.Name FIELD_NAME_ETA = new hydra.core.Name("eta");
  
  public static final hydra.core.Name FIELD_NAME_REPEATED = new hydra.core.Name("repeated");
  
  public static final hydra.core.Name FIELD_NAME_PARAM = new hydra.core.Name("param");
  
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
  
  public static final class Lit extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Lit value;
    
    public Lit (hydra.langs.scala.meta.Lit value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Ref extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_Ref value;
    
    public Ref (hydra.langs.scala.meta.Data_Ref value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Interpolate extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_Interpolate value;
    
    public Interpolate (hydra.langs.scala.meta.Data_Interpolate value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Xml extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_Xml value;
    
    public Xml (hydra.langs.scala.meta.Data_Xml value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Apply extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_Apply value;
    
    public Apply (hydra.langs.scala.meta.Data_Apply value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class ApplyUsing extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_ApplyUsing value;
    
    public ApplyUsing (hydra.langs.scala.meta.Data_ApplyUsing value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class ApplyType extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_ApplyType value;
    
    public ApplyType (hydra.langs.scala.meta.Data_ApplyType value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Assign extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_Assign value;
    
    public Assign (hydra.langs.scala.meta.Data_Assign value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Return extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_Return value;
    
    public Return (hydra.langs.scala.meta.Data_Return value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Throw extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_Throw value;
    
    public Throw (hydra.langs.scala.meta.Data_Throw value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Ascribe extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_Ascribe value;
    
    public Ascribe (hydra.langs.scala.meta.Data_Ascribe value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Annotate extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_Annotate value;
    
    public Annotate (hydra.langs.scala.meta.Data_Annotate value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Tuple extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_Tuple value;
    
    public Tuple (hydra.langs.scala.meta.Data_Tuple value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Block extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_Block value;
    
    public Block (hydra.langs.scala.meta.Data_Block value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class EndMarker extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_EndMarker value;
    
    public EndMarker (hydra.langs.scala.meta.Data_EndMarker value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class If extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_If value;
    
    public If (hydra.langs.scala.meta.Data_If value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class QuotedMacroExpr extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_QuotedMacroExpr value;
    
    public QuotedMacroExpr (hydra.langs.scala.meta.Data_QuotedMacroExpr value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class QuotedMacroType extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_QuotedMacroType value;
    
    public QuotedMacroType (hydra.langs.scala.meta.Data_QuotedMacroType value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class SplicedMacroExpr extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_SplicedMacroExpr value;
    
    public SplicedMacroExpr (hydra.langs.scala.meta.Data_SplicedMacroExpr value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Match extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_Match value;
    
    public Match (hydra.langs.scala.meta.Data_Match value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Try extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_Try value;
    
    public Try (hydra.langs.scala.meta.Data_Try value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class TryWithHandler extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_TryWithHandler value;
    
    public TryWithHandler (hydra.langs.scala.meta.Data_TryWithHandler value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class FunctionData extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_FunctionData value;
    
    public FunctionData (hydra.langs.scala.meta.Data_FunctionData value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class PolyFunction extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_PolyFunction value;
    
    public PolyFunction (hydra.langs.scala.meta.Data_PolyFunction value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class PartialFunction extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_PartialFunction value;
    
    public PartialFunction (hydra.langs.scala.meta.Data_PartialFunction value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class While extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_While value;
    
    public While (hydra.langs.scala.meta.Data_While value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Do extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_Do value;
    
    public Do (hydra.langs.scala.meta.Data_Do value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class For extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_For value;
    
    public For (hydra.langs.scala.meta.Data_For value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class ForYield extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_ForYield value;
    
    public ForYield (hydra.langs.scala.meta.Data_ForYield value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class New extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_New value;
    
    public New (hydra.langs.scala.meta.Data_New value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class NewAnonymous extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_NewAnonymous value;
    
    public NewAnonymous (hydra.langs.scala.meta.Data_NewAnonymous value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Placeholder extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_Placeholder value;
    
    public Placeholder (hydra.langs.scala.meta.Data_Placeholder value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Eta extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_Eta value;
    
    public Eta (hydra.langs.scala.meta.Data_Eta value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Repeated extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_Repeated value;
    
    public Repeated (hydra.langs.scala.meta.Data_Repeated value) {
      java.util.Objects.requireNonNull((value));
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
  
  public static final class Param extends hydra.langs.scala.meta.Data implements Serializable {
    public final hydra.langs.scala.meta.Data_Param value;
    
    public Param (hydra.langs.scala.meta.Data_Param value) {
      java.util.Objects.requireNonNull((value));
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