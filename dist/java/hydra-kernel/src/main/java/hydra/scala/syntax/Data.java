// Note: this is an automatically generated file. Do not edit.

package hydra.scala.syntax;

import java.io.Serializable;

public abstract class Data implements Serializable, Comparable<Data> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.scala.syntax.Data");

  public static final hydra.core.Name LIT = new hydra.core.Name("lit");

  public static final hydra.core.Name REF = new hydra.core.Name("ref");

  public static final hydra.core.Name INTERPOLATE = new hydra.core.Name("interpolate");

  public static final hydra.core.Name XML = new hydra.core.Name("xml");

  public static final hydra.core.Name APPLY = new hydra.core.Name("apply");

  public static final hydra.core.Name APPLY_USING = new hydra.core.Name("applyUsing");

  public static final hydra.core.Name APPLY_TYPE = new hydra.core.Name("applyType");

  public static final hydra.core.Name ASSIGN = new hydra.core.Name("assign");

  public static final hydra.core.Name RETURN = new hydra.core.Name("return");

  public static final hydra.core.Name THROW = new hydra.core.Name("throw");

  public static final hydra.core.Name ASCRIBE = new hydra.core.Name("ascribe");

  public static final hydra.core.Name ANNOTATE = new hydra.core.Name("annotate");

  public static final hydra.core.Name TUPLE = new hydra.core.Name("tuple");

  public static final hydra.core.Name BLOCK = new hydra.core.Name("block");

  public static final hydra.core.Name END_MARKER = new hydra.core.Name("endMarker");

  public static final hydra.core.Name IF = new hydra.core.Name("if");

  public static final hydra.core.Name QUOTED_MACRO_EXPR = new hydra.core.Name("quotedMacroExpr");

  public static final hydra.core.Name QUOTED_MACRO_TYPE = new hydra.core.Name("quotedMacroType");

  public static final hydra.core.Name SPLICED_MACRO_EXPR = new hydra.core.Name("splicedMacroExpr");

  public static final hydra.core.Name MATCH = new hydra.core.Name("match");

  public static final hydra.core.Name TRY = new hydra.core.Name("try");

  public static final hydra.core.Name TRY_WITH_HANDLER = new hydra.core.Name("tryWithHandler");

  public static final hydra.core.Name FUNCTION_DATA = new hydra.core.Name("functionData");

  public static final hydra.core.Name POLY_FUNCTION = new hydra.core.Name("polyFunction");

  public static final hydra.core.Name PARTIAL_FUNCTION = new hydra.core.Name("partialFunction");

  public static final hydra.core.Name WHILE = new hydra.core.Name("while");

  public static final hydra.core.Name DO = new hydra.core.Name("do");

  public static final hydra.core.Name FOR = new hydra.core.Name("for");

  public static final hydra.core.Name FOR_YIELD = new hydra.core.Name("forYield");

  public static final hydra.core.Name NEW = new hydra.core.Name("new");

  public static final hydra.core.Name NEW_ANONYMOUS = new hydra.core.Name("newAnonymous");

  public static final hydra.core.Name PLACEHOLDER = new hydra.core.Name("placeholder");

  public static final hydra.core.Name ETA = new hydra.core.Name("eta");

  public static final hydra.core.Name REPEATED = new hydra.core.Name("repeated");

  public static final hydra.core.Name PARAM = new hydra.core.Name("param");

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
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + instance);
    }

    default R visit(Lit instance) {
      return otherwise(instance);
    }

    default R visit(Ref instance) {
      return otherwise(instance);
    }

    default R visit(Interpolate instance) {
      return otherwise(instance);
    }

    default R visit(Xml instance) {
      return otherwise(instance);
    }

    default R visit(Apply instance) {
      return otherwise(instance);
    }

    default R visit(ApplyUsing instance) {
      return otherwise(instance);
    }

    default R visit(ApplyType instance) {
      return otherwise(instance);
    }

    default R visit(Assign instance) {
      return otherwise(instance);
    }

    default R visit(Return instance) {
      return otherwise(instance);
    }

    default R visit(Throw instance) {
      return otherwise(instance);
    }

    default R visit(Ascribe instance) {
      return otherwise(instance);
    }

    default R visit(Annotate instance) {
      return otherwise(instance);
    }

    default R visit(Tuple instance) {
      return otherwise(instance);
    }

    default R visit(Block instance) {
      return otherwise(instance);
    }

    default R visit(EndMarker instance) {
      return otherwise(instance);
    }

    default R visit(If instance) {
      return otherwise(instance);
    }

    default R visit(QuotedMacroExpr instance) {
      return otherwise(instance);
    }

    default R visit(QuotedMacroType instance) {
      return otherwise(instance);
    }

    default R visit(SplicedMacroExpr instance) {
      return otherwise(instance);
    }

    default R visit(Match instance) {
      return otherwise(instance);
    }

    default R visit(Try instance) {
      return otherwise(instance);
    }

    default R visit(TryWithHandler instance) {
      return otherwise(instance);
    }

    default R visit(FunctionData instance) {
      return otherwise(instance);
    }

    default R visit(PolyFunction instance) {
      return otherwise(instance);
    }

    default R visit(PartialFunction instance) {
      return otherwise(instance);
    }

    default R visit(While instance) {
      return otherwise(instance);
    }

    default R visit(Do instance) {
      return otherwise(instance);
    }

    default R visit(For instance) {
      return otherwise(instance);
    }

    default R visit(ForYield instance) {
      return otherwise(instance);
    }

    default R visit(New instance) {
      return otherwise(instance);
    }

    default R visit(NewAnonymous instance) {
      return otherwise(instance);
    }

    default R visit(Placeholder instance) {
      return otherwise(instance);
    }

    default R visit(Eta instance) {
      return otherwise(instance);
    }

    default R visit(Repeated instance) {
      return otherwise(instance);
    }

    default R visit(Param instance) {
      return otherwise(instance);
    }
  }

  public static final class Lit extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Lit value;

    public Lit (hydra.scala.syntax.Lit value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Lit)) {
        return false;
      }
      Lit o = (Lit) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Lit o = (Lit) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Ref extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_Ref value;

    public Ref (hydra.scala.syntax.Data_Ref value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ref)) {
        return false;
      }
      Ref o = (Ref) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Ref o = (Ref) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Interpolate extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_Interpolate value;

    public Interpolate (hydra.scala.syntax.Data_Interpolate value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Interpolate)) {
        return false;
      }
      Interpolate o = (Interpolate) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Interpolate o = (Interpolate) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Xml extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_Xml value;

    public Xml (hydra.scala.syntax.Data_Xml value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Xml)) {
        return false;
      }
      Xml o = (Xml) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Xml o = (Xml) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Apply extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_Apply value;

    public Apply (hydra.scala.syntax.Data_Apply value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Apply)) {
        return false;
      }
      Apply o = (Apply) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Apply o = (Apply) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ApplyUsing extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_ApplyUsing value;

    public ApplyUsing (hydra.scala.syntax.Data_ApplyUsing value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ApplyUsing)) {
        return false;
      }
      ApplyUsing o = (ApplyUsing) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ApplyUsing o = (ApplyUsing) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ApplyType extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_ApplyType value;

    public ApplyType (hydra.scala.syntax.Data_ApplyType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ApplyType)) {
        return false;
      }
      ApplyType o = (ApplyType) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ApplyType o = (ApplyType) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Assign extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_Assign value;

    public Assign (hydra.scala.syntax.Data_Assign value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Assign)) {
        return false;
      }
      Assign o = (Assign) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Assign o = (Assign) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Return extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_Return value;

    public Return (hydra.scala.syntax.Data_Return value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Return)) {
        return false;
      }
      Return o = (Return) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Return o = (Return) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Throw extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_Throw value;

    public Throw (hydra.scala.syntax.Data_Throw value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Throw)) {
        return false;
      }
      Throw o = (Throw) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Throw o = (Throw) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Ascribe extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_Ascribe value;

    public Ascribe (hydra.scala.syntax.Data_Ascribe value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Ascribe)) {
        return false;
      }
      Ascribe o = (Ascribe) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Ascribe o = (Ascribe) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Annotate extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_Annotate value;

    public Annotate (hydra.scala.syntax.Data_Annotate value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Annotate)) {
        return false;
      }
      Annotate o = (Annotate) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Annotate o = (Annotate) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Tuple extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_Tuple value;

    public Tuple (hydra.scala.syntax.Data_Tuple value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Tuple)) {
        return false;
      }
      Tuple o = (Tuple) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Tuple o = (Tuple) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Block extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_Block value;

    public Block (hydra.scala.syntax.Data_Block value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Block)) {
        return false;
      }
      Block o = (Block) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Block o = (Block) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class EndMarker extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_EndMarker value;

    public EndMarker (hydra.scala.syntax.Data_EndMarker value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof EndMarker)) {
        return false;
      }
      EndMarker o = (EndMarker) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      EndMarker o = (EndMarker) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class If extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_If value;

    public If (hydra.scala.syntax.Data_If value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof If)) {
        return false;
      }
      If o = (If) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      If o = (If) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class QuotedMacroExpr extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_QuotedMacroExpr value;

    public QuotedMacroExpr (hydra.scala.syntax.Data_QuotedMacroExpr value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof QuotedMacroExpr)) {
        return false;
      }
      QuotedMacroExpr o = (QuotedMacroExpr) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      QuotedMacroExpr o = (QuotedMacroExpr) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class QuotedMacroType extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_QuotedMacroType value;

    public QuotedMacroType (hydra.scala.syntax.Data_QuotedMacroType value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof QuotedMacroType)) {
        return false;
      }
      QuotedMacroType o = (QuotedMacroType) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      QuotedMacroType o = (QuotedMacroType) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class SplicedMacroExpr extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_SplicedMacroExpr value;

    public SplicedMacroExpr (hydra.scala.syntax.Data_SplicedMacroExpr value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SplicedMacroExpr)) {
        return false;
      }
      SplicedMacroExpr o = (SplicedMacroExpr) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      SplicedMacroExpr o = (SplicedMacroExpr) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Match extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_Match value;

    public Match (hydra.scala.syntax.Data_Match value) {
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Match o = (Match) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Try extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_Try value;

    public Try (hydra.scala.syntax.Data_Try value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Try)) {
        return false;
      }
      Try o = (Try) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Try o = (Try) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class TryWithHandler extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_TryWithHandler value;

    public TryWithHandler (hydra.scala.syntax.Data_TryWithHandler value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof TryWithHandler)) {
        return false;
      }
      TryWithHandler o = (TryWithHandler) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      TryWithHandler o = (TryWithHandler) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class FunctionData extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_FunctionData value;

    public FunctionData (hydra.scala.syntax.Data_FunctionData value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FunctionData)) {
        return false;
      }
      FunctionData o = (FunctionData) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      FunctionData o = (FunctionData) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class PolyFunction extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_PolyFunction value;

    public PolyFunction (hydra.scala.syntax.Data_PolyFunction value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PolyFunction)) {
        return false;
      }
      PolyFunction o = (PolyFunction) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PolyFunction o = (PolyFunction) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class PartialFunction extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_PartialFunction value;

    public PartialFunction (hydra.scala.syntax.Data_PartialFunction value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof PartialFunction)) {
        return false;
      }
      PartialFunction o = (PartialFunction) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      PartialFunction o = (PartialFunction) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class While extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_While value;

    public While (hydra.scala.syntax.Data_While value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof While)) {
        return false;
      }
      While o = (While) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      While o = (While) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Do extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_Do value;

    public Do (hydra.scala.syntax.Data_Do value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Do)) {
        return false;
      }
      Do o = (Do) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Do o = (Do) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class For extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_For value;

    public For (hydra.scala.syntax.Data_For value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof For)) {
        return false;
      }
      For o = (For) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      For o = (For) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class ForYield extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_ForYield value;

    public ForYield (hydra.scala.syntax.Data_ForYield value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ForYield)) {
        return false;
      }
      ForYield o = (ForYield) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      ForYield o = (ForYield) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class New extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_New value;

    public New (hydra.scala.syntax.Data_New value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof New)) {
        return false;
      }
      New o = (New) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      New o = (New) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class NewAnonymous extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_NewAnonymous value;

    public NewAnonymous (hydra.scala.syntax.Data_NewAnonymous value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NewAnonymous)) {
        return false;
      }
      NewAnonymous o = (NewAnonymous) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      NewAnonymous o = (NewAnonymous) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Placeholder extends hydra.scala.syntax.Data implements Serializable {
    public Placeholder () {

    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Placeholder)) {
        return false;
      }
      Placeholder o = (Placeholder) other;
      return true;
    }

    @Override
    public int hashCode() {
      return 0;
    }

    @Override
    @SuppressWarnings("unchecked")
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
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

  public static final class Eta extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_Eta value;

    public Eta (hydra.scala.syntax.Data_Eta value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Eta)) {
        return false;
      }
      Eta o = (Eta) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Eta o = (Eta) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Repeated extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_Repeated value;

    public Repeated (hydra.scala.syntax.Data_Repeated value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Repeated)) {
        return false;
      }
      Repeated o = (Repeated) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Repeated o = (Repeated) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }

  public static final class Param extends hydra.scala.syntax.Data implements Serializable {
    public final hydra.scala.syntax.Data_Param value;

    public Param (hydra.scala.syntax.Data_Param value) {
      this.value = value;
    }

    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Param)) {
        return false;
      }
      Param o = (Param) other;
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
    public int compareTo(Data other) {
      int tagCmp = this.getClass().getName().compareTo(other.getClass().getName());
      if (tagCmp != 0) {
        return tagCmp;
      }
      Param o = (Param) other;
      return hydra.util.Comparing.compare(
        value,
        o.value);
    }

    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}
