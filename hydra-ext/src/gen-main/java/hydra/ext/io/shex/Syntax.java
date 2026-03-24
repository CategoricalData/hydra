// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex;

/**
 * A Shex model. Based on the BNF at:
 *   https://github.com/shexSpec/grammar/blob/master/bnf
 */
public interface Syntax {
  static hydra.core.Type shexDoc() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("listOfDirective"), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Directive")))),
      new hydra.core.FieldType(new hydra.core.Name("Sequence"), new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.ShexDoc_Sequence_Option")))),
      new hydra.core.FieldType(new hydra.core.Name("PrefixDecl"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.PrefixDecl")))));
  }

  static hydra.core.Type shexDoc_Sequence_Option() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("alts"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.ShexDoc_Sequence_Option_Alts"))),
      new hydra.core.FieldType(new hydra.core.Name("listOfStatement"), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Statement"))))));
  }

  static hydra.core.Type shexDoc_Sequence_Option_Alts() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("NotStartAction"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.NotStartAction"))),
      new hydra.core.FieldType(new hydra.core.Name("StartActions"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.StartActions")))));
  }

  static hydra.core.Type directive() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("BaseDecl"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.BaseDecl"))),
      new hydra.core.FieldType(new hydra.core.Name("PrefixDecl"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.PrefixDecl")))));
  }

  static hydra.core.Type baseDecl() {
    return new hydra.core.Type.Wrap(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.IriRef")));
  }

  static hydra.core.Type prefixDecl() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("PnameNs"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.PnameNs"))),
      new hydra.core.FieldType(new hydra.core.Name("IriRef"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.IriRef")))));
  }

  static hydra.core.Type notStartAction() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("start"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.ShapeExpression"))),
      new hydra.core.FieldType(new hydra.core.Name("shapeExprDecl"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.NotStartAction_ShapeExprDecl")))));
  }

  static hydra.core.Type notStartAction_ShapeExprDecl() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("ShapeExprLabel"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.ShapeExprLabel"))),
      new hydra.core.FieldType(new hydra.core.Name("alts"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.NotStartAction_ShapeExprDecl_Alts")))));
  }

  static hydra.core.Type notStartAction_ShapeExprDecl_Alts() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("ShapeExpression"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.ShapeExpression"))),
      new hydra.core.FieldType(new hydra.core.Name("EXTERNAL"), new hydra.core.Type.Unit())));
  }

  static hydra.core.Type startActions() {
    return new hydra.core.Type.Wrap(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.CodeDecl"))));
  }

  static hydra.core.Type statement() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("Directive"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Directive"))),
      new hydra.core.FieldType(new hydra.core.Name("NotStartAction"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.NotStartAction")))));
  }

  static hydra.core.Type shapeExpression() {
    return new hydra.core.Type.Wrap(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.ShapeOr")));
  }

  static hydra.core.Type inlineShapeExpression() {
    return new hydra.core.Type.Wrap(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.InlineShapeOr")));
  }

  static hydra.core.Type shapeOr() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("ShapeAnd"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.ShapeAnd"))),
      new hydra.core.FieldType(new hydra.core.Name("listOfSequence"), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.ShapeAnd"))))));
  }

  static hydra.core.Type inlineShapeOr() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("ShapeAnd"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.ShapeAnd"))),
      new hydra.core.FieldType(new hydra.core.Name("listOfSequence"), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.InlineShapeAnd"))))));
  }

  static hydra.core.Type shapeAnd() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("ShapeNot"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.ShapeNot"))),
      new hydra.core.FieldType(new hydra.core.Name("listOfSequence"), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.ShapeNot"))))));
  }

  static hydra.core.Type inlineShapeAnd() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("InlineShapeNot"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.InlineShapeNot"))),
      new hydra.core.FieldType(new hydra.core.Name("listOfSequence"), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.InlineShapeNot"))))));
  }

  static hydra.core.Type shapeNot() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("NOT"), new hydra.core.Type.Maybe(new hydra.core.Type.Unit())),
      new hydra.core.FieldType(new hydra.core.Name("ShapeAtom"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.ShapeAtom")))));
  }

  static hydra.core.Type inlineShapeNot() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("NOT"), new hydra.core.Type.Maybe(new hydra.core.Type.Unit())),
      new hydra.core.FieldType(new hydra.core.Name("InlineShapeAtom"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.InlineShapeAtom")))));
  }

  static hydra.core.Type shapeAtom() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("sequence"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.ShapeAtom_Sequence"))),
      new hydra.core.FieldType(new hydra.core.Name("ShapeOrRef"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.ShapeOrRef"))),
      new hydra.core.FieldType(new hydra.core.Name("sequence2"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.ShapeExpression"))),
      new hydra.core.FieldType(new hydra.core.Name("Period"), new hydra.core.Type.Unit())));
  }

  static hydra.core.Type shapeAtom_Sequence() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("NodeConstraint"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.NodeConstraint"))),
      new hydra.core.FieldType(new hydra.core.Name("ShapeOrRef"), new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.ShapeOrRef"))))));
  }

  static hydra.core.Type inlineShapeAtom() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("sequence"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.InlineShapeAtom_Sequence"))),
      new hydra.core.FieldType(new hydra.core.Name("sequence2"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.InlineShapeAtom_Sequence2"))),
      new hydra.core.FieldType(new hydra.core.Name("sequence3"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.ShapeExpression"))),
      new hydra.core.FieldType(new hydra.core.Name("Period"), new hydra.core.Type.Unit())));
  }

  static hydra.core.Type inlineShapeAtom_Sequence() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("NodeConstraint"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.NodeConstraint"))),
      new hydra.core.FieldType(new hydra.core.Name("InlineShapeOrRef"), new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.InlineShapeOrRef"))))));
  }

  static hydra.core.Type inlineShapeAtom_Sequence2() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("InlineShapeOrRef"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.InlineShapeOrRef"))),
      new hydra.core.FieldType(new hydra.core.Name("NodeConstraint"), new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.NodeConstraint"))))));
  }

  static hydra.core.Type shapeOrRef() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("ShapeDefinition"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.ShapeDefinition"))),
      new hydra.core.FieldType(new hydra.core.Name("AtpNameLn"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.AtpNameLn"))),
      new hydra.core.FieldType(new hydra.core.Name("AtpNameNs"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.AtpNameNs"))),
      new hydra.core.FieldType(new hydra.core.Name("sequence"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.ShapeExprLabel")))));
  }

  static hydra.core.Type inlineShapeOrRef() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("InlineShapeDefinition"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.InlineShapeDefinition"))),
      new hydra.core.FieldType(new hydra.core.Name("AtpNameLn"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.AtpNameLn"))),
      new hydra.core.FieldType(new hydra.core.Name("AtpNameNs"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.AtpNameNs"))),
      new hydra.core.FieldType(new hydra.core.Name("sequence"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.ShapeExprLabel")))));
  }

  static hydra.core.Type nodeConstraint() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("sequence"), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.XsFacet")))),
      new hydra.core.FieldType(new hydra.core.Name("sequence2"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.NodeConstraint_Sequence2"))),
      new hydra.core.FieldType(new hydra.core.Name("sequence3"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.NodeConstraint_Sequence3"))),
      new hydra.core.FieldType(new hydra.core.Name("sequence4"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.NodeConstraint_Sequence4"))),
      new hydra.core.FieldType(new hydra.core.Name("sequence5"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.NodeConstraint_Sequence5"))),
      new hydra.core.FieldType(new hydra.core.Name("listOfXsFacet"), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.XsFacet"))))));
  }

  static hydra.core.Type nodeConstraint_Sequence2() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("NonLiteralKind"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.NonLiteralKind"))),
      new hydra.core.FieldType(new hydra.core.Name("listOfStringFacet"), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.StringFacet"))))));
  }

  static hydra.core.Type nodeConstraint_Sequence3() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("Datatype"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Datatype"))),
      new hydra.core.FieldType(new hydra.core.Name("listOfXsFacet"), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.XsFacet"))))));
  }

  static hydra.core.Type nodeConstraint_Sequence4() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("ValueSet"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.ValueSet"))),
      new hydra.core.FieldType(new hydra.core.Name("listOfXsFacet"), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.XsFacet"))))));
  }

  static hydra.core.Type nodeConstraint_Sequence5() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("ValueSet"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.ValueSet"))),
      new hydra.core.FieldType(new hydra.core.Name("listOfXsFacet"), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.XsFacet"))))));
  }

  static hydra.core.Type nonLiteralKind() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("IRI"), new hydra.core.Type.Unit()),
      new hydra.core.FieldType(new hydra.core.Name("BNODE"), new hydra.core.Type.Unit()),
      new hydra.core.FieldType(new hydra.core.Name("NONLITERAL"), new hydra.core.Type.Unit())));
  }

  static hydra.core.Type xsFacet() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("StringFacet"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.StringFacet"))),
      new hydra.core.FieldType(new hydra.core.Name("NumericFacet"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.NumericFacet")))));
  }

  static hydra.core.Type stringFacet() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("sequence"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.StringFacet_Sequence"))),
      new hydra.core.FieldType(new hydra.core.Name("Regexp"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Regexp")))));
  }

  static hydra.core.Type stringFacet_Sequence() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("StringLength"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.StringLength"))),
      new hydra.core.FieldType(new hydra.core.Name("Integer"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Integer")))));
  }

  static hydra.core.Type stringLength() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("LENGTH"), new hydra.core.Type.Unit()),
      new hydra.core.FieldType(new hydra.core.Name("MINLENGTH"), new hydra.core.Type.Unit()),
      new hydra.core.FieldType(new hydra.core.Name("MAXLENGTH"), new hydra.core.Type.Unit())));
  }

  static hydra.core.Type numericFacet() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("sequence"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.NumericFacet_Sequence"))),
      new hydra.core.FieldType(new hydra.core.Name("sequence2"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.NumericFacet_Sequence2")))));
  }

  static hydra.core.Type numericFacet_Sequence() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("NumericRange"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.NumericRange"))),
      new hydra.core.FieldType(new hydra.core.Name("NumericLiteral"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.NumericLiteral")))));
  }

  static hydra.core.Type numericFacet_Sequence2() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("NumericLength"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.NumericLength"))),
      new hydra.core.FieldType(new hydra.core.Name("Integer"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Integer")))));
  }

  static hydra.core.Type numericRange() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("MININCLUSIVE"), new hydra.core.Type.Unit()),
      new hydra.core.FieldType(new hydra.core.Name("MINEXCLUSIVE"), new hydra.core.Type.Unit()),
      new hydra.core.FieldType(new hydra.core.Name("MAXINCLUSIVE"), new hydra.core.Type.Unit()),
      new hydra.core.FieldType(new hydra.core.Name("MAXEXCLUSIVE"), new hydra.core.Type.Unit())));
  }

  static hydra.core.Type numericLength() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("TOTALDIGITS"), new hydra.core.Type.Unit()),
      new hydra.core.FieldType(new hydra.core.Name("FRACTIONDIGITS"), new hydra.core.Type.Unit())));
  }

  static hydra.core.Type shapeDefinition() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("listOfAlts"), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.ShapeDefinition_ListOfAlts_Elmt")))),
      new hydra.core.FieldType(new hydra.core.Name("TripleExpression"), new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.TripleExpression")))),
      new hydra.core.FieldType(new hydra.core.Name("listOfAnnotation"), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Annotation")))),
      new hydra.core.FieldType(new hydra.core.Name("SemanticActions"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.SemanticActions")))));
  }

  static hydra.core.Type shapeDefinition_ListOfAlts_Elmt() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("IncludeSet"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.IncludeSet"))),
      new hydra.core.FieldType(new hydra.core.Name("ExtraPropertySet"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.ExtraPropertySet"))),
      new hydra.core.FieldType(new hydra.core.Name("CLOSED"), new hydra.core.Type.Unit())));
  }

  static hydra.core.Type inlineShapeDefinition() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("listOfAlts"), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.InlineShapeDefinition_ListOfAlts_Elmt")))),
      new hydra.core.FieldType(new hydra.core.Name("TripleExpression"), new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.TripleExpression"))))));
  }

  static hydra.core.Type inlineShapeDefinition_ListOfAlts_Elmt() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("IncludeSet"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.IncludeSet"))),
      new hydra.core.FieldType(new hydra.core.Name("ExtraPropertySet"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.ExtraPropertySet"))),
      new hydra.core.FieldType(new hydra.core.Name("CLOSED"), new hydra.core.Type.Unit())));
  }

  static hydra.core.Type extraPropertySet() {
    return new hydra.core.Type.Wrap(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Predicate"))));
  }

  static hydra.core.Type tripleExpression() {
    return new hydra.core.Type.Wrap(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.OneOfTripleExpr")));
  }

  static hydra.core.Type oneOfTripleExpr() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("GroupTripleExpr"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.GroupTripleExpr"))),
      new hydra.core.FieldType(new hydra.core.Name("MultiElementOneOf"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.MultiElementOneOf")))));
  }

  static hydra.core.Type multiElementOneOf() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("GroupTripleExpr"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.GroupTripleExpr"))),
      new hydra.core.FieldType(new hydra.core.Name("listOfSequence"), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.GroupTripleExpr"))))));
  }

  static hydra.core.Type innerTripleExpr() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("MultiElementGroup"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.MultiElementGroup"))),
      new hydra.core.FieldType(new hydra.core.Name("MultiElementOneOf"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.MultiElementOneOf")))));
  }

  static hydra.core.Type groupTripleExpr() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("SingleElementGroup"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.SingleElementGroup"))),
      new hydra.core.FieldType(new hydra.core.Name("MultiElementGroup"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.MultiElementGroup")))));
  }

  static hydra.core.Type singleElementGroup() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("UnaryTripleExpr"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.UnaryTripleExpr"))),
      new hydra.core.FieldType(new hydra.core.Name("Semi"), new hydra.core.Type.Maybe(new hydra.core.Type.Unit()))));
  }

  static hydra.core.Type multiElementGroup() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("UnaryTripleExpr"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.UnaryTripleExpr"))),
      new hydra.core.FieldType(new hydra.core.Name("listOfSequence"), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.UnaryTripleExpr")))),
      new hydra.core.FieldType(new hydra.core.Name("Semi"), new hydra.core.Type.Maybe(new hydra.core.Type.Unit()))));
  }

  static hydra.core.Type unaryTripleExpr() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("sequence"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.UnaryTripleExpr_Sequence"))),
      new hydra.core.FieldType(new hydra.core.Name("Include"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Include")))));
  }

  static hydra.core.Type unaryTripleExpr_Sequence() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("Sequence"), new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.TripleExprLabel")))),
      new hydra.core.FieldType(new hydra.core.Name("alts"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.UnaryTripleExpr_Sequence_Alts")))));
  }

  static hydra.core.Type unaryTripleExpr_Sequence_Alts() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("TripleConstraint"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.TripleConstraint"))),
      new hydra.core.FieldType(new hydra.core.Name("BracketedTripleExpr"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.BracketedTripleExpr")))));
  }

  static hydra.core.Type bracketedTripleExpr() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("InnerTripleExpr"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.InnerTripleExpr"))),
      new hydra.core.FieldType(new hydra.core.Name("Cardinality"), new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Cardinality")))),
      new hydra.core.FieldType(new hydra.core.Name("listOfAnnotation"), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Annotation")))),
      new hydra.core.FieldType(new hydra.core.Name("SemanticActions"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.SemanticActions")))));
  }

  static hydra.core.Type tripleConstraint() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("SenseFlags"), new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.SenseFlags")))),
      new hydra.core.FieldType(new hydra.core.Name("Predicate"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Predicate"))),
      new hydra.core.FieldType(new hydra.core.Name("InlineShapeExpression"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.InlineShapeExpression"))),
      new hydra.core.FieldType(new hydra.core.Name("Cardinality"), new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Cardinality")))),
      new hydra.core.FieldType(new hydra.core.Name("listOfAnnotation"), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Annotation")))),
      new hydra.core.FieldType(new hydra.core.Name("SemanticActions"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.SemanticActions")))));
  }

  static hydra.core.Type cardinality() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("Ast"), new hydra.core.Type.Unit()),
      new hydra.core.FieldType(new hydra.core.Name("Plus"), new hydra.core.Type.Unit()),
      new hydra.core.FieldType(new hydra.core.Name("Quest"), new hydra.core.Type.Unit()),
      new hydra.core.FieldType(new hydra.core.Name("RepeatRange"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.RepeatRange")))));
  }

  static hydra.core.Type senseFlags() {
    return new hydra.core.Type.Wrap(new hydra.core.Type.Unit());
  }

  static hydra.core.Type valueSet() {
    return new hydra.core.Type.Wrap(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.ValueSetValue"))));
  }

  static hydra.core.Type valueSetValue() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("IriRange"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.IriRange"))),
      new hydra.core.FieldType(new hydra.core.Name("Literal"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Literal")))));
  }

  static hydra.core.Type iriRange() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("sequence"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.IriRange_Sequence"))),
      new hydra.core.FieldType(new hydra.core.Name("sequence2"), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Exclusion"))))));
  }

  static hydra.core.Type iriRange_Sequence() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("Iri"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Iri"))),
      new hydra.core.FieldType(new hydra.core.Name("Sequence"), new hydra.core.Type.Maybe(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Exclusion")))))));
  }

  static hydra.core.Type exclusion() {
    return new hydra.core.Type.Wrap(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Iri")));
  }

  static hydra.core.Type include() {
    return new hydra.core.Type.Wrap(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.TripleExprLabel")));
  }

  static hydra.core.Type annotation() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("Predicate"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Predicate"))),
      new hydra.core.FieldType(new hydra.core.Name("alts"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Annotation_Alts")))));
  }

  static hydra.core.Type annotation_Alts() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("Iri"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Iri"))),
      new hydra.core.FieldType(new hydra.core.Name("Literal"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Literal")))));
  }

  static hydra.core.Type semanticActions() {
    return new hydra.core.Type.Wrap(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.CodeDecl"))));
  }

  static hydra.core.Type codeDecl() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("Iri"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Iri"))),
      new hydra.core.FieldType(new hydra.core.Name("alts"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.CodeDecl_Alts")))));
  }

  static hydra.core.Type codeDecl_Alts() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("Code"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Code"))),
      new hydra.core.FieldType(new hydra.core.Name("Percnt"), new hydra.core.Type.Unit())));
  }

  static hydra.core.Type literal() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("RdfLiteral"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.RdfLiteral"))),
      new hydra.core.FieldType(new hydra.core.Name("NumericLiteral"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.NumericLiteral"))),
      new hydra.core.FieldType(new hydra.core.Name("BooleanLiteral"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.BooleanLiteral")))));
  }

  static hydra.core.Type predicate() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("Iri"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Iri"))),
      new hydra.core.FieldType(new hydra.core.Name("RdfType"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.RdfType")))));
  }

  static hydra.core.Type datatype() {
    return new hydra.core.Type.Wrap(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Iri")));
  }

  static hydra.core.Type shapeExprLabel() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("Iri"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Iri"))),
      new hydra.core.FieldType(new hydra.core.Name("BlankNode"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.BlankNode")))));
  }

  static hydra.core.Type tripleExprLabel() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("Iri"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Iri"))),
      new hydra.core.FieldType(new hydra.core.Name("BlankNode"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.BlankNode")))));
  }

  static hydra.core.Type numericLiteral() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("Integer"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Integer"))),
      new hydra.core.FieldType(new hydra.core.Name("Decimal"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Decimal"))),
      new hydra.core.FieldType(new hydra.core.Name("Double"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Double")))));
  }

  static hydra.core.Type rdfLiteral() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("String"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.String"))),
      new hydra.core.FieldType(new hydra.core.Name("Alts"), new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.RdfLiteral_Alts_Option"))))));
  }

  static hydra.core.Type rdfLiteral_Alts_Option() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("LangTag"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.LangTag"))),
      new hydra.core.FieldType(new hydra.core.Name("sequence"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Datatype")))));
  }

  static hydra.core.Type booleanLiteral() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("True"), new hydra.core.Type.Unit()),
      new hydra.core.FieldType(new hydra.core.Name("False"), new hydra.core.Type.Unit())));
  }

  static hydra.core.Type string() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("StringLiteral1"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteral1"))),
      new hydra.core.FieldType(new hydra.core.Name("StringLiteralLong1"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteralLong1"))),
      new hydra.core.FieldType(new hydra.core.Name("StringLiteral2"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteral2"))),
      new hydra.core.FieldType(new hydra.core.Name("StringLiteralLong2"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteralLong2")))));
  }

  static hydra.core.Type iri() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("IriRef"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.IriRef"))),
      new hydra.core.FieldType(new hydra.core.Name("PrefixedName"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.PrefixedName")))));
  }

  static hydra.core.Type prefixedName() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("PnameLn"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.PnameLn"))),
      new hydra.core.FieldType(new hydra.core.Name("PnameNs"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.PnameNs")))));
  }

  static hydra.core.Type blankNode() {
    return new hydra.core.Type.Wrap(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.BlankNodeLabel")));
  }

  static hydra.core.Type includeSet() {
    return new hydra.core.Type.Wrap(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.ShapeExprLabel"))));
  }

  static hydra.core.Type code() {
    return new hydra.core.Type.Wrap(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Code_Elmt"))));
  }

  static hydra.core.Type code_Elmt() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("regex"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
      new hydra.core.FieldType(new hydra.core.Name("sequence"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
      new hydra.core.FieldType(new hydra.core.Name("Uchar"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Uchar")))));
  }

  static hydra.core.Type repeatRange() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("Integer"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Integer"))),
      new hydra.core.FieldType(new hydra.core.Name("Sequence"), new hydra.core.Type.Maybe(new hydra.core.Type.Maybe(new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.RepeatRange_Sequence_Option_Option_Option"))))))));
  }

  static hydra.core.Type repeatRange_Sequence_Option_Option_Option() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("Integer"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Integer"))),
      new hydra.core.FieldType(new hydra.core.Name("Ast"), new hydra.core.Type.Unit())));
  }

  static hydra.core.Type rdfType() {
    return new hydra.core.Type.Wrap(new hydra.core.Type.Unit());
  }

  static hydra.core.Type iriRef() {
    return new hydra.core.Type.Wrap(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.IriRef_Elmt"))));
  }

  static hydra.core.Type iriRef_Elmt() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("regex"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
      new hydra.core.FieldType(new hydra.core.Name("Uchar"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Uchar")))));
  }

  static hydra.core.Type pnameNs() {
    return new hydra.core.Type.Wrap(new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.PnPrefix"))));
  }

  static hydra.core.Type pnameLn() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("PnameNs"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.PnameNs"))),
      new hydra.core.FieldType(new hydra.core.Name("PnLocal"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.PnLocal")))));
  }

  static hydra.core.Type atpNameNs() {
    return new hydra.core.Type.Wrap(new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.PnPrefix"))));
  }

  static hydra.core.Type atpNameLn() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("PnameNs"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.PnameNs"))),
      new hydra.core.FieldType(new hydra.core.Name("PnLocal"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.PnLocal")))));
  }

  static hydra.core.Type regexp() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("listOfAlts"), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Regexp_ListOfAlts_Elmt")))),
      new hydra.core.FieldType(new hydra.core.Name("listOfRegex"), new hydra.core.Type.List(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())))));
  }

  static hydra.core.Type regexp_ListOfAlts_Elmt() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("regex"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
      new hydra.core.FieldType(new hydra.core.Name("sequence"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
      new hydra.core.FieldType(new hydra.core.Name("Uchar"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Uchar")))));
  }

  static hydra.core.Type blankNodeLabel() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("alts"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.BlankNodeLabel_Alts"))),
      new hydra.core.FieldType(new hydra.core.Name("ListOfAlts"), new hydra.core.Type.Maybe(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.BlankNodeLabel_ListOfAlts_Option_Elmt"))))),
      new hydra.core.FieldType(new hydra.core.Name("PnChars"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.PnChars")))));
  }

  static hydra.core.Type blankNodeLabel_Alts() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("PnCharsU"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.PnCharsU"))),
      new hydra.core.FieldType(new hydra.core.Name("regex"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))));
  }

  static hydra.core.Type blankNodeLabel_ListOfAlts_Option_Elmt() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("PnChars"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.PnChars"))),
      new hydra.core.FieldType(new hydra.core.Name("Period"), new hydra.core.Type.Unit())));
  }

  static hydra.core.Type langTag() {
    return new hydra.core.Type.Wrap(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()));
  }

  static hydra.core.Type integer() {
    return new hydra.core.Type.Wrap(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()));
  }

  static hydra.core.Type decimal() {
    return new hydra.core.Type.Wrap(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()));
  }

  static hydra.core.Type double_() {
    return new hydra.core.Type.Wrap(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()));
  }

  static hydra.core.Type stringLiteral1() {
    return new hydra.core.Type.Wrap(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteral1_Elmt"))));
  }

  static hydra.core.Type stringLiteral1_Elmt() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("regex"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
      new hydra.core.FieldType(new hydra.core.Name("Echar"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Echar"))),
      new hydra.core.FieldType(new hydra.core.Name("Uchar"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Uchar")))));
  }

  static hydra.core.Type stringLiteral2() {
    return new hydra.core.Type.Wrap(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteral2_Elmt"))));
  }

  static hydra.core.Type stringLiteral2_Elmt() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("regex"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
      new hydra.core.FieldType(new hydra.core.Name("Echar"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Echar"))),
      new hydra.core.FieldType(new hydra.core.Name("Uchar"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Uchar")))));
  }

  static hydra.core.Type stringLiteralLong1() {
    return new hydra.core.Type.Wrap(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteralLong1_Elmt"))));
  }

  static hydra.core.Type stringLiteralLong1_Elmt() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("sequence"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteralLong1_Elmt_Sequence"))),
      new hydra.core.FieldType(new hydra.core.Name("Echar"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Echar"))),
      new hydra.core.FieldType(new hydra.core.Name("Uchar"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Uchar")))));
  }

  static hydra.core.Type stringLiteralLong1_Elmt_Sequence() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("Alts"), new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteralLong1_Elmt_Sequence_Alts_Option")))),
      new hydra.core.FieldType(new hydra.core.Name("regex"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))));
  }

  static hydra.core.Type stringLiteralLong1_Elmt_Sequence_Alts_Option() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("Apos"), new hydra.core.Type.Unit()),
      new hydra.core.FieldType(new hydra.core.Name("sequence"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteralLong1_Elmt_Sequence_Alts_Option_Sequence")))));
  }

  static hydra.core.Type stringLiteralLong1_Elmt_Sequence_Alts_Option_Sequence() {
    return new hydra.core.Type.Record((hydra.util.ConsList<hydra.core.FieldType>) (hydra.util.ConsList.<hydra.core.FieldType>empty()));
  }

  static hydra.core.Type stringLiteralLong2() {
    return new hydra.core.Type.Wrap(new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteralLong2_Elmt"))));
  }

  static hydra.core.Type stringLiteralLong2_Elmt() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("sequence"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteralLong2_Elmt_Sequence"))),
      new hydra.core.FieldType(new hydra.core.Name("Echar"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Echar"))),
      new hydra.core.FieldType(new hydra.core.Name("Uchar"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Uchar")))));
  }

  static hydra.core.Type stringLiteralLong2_Elmt_Sequence() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("Alts"), new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteralLong2_Elmt_Sequence_Alts_Option")))),
      new hydra.core.FieldType(new hydra.core.Name("regex"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))));
  }

  static hydra.core.Type stringLiteralLong2_Elmt_Sequence_Alts_Option() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("Quot"), new hydra.core.Type.Unit()),
      new hydra.core.FieldType(new hydra.core.Name("sequence"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.StringLiteralLong2_Elmt_Sequence_Alts_Option_Sequence")))));
  }

  static hydra.core.Type stringLiteralLong2_Elmt_Sequence_Alts_Option_Sequence() {
    return new hydra.core.Type.Record((hydra.util.ConsList<hydra.core.FieldType>) (hydra.util.ConsList.<hydra.core.FieldType>empty()));
  }

  static hydra.core.Type uchar() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("sequence"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Uchar_Sequence"))),
      new hydra.core.FieldType(new hydra.core.Name("sequence2"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Uchar_Sequence2")))));
  }

  static hydra.core.Type uchar_Sequence() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("Hex"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Hex"))),
      new hydra.core.FieldType(new hydra.core.Name("Hex2"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Hex"))),
      new hydra.core.FieldType(new hydra.core.Name("Hex3"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Hex"))),
      new hydra.core.FieldType(new hydra.core.Name("Hex4"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Hex")))));
  }

  static hydra.core.Type uchar_Sequence2() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("Hex"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Hex"))),
      new hydra.core.FieldType(new hydra.core.Name("Hex2"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Hex"))),
      new hydra.core.FieldType(new hydra.core.Name("Hex3"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Hex"))),
      new hydra.core.FieldType(new hydra.core.Name("Hex4"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Hex"))),
      new hydra.core.FieldType(new hydra.core.Name("Hex5"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Hex"))),
      new hydra.core.FieldType(new hydra.core.Name("Hex6"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Hex"))),
      new hydra.core.FieldType(new hydra.core.Name("Hex7"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Hex"))),
      new hydra.core.FieldType(new hydra.core.Name("Hex8"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Hex")))));
  }

  static hydra.core.Type echar() {
    return new hydra.core.Type.Wrap(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()));
  }

  static hydra.core.Type pnCharsBase() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("regex"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
      new hydra.core.FieldType(new hydra.core.Name("regex2"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))));
  }

  static hydra.core.Type pnCharsU() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("PnCharsBase"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.PnCharsBase"))),
      new hydra.core.FieldType(new hydra.core.Name("Lowbar"), new hydra.core.Type.Unit())));
  }

  static hydra.core.Type pnChars() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("PnCharsU"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.PnCharsU"))),
      new hydra.core.FieldType(new hydra.core.Name("Minus"), new hydra.core.Type.Unit()),
      new hydra.core.FieldType(new hydra.core.Name("regex"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()))));
  }

  static hydra.core.Type pnPrefix() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("PnCharsBase"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.PnCharsBase"))),
      new hydra.core.FieldType(new hydra.core.Name("Sequence"), new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.PnPrefix_Sequence_Option"))))));
  }

  static hydra.core.Type pnPrefix_Sequence_Option() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("alts"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.PnPrefix_Sequence_Option_Alts"))),
      new hydra.core.FieldType(new hydra.core.Name("PnChars"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.PnChars")))));
  }

  static hydra.core.Type pnPrefix_Sequence_Option_Alts() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("PnChars"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.PnChars"))),
      new hydra.core.FieldType(new hydra.core.Name("Period"), new hydra.core.Type.Unit())));
  }

  static hydra.core.Type pnLocal() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("alts"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.PnLocal_Alts"))),
      new hydra.core.FieldType(new hydra.core.Name("Sequence"), new hydra.core.Type.Maybe(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.PnLocal_Sequence_Option"))))));
  }

  static hydra.core.Type pnLocal_Alts() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("PnCharsU"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.PnCharsU"))),
      new hydra.core.FieldType(new hydra.core.Name("Colon"), new hydra.core.Type.Unit()),
      new hydra.core.FieldType(new hydra.core.Name("regex"), new hydra.core.Type.Literal(new hydra.core.LiteralType.String_())),
      new hydra.core.FieldType(new hydra.core.Name("Plx"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Plx")))));
  }

  static hydra.core.Type pnLocal_Sequence_Option() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("listOfAlts"), new hydra.core.Type.List(new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.PnLocal_Sequence_Option_ListOfAlts_Elmt")))),
      new hydra.core.FieldType(new hydra.core.Name("alts"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.PnLocal_Sequence_Option_Alts")))));
  }

  static hydra.core.Type pnLocal_Sequence_Option_ListOfAlts_Elmt() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("PnChars"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.PnChars"))),
      new hydra.core.FieldType(new hydra.core.Name("Period"), new hydra.core.Type.Unit()),
      new hydra.core.FieldType(new hydra.core.Name("Colon"), new hydra.core.Type.Unit()),
      new hydra.core.FieldType(new hydra.core.Name("Plx"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Plx")))));
  }

  static hydra.core.Type pnLocal_Sequence_Option_Alts() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("PnChars"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.PnChars"))),
      new hydra.core.FieldType(new hydra.core.Name("Colon"), new hydra.core.Type.Unit()),
      new hydra.core.FieldType(new hydra.core.Name("Plx"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Plx")))));
  }

  static hydra.core.Type plx() {
    return new hydra.core.Type.Union(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("Percent"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Percent"))),
      new hydra.core.FieldType(new hydra.core.Name("PnLocalEsc"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.PnLocalEsc")))));
  }

  static hydra.core.Type percent() {
    return new hydra.core.Type.Record(hydra.util.ConsList.of(
      new hydra.core.FieldType(new hydra.core.Name("Hex"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Hex"))),
      new hydra.core.FieldType(new hydra.core.Name("Hex2"), new hydra.core.Type.Variable(new hydra.core.Name("hydra.ext.io.shex.syntax.Hex")))));
  }

  static hydra.core.Type hex() {
    return new hydra.core.Type.Wrap(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()));
  }

  static hydra.core.Type pnLocalEsc() {
    return new hydra.core.Type.Wrap(new hydra.core.Type.Literal(new hydra.core.LiteralType.String_()));
  }
}
