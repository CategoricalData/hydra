package hydra.langs.cypher;

import hydra.langs.cypher.openCypher.AddOrSubtractExpression;
import hydra.langs.cypher.openCypher.AddOrSubtractOperator;
import hydra.langs.cypher.openCypher.AddOrSubtractRightHandSide;
import hydra.langs.cypher.openCypher.AndExpression;
import hydra.langs.cypher.openCypher.AnonymousPatternPart;
import hydra.langs.cypher.openCypher.Atom;
import hydra.langs.cypher.openCypher.CaseAlternative;
import hydra.langs.cypher.openCypher.CaseExpression;
import hydra.langs.cypher.openCypher.ComparisonExpression;
import hydra.langs.cypher.openCypher.ComparisonOperator;
import hydra.langs.cypher.openCypher.Create;
import hydra.langs.cypher.openCypher.Delete;
import hydra.langs.cypher.openCypher.ExistentialSubquery;
import hydra.langs.cypher.openCypher.ExplicitProcedureInvocation;
import hydra.langs.cypher.openCypher.Expression;
import hydra.langs.cypher.openCypher.FilterExpression;
import hydra.langs.cypher.openCypher.FunctionInvocation;
import hydra.langs.cypher.openCypher.IdInColl;
import hydra.langs.cypher.openCypher.ImplicitProcedureInvocation;
import hydra.langs.cypher.openCypher.InQueryCall;
import hydra.langs.cypher.openCypher.KeyValuePair;
import hydra.langs.cypher.openCypher.Limit;
import hydra.langs.cypher.openCypher.ListComprehension;
import hydra.langs.cypher.openCypher.ListLiteral;
import hydra.langs.cypher.openCypher.ListOperatorExpression;
import hydra.langs.cypher.openCypher.ListOperatorExpressionOrPropertyLookup;
import hydra.langs.cypher.openCypher.ListPredicateExpression;
import hydra.langs.cypher.openCypher.Literal;
import hydra.langs.cypher.openCypher.MapLiteral;
import hydra.langs.cypher.openCypher.Match;
import hydra.langs.cypher.openCypher.MatchOrCreate;
import hydra.langs.cypher.openCypher.Merge;
import hydra.langs.cypher.openCypher.MergeAction;
import hydra.langs.cypher.openCypher.MultiPartQuery;
import hydra.langs.cypher.openCypher.MultiplyDivideModuloExpression;
import hydra.langs.cypher.openCypher.MultiplyDivideModuloOperator;
import hydra.langs.cypher.openCypher.MultiplyDivideModuloRightHandSide;
import hydra.langs.cypher.openCypher.NodeLabel;
import hydra.langs.cypher.openCypher.NodeLabels;
import hydra.langs.cypher.openCypher.NodePattern;
import hydra.langs.cypher.openCypher.NodePatternChain;
import hydra.langs.cypher.openCypher.NonArithmeticOperatorExpression;
import hydra.langs.cypher.openCypher.NotExpression;
import hydra.langs.cypher.openCypher.NullPredicateExpression;
import hydra.langs.cypher.openCypher.NumberLiteral;
import hydra.langs.cypher.openCypher.OrExpression;
import hydra.langs.cypher.openCypher.Order;
import hydra.langs.cypher.openCypher.Parameter;
import hydra.langs.cypher.openCypher.ParenthesizedExpression;
import hydra.langs.cypher.openCypher.PartialComparisonExpression;
import hydra.langs.cypher.openCypher.Pattern;
import hydra.langs.cypher.openCypher.PatternComprehension;
import hydra.langs.cypher.openCypher.PatternElement;
import hydra.langs.cypher.openCypher.PatternElementChain;
import hydra.langs.cypher.openCypher.PatternPart;
import hydra.langs.cypher.openCypher.PatternPredicate;
import hydra.langs.cypher.openCypher.PatternWhere;
import hydra.langs.cypher.openCypher.PowerOfExpression;
import hydra.langs.cypher.openCypher.ProcedureInvocation;
import hydra.langs.cypher.openCypher.ProcedureResultField;
import hydra.langs.cypher.openCypher.ProjectionBody;
import hydra.langs.cypher.openCypher.ProjectionItem;
import hydra.langs.cypher.openCypher.ProjectionItems;
import hydra.langs.cypher.openCypher.Properties;
import hydra.langs.cypher.openCypher.PropertyEquals;
import hydra.langs.cypher.openCypher.PropertyExpression;
import hydra.langs.cypher.openCypher.PropertyKeyName;
import hydra.langs.cypher.openCypher.PropertyLookup;
import hydra.langs.cypher.openCypher.QualifiedName;
import hydra.langs.cypher.openCypher.Quantifier;
import hydra.langs.cypher.openCypher.QuantifierOperator;
import hydra.langs.cypher.openCypher.Query;
import hydra.langs.cypher.openCypher.RangeExpression;
import hydra.langs.cypher.openCypher.RangeLiteral;
import hydra.langs.cypher.openCypher.ReadingClause;
import hydra.langs.cypher.openCypher.RegularQuery;
import hydra.langs.cypher.openCypher.RelTypeName;
import hydra.langs.cypher.openCypher.RelationshipDetail;
import hydra.langs.cypher.openCypher.RelationshipPattern;
import hydra.langs.cypher.openCypher.RelationshipTypes;
import hydra.langs.cypher.openCypher.RelationshipsPattern;
import hydra.langs.cypher.openCypher.Remove;
import hydra.langs.cypher.openCypher.RemoveItem;
import hydra.langs.cypher.openCypher.Return;
import hydra.langs.cypher.openCypher.Set;
import hydra.langs.cypher.openCypher.SetItem;
import hydra.langs.cypher.openCypher.SinglePartQuery;
import hydra.langs.cypher.openCypher.SingleQuery;
import hydra.langs.cypher.openCypher.Skip;
import hydra.langs.cypher.openCypher.SortItem;
import hydra.langs.cypher.openCypher.SortOrder;
import hydra.langs.cypher.openCypher.StandaloneCall;
import hydra.langs.cypher.openCypher.StarOrYieldItems;
import hydra.langs.cypher.openCypher.StringListNullPredicateExpression;
import hydra.langs.cypher.openCypher.StringListNullPredicateRightHandSide;
import hydra.langs.cypher.openCypher.StringLiteral;
import hydra.langs.cypher.openCypher.StringPredicateExpression;
import hydra.langs.cypher.openCypher.StringPredicateOperator;
import hydra.langs.cypher.openCypher.UnaryAddOrSubtractExpression;
import hydra.langs.cypher.openCypher.Union;
import hydra.langs.cypher.openCypher.Unwind;
import hydra.langs.cypher.openCypher.UpdatingClause;
import hydra.langs.cypher.openCypher.Variable;
import hydra.langs.cypher.openCypher.VariableAndNodeLabels;
import hydra.langs.cypher.openCypher.VariableEquals;
import hydra.langs.cypher.openCypher.VariablePlusEquals;
import hydra.langs.cypher.openCypher.Where;
import hydra.langs.cypher.openCypher.With;
import hydra.langs.cypher.openCypher.WithClause;
import hydra.langs.cypher.openCypher.XorExpression;
import hydra.langs.cypher.openCypher.YieldItem;
import hydra.langs.cypher.openCypher.YieldItems;
import hydra.tools.AntlrReaderBase;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.ParseTree;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.apache.commons.text.StringEscapeUtils;
import org.neo4j.CypherLexer;
import org.neo4j.CypherParser;

import java.math.BigInteger;
import java.util.ArrayList;
import java.util.List;
import java.util.Optional;

/**
 * A parser which constructs Cypher queries using the hydra/langs/cypher/openCypher model.
 *
 * Notes/limitations:
 *   * Many syntax features are not yet implemented; the parser will throw an exception when it encounters them.
 *   * The encodings for strings, doubles, and integers are inherited from Java and may not completely agree with the OpenCypher specification.
 */
public class CypherReader extends AntlrReaderBase {

    public static Query read(String query) {
        CypherLexer lexer = new CypherLexer(CharStreams.fromString(query));
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        CypherParser parser = new CypherParser(tokens);
        return read(parser.oC_Query());
    }

    ////////////////////////////////////

    private static Expression read(CypherParser.OC_ExpressionContext ctx) {
        return new Expression(required(ctx, CypherParser.OC_ExpressionContext::oC_OrExpression, CypherReader::read));
    }

    private static OrExpression read(CypherParser.OC_OrExpressionContext ctx) {
        return new OrExpression(list(ctx, CypherParser.OC_OrExpressionContext::oC_XorExpression, CypherReader::read));
    }

    private static XorExpression read(CypherParser.OC_XorExpressionContext ctx) {
        return new XorExpression(list(ctx, CypherParser.OC_XorExpressionContext::oC_AndExpression, CypherReader::read));
    }

    private static AndExpression read(CypherParser.OC_AndExpressionContext ctx) {
        return new AndExpression(list(ctx, CypherParser.OC_AndExpressionContext::oC_NotExpression, CypherReader::read));
    }

    private static NotExpression read(CypherParser.OC_NotExpressionContext ctx) {
        return new NotExpression(
                ctx.NOT() != null && ctx.NOT().size() > 0,
                required(ctx, CypherParser.OC_NotExpressionContext::oC_ComparisonExpression, CypherReader::read)
        );
    }

    private static ComparisonExpression read(CypherParser.OC_ComparisonExpressionContext ctx) {
        return new ComparisonExpression(
                required(ctx, CypherParser.OC_ComparisonExpressionContext::oC_StringListNullPredicateExpression, CypherReader::read),
                list(ctx, CypherParser.OC_ComparisonExpressionContext::oC_PartialComparisonExpression, CypherReader::read));
    }

    private static PartialComparisonExpression read(CypherParser.OC_PartialComparisonExpressionContext ctx) {
        return new PartialComparisonExpression(
                required(ctx, CypherParser.OC_PartialComparisonExpressionContext::oC_ComparisonOperator, CypherReader::read),
                required(ctx, CypherParser.OC_PartialComparisonExpressionContext::oC_StringListNullPredicateExpression, CypherReader::read));
    }

    private static ComparisonOperator read(CypherParser.OC_ComparisonOperatorContext ctx) {
        switch (ctx.getText()) {
            case "=":
                return new ComparisonOperator.Eq();
            case "<>":
                return new ComparisonOperator.Neq();
            case "<":
                return new ComparisonOperator.Lt();
            case ">":
                return new ComparisonOperator.Gt();
            case "<=":
                return new ComparisonOperator.Lte();
            case ">=":
                return new ComparisonOperator.Gte();
            default:
                return invalid("Unknown comparison operator: " + ctx.getText());
        }
    }

    private static StringListNullPredicateExpression read(CypherParser.OC_StringListNullPredicateExpressionContext ctx) {
        return new StringListNullPredicateExpression(
                required(ctx, CypherParser.OC_StringListNullPredicateExpressionContext::oC_AddOrSubtractExpression, CypherReader::read),
                list(ctx, CypherParser.OC_StringListNullPredicateExpressionContext::oC_StringListNullPredicateExpression_RHS, CypherReader::read));
    }

    private static StringListNullPredicateRightHandSide read(CypherParser.OC_StringListNullPredicateExpression_RHSContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_StringListNullPredicateExpression_RHSContext::oC_StringPredicateExpression, CypherReader::read, StringListNullPredicateRightHandSide.String_::new),
                matchCase(CypherParser.OC_StringListNullPredicateExpression_RHSContext::oC_ListPredicateExpression, CypherReader::read, StringListNullPredicateRightHandSide.List::new),
                matchCase(CypherParser.OC_StringListNullPredicateExpression_RHSContext::oC_NullPredicateExpression, CypherReader::read, StringListNullPredicateRightHandSide.Null::new));
    }

    private static StringPredicateExpression read(CypherParser.OC_StringPredicateExpressionContext ctx) {
        return new StringPredicateExpression(
                required(ctx, CypherParser.OC_StringPredicateExpressionContext::oC_StringPredicateOperator, CypherReader::read),
                required(ctx, CypherParser.OC_StringPredicateExpressionContext::oC_AddOrSubtractExpression, CypherReader::read));
    }

    private static StringPredicateOperator read(CypherParser.OC_StringPredicateOperatorContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_StringPredicateOperatorContext::STARTS, new StringPredicateOperator.StartsWith()),
                matchCase(CypherParser.OC_StringPredicateOperatorContext::ENDS, new StringPredicateOperator.EndsWith()),
                matchCase(CypherParser.OC_StringPredicateOperatorContext::CONTAINS, new StringPredicateOperator.Contains()));
    }

    private static ListPredicateExpression read(CypherParser.OC_ListPredicateExpressionContext ctx) {
        return new ListPredicateExpression(
                required(ctx, CypherParser.OC_ListPredicateExpressionContext::oC_AddOrSubtractExpression, CypherReader::read));
    }

    private static NullPredicateExpression read(CypherParser.OC_NullPredicateExpressionContext ctx) {
        return new NullPredicateExpression(ctx.NOT() == null);
    }

    private static AddOrSubtractExpression read(CypherParser.OC_AddOrSubtractExpressionContext ctx) {
        return new AddOrSubtractExpression(
                required(ctx, CypherParser.OC_AddOrSubtractExpressionContext::oC_MultiplyDivideModuloExpression, CypherReader::read),
                list(ctx, CypherParser.OC_AddOrSubtractExpressionContext::oC_AddOrSubtractExpression_RHS, CypherReader::read));
    }

    private static AddOrSubtractRightHandSide read(CypherParser.OC_AddOrSubtractExpression_RHSContext ctx) {
        return new AddOrSubtractRightHandSide(
                required(ctx, CypherParser.OC_AddOrSubtractExpression_RHSContext::oC_AddOrSubtractExpression_Operator, CypherReader::read),
                required(ctx, CypherParser.OC_AddOrSubtractExpression_RHSContext::oC_MultiplyDivideModuloExpression, CypherReader::read));
    }

    private static AddOrSubtractOperator read(CypherParser.OC_AddOrSubtractExpression_OperatorContext ctx) {
        switch (ctx.getText()) {
            case "+":
                return new AddOrSubtractOperator.Add();
            case "-":
                return new AddOrSubtractOperator.Subtract();
            default:
                return invalid("Unknown add or subtract operator: " + ctx.getText());
        }
    }

    private static MultiplyDivideModuloExpression read(CypherParser.OC_MultiplyDivideModuloExpressionContext ctx) {
        return new MultiplyDivideModuloExpression(
                required(ctx, CypherParser.OC_MultiplyDivideModuloExpressionContext::oC_PowerOfExpression, CypherReader::read),
                list(ctx, CypherParser.OC_MultiplyDivideModuloExpressionContext::oC_MultiplyDivideModuloExpression_RHS, CypherReader::read));
    }

    private static MultiplyDivideModuloRightHandSide read(CypherParser.OC_MultiplyDivideModuloExpression_RHSContext ctx) {
        return new MultiplyDivideModuloRightHandSide(
                required(ctx, CypherParser.OC_MultiplyDivideModuloExpression_RHSContext::oC_MultiplyDivideModuloExpression_Operator, CypherReader::read),
                required(ctx, CypherParser.OC_MultiplyDivideModuloExpression_RHSContext::oC_PowerOfExpression, CypherReader::read));
    }

    private static MultiplyDivideModuloOperator read(CypherParser.OC_MultiplyDivideModuloExpression_OperatorContext ctx) {
        switch (ctx.getText()) {
            case "*":
                return new MultiplyDivideModuloOperator.Multiply();
            case "/":
                return new MultiplyDivideModuloOperator.Divide();
            case "%":
                return new MultiplyDivideModuloOperator.Modulo();
            default:
                return invalid("Unknown multiply, divide or modulo operator: " + ctx.getText());
        }
    }

    private static PowerOfExpression read(CypherParser.OC_PowerOfExpressionContext ctx) {
        return new PowerOfExpression(list(ctx, CypherParser.OC_PowerOfExpressionContext::oC_UnaryAddOrSubtractExpression, CypherReader::read));
    }

    private static UnaryAddOrSubtractExpression read(CypherParser.OC_UnaryAddOrSubtractExpressionContext ctx) {
        return new UnaryAddOrSubtractExpression(optional(ctx, CypherParser.OC_UnaryAddOrSubtractExpressionContext::oC_UnaryAddOrSubtractExpression_Operator, CypherReader::read),
                required(ctx, CypherParser.OC_UnaryAddOrSubtractExpressionContext::oC_NonArithmeticOperatorExpression, CypherReader::read));
    }

    private static NonArithmeticOperatorExpression read(CypherParser.OC_NonArithmeticOperatorExpressionContext ctx) {
      return new NonArithmeticOperatorExpression(
              required(ctx, CypherParser.OC_NonArithmeticOperatorExpressionContext::oC_Atom, CypherReader::read),
              list(ctx, CypherParser.OC_NonArithmeticOperatorExpressionContext::oC_ListOperatorExpressionOrPropertyLookup, CypherReader::read),
              optional(ctx, CypherParser.OC_NonArithmeticOperatorExpressionContext::oC_NodeLabels, CypherReader::read));
    }

    private static ListOperatorExpressionOrPropertyLookup read(CypherParser.OC_ListOperatorExpressionOrPropertyLookupContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_ListOperatorExpressionOrPropertyLookupContext::oC_ListOperatorExpression, CypherReader::read, ListOperatorExpressionOrPropertyLookup.List::new),
                matchCase(CypherParser.OC_ListOperatorExpressionOrPropertyLookupContext::oC_PropertyLookup, CypherReader::read, ListOperatorExpressionOrPropertyLookup.Property::new));
    }

    private static PropertyLookup read(CypherParser.OC_PropertyLookupContext ctx) {
        return new PropertyLookup(required(ctx, CypherParser.OC_PropertyLookupContext::oC_PropertyKeyName, CypherReader::read));
    }

    private static ListOperatorExpression read(CypherParser.OC_ListOperatorExpressionContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_ListOperatorExpressionContext::oC_ListOperatorExpressionSingle, CypherReader::read, ListOperatorExpression.Single::new),
                matchCase(CypherParser.OC_ListOperatorExpressionContext::oC_ListOperatorExpressionRange, CypherReader::read, ListOperatorExpression.Range::new));
    }

    private static RangeExpression read(CypherParser.OC_ListOperatorExpressionRangeContext ctx) {
        return new RangeExpression(
                required(ctx, CypherParser.OC_ListOperatorExpressionRangeContext::oC_ListOperatorExpressionRangeLHS, CypherReader::read),
                required(ctx, CypherParser.OC_ListOperatorExpressionRangeContext::oC_ListOperatorExpressionRangeRHS, CypherReader::read));
    }

    private static Optional<Expression> read(CypherParser.OC_ListOperatorExpressionRangeRHSContext ctx) {
        return optional(ctx, CypherParser.OC_ListOperatorExpressionRangeRHSContext::oC_Expression, CypherReader::read);
    }

    private static Optional<Expression> read(CypherParser.OC_ListOperatorExpressionRangeLHSContext ctx) {
        return optional(ctx, CypherParser.OC_ListOperatorExpressionRangeLHSContext::oC_Expression, CypherReader::read);
    }

    private static Expression read(CypherParser.OC_ListOperatorExpressionSingleContext ctx) {
        return required(ctx, CypherParser.OC_ListOperatorExpressionSingleContext::oC_Expression, CypherReader::read);
    }

    private static Atom read(CypherParser.OC_AtomContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_AtomContext::oC_Literal, CypherReader::read, Atom.Literal::new),
                matchCase(CypherParser.OC_AtomContext::oC_Parameter, CypherReader::read, Atom.Parameter::new),
                matchCase(CypherParser.OC_AtomContext::oC_CaseExpression, CypherReader::read, Atom.Case::new),
                matchCase(CypherParser.OC_AtomContext::COUNT, new Atom.CountStar()),
                matchCase(CypherParser.OC_AtomContext::oC_ListComprehension, CypherReader::read, Atom.ListComprehension::new),
                matchCase(CypherParser.OC_AtomContext::oC_PatternComprehension, CypherReader::read, Atom.PatternComprehension::new),
                matchCase(CypherParser.OC_AtomContext::oC_Quantifier, CypherReader::read, Atom.Quantifier::new),
                matchCase(CypherParser.OC_AtomContext::oC_PatternPredicate, CypherReader::read, Atom.PatternPredicate::new),
                matchCase(CypherParser.OC_AtomContext::oC_ParenthesizedExpression, CypherReader::read, Atom.Parenthesized::new),
                matchCase(CypherParser.OC_AtomContext::oC_FunctionInvocation, CypherReader::read, Atom.FunctionInvocation::new),
                matchCase(CypherParser.OC_AtomContext::oC_ExistentialSubquery, CypherReader::read, Atom.ExistentialSubquery::new),
                matchCase(CypherParser.OC_AtomContext::oC_Variable, CypherReader::read, Atom.Variable::new));
    }

    private static ExistentialSubquery read(CypherParser.OC_ExistentialSubqueryContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_ExistentialSubqueryContext::oC_RegularQuery, CypherReader::read, ExistentialSubquery.Regular::new),
                matchCase(CypherParser.OC_ExistentialSubqueryContext::oC_PatternWhere, CypherReader::read, ExistentialSubquery.Pattern::new));
    }

    private static PatternWhere read(CypherParser.OC_PatternWhereContext ctx) {
        return new PatternWhere(
                required(ctx, CypherParser.OC_PatternWhereContext::oC_Pattern, CypherReader::read),
                optional(ctx, CypherParser.OC_PatternWhereContext::oC_Where, CypherReader::read));
    }

    private static FunctionInvocation read(CypherParser.OC_FunctionInvocationContext ctx) {
        return new FunctionInvocation(
                required(ctx, CypherParser.OC_FunctionInvocationContext::oC_FunctionName, CypherReader::read),
                ctx.DISTINCT() != null,
                list(ctx, CypherParser.OC_FunctionInvocationContext::oC_Expression, CypherReader::read));
    }

    private static QualifiedName read(CypherParser.OC_FunctionNameContext ctx) {
        return new QualifiedName(
                required(ctx, CypherParser.OC_FunctionNameContext::oC_Namespace, CypherReader::read),
                required(ctx, CypherParser.OC_FunctionNameContext::oC_SymbolicName, CypherReader::read));
    }

    private static String read(CypherParser.OC_SymbolicNameContext ctx) {
        return ctx.getText();
    }

    private static String read(CypherParser.OC_NamespaceContext ctx) {
        return ctx.getText();
    }

    private static ParenthesizedExpression read(CypherParser.OC_ParenthesizedExpressionContext ctx) {
        return new ParenthesizedExpression(
                required(ctx, CypherParser.OC_ParenthesizedExpressionContext::oC_Expression, CypherReader::read));
    }

    private static Quantifier read(CypherParser.OC_QuantifierContext ctx) {
        return new Quantifier(
                required(ctx, CypherParser.OC_QuantifierContext::oC_QuantifierOperator, CypherReader::read),
                required(ctx, CypherParser.OC_QuantifierContext::oC_FilterExpression, CypherReader::read));
    }

    private static FilterExpression read(CypherParser.OC_FilterExpressionContext ctx) {
        return new FilterExpression(
                required(ctx, CypherParser.OC_FilterExpressionContext::oC_IdInColl, CypherReader::read),
                optional(ctx, CypherParser.OC_FilterExpressionContext::oC_Where, CypherReader::read));
    }

    private static IdInColl read(CypherParser.OC_IdInCollContext ctx) {
        return new IdInColl(
                required(ctx, CypherParser.OC_IdInCollContext::oC_Variable, CypherReader::read),
                required(ctx, CypherParser.OC_IdInCollContext::oC_Expression, CypherReader::read));
    }

    private static QuantifierOperator read(CypherParser.OC_QuantifierOperatorContext ctx) {
        switch (ctx.getText()) {
            case "ALL":
                return new QuantifierOperator.All();
            case "ANY":
                return new QuantifierOperator.Any();
            case "NONE":
                return new QuantifierOperator.None();
            case "SINGLE":
                return new QuantifierOperator.Single();
            default:
                throw new IllegalArgumentException("Unknown quantifier operator: " + ctx.getText());
        }
    }

    private static PatternPredicate read(CypherParser.OC_PatternPredicateContext ctx) {
        return new PatternPredicate(required(ctx, CypherParser.OC_PatternPredicateContext::oC_RelationshipsPattern, CypherReader::read));
    }

    private static RelationshipsPattern read(CypherParser.OC_RelationshipsPatternContext ctx) {
        return new RelationshipsPattern(
                required(ctx, CypherParser.OC_RelationshipsPatternContext::oC_NodePattern, CypherReader::read),
                nonemptyList(ctx, CypherParser.OC_RelationshipsPatternContext::oC_PatternElementChain, CypherReader::read));
    }

    private static PatternComprehension read(CypherParser.OC_PatternComprehensionContext ctx) {
        return new PatternComprehension(
                optional(ctx, CypherParser.OC_PatternComprehensionContext::oC_Variable, CypherReader::read),
                required(ctx, CypherParser.OC_PatternComprehensionContext::oC_RelationshipsPattern, CypherReader::read),
                optional(ctx, CypherParser.OC_PatternComprehensionContext::oC_Where, CypherReader::read),
                required(ctx, CypherParser.OC_PatternComprehensionContext::oC_Expression, CypherReader::read));
    }

    private static ListComprehension read(CypherParser.OC_ListComprehensionContext ctx) {
        return new ListComprehension(
                required(ctx, CypherParser.OC_ListComprehensionContext::oC_FilterExpression, CypherReader::read),
                optional(ctx, CypherParser.OC_ListComprehensionContext::oC_Expression, CypherReader::read));
    }

    private static CaseExpression read(CypherParser.OC_CaseExpressionContext ctx) {
        return new CaseExpression(optional(ctx, CypherParser.OC_CaseExpressionContext::oC_Expression, CypherReader::read),
                list(ctx, CypherParser.OC_CaseExpressionContext::oC_CaseAlternative, CypherReader::read),
                optional(ctx, CypherParser.OC_CaseExpressionContext::oc_CaseElse, CypherReader::read));
    }

    private static Expression read(CypherParser.Oc_CaseElseContext ctx) {
        return required(ctx, CypherParser.Oc_CaseElseContext::oC_Expression, CypherReader::read);
    }

    private static CaseAlternative read(CypherParser.OC_CaseAlternativeContext ctx) {
        return new CaseAlternative(
                required(ctx, 0, CypherParser.OC_CaseAlternativeContext::oC_Expression, CypherReader::read),
                required(ctx, 1, CypherParser.OC_CaseAlternativeContext::oC_Expression, CypherReader::read));
    }

    private static Parameter read(CypherParser.OC_ParameterContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_ParameterContext::oC_SymbolicName, CypherReader::read, Parameter.Symbolic::new),
                matchCase(CypherParser.OC_ParameterContext::DecimalInteger, c -> new BigInteger(c.getText()), Parameter.Integer_::new));
    }

    private static Literal read(CypherParser.OC_LiteralContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_LiteralContext::oC_BooleanLiteral, CypherReader::read, Literal.Boolean_::new),
                matchCase(CypherParser.OC_LiteralContext::NULL, new Literal.Null()),
                matchCase(CypherParser.OC_LiteralContext::oC_NumberLiteral, CypherReader::read, Literal.Number_::new),
                matchCase(CypherParser.OC_LiteralContext::StringLiteral, CypherReader::read, Literal.String_::new),
                matchCase(CypherParser.OC_LiteralContext::oC_ListLiteral, CypherReader::read, Literal.List::new),
                matchCase(CypherParser.OC_LiteralContext::oC_MapLiteral, CypherReader::read, Literal.Map::new));
    }

    private static ListLiteral read(CypherParser.OC_ListLiteralContext ctx) {
        return new ListLiteral(list(ctx, CypherParser.OC_ListLiteralContext::oC_Expression, CypherReader::read));
    }

    private static StringLiteral read(TerminalNode n) {
        return new StringLiteral(unescapeSingleQuotedString(n.getText()));
    }

    private static NumberLiteral read(CypherParser.OC_NumberLiteralContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_NumberLiteralContext::oC_DoubleLiteral, CypherReader::read, NumberLiteral.Double_::new),
                matchCase(CypherParser.OC_NumberLiteralContext::oC_IntegerLiteral, CypherReader::read, NumberLiteral.Integer_::new));
    }

    private static BigInteger read(CypherParser.OC_IntegerLiteralContext ctx) {
        return new BigInteger(ctx.getText());
    }

    private static Double read(CypherParser.OC_DoubleLiteralContext ctx) {
        return Double.parseDouble(ctx.getText());
    }

    private static Boolean read(CypherParser.OC_BooleanLiteralContext ctx) {
        switch (ctx.getText()) {
            case "true":
                return true;
            case "false":
                return false;
            default:
                return invalid("Unknown boolean literal: " + ctx.getText());
        }
    }

    private static AddOrSubtractOperator read(CypherParser.OC_UnaryAddOrSubtractExpression_OperatorContext ctx) {
        switch (ctx.getText()) {
            case "+":
                return new AddOrSubtractOperator.Add();
            case "-":
                return new AddOrSubtractOperator.Subtract();
            default:
                return invalid("Unknown unary add or subtract operator: " + ctx.getText());
        }
    }

    private static Match read(CypherParser.OC_MatchContext ctx) {
        return new Match(
                ctx.OPTIONAL() != null,
                required(ctx, CypherParser.OC_MatchContext::oC_Pattern, CypherReader::read),
                optional(ctx, CypherParser.OC_MatchContext::oC_Where, CypherReader::read)
        );
    }

    private static MultiPartQuery read(CypherParser.OC_MultiPartQueryContext ctx) {
        return new MultiPartQuery(
                list(ctx, CypherParser.OC_MultiPartQueryContext::oc_WithClause, CypherReader::read),
                required(ctx, CypherParser.OC_MultiPartQueryContext::oC_SinglePartQuery, CypherReader::read));
    }

    private static WithClause read(CypherParser.Oc_WithClauseContext ctx) {
        return new WithClause(
                list(ctx, CypherParser.Oc_WithClauseContext::oC_ReadingClause, CypherReader::read),
                list(ctx, CypherParser.Oc_WithClauseContext::oC_UpdatingClause, CypherReader::read),
                required(ctx, CypherParser.Oc_WithClauseContext::oC_With, CypherReader::read));
    }

    private static Pattern read(CypherParser.OC_PatternContext ctx) {
        return new Pattern(
                nonemptyList(ctx, CypherParser.OC_PatternContext::oC_PatternPart, CypherReader::read));
    }

    private static PatternPart read(CypherParser.OC_PatternPartContext ctx) {
        return new PatternPart(
                optional(ctx, CypherParser.OC_PatternPartContext::oC_Variable, CypherReader::read),
                required(ctx, CypherParser.OC_PatternPartContext::oC_AnonymousPatternPart, CypherReader::read));
    }

    private static AnonymousPatternPart read(CypherParser.OC_AnonymousPatternPartContext ctx) {
        return new AnonymousPatternPart(required(ctx, CypherParser.OC_AnonymousPatternPartContext::oC_PatternElement, CypherReader::read));
    }

    private static PatternElement read(CypherParser.OC_PatternElementContext ctx) {
        Optional<NodePattern> np = optional(ctx, CypherParser.OC_PatternElementContext::oC_NodePattern, CypherReader::read);
        return np.isPresent()
                ? new PatternElement.Chained(
                new NodePatternChain(np.get(), list(ctx, CypherParser.OC_PatternElementContext::oC_PatternElementChain, CypherReader::read)))
                : new PatternElement.Parenthesized(
                required(ctx, CypherParser.OC_PatternElementContext::oC_PatternElement, CypherReader::read));
    }

    private static NodePattern read(CypherParser.OC_NodePatternContext ctx) {
        return new NodePattern(
                optional(ctx, CypherParser.OC_NodePatternContext::oC_Variable, CypherReader::read),
                optional(ctx, CypherParser.OC_NodePatternContext::oC_NodeLabels, CypherReader::read),
                optional(ctx, CypherParser.OC_NodePatternContext::oC_Properties, CypherReader::read));
    }

    private static Properties read(CypherParser.OC_PropertiesContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_PropertiesContext::oC_MapLiteral, CypherReader::read, Properties.Map::new),
                matchCase(CypherParser.OC_PropertiesContext::oC_Parameter, CypherReader::read, Properties.Parameter::new));
    }

    private static MapLiteral read(CypherParser.OC_MapLiteralContext ctx) {
        return new MapLiteral(list(ctx, CypherParser.OC_MapLiteralContext::oc_KeyValuePair, CypherReader::read));
    }

    private static KeyValuePair read(CypherParser.Oc_KeyValuePairContext ctx) {
        return new KeyValuePair(
                required(ctx, CypherParser.Oc_KeyValuePairContext::oC_PropertyKeyName, CypherReader::read),
                required(ctx, CypherParser.Oc_KeyValuePairContext::oC_Expression, CypherReader::read));
    }

    private static PropertyKeyName read(CypherParser.OC_PropertyKeyNameContext ctx) {
        return new PropertyKeyName(ctx.getText());
    }

    private static NodeLabels read(CypherParser.OC_NodeLabelsContext ctx) {
        return new NodeLabels(nonemptyList(ctx, CypherParser.OC_NodeLabelsContext::oC_NodeLabel, CypherReader::read));
    }

    private static NodeLabel read(CypherParser.OC_NodeLabelContext ctx) {
        String s = ctx.getText();
        return new NodeLabel(s.startsWith(":") ? s.substring(1) : s);
    }

    private static PatternElementChain read(CypherParser.OC_PatternElementChainContext ctx) {
        return new PatternElementChain(
                required(ctx, CypherParser.OC_PatternElementChainContext::oC_RelationshipPattern, CypherReader::read),
                required(ctx, CypherParser.OC_PatternElementChainContext::oC_NodePattern, CypherReader::read));
    }

    private static RelationshipPattern read(CypherParser.OC_RelationshipPatternContext ctx) {
        return new RelationshipPattern(
                null != ctx.oC_LeftArrowHead(),
                optional(ctx, CypherParser.OC_RelationshipPatternContext::oC_RelationshipDetail, CypherReader::read),
                null != ctx.oC_RightArrowHead());
    }

    private static RelationshipDetail read(CypherParser.OC_RelationshipDetailContext ctx) {
        return new RelationshipDetail(
                optional(ctx, CypherParser.OC_RelationshipDetailContext::oC_Variable, CypherReader::read),
                optional(ctx, CypherParser.OC_RelationshipDetailContext::oC_RelationshipTypes, CypherReader::read),
                optional(ctx, CypherParser.OC_RelationshipDetailContext::oC_RangeLiteral, CypherReader::read),
                optional(ctx, CypherParser.OC_RelationshipDetailContext::oC_Properties, CypherReader::read));
    }

    private static RangeLiteral read(CypherParser.OC_RangeLiteralContext ctx) {
        return new RangeLiteral(
                optional(ctx, CypherParser.OC_RangeLiteralContext::oC_IntegerLiteral, CypherReader::read),
                optional(ctx, CypherParser.OC_RangeLiteralContext::oC_RangeLiteralUpperBound, CypherReader::read));
    }

    private static BigInteger read(CypherParser.OC_RangeLiteralUpperBoundContext ctx) {
        return new BigInteger(ctx.getText());
    }

    private static RelationshipTypes read(CypherParser.OC_RelationshipTypesContext ctx) {
        return new RelationshipTypes(list(ctx, CypherParser.OC_RelationshipTypesContext::oC_RelTypeName, CypherReader::read));
    }

    private static RelTypeName read(CypherParser.OC_RelTypeNameContext ctx) {
        return new RelTypeName(ctx.getText());
    }

    private static Query read(CypherParser.OC_QueryContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_QueryContext::oC_RegularQuery, CypherReader::read, Query.Regular::new),
                matchCase(CypherParser.OC_QueryContext::oC_StandaloneCall, CypherReader::read, Query.Standalone::new));
    }

    private static StandaloneCall read(CypherParser.OC_StandaloneCallContext ctx) {
        return new StandaloneCall(
                required(ctx, CypherParser.OC_StandaloneCallContext::oC_ProcedureInvocation, CypherReader::read),
                optional(ctx, CypherParser.OC_StandaloneCallContext::oC_StarOrYieldItems, CypherReader::read));
    }

    private static StarOrYieldItems read(CypherParser.OC_StarOrYieldItemsContext ctx) {
        return null == ctx.oC_YieldItems()
                ? new StarOrYieldItems.Star()
                : new StarOrYieldItems.Items(required(ctx, CypherParser.OC_StarOrYieldItemsContext::oC_YieldItems, CypherReader::read));
    }


    private static ProcedureInvocation read(CypherParser.OC_ProcedureInvocationContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_ProcedureInvocationContext::oC_ExplicitProcedureInvocation, CypherReader::read, ProcedureInvocation.Explicit::new),
                matchCase(CypherParser.OC_ProcedureInvocationContext::oC_ImplicitProcedureInvocation, CypherReader::read, ProcedureInvocation.Implicit::new));
    }

    private static ImplicitProcedureInvocation read(CypherParser.OC_ImplicitProcedureInvocationContext ctx) {
        return new ImplicitProcedureInvocation(
                required(ctx, CypherParser.OC_ImplicitProcedureInvocationContext::oC_ProcedureName, CypherReader::read));
    }

    private static ReadingClause read(CypherParser.OC_ReadingClauseContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_ReadingClauseContext::oC_Match, CypherReader::read, ReadingClause.Match::new),
                matchCase(CypherParser.OC_ReadingClauseContext::oC_Unwind, CypherReader::read, ReadingClause.Unwind::new),
                matchCase(CypherParser.OC_ReadingClauseContext::oC_InQueryCall, CypherReader::read, ReadingClause.InQueryCall::new));
    }

    private static InQueryCall read(CypherParser.OC_InQueryCallContext oc_inQueryCallContext) {
        return new InQueryCall(
                required(oc_inQueryCallContext, CypherParser.OC_InQueryCallContext::oC_ExplicitProcedureInvocation, CypherReader::read),
                optional(oc_inQueryCallContext, CypherParser.OC_InQueryCallContext::oC_YieldItems, CypherReader::read));
    }

    private static YieldItems read(CypherParser.OC_YieldItemsContext ctx) {
        return new YieldItems(
                nonemptyList(ctx, CypherParser.OC_YieldItemsContext::oC_YieldItem, CypherReader::read),
                optional(ctx, CypherParser.OC_YieldItemsContext::oC_Where, CypherReader::read));
    }

    private static YieldItem read(CypherParser.OC_YieldItemContext ctx) {
        return new YieldItem(
                optional(ctx, CypherParser.OC_YieldItemContext::oC_ProcedureResultField, CypherReader::read),
                required(ctx, CypherParser.OC_YieldItemContext::oC_Variable, CypherReader::read));
    }

    private static ProcedureResultField read(CypherParser.OC_ProcedureResultFieldContext ctx) {
        return new ProcedureResultField(ctx.getText());
    }

    private static ExplicitProcedureInvocation read(CypherParser.OC_ExplicitProcedureInvocationContext ctx) {
        return new ExplicitProcedureInvocation(
                required(ctx, CypherParser.OC_ExplicitProcedureInvocationContext::oC_ProcedureName, CypherReader::read),
                list(ctx, CypherParser.OC_ExplicitProcedureInvocationContext::oC_Expression, CypherReader::read));
    }

    private static QualifiedName read(CypherParser.OC_ProcedureNameContext ctx) {
        return new QualifiedName(required(ctx, CypherParser.OC_ProcedureNameContext::oC_Namespace, CypherReader::read),
                required(ctx, CypherParser.OC_ProcedureNameContext::oC_SymbolicName, CypherReader::read));
    }

    private static Unwind read(CypherParser.OC_UnwindContext ctx) {
        return new Unwind(
                required(ctx, CypherParser.OC_UnwindContext::oC_Expression, CypherReader::read),
                required(ctx, CypherParser.OC_UnwindContext::oC_Variable, CypherReader::read));
    }

    private static RegularQuery read(CypherParser.OC_RegularQueryContext ctx) {
        SingleQuery head = required(ctx,
                CypherParser.OC_RegularQueryContext::oC_SingleQuery,
                CypherReader::read);

        List<Union> rest = list(ctx,
                CypherParser.OC_RegularQueryContext::oC_Union,
                CypherReader::read);

        return new RegularQuery(head, rest);
    }

    private static SinglePartQuery read(CypherParser.OC_SinglePartQueryContext ctx) {
        List<ReadingClause> rc = list(ctx,
                CypherParser.OC_SinglePartQueryContext::oC_ReadingClause,
                CypherReader::read);
        List<UpdatingClause> uc = list(ctx,
                CypherParser.OC_SinglePartQueryContext::oC_UpdatingClause,
                CypherReader::read);
        Optional<Return> ret = optional(ctx,
                CypherParser.OC_SinglePartQueryContext::oC_Return,
                CypherReader::read);
        return new SinglePartQuery(rc, uc, ret);
    }

    private static Return read(CypherParser.OC_ReturnContext ctx) {
        return new Return(
                required(ctx, CypherParser.OC_ReturnContext::oC_ProjectionBody, CypherReader::read));
    }

    private static ProjectionBody read(CypherParser.OC_ProjectionBodyContext ctx) {
        return new ProjectionBody(
                ctx.DISTINCT() != null,
                required(ctx, CypherParser.OC_ProjectionBodyContext::oC_ProjectionItems, CypherReader::read),
                optional(ctx, CypherParser.OC_ProjectionBodyContext::oC_Order, CypherReader::read),
                optional(ctx, CypherParser.OC_ProjectionBodyContext::oC_Skip, CypherReader::read),
                optional(ctx, CypherParser.OC_ProjectionBodyContext::oC_Limit, CypherReader::read));
    }

    private static Limit read(CypherParser.OC_LimitContext ctx) {
        return new Limit(required(ctx, CypherParser.OC_LimitContext::oC_Expression, CypherReader::read));
    }

    private static Skip read(CypherParser.OC_SkipContext ctx) {
        return new Skip(required(ctx, CypherParser.OC_SkipContext::oC_Expression, CypherReader::read));
    }

    private static Order read(CypherParser.OC_OrderContext ctx) {
        return new Order(list(ctx, CypherParser.OC_OrderContext::oC_SortItem, CypherReader::read));
    }

    private static SortItem read(CypherParser.OC_SortItemContext ctx) {
        return new SortItem(
                required(ctx, CypherParser.OC_SortItemContext::oC_Expression, CypherReader::read),
                optional(ctx, CypherParser.OC_SortItemContext::oC_SortOrder, CypherReader::read));
    }

    private static SortOrder read(CypherParser.OC_SortOrderContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_SortOrderContext::oC_SortOrder_Ascending, new SortOrder.Ascending()),
                matchCase(CypherParser.OC_SortOrderContext::oC_SortOrder_Descending, new SortOrder.Descending()));
    }

    private static ProjectionItems read(CypherParser.OC_ProjectionItemsContext ctx) {
        return new ProjectionItems(
                ctx.getText().startsWith("*"),
                list(ctx, CypherParser.OC_ProjectionItemsContext::oC_ProjectionItem, CypherReader::read));
    }

    private static ProjectionItem read(CypherParser.OC_ProjectionItemContext ctx) {
        return new ProjectionItem(
                required(ctx, CypherParser.OC_ProjectionItemContext::oC_Expression, CypherReader::read),
                optional(ctx, CypherParser.OC_ProjectionItemContext::oC_Variable, CypherReader::read));
    }

    private static UpdatingClause read(CypherParser.OC_UpdatingClauseContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_UpdatingClauseContext::oC_Create, CypherReader::read, UpdatingClause.Create::new),
                matchCase(CypherParser.OC_UpdatingClauseContext::oC_Merge, CypherReader::read, UpdatingClause.Merge::new),
                matchCase(CypherParser.OC_UpdatingClauseContext::oC_Delete, CypherReader::read, UpdatingClause.Delete::new),
                matchCase(CypherParser.OC_UpdatingClauseContext::oC_Set, CypherReader::read, UpdatingClause.Set::new),
                matchCase(CypherParser.OC_UpdatingClauseContext::oC_Remove, CypherReader::read, UpdatingClause.Remove::new));
    }

    private static Remove read(CypherParser.OC_RemoveContext ctx) {
        return new Remove(nonemptyList(ctx, CypherParser.OC_RemoveContext::oC_RemoveItem, CypherReader::read));
    }

    private static RemoveItem read(CypherParser.OC_RemoveItemContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_RemoveItemContext::oC_VariableAndNodeLabels, CypherReader::read, RemoveItem.VariableLabels::new),
                matchCase(CypherParser.OC_RemoveItemContext::oC_PropertyExpression, CypherReader::read, RemoveItem.Property::new));
    }

    private static VariableAndNodeLabels read(CypherParser.OC_VariableAndNodeLabelsContext ctx) {
        return new VariableAndNodeLabels(
                required(ctx, CypherParser.OC_VariableAndNodeLabelsContext::oC_Variable, CypherReader::read),
                required(ctx, CypherParser.OC_VariableAndNodeLabelsContext::oC_NodeLabels, CypherReader::read));
    }

    private static Set read(CypherParser.OC_SetContext ctx) {
        return new Set(list(ctx, CypherParser.OC_SetContext::oC_SetItem, CypherReader::read));
    }

    private static SetItem read(CypherParser.OC_SetItemContext ctx) {
      return match(ctx,
              matchCase(CypherParser.OC_SetItemContext::oC_SetItem_Property, CypherReader::read, SetItem.Property::new),
              matchCase(CypherParser.OC_SetItemContext::oC_SetItem_Equal, CypherReader::read, SetItem.VariableEqual::new),
              matchCase(CypherParser.OC_SetItemContext::oC_SetItem_PlusEqual, CypherReader::read, SetItem.VariablePlusEqual::new),
              matchCase(CypherParser.OC_SetItemContext::oC_SetItem_NodeLabels, CypherReader::read, SetItem.VariableLabels::new));
    }

    private static VariableAndNodeLabels read(CypherParser.OC_SetItem_NodeLabelsContext ctx) {
        return new VariableAndNodeLabels(
                required(ctx, CypherParser.OC_SetItem_NodeLabelsContext::oC_Variable, CypherReader::read),
                required(ctx, CypherParser.OC_SetItem_NodeLabelsContext::oC_NodeLabels, CypherReader::read));
    }

    private static VariablePlusEquals read(CypherParser.OC_SetItem_PlusEqualContext ctx) {
        return new VariablePlusEquals(
                required(ctx, CypherParser.OC_SetItem_PlusEqualContext::oC_Variable, CypherReader::read),
                required(ctx, CypherParser.OC_SetItem_PlusEqualContext::oC_Expression, CypherReader::read));
    }

    private static VariableEquals read(CypherParser.OC_SetItem_EqualContext ctx) {
        return new VariableEquals(
                required(ctx, CypherParser.OC_SetItem_EqualContext::oC_Variable, CypherReader::read),
                required(ctx, CypherParser.OC_SetItem_EqualContext::oC_Expression, CypherReader::read));
    }

    private static PropertyEquals read(CypherParser.OC_SetItem_PropertyContext ctx) {
        return new PropertyEquals(
                required(ctx, CypherParser.OC_SetItem_PropertyContext::oC_PropertyExpression, CypherReader::read),
                required(ctx, CypherParser.OC_SetItem_PropertyContext::oC_Expression, CypherReader::read));
    }

    private static PropertyExpression read(CypherParser.OC_PropertyExpressionContext ctx) {
        return new PropertyExpression(
                required(ctx, CypherParser.OC_PropertyExpressionContext::oC_Atom, CypherReader::read),
                nonemptyList(ctx, CypherParser.OC_PropertyExpressionContext::oC_PropertyLookup, CypherReader::read));
    }

    private static Delete read(CypherParser.OC_DeleteContext ctx) {
        return new Delete(
                ctx.DETACH() != null,
                list(ctx, CypherParser.OC_DeleteContext::oC_Expression, CypherReader::read));
    }

    private static Merge read(CypherParser.OC_MergeContext ctx) {
        return new Merge(
                required(ctx, CypherParser.OC_MergeContext::oC_PatternPart, CypherReader::read),
                list(ctx, CypherParser.OC_MergeContext::oC_MergeAction, CypherReader::read));
    }

    private static MergeAction read(CypherParser.OC_MergeActionContext ctx) {
      return new MergeAction(
              required(ctx, CypherParser.OC_MergeActionContext::oC_MatchOrCreate, CypherReader::read),
              required(ctx, CypherParser.OC_MergeActionContext::oC_Set, CypherReader::read));
    }

    private static MatchOrCreate read(CypherParser.OC_MatchOrCreateContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_MatchOrCreateContext::MATCH, new MatchOrCreate.Match()),
                matchCase(CypherParser.OC_MatchOrCreateContext::CREATE, new MatchOrCreate.Create()));
    }

    private static Create read(CypherParser.OC_CreateContext ctx) {
        return new Create(required(ctx, CypherParser.OC_CreateContext::oC_Pattern, CypherReader::read));
    }

    private static SingleQuery read(CypherParser.OC_SingleQueryContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_SingleQueryContext::oC_SinglePartQuery, CypherReader::read, SingleQuery.SinglePart::new),
                matchCase(CypherParser.OC_SingleQueryContext::oC_MultiPartQuery, CypherReader::read, SingleQuery.MultiPart::new));
    }

    private static Union read(CypherParser.OC_UnionContext ctx) {
        return new Union(
                ctx.ALL() != null,
                required(ctx, CypherParser.OC_UnionContext::oC_SingleQuery, CypherReader::read));
    }

    private static Variable read(CypherParser.OC_VariableContext ctx) {
        return new Variable(ctx.getText());
    }

    private static Where read(CypherParser.OC_WhereContext ctx) {
        return new Where(
                required(ctx, CypherParser.OC_WhereContext::oC_Expression, CypherReader::read));
    }

    private static With read(CypherParser.OC_WithContext ctx) {
        return new With(
                required(ctx, CypherParser.OC_WithContext::oC_ProjectionBody, CypherReader::read),
                optional(ctx, CypherParser.OC_WithContext::oC_Where, CypherReader::read));
    }

    // TODO: this may not match Neo4j's behavior exactly
    private static String unescapeSingleQuotedString(String input) {
        String mid = input.substring(1, input.length() - 1);
        String tempEscaped = mid.replace("\\'", "_TEMP_");
        String unescaped = StringEscapeUtils.unescapeJava(tempEscaped);
        return unescaped.replace("_TEMP_", "'");
    }
}