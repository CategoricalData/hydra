package hydra.ext.cypher;

import hydra.ext.cypher.openCypher.AddOrSubtractExpression;
import hydra.ext.cypher.openCypher.AddOrSubtractOperator;
import hydra.ext.cypher.openCypher.AddOrSubtractRightHandSide;
import hydra.ext.cypher.openCypher.AndExpression;
import hydra.ext.cypher.openCypher.AnonymousPatternPart;
import hydra.ext.cypher.openCypher.Atom;
import hydra.ext.cypher.openCypher.CaseAlternative;
import hydra.ext.cypher.openCypher.CaseExpression;
import hydra.ext.cypher.openCypher.ComparisonExpression;
import hydra.ext.cypher.openCypher.ComparisonOperator;
import hydra.ext.cypher.openCypher.Create;
import hydra.ext.cypher.openCypher.Delete;
import hydra.ext.cypher.openCypher.ExistentialSubquery;
import hydra.ext.cypher.openCypher.ExplicitProcedureInvocation;
import hydra.ext.cypher.openCypher.Expression;
import hydra.ext.cypher.openCypher.FilterExpression;
import hydra.ext.cypher.openCypher.FunctionInvocation;
import hydra.ext.cypher.openCypher.IdInColl;
import hydra.ext.cypher.openCypher.ImplicitProcedureInvocation;
import hydra.ext.cypher.openCypher.InQueryCall;
import hydra.ext.cypher.openCypher.KeyValuePair;
import hydra.ext.cypher.openCypher.Limit;
import hydra.ext.cypher.openCypher.ListComprehension;
import hydra.ext.cypher.openCypher.ListLiteral;
import hydra.ext.cypher.openCypher.ListOperatorExpression;
import hydra.ext.cypher.openCypher.ListOperatorExpressionOrPropertyLookup;
import hydra.ext.cypher.openCypher.ListPredicateExpression;
import hydra.ext.cypher.openCypher.Literal;
import hydra.ext.cypher.openCypher.MapLiteral;
import hydra.ext.cypher.openCypher.Match;
import hydra.ext.cypher.openCypher.MatchOrCreate;
import hydra.ext.cypher.openCypher.Merge;
import hydra.ext.cypher.openCypher.MergeAction;
import hydra.ext.cypher.openCypher.MultiPartQuery;
import hydra.ext.cypher.openCypher.MultiplyDivideModuloExpression;
import hydra.ext.cypher.openCypher.MultiplyDivideModuloOperator;
import hydra.ext.cypher.openCypher.MultiplyDivideModuloRightHandSide;
import hydra.ext.cypher.openCypher.NodeLabel;
import hydra.ext.cypher.openCypher.NodeLabels;
import hydra.ext.cypher.openCypher.NodePattern;
import hydra.ext.cypher.openCypher.NodePatternChain;
import hydra.ext.cypher.openCypher.NonArithmeticOperatorExpression;
import hydra.ext.cypher.openCypher.NotExpression;
import hydra.ext.cypher.openCypher.NullPredicateExpression;
import hydra.ext.cypher.openCypher.NumberLiteral;
import hydra.ext.cypher.openCypher.OrExpression;
import hydra.ext.cypher.openCypher.Order;
import hydra.ext.cypher.openCypher.Parameter;
import hydra.ext.cypher.openCypher.ParenthesizedExpression;
import hydra.ext.cypher.openCypher.PartialComparisonExpression;
import hydra.ext.cypher.openCypher.Pattern;
import hydra.ext.cypher.openCypher.PatternComprehension;
import hydra.ext.cypher.openCypher.PatternElement;
import hydra.ext.cypher.openCypher.PatternElementChain;
import hydra.ext.cypher.openCypher.PatternPart;
import hydra.ext.cypher.openCypher.PatternPredicate;
import hydra.ext.cypher.openCypher.PatternWhere;
import hydra.ext.cypher.openCypher.PowerOfExpression;
import hydra.ext.cypher.openCypher.ProcedureInvocation;
import hydra.ext.cypher.openCypher.ProcedureResultField;
import hydra.ext.cypher.openCypher.ProjectionBody;
import hydra.ext.cypher.openCypher.ProjectionItem;
import hydra.ext.cypher.openCypher.ProjectionItems;
import hydra.ext.cypher.openCypher.Properties;
import hydra.ext.cypher.openCypher.PropertyEquals;
import hydra.ext.cypher.openCypher.PropertyExpression;
import hydra.ext.cypher.openCypher.PropertyKeyName;
import hydra.ext.cypher.openCypher.PropertyLookup;
import hydra.ext.cypher.openCypher.QualifiedName;
import hydra.ext.cypher.openCypher.Quantifier;
import hydra.ext.cypher.openCypher.QuantifierOperator;
import hydra.ext.cypher.openCypher.Query;
import hydra.ext.cypher.openCypher.RangeExpression;
import hydra.ext.cypher.openCypher.RangeLiteral;
import hydra.ext.cypher.openCypher.ReadingClause;
import hydra.ext.cypher.openCypher.RegularQuery;
import hydra.ext.cypher.openCypher.RelTypeName;
import hydra.ext.cypher.openCypher.RelationshipDetail;
import hydra.ext.cypher.openCypher.RelationshipPattern;
import hydra.ext.cypher.openCypher.RelationshipTypes;
import hydra.ext.cypher.openCypher.RelationshipsPattern;
import hydra.ext.cypher.openCypher.Remove;
import hydra.ext.cypher.openCypher.RemoveItem;
import hydra.ext.cypher.openCypher.Return;
import hydra.ext.cypher.openCypher.Set;
import hydra.ext.cypher.openCypher.SetItem;
import hydra.ext.cypher.openCypher.SinglePartQuery;
import hydra.ext.cypher.openCypher.SingleQuery;
import hydra.ext.cypher.openCypher.Skip;
import hydra.ext.cypher.openCypher.SortItem;
import hydra.ext.cypher.openCypher.SortOrder;
import hydra.ext.cypher.openCypher.StandaloneCall;
import hydra.ext.cypher.openCypher.StarOrYieldItems;
import hydra.ext.cypher.openCypher.StringListNullPredicateExpression;
import hydra.ext.cypher.openCypher.StringListNullPredicateRightHandSide;
import hydra.ext.cypher.openCypher.StringLiteral;
import hydra.ext.cypher.openCypher.StringPredicateExpression;
import hydra.ext.cypher.openCypher.StringPredicateOperator;
import hydra.ext.cypher.openCypher.UnaryAddOrSubtractExpression;
import hydra.ext.cypher.openCypher.Union;
import hydra.ext.cypher.openCypher.Unwind;
import hydra.ext.cypher.openCypher.UpdatingClause;
import hydra.ext.cypher.openCypher.Variable;
import hydra.ext.cypher.openCypher.VariableAndNodeLabels;
import hydra.ext.cypher.openCypher.VariableEquals;
import hydra.ext.cypher.openCypher.VariablePlusEquals;
import hydra.ext.cypher.openCypher.Where;
import hydra.ext.cypher.openCypher.With;
import hydra.ext.cypher.openCypher.WithClause;
import hydra.ext.cypher.openCypher.XorExpression;
import hydra.ext.cypher.openCypher.YieldItem;
import hydra.ext.cypher.openCypher.YieldItems;
import hydra.tools.AntlrReaderBase;
import org.antlr.v4.runtime.CharStreams;
import org.antlr.v4.runtime.CommonTokenStream;
import org.antlr.v4.runtime.tree.TerminalNode;
import org.apache.commons.text.StringEscapeUtils;
import org.neo4j.CypherLexer;
import org.neo4j.CypherParser;

import java.math.BigInteger;
import java.util.List;

import hydra.util.Maybe;

/**
 * A parser which constructs Cypher queries using the hydra.ext.cypher.openCypher model.
 *
 * <p>Notes/limitations:
 * * Many syntax features are not yet implemented; the parser will throw an exception when it encounters them.
 * * The encodings for strings, doubles, and integers are inherited from Java and may not completely agree
 * with the OpenCypher specification.
 */
public class CypherReader extends AntlrReaderBase {

    /**
     * Parse a Cypher query string into a Query object.
     *
     * @param query the Cypher query string to parse
     * @return the parsed Query object
     */
    public static Query read(String query) {
        CypherLexer lexer = new CypherLexer(CharStreams.fromString(query));
        CommonTokenStream tokens = new CommonTokenStream(lexer);
        CypherParser parser = new CypherParser(tokens);
        return read(parser.oC_Query());
    }

    ////////////////////////////////////

    /**
     * Parse an expression context into an Expression.
     *
     * @param ctx the ANTLR expression context
     * @return the parsed Expression
     */
    private static Expression read(CypherParser.OC_ExpressionContext ctx) {
        return new Expression(required(ctx, CypherParser.OC_ExpressionContext::oC_OrExpression, CypherReader::read));
    }

    /**
     * Parse an OR expression context into an OrExpression.
     *
     * @param ctx the ANTLR OR expression context
     * @return the parsed OrExpression
     */
    private static OrExpression read(CypherParser.OC_OrExpressionContext ctx) {
        return new OrExpression(list(ctx, CypherParser.OC_OrExpressionContext::oC_XorExpression, CypherReader::read));
    }

    /**
     * Parse an XOR expression context into an XorExpression.
     *
     * @param ctx the ANTLR XOR expression context
     * @return the parsed XorExpression
     */
    private static XorExpression read(CypherParser.OC_XorExpressionContext ctx) {
        return new XorExpression(list(ctx, CypherParser.OC_XorExpressionContext::oC_AndExpression, CypherReader::read));
    }

    /**
     * Parse an AND expression context into an AndExpression.
     *
     * @param ctx the ANTLR AND expression context
     * @return the parsed AndExpression
     */
    private static AndExpression read(CypherParser.OC_AndExpressionContext ctx) {
        return new AndExpression(list(ctx, CypherParser.OC_AndExpressionContext::oC_NotExpression, CypherReader::read));
    }

    /**
     * Parse a NOT expression context into a NotExpression.
     *
     * @param ctx the ANTLR NOT expression context
     * @return the parsed NotExpression
     */
    private static NotExpression read(CypherParser.OC_NotExpressionContext ctx) {
        return new NotExpression(
                ctx.NOT() != null && ctx.NOT().size() > 0,
                required(ctx, CypherParser.OC_NotExpressionContext::oC_ComparisonExpression, CypherReader::read)
        );
    }

    /**
     * Parse a comparison expression context into a ComparisonExpression.
     *
     * @param ctx the ANTLR comparison expression context
     * @return the parsed ComparisonExpression
     */
    private static ComparisonExpression read(CypherParser.OC_ComparisonExpressionContext ctx) {
        return new ComparisonExpression(
                required(ctx, CypherParser.OC_ComparisonExpressionContext::oC_StringListNullPredicateExpression,
                        CypherReader::read),
                list(ctx, CypherParser.OC_ComparisonExpressionContext::oC_PartialComparisonExpression,
                        CypherReader::read));
    }

    /**
     * Parse a partial comparison expression context into a PartialComparisonExpression.
     *
     * @param ctx the ANTLR partial comparison expression context
     * @return the parsed PartialComparisonExpression
     */
    private static PartialComparisonExpression read(CypherParser.OC_PartialComparisonExpressionContext ctx) {
        return new PartialComparisonExpression(
                required(ctx, CypherParser.OC_PartialComparisonExpressionContext::oC_ComparisonOperator,
                        CypherReader::read),
                required(ctx, CypherParser.OC_PartialComparisonExpressionContext::oC_StringListNullPredicateExpression,
                        CypherReader::read));
    }

    /**
     * Parse a comparison operator context into a ComparisonOperator.
     *
     * @param ctx the ANTLR comparison operator context
     * @return the parsed ComparisonOperator
     */
    private static ComparisonOperator read(CypherParser.OC_ComparisonOperatorContext ctx) {
        switch (ctx.getText()) {
            case "=":
                return new ComparisonOperator.Eq(false);
            case "<>":
                return new ComparisonOperator.Neq(false);
            case "<":
                return new ComparisonOperator.Lt(false);
            case ">":
                return new ComparisonOperator.Gt(false);
            case "<=":
                return new ComparisonOperator.Lte(false);
            case ">=":
                return new ComparisonOperator.Gte(false);
            default:
                return invalid("Unknown comparison operator: " + ctx.getText());
        }
    }

    /**
     * Parse a string/list/null predicate expression context into a StringListNullPredicateExpression.
     *
     * @param ctx the ANTLR string/list/null predicate expression context
     * @return the parsed StringListNullPredicateExpression
     */
    private static StringListNullPredicateExpression read(
            CypherParser.OC_StringListNullPredicateExpressionContext ctx) {
        return new StringListNullPredicateExpression(
                required(ctx,
                        CypherParser.OC_StringListNullPredicateExpressionContext::oC_AddOrSubtractExpression,
                        CypherReader::read),
                list(ctx, CypherParser.OC_StringListNullPredicateExpressionContext
                        ::oC_StringListNullPredicateExpression_RHS, CypherReader::read));
    }

    /**
     * Parse a string/list/null predicate right-hand side context into a StringListNullPredicateRightHandSide.
     *
     * @param ctx the ANTLR string/list/null predicate right-hand side context
     * @return the parsed StringListNullPredicateRightHandSide
     */
    private static StringListNullPredicateRightHandSide read(
            CypherParser.OC_StringListNullPredicateExpression_RHSContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_StringListNullPredicateExpression_RHSContext::oC_StringPredicateExpression,
                        CypherReader::read, StringListNullPredicateRightHandSide.String_::new),
                matchCase(CypherParser.OC_StringListNullPredicateExpression_RHSContext::oC_ListPredicateExpression,
                        CypherReader::read, StringListNullPredicateRightHandSide.List::new),
                matchCase(CypherParser.OC_StringListNullPredicateExpression_RHSContext::oC_NullPredicateExpression,
                        CypherReader::read, StringListNullPredicateRightHandSide.Null::new));
    }

    /**
     * Parse a string predicate expression context into a StringPredicateExpression.
     *
     * @param ctx the ANTLR string predicate expression context
     * @return the parsed StringPredicateExpression
     */
    private static StringPredicateExpression read(CypherParser.OC_StringPredicateExpressionContext ctx) {
        return new StringPredicateExpression(
                required(ctx, CypherParser.OC_StringPredicateExpressionContext::oC_StringPredicateOperator,
                        CypherReader::read),
                required(ctx, CypherParser.OC_StringPredicateExpressionContext::oC_AddOrSubtractExpression,
                        CypherReader::read));
    }

    /**
     * Parse a string predicate operator context into a StringPredicateOperator.
     *
     * @param ctx the ANTLR string predicate operator context
     * @return the parsed StringPredicateOperator
     */
    private static StringPredicateOperator read(CypherParser.OC_StringPredicateOperatorContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_StringPredicateOperatorContext::STARTS,
                        new StringPredicateOperator.StartsWith(false)),
                matchCase(CypherParser.OC_StringPredicateOperatorContext::ENDS,
                        new StringPredicateOperator.EndsWith(false)),
                matchCase(CypherParser.OC_StringPredicateOperatorContext::CONTAINS,
                        new StringPredicateOperator.Contains(false)));
    }

    /**
     * Parse a list predicate expression context into a ListPredicateExpression.
     *
     * @param ctx the ANTLR list predicate expression context
     * @return the parsed ListPredicateExpression
     */
    private static ListPredicateExpression read(CypherParser.OC_ListPredicateExpressionContext ctx) {
        return new ListPredicateExpression(
                required(ctx, CypherParser.OC_ListPredicateExpressionContext::oC_AddOrSubtractExpression,
                        CypherReader::read));
    }

    /**
     * Parse a null predicate expression context into a NullPredicateExpression.
     *
     * @param ctx the ANTLR null predicate expression context
     * @return the parsed NullPredicateExpression
     */
    private static NullPredicateExpression read(CypherParser.OC_NullPredicateExpressionContext ctx) {
        return new NullPredicateExpression(ctx.NOT() == null);
    }

    /**
     * Parse an add or subtract expression context into an AddOrSubtractExpression.
     *
     * @param ctx the ANTLR add or subtract expression context
     * @return the parsed AddOrSubtractExpression
     */
    private static AddOrSubtractExpression read(CypherParser.OC_AddOrSubtractExpressionContext ctx) {
        return new AddOrSubtractExpression(
                required(ctx, CypherParser.OC_AddOrSubtractExpressionContext::oC_MultiplyDivideModuloExpression,
                        CypherReader::read),
                list(ctx, CypherParser.OC_AddOrSubtractExpressionContext::oC_AddOrSubtractExpression_RHS,
                        CypherReader::read));
    }

    /**
     * Parse an add or subtract right-hand side context into an AddOrSubtractRightHandSide.
     *
     * @param ctx the ANTLR add or subtract right-hand side context
     * @return the parsed AddOrSubtractRightHandSide
     */
    private static AddOrSubtractRightHandSide read(CypherParser.OC_AddOrSubtractExpression_RHSContext ctx) {
        return new AddOrSubtractRightHandSide(
                required(ctx, CypherParser.OC_AddOrSubtractExpression_RHSContext::oC_AddOrSubtractExpression_Operator,
                        CypherReader::read),
                required(ctx, CypherParser.OC_AddOrSubtractExpression_RHSContext::oC_MultiplyDivideModuloExpression,
                        CypherReader::read));
    }

    /**
     * Parse an add or subtract operator context into an AddOrSubtractOperator.
     *
     * @param ctx the ANTLR add or subtract operator context
     * @return the parsed AddOrSubtractOperator
     */
    private static AddOrSubtractOperator read(CypherParser.OC_AddOrSubtractExpression_OperatorContext ctx) {
        switch (ctx.getText()) {
            case "+":
                return new AddOrSubtractOperator.Add(false);
            case "-":
                return new AddOrSubtractOperator.Subtract(false);
            default:
                return invalid("Unknown add or subtract operator: " + ctx.getText());
        }
    }

    /**
     * Parse a multiply/divide/modulo expression context into a MultiplyDivideModuloExpression.
     *
     * @param ctx the ANTLR multiply/divide/modulo expression context
     * @return the parsed MultiplyDivideModuloExpression
     */
    private static MultiplyDivideModuloExpression read(CypherParser.OC_MultiplyDivideModuloExpressionContext ctx) {
        return new MultiplyDivideModuloExpression(
                required(ctx, CypherParser.OC_MultiplyDivideModuloExpressionContext::oC_PowerOfExpression,
                        CypherReader::read),
                list(ctx, CypherParser.OC_MultiplyDivideModuloExpressionContext::oC_MultiplyDivideModuloExpression_RHS,
                        CypherReader::read));
    }

    /**
     * Parse a multiply/divide/modulo right-hand side context into a MultiplyDivideModuloRightHandSide.
     *
     * @param ctx the ANTLR multiply/divide/modulo right-hand side context
     * @return the parsed MultiplyDivideModuloRightHandSide
     */
    private static MultiplyDivideModuloRightHandSide read(
            CypherParser.OC_MultiplyDivideModuloExpression_RHSContext ctx) {
        return new MultiplyDivideModuloRightHandSide(
                required(ctx, CypherParser.OC_MultiplyDivideModuloExpression_RHSContext
                        ::oC_MultiplyDivideModuloExpression_Operator, CypherReader::read),
                required(ctx, CypherParser.OC_MultiplyDivideModuloExpression_RHSContext
                        ::oC_PowerOfExpression, CypherReader::read));
    }

    /**
     * Parse a multiply/divide/modulo operator context into a MultiplyDivideModuloOperator.
     *
     * @param ctx the ANTLR multiply/divide/modulo operator context
     * @return the parsed MultiplyDivideModuloOperator
     */
    private static MultiplyDivideModuloOperator read(
            CypherParser.OC_MultiplyDivideModuloExpression_OperatorContext ctx) {
        switch (ctx.getText()) {
            case "*":
                return new MultiplyDivideModuloOperator.Multiply(false);
            case "/":
                return new MultiplyDivideModuloOperator.Divide(false);
            case "%":
                return new MultiplyDivideModuloOperator.Modulo(false);
            default:
                return invalid("Unknown multiply, divide or modulo operator: " + ctx.getText());
        }
    }

    /**
     * Parse a power-of expression context into a PowerOfExpression.
     *
     * @param ctx the ANTLR power-of expression context
     * @return the parsed PowerOfExpression
     */
    private static PowerOfExpression read(CypherParser.OC_PowerOfExpressionContext ctx) {
        return new PowerOfExpression(
                list(ctx, CypherParser.OC_PowerOfExpressionContext::oC_UnaryAddOrSubtractExpression,
                        CypherReader::read));
    }

    /**
     * Parse a unary add or subtract expression context into a UnaryAddOrSubtractExpression.
     *
     * @param ctx the ANTLR unary add or subtract expression context
     * @return the parsed UnaryAddOrSubtractExpression
     */
    private static UnaryAddOrSubtractExpression read(CypherParser.OC_UnaryAddOrSubtractExpressionContext ctx) {
        return new UnaryAddOrSubtractExpression(
                optional(ctx, CypherParser.OC_UnaryAddOrSubtractExpressionContext
                        ::oC_UnaryAddOrSubtractExpression_Operator, CypherReader::read),
                required(ctx, CypherParser.OC_UnaryAddOrSubtractExpressionContext
                        ::oC_NonArithmeticOperatorExpression, CypherReader::read));
    }

    /**
     * Parse a non-arithmetic operator expression context into a NonArithmeticOperatorExpression.
     *
     * @param ctx the ANTLR non-arithmetic operator expression context
     * @return the parsed NonArithmeticOperatorExpression
     */
    private static NonArithmeticOperatorExpression read(CypherParser.OC_NonArithmeticOperatorExpressionContext ctx) {
        return new NonArithmeticOperatorExpression(
                required(ctx, CypherParser.OC_NonArithmeticOperatorExpressionContext::oC_Atom, CypherReader::read),
                list(ctx, CypherParser.OC_NonArithmeticOperatorExpressionContext
                        ::oC_ListOperatorExpressionOrPropertyLookup, CypherReader::read),
                optional(ctx, CypherParser.OC_NonArithmeticOperatorExpressionContext
                        ::oC_NodeLabels, CypherReader::read));
    }

    /**
     * Parse a list operator expression or property lookup context.
     *
     * @param ctx the ANTLR list operator expression or property lookup context
     * @return the parsed ListOperatorExpressionOrPropertyLookup
     */
    private static ListOperatorExpressionOrPropertyLookup read(
            CypherParser.OC_ListOperatorExpressionOrPropertyLookupContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_ListOperatorExpressionOrPropertyLookupContext::oC_ListOperatorExpression,
                        CypherReader::read, ListOperatorExpressionOrPropertyLookup.List::new),
                matchCase(CypherParser.OC_ListOperatorExpressionOrPropertyLookupContext::oC_PropertyLookup,
                        CypherReader::read, ListOperatorExpressionOrPropertyLookup.Property::new));
    }

    /**
     * Parse a property lookup context into a PropertyLookup.
     *
     * @param ctx the ANTLR property lookup context
     * @return the parsed PropertyLookup
     */
    private static PropertyLookup read(CypherParser.OC_PropertyLookupContext ctx) {
        return new PropertyLookup(required(ctx, CypherParser.OC_PropertyLookupContext::oC_PropertyKeyName,
                CypherReader::read));
    }

    /**
     * Parse a list operator expression context into a ListOperatorExpression.
     *
     * @param ctx the ANTLR list operator expression context
     * @return the parsed ListOperatorExpression
     */
    private static ListOperatorExpression read(CypherParser.OC_ListOperatorExpressionContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_ListOperatorExpressionContext::oC_ListOperatorExpressionSingle,
                        CypherReader::read, ListOperatorExpression.Single::new),
                matchCase(CypherParser.OC_ListOperatorExpressionContext::oC_ListOperatorExpressionRange,
                        CypherReader::read, ListOperatorExpression.Range::new));
    }

    /**
     * Parse a range expression context into a RangeExpression.
     *
     * @param ctx the ANTLR range expression context
     * @return the parsed RangeExpression
     */
    private static RangeExpression read(CypherParser.OC_ListOperatorExpressionRangeContext ctx) {
        return new RangeExpression(
                required(ctx, CypherParser.OC_ListOperatorExpressionRangeContext::oC_ListOperatorExpressionRangeLHS,
                        CypherReader::read),
                required(ctx, CypherParser.OC_ListOperatorExpressionRangeContext::oC_ListOperatorExpressionRangeRHS,
                        CypherReader::read));
    }

    /**
     * Parse a range expression right-hand side context into an optional Expression.
     *
     * @param ctx the ANTLR range expression right-hand side context
     * @return the parsed optional Expression
     */
    private static Maybe<Expression> read(CypherParser.OC_ListOperatorExpressionRangeRHSContext ctx) {
        return optional(ctx, CypherParser.OC_ListOperatorExpressionRangeRHSContext::oC_Expression, CypherReader::read);
    }

    /**
     * Parse a range expression left-hand side context into an optional Expression.
     *
     * @param ctx the ANTLR range expression left-hand side context
     * @return the parsed optional Expression
     */
    private static Maybe<Expression> read(CypherParser.OC_ListOperatorExpressionRangeLHSContext ctx) {
        return optional(ctx, CypherParser.OC_ListOperatorExpressionRangeLHSContext::oC_Expression, CypherReader::read);
    }

    /**
     * Parse a single list operator expression context into an Expression.
     *
     * @param ctx the ANTLR single list operator expression context
     * @return the parsed Expression
     */
    private static Expression read(CypherParser.OC_ListOperatorExpressionSingleContext ctx) {
        return required(ctx, CypherParser.OC_ListOperatorExpressionSingleContext::oC_Expression, CypherReader::read);
    }

    /**
     * Parse an atom context into an Atom.
     *
     * @param ctx the ANTLR atom context
     * @return the parsed Atom
     */
    private static Atom read(CypherParser.OC_AtomContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_AtomContext::oC_Literal, CypherReader::read, Atom.Literal::new),
                matchCase(CypherParser.OC_AtomContext::oC_Parameter, CypherReader::read, Atom.Parameter::new),
                matchCase(CypherParser.OC_AtomContext::oC_CaseExpression, CypherReader::read, Atom.Case::new),
                matchCase(CypherParser.OC_AtomContext::COUNT, new Atom.CountStar(false)),
                matchCase(CypherParser.OC_AtomContext::oC_ListComprehension, CypherReader::read,
                        Atom.ListComprehension::new),
                matchCase(CypherParser.OC_AtomContext::oC_PatternComprehension, CypherReader::read,
                        Atom.PatternComprehension::new),
                matchCase(CypherParser.OC_AtomContext::oC_Quantifier, CypherReader::read, Atom.Quantifier::new),
                matchCase(CypherParser.OC_AtomContext::oC_PatternPredicate, CypherReader::read,
                        Atom.PatternPredicate::new),
                matchCase(CypherParser.OC_AtomContext::oC_ParenthesizedExpression, CypherReader::read,
                        Atom.Parenthesized::new),
                matchCase(CypherParser.OC_AtomContext::oC_FunctionInvocation, CypherReader::read,
                        Atom.FunctionInvocation::new),
                matchCase(CypherParser.OC_AtomContext::oC_ExistentialSubquery, CypherReader::read,
                        Atom.ExistentialSubquery::new),
                matchCase(CypherParser.OC_AtomContext::oC_Variable, CypherReader::read, Atom.Variable::new));
    }

    /**
     * Parse an existential subquery context into an ExistentialSubquery.
     *
     * @param ctx the ANTLR existential subquery context
     * @return the parsed ExistentialSubquery
     */
    private static ExistentialSubquery read(CypherParser.OC_ExistentialSubqueryContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_ExistentialSubqueryContext::oC_RegularQuery, CypherReader::read,
                        ExistentialSubquery.Regular::new),
                matchCase(CypherParser.OC_ExistentialSubqueryContext::oC_PatternWhere, CypherReader::read,
                        ExistentialSubquery.Pattern::new));
    }

    /**
     * Parse a pattern-where context into a PatternWhere.
     *
     * @param ctx the ANTLR pattern-where context
     * @return the parsed PatternWhere
     */
    private static PatternWhere read(CypherParser.OC_PatternWhereContext ctx) {
        return new PatternWhere(
                required(ctx, CypherParser.OC_PatternWhereContext::oC_Pattern, CypherReader::read),
                optional(ctx, CypherParser.OC_PatternWhereContext::oC_Where, CypherReader::read));
    }

    /**
     * Parse a function invocation context into a FunctionInvocation.
     *
     * @param ctx the ANTLR function invocation context
     * @return the parsed FunctionInvocation
     */
    private static FunctionInvocation read(CypherParser.OC_FunctionInvocationContext ctx) {
        return new FunctionInvocation(
                required(ctx, CypherParser.OC_FunctionInvocationContext::oC_FunctionName, CypherReader::read),
                ctx.DISTINCT() != null,
                list(ctx, CypherParser.OC_FunctionInvocationContext::oC_Expression, CypherReader::read));
    }

    /**
     * Parse a function name context into a QualifiedName.
     *
     * @param ctx the ANTLR function name context
     * @return the parsed QualifiedName
     */
    private static QualifiedName read(CypherParser.OC_FunctionNameContext ctx) {
        return new QualifiedName(
                required(ctx, CypherParser.OC_FunctionNameContext::oC_Namespace, CypherReader::read),
                required(ctx, CypherParser.OC_FunctionNameContext::oC_SymbolicName, CypherReader::read));
    }

    /**
     * Parse a symbolic name context into a String.
     *
     * @param ctx the ANTLR symbolic name context
     * @return the symbolic name as a String
     */
    private static String read(CypherParser.OC_SymbolicNameContext ctx) {
        return ctx.getText();
    }

    /**
     * Parse a namespace context into a String.
     *
     * @param ctx the ANTLR namespace context
     * @return the namespace as a String
     */
    private static String read(CypherParser.OC_NamespaceContext ctx) {
        return ctx.getText();
    }

    /**
     * Parse a parenthesized expression context into a ParenthesizedExpression.
     *
     * @param ctx the ANTLR parenthesized expression context
     * @return the parsed ParenthesizedExpression
     */
    private static ParenthesizedExpression read(CypherParser.OC_ParenthesizedExpressionContext ctx) {
        return new ParenthesizedExpression(
                required(ctx, CypherParser.OC_ParenthesizedExpressionContext::oC_Expression, CypherReader::read));
    }

    /**
     * Parse a quantifier context into a Quantifier.
     *
     * @param ctx the ANTLR quantifier context
     * @return the parsed Quantifier
     */
    private static Quantifier read(CypherParser.OC_QuantifierContext ctx) {
        return new Quantifier(
                required(ctx, CypherParser.OC_QuantifierContext::oC_QuantifierOperator, CypherReader::read),
                required(ctx, CypherParser.OC_QuantifierContext::oC_FilterExpression, CypherReader::read));
    }

    /**
     * Parse a filter expression context into a FilterExpression.
     *
     * @param ctx the ANTLR filter expression context
     * @return the parsed FilterExpression
     */
    private static FilterExpression read(CypherParser.OC_FilterExpressionContext ctx) {
        return new FilterExpression(
                required(ctx, CypherParser.OC_FilterExpressionContext::oC_IdInColl, CypherReader::read),
                optional(ctx, CypherParser.OC_FilterExpressionContext::oC_Where, CypherReader::read));
    }

    /**
     * Parse an id-in-collection context into an IdInColl.
     *
     * @param ctx the ANTLR id-in-collection context
     * @return the parsed IdInColl
     */
    private static IdInColl read(CypherParser.OC_IdInCollContext ctx) {
        return new IdInColl(
                required(ctx, CypherParser.OC_IdInCollContext::oC_Variable, CypherReader::read),
                required(ctx, CypherParser.OC_IdInCollContext::oC_Expression, CypherReader::read));
    }

    /**
     * Parse a quantifier operator context into a QuantifierOperator.
     *
     * @param ctx the ANTLR quantifier operator context
     * @return the parsed QuantifierOperator
     */
    private static QuantifierOperator read(CypherParser.OC_QuantifierOperatorContext ctx) {
        switch (ctx.getText()) {
            case "ALL":
                return new QuantifierOperator.All(false);
            case "ANY":
                return new QuantifierOperator.Any(false);
            case "NONE":
                return new QuantifierOperator.None(false);
            case "SINGLE":
                return new QuantifierOperator.Single(false);
            default:
                throw new IllegalArgumentException("Unknown quantifier operator: " + ctx.getText());
        }
    }

    /**
     * Parse a pattern predicate context into a PatternPredicate.
     *
     * @param ctx the ANTLR pattern predicate context
     * @return the parsed PatternPredicate
     */
    private static PatternPredicate read(CypherParser.OC_PatternPredicateContext ctx) {
        return new PatternPredicate(required(ctx, CypherParser.OC_PatternPredicateContext::oC_RelationshipsPattern,
                CypherReader::read));
    }

    /**
     * Parse a relationships pattern context into a RelationshipsPattern.
     *
     * @param ctx the ANTLR relationships pattern context
     * @return the parsed RelationshipsPattern
     */
    private static RelationshipsPattern read(CypherParser.OC_RelationshipsPatternContext ctx) {
        return new RelationshipsPattern(
                required(ctx, CypherParser.OC_RelationshipsPatternContext::oC_NodePattern, CypherReader::read),
                nonemptyList(ctx, CypherParser.OC_RelationshipsPatternContext::oC_PatternElementChain,
                        CypherReader::read));
    }

    /**
     * Parse a pattern comprehension context into a PatternComprehension.
     *
     * @param ctx the ANTLR pattern comprehension context
     * @return the parsed PatternComprehension
     */
    private static PatternComprehension read(CypherParser.OC_PatternComprehensionContext ctx) {
        return new PatternComprehension(
                optional(ctx, CypherParser.OC_PatternComprehensionContext::oC_Variable, CypherReader::read),
                required(ctx, CypherParser.OC_PatternComprehensionContext::oC_RelationshipsPattern, CypherReader::read),
                optional(ctx, CypherParser.OC_PatternComprehensionContext::oC_Where, CypherReader::read),
                required(ctx, CypherParser.OC_PatternComprehensionContext::oC_Expression, CypherReader::read));
    }

    /**
     * Parse a list comprehension context into a ListComprehension.
     *
     * @param ctx the ANTLR list comprehension context
     * @return the parsed ListComprehension
     */
    private static ListComprehension read(CypherParser.OC_ListComprehensionContext ctx) {
        return new ListComprehension(
                required(ctx, CypherParser.OC_ListComprehensionContext::oC_FilterExpression, CypherReader::read),
                optional(ctx, CypherParser.OC_ListComprehensionContext::oC_Expression, CypherReader::read));
    }

    /**
     * Parse a case expression context into a CaseExpression.
     *
     * @param ctx the ANTLR case expression context
     * @return the parsed CaseExpression
     */
    private static CaseExpression read(CypherParser.OC_CaseExpressionContext ctx) {
        return new CaseExpression(optional(ctx, CypherParser.OC_CaseExpressionContext::oC_Expression,
                CypherReader::read),
                list(ctx, CypherParser.OC_CaseExpressionContext::oC_CaseAlternative, CypherReader::read),
                optional(ctx, CypherParser.OC_CaseExpressionContext::oc_CaseElse, CypherReader::read));
    }

    /**
     * Parse a case-else context into an Expression.
     *
     * @param ctx the ANTLR case-else context
     * @return the parsed Expression
     */
    private static Expression read(CypherParser.Oc_CaseElseContext ctx) {
        return required(ctx, CypherParser.Oc_CaseElseContext::oC_Expression, CypherReader::read);
    }

    /**
     * Parse a case alternative context into a CaseAlternative.
     *
     * @param ctx the ANTLR case alternative context
     * @return the parsed CaseAlternative
     */
    private static CaseAlternative read(CypherParser.OC_CaseAlternativeContext ctx) {
        return new CaseAlternative(
                required(ctx, 0, CypherParser.OC_CaseAlternativeContext::oC_Expression, CypherReader::read),
                required(ctx, 1, CypherParser.OC_CaseAlternativeContext::oC_Expression, CypherReader::read));
    }

    /**
     * Parse a parameter context into a Parameter.
     *
     * @param ctx the ANTLR parameter context
     * @return the parsed Parameter
     */
    private static Parameter read(CypherParser.OC_ParameterContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_ParameterContext::oC_SymbolicName, CypherReader::read,
                        Parameter.Symbolic::new),
                matchCase(CypherParser.OC_ParameterContext::DecimalInteger, c -> new BigInteger(c.getText()),
                        Parameter.Integer_::new));
    }

    /**
     * Parse a literal context into a Literal.
     *
     * @param ctx the ANTLR literal context
     * @return the parsed Literal
     */
    private static Literal read(CypherParser.OC_LiteralContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_LiteralContext::oC_BooleanLiteral, CypherReader::read, Literal.Boolean_::new),
                matchCase(CypherParser.OC_LiteralContext::NULL, new Literal.Null(false)),
                matchCase(CypherParser.OC_LiteralContext::oC_NumberLiteral, CypherReader::read, Literal.Number_::new),
                matchCase(CypherParser.OC_LiteralContext::StringLiteral, CypherReader::read, Literal.String_::new),
                matchCase(CypherParser.OC_LiteralContext::oC_ListLiteral, CypherReader::read, Literal.List::new),
                matchCase(CypherParser.OC_LiteralContext::oC_MapLiteral, CypherReader::read, Literal.Map::new));
    }

    /**
     * Parse a list literal context into a ListLiteral.
     *
     * @param ctx the ANTLR list literal context
     * @return the parsed ListLiteral
     */
    private static ListLiteral read(CypherParser.OC_ListLiteralContext ctx) {
        return new ListLiteral(list(ctx, CypherParser.OC_ListLiteralContext::oC_Expression, CypherReader::read));
    }

    /**
     * Parse a terminal node (string literal) into a StringLiteral.
     *
     * @param n the ANTLR terminal node
     * @return the parsed StringLiteral
     */
    private static StringLiteral read(TerminalNode n) {
        return new StringLiteral(unescapeSingleQuotedString(n.getText()));
    }

    /**
     * Parse a number literal context into a NumberLiteral.
     *
     * @param ctx the ANTLR number literal context
     * @return the parsed NumberLiteral
     */
    private static NumberLiteral read(CypherParser.OC_NumberLiteralContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_NumberLiteralContext::oC_DoubleLiteral, CypherReader::read,
                        NumberLiteral.Double_::new),
                matchCase(CypherParser.OC_NumberLiteralContext::oC_IntegerLiteral, CypherReader::read,
                        NumberLiteral.Integer_::new));
    }

    /**
     * Parse an integer literal context into a BigInteger.
     *
     * @param ctx the ANTLR integer literal context
     * @return the parsed BigInteger
     */
    private static BigInteger read(CypherParser.OC_IntegerLiteralContext ctx) {
        return new BigInteger(ctx.getText());
    }

    /**
     * Parse a double literal context into a Double.
     *
     * @param ctx the ANTLR double literal context
     * @return the parsed Double
     */
    private static Double read(CypherParser.OC_DoubleLiteralContext ctx) {
        return Double.parseDouble(ctx.getText());
    }

    /**
     * Parse a boolean literal context into a Boolean.
     *
     * @param ctx the ANTLR boolean literal context
     * @return the parsed Boolean
     */
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

    /**
     * Parse a unary add or subtract operator context into an AddOrSubtractOperator.
     *
     * @param ctx the ANTLR unary add or subtract operator context
     * @return the parsed AddOrSubtractOperator
     */
    private static AddOrSubtractOperator read(CypherParser.OC_UnaryAddOrSubtractExpression_OperatorContext ctx) {
        switch (ctx.getText()) {
            case "+":
                return new AddOrSubtractOperator.Add(false);
            case "-":
                return new AddOrSubtractOperator.Subtract(false);
            default:
                return invalid("Unknown unary add or subtract operator: " + ctx.getText());
        }
    }

    /**
     * Parse a match context into a Match.
     *
     * @param ctx the ANTLR match context
     * @return the parsed Match
     */
    private static Match read(CypherParser.OC_MatchContext ctx) {
        return new Match(
                ctx.OPTIONAL() != null,
                required(ctx, CypherParser.OC_MatchContext::oC_Pattern, CypherReader::read),
                optional(ctx, CypherParser.OC_MatchContext::oC_Where, CypherReader::read)
        );
    }

    /**
     * Parse a multi-part query context into a MultiPartQuery.
     *
     * @param ctx the ANTLR multi-part query context
     * @return the parsed MultiPartQuery
     */
    private static MultiPartQuery read(CypherParser.OC_MultiPartQueryContext ctx) {
        return new MultiPartQuery(
                list(ctx, CypherParser.OC_MultiPartQueryContext::oc_WithClause, CypherReader::read),
                required(ctx, CypherParser.OC_MultiPartQueryContext::oC_SinglePartQuery, CypherReader::read));
    }

    /**
     * Parse a with-clause context into a WithClause.
     *
     * @param ctx the ANTLR with-clause context
     * @return the parsed WithClause
     */
    private static WithClause read(CypherParser.Oc_WithClauseContext ctx) {
        return new WithClause(
                list(ctx, CypherParser.Oc_WithClauseContext::oC_ReadingClause, CypherReader::read),
                list(ctx, CypherParser.Oc_WithClauseContext::oC_UpdatingClause, CypherReader::read),
                required(ctx, CypherParser.Oc_WithClauseContext::oC_With, CypherReader::read));
    }

    /**
     * Parse a pattern context into a Pattern.
     *
     * @param ctx the ANTLR pattern context
     * @return the parsed Pattern
     */
    private static Pattern read(CypherParser.OC_PatternContext ctx) {
        return new Pattern(
                nonemptyList(ctx, CypherParser.OC_PatternContext::oC_PatternPart, CypherReader::read));
    }

    /**
     * Parse a pattern part context into a PatternPart.
     *
     * @param ctx the ANTLR pattern part context
     * @return the parsed PatternPart
     */
    private static PatternPart read(CypherParser.OC_PatternPartContext ctx) {
        return new PatternPart(
                optional(ctx, CypherParser.OC_PatternPartContext::oC_Variable, CypherReader::read),
                required(ctx, CypherParser.OC_PatternPartContext::oC_AnonymousPatternPart, CypherReader::read));
    }

    /**
     * Parse an anonymous pattern part context into an AnonymousPatternPart.
     *
     * @param ctx the ANTLR anonymous pattern part context
     * @return the parsed AnonymousPatternPart
     */
    private static AnonymousPatternPart read(CypherParser.OC_AnonymousPatternPartContext ctx) {
        return new AnonymousPatternPart(required(ctx, CypherParser.OC_AnonymousPatternPartContext::oC_PatternElement,
                CypherReader::read));
    }

    /**
     * Parse a pattern element context into a PatternElement.
     *
     * @param ctx the ANTLR pattern element context
     * @return the parsed PatternElement
     */
    private static PatternElement read(CypherParser.OC_PatternElementContext ctx) {
        Maybe<NodePattern> np
                = optional(ctx, CypherParser.OC_PatternElementContext::oC_NodePattern, CypherReader::read);
        return np.isJust()
                ? new PatternElement.Chained(
                new NodePatternChain(np.fromJust(),
                        list(ctx, CypherParser.OC_PatternElementContext::oC_PatternElementChain,
                        CypherReader::read)))
                : new PatternElement.Parenthesized(
                required(ctx, CypherParser.OC_PatternElementContext::oC_PatternElement, CypherReader::read));
    }

    /**
     * Parse a node pattern context into a NodePattern.
     *
     * @param ctx the ANTLR node pattern context
     * @return the parsed NodePattern
     */
    private static NodePattern read(CypherParser.OC_NodePatternContext ctx) {
        return new NodePattern(
                optional(ctx, CypherParser.OC_NodePatternContext::oC_Variable, CypherReader::read),
                optional(ctx, CypherParser.OC_NodePatternContext::oC_NodeLabels, CypherReader::read),
                optional(ctx, CypherParser.OC_NodePatternContext::oC_Properties, CypherReader::read));
    }

    /**
     * Parse a properties context into a Properties.
     *
     * @param ctx the ANTLR properties context
     * @return the parsed Properties
     */
    private static Properties read(CypherParser.OC_PropertiesContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_PropertiesContext::oC_MapLiteral, CypherReader::read, Properties.Map::new),
                matchCase(CypherParser.OC_PropertiesContext::oC_Parameter, CypherReader::read,
                        Properties.Parameter::new));
    }

    /**
     * Parse a map literal context into a MapLiteral.
     *
     * @param ctx the ANTLR map literal context
     * @return the parsed MapLiteral
     */
    private static MapLiteral read(CypherParser.OC_MapLiteralContext ctx) {
        return new MapLiteral(list(ctx, CypherParser.OC_MapLiteralContext::oc_KeyValuePair, CypherReader::read));
    }

    /**
     * Parse a key-value pair context into a KeyValuePair.
     *
     * @param ctx the ANTLR key-value pair context
     * @return the parsed KeyValuePair
     */
    private static KeyValuePair read(CypherParser.Oc_KeyValuePairContext ctx) {
        return new KeyValuePair(
                required(ctx, CypherParser.Oc_KeyValuePairContext::oC_PropertyKeyName, CypherReader::read),
                required(ctx, CypherParser.Oc_KeyValuePairContext::oC_Expression, CypherReader::read));
    }

    /**
     * Parse a property key name context into a PropertyKeyName.
     *
     * @param ctx the ANTLR property key name context
     * @return the parsed PropertyKeyName
     */
    private static PropertyKeyName read(CypherParser.OC_PropertyKeyNameContext ctx) {
        return new PropertyKeyName(ctx.getText());
    }

    /**
     * Parse node labels context into NodeLabels.
     *
     * @param ctx the ANTLR node labels context
     * @return the parsed NodeLabels
     */
    private static NodeLabels read(CypherParser.OC_NodeLabelsContext ctx) {
        return new NodeLabels(nonemptyList(ctx, CypherParser.OC_NodeLabelsContext::oC_NodeLabel, CypherReader::read));
    }

    /**
     * Parse a node label context into a NodeLabel.
     *
     * @param ctx the ANTLR node label context
     * @return the parsed NodeLabel
     */
    private static NodeLabel read(CypherParser.OC_NodeLabelContext ctx) {
        String s = ctx.getText();
        return new NodeLabel(s.startsWith(":") ? s.substring(1) : s);
    }

    /**
     * Parse a pattern element chain context into a PatternElementChain.
     *
     * @param ctx the ANTLR pattern element chain context
     * @return the parsed PatternElementChain
     */
    private static PatternElementChain read(CypherParser.OC_PatternElementChainContext ctx) {
        return new PatternElementChain(
                required(ctx, CypherParser.OC_PatternElementChainContext::oC_RelationshipPattern, CypherReader::read),
                required(ctx, CypherParser.OC_PatternElementChainContext::oC_NodePattern, CypherReader::read));
    }

    /**
     * Parse a relationship pattern context into a RelationshipPattern.
     *
     * @param ctx the ANTLR relationship pattern context
     * @return the parsed RelationshipPattern
     */
    private static RelationshipPattern read(CypherParser.OC_RelationshipPatternContext ctx) {
        return new RelationshipPattern(
                null != ctx.oC_LeftArrowHead(),
                optional(ctx, CypherParser.OC_RelationshipPatternContext::oC_RelationshipDetail, CypherReader::read),
                null != ctx.oC_RightArrowHead());
    }

    /**
     * Parse a relationship detail context into a RelationshipDetail.
     *
     * @param ctx the ANTLR relationship detail context
     * @return the parsed RelationshipDetail
     */
    private static RelationshipDetail read(CypherParser.OC_RelationshipDetailContext ctx) {
        return new RelationshipDetail(
                optional(ctx, CypherParser.OC_RelationshipDetailContext::oC_Variable, CypherReader::read),
                optional(ctx, CypherParser.OC_RelationshipDetailContext::oC_RelationshipTypes, CypherReader::read),
                optional(ctx, CypherParser.OC_RelationshipDetailContext::oC_RangeLiteral, CypherReader::read),
                optional(ctx, CypherParser.OC_RelationshipDetailContext::oC_Properties, CypherReader::read));
    }

    /**
     * Parse a range literal context into a RangeLiteral.
     *
     * @param ctx the ANTLR range literal context
     * @return the parsed RangeLiteral
     */
    private static RangeLiteral read(CypherParser.OC_RangeLiteralContext ctx) {
        return new RangeLiteral(
                optional(ctx, CypherParser.OC_RangeLiteralContext::oC_IntegerLiteral, CypherReader::read),
                optional(ctx, CypherParser.OC_RangeLiteralContext::oC_RangeLiteralUpperBound, CypherReader::read));
    }

    /**
     * Parse a range literal upper bound context into a BigInteger.
     *
     * @param ctx the ANTLR range literal upper bound context
     * @return the parsed BigInteger
     */
    private static BigInteger read(CypherParser.OC_RangeLiteralUpperBoundContext ctx) {
        return new BigInteger(ctx.getText());
    }

    /**
     * Parse a relationship types context into a RelationshipTypes.
     *
     * @param ctx the ANTLR relationship types context
     * @return the parsed RelationshipTypes
     */
    private static RelationshipTypes read(CypherParser.OC_RelationshipTypesContext ctx) {
        return new RelationshipTypes(list(ctx, CypherParser.OC_RelationshipTypesContext::oC_RelTypeName,
                CypherReader::read));
    }

    /**
     * Parse a relationship type name context into a RelTypeName.
     *
     * @param ctx the ANTLR relationship type name context
     * @return the parsed RelTypeName
     */
    private static RelTypeName read(CypherParser.OC_RelTypeNameContext ctx) {
        return new RelTypeName(ctx.getText());
    }

    /**
     * Parse a query context into a Query.
     *
     * @param ctx the ANTLR query context
     * @return the parsed Query
     */
    private static Query read(CypherParser.OC_QueryContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_QueryContext::oC_RegularQuery, CypherReader::read, Query.Regular::new),
                matchCase(CypherParser.OC_QueryContext::oC_StandaloneCall, CypherReader::read, Query.Standalone::new));
    }

    /**
     * Parse a standalone call context into a StandaloneCall.
     *
     * @param ctx the ANTLR standalone call context
     * @return the parsed StandaloneCall
     */
    private static StandaloneCall read(CypherParser.OC_StandaloneCallContext ctx) {
        return new StandaloneCall(
                required(ctx, CypherParser.OC_StandaloneCallContext::oC_ProcedureInvocation, CypherReader::read),
                optional(ctx, CypherParser.OC_StandaloneCallContext::oC_StarOrYieldItems, CypherReader::read));
    }

    /**
     * Parse a star-or-yield-items context into a StarOrYieldItems.
     *
     * @param ctx the ANTLR star-or-yield-items context
     * @return the parsed StarOrYieldItems
     */
    private static StarOrYieldItems read(CypherParser.OC_StarOrYieldItemsContext ctx) {
        return null == ctx.oC_YieldItems()
                ? new StarOrYieldItems.Star(false)
                : new StarOrYieldItems.Items(required(ctx, CypherParser.OC_StarOrYieldItemsContext::oC_YieldItems,
                CypherReader::read));
    }

    /**
     * Parse a procedure invocation context into a ProcedureInvocation.
     *
     * @param ctx the ANTLR procedure invocation context
     * @return the parsed ProcedureInvocation
     */
    private static ProcedureInvocation read(CypherParser.OC_ProcedureInvocationContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_ProcedureInvocationContext::oC_ExplicitProcedureInvocation,
                        CypherReader::read, ProcedureInvocation.Explicit::new),
                matchCase(CypherParser.OC_ProcedureInvocationContext::oC_ImplicitProcedureInvocation,
                        CypherReader::read, ProcedureInvocation.Implicit::new));
    }

    /**
     * Parse an implicit procedure invocation context into an ImplicitProcedureInvocation.
     *
     * @param ctx the ANTLR implicit procedure invocation context
     * @return the parsed ImplicitProcedureInvocation
     */
    private static ImplicitProcedureInvocation read(CypherParser.OC_ImplicitProcedureInvocationContext ctx) {
        return new ImplicitProcedureInvocation(
                required(ctx, CypherParser.OC_ImplicitProcedureInvocationContext::oC_ProcedureName,
                        CypherReader::read));
    }

    /**
     * Parse a reading clause context into a ReadingClause.
     *
     * @param ctx the ANTLR reading clause context
     * @return the parsed ReadingClause
     */
    private static ReadingClause read(CypherParser.OC_ReadingClauseContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_ReadingClauseContext::oC_Match, CypherReader::read,
                        ReadingClause.Match::new),
                matchCase(CypherParser.OC_ReadingClauseContext::oC_Unwind, CypherReader::read,
                        ReadingClause.Unwind::new),
                matchCase(CypherParser.OC_ReadingClauseContext::oC_InQueryCall, CypherReader::read,
                        ReadingClause.InQueryCall::new));
    }

    /**
     * Parse an in-query call context into an InQueryCall.
     *
     * @param oc_inQueryCallContext the ANTLR in-query call context
     * @return the parsed InQueryCall
     */
    private static InQueryCall read(CypherParser.OC_InQueryCallContext ctx) {
        return new InQueryCall(
                required(ctx, CypherParser.OC_InQueryCallContext::oC_ExplicitProcedureInvocation,
                        CypherReader::read),
                optional(ctx, CypherParser.OC_InQueryCallContext::oC_YieldItems, CypherReader::read));
    }

    /**
     * Parse yield items context into YieldItems.
     *
     * @param ctx the ANTLR yield items context
     * @return the parsed YieldItems
     */
    private static YieldItems read(CypherParser.OC_YieldItemsContext ctx) {
        return new YieldItems(
                nonemptyList(ctx, CypherParser.OC_YieldItemsContext::oC_YieldItem, CypherReader::read),
                optional(ctx, CypherParser.OC_YieldItemsContext::oC_Where, CypherReader::read));
    }

    /**
     * Parse a yield item context into a YieldItem.
     *
     * @param ctx the ANTLR yield item context
     * @return the parsed YieldItem
     */
    private static YieldItem read(CypherParser.OC_YieldItemContext ctx) {
        return new YieldItem(
                optional(ctx, CypherParser.OC_YieldItemContext::oC_ProcedureResultField, CypherReader::read),
                required(ctx, CypherParser.OC_YieldItemContext::oC_Variable, CypherReader::read));
    }

    /**
     * Parse a procedure result field context into a ProcedureResultField.
     *
     * @param ctx the ANTLR procedure result field context
     * @return the parsed ProcedureResultField
     */
    private static ProcedureResultField read(CypherParser.OC_ProcedureResultFieldContext ctx) {
        return new ProcedureResultField(ctx.getText());
    }

    /**
     * Parse an explicit procedure invocation context into an ExplicitProcedureInvocation.
     *
     * @param ctx the ANTLR explicit procedure invocation context
     * @return the parsed ExplicitProcedureInvocation
     */
    private static ExplicitProcedureInvocation read(CypherParser.OC_ExplicitProcedureInvocationContext ctx) {
        return new ExplicitProcedureInvocation(
                required(ctx, CypherParser.OC_ExplicitProcedureInvocationContext::oC_ProcedureName, CypherReader::read),
                list(ctx, CypherParser.OC_ExplicitProcedureInvocationContext::oC_Expression, CypherReader::read));
    }

    /**
     * Parse a procedure name context into a QualifiedName.
     *
     * @param ctx the ANTLR procedure name context
     * @return the parsed QualifiedName
     */
    private static QualifiedName read(CypherParser.OC_ProcedureNameContext ctx) {
        return new QualifiedName(required(ctx, CypherParser.OC_ProcedureNameContext::oC_Namespace, CypherReader::read),
                required(ctx, CypherParser.OC_ProcedureNameContext::oC_SymbolicName, CypherReader::read));
    }

    /**
     * Parse an unwind context into an Unwind.
     *
     * @param ctx the ANTLR unwind context
     * @return the parsed Unwind
     */
    private static Unwind read(CypherParser.OC_UnwindContext ctx) {
        return new Unwind(
                required(ctx, CypherParser.OC_UnwindContext::oC_Expression, CypherReader::read),
                required(ctx, CypherParser.OC_UnwindContext::oC_Variable, CypherReader::read));
    }

    /**
     * Parse a regular query context into a RegularQuery.
     *
     * @param ctx the ANTLR regular query context
     * @return the parsed RegularQuery
     */
    private static RegularQuery read(CypherParser.OC_RegularQueryContext ctx) {
        SingleQuery head = required(ctx,
                CypherParser.OC_RegularQueryContext::oC_SingleQuery,
                CypherReader::read);

        List<Union> rest = list(ctx,
                CypherParser.OC_RegularQueryContext::oC_Union,
                CypherReader::read);

        return new RegularQuery(head, rest);
    }

    /**
     * Parse a single-part query context into a SinglePartQuery.
     *
     * @param ctx the ANTLR single-part query context
     * @return the parsed SinglePartQuery
     */
    private static SinglePartQuery read(CypherParser.OC_SinglePartQueryContext ctx) {
        List<ReadingClause> rc = list(ctx,
                CypherParser.OC_SinglePartQueryContext::oC_ReadingClause,
                CypherReader::read);
        List<UpdatingClause> uc = list(ctx,
                CypherParser.OC_SinglePartQueryContext::oC_UpdatingClause,
                CypherReader::read);
        Maybe<Return> ret = optional(ctx,
                CypherParser.OC_SinglePartQueryContext::oC_Return,
                CypherReader::read);
        return new SinglePartQuery(rc, uc, ret);
    }

    /**
     * Parse a return context into a Return.
     *
     * @param ctx the ANTLR return context
     * @return the parsed Return
     */
    private static Return read(CypherParser.OC_ReturnContext ctx) {
        return new Return(
                required(ctx, CypherParser.OC_ReturnContext::oC_ProjectionBody, CypherReader::read));
    }

    /**
     * Parse a projection body context into a ProjectionBody.
     *
     * @param ctx the ANTLR projection body context
     * @return the parsed ProjectionBody
     */
    private static ProjectionBody read(CypherParser.OC_ProjectionBodyContext ctx) {
        return new ProjectionBody(
                ctx.DISTINCT() != null,
                required(ctx, CypherParser.OC_ProjectionBodyContext::oC_ProjectionItems, CypherReader::read),
                optional(ctx, CypherParser.OC_ProjectionBodyContext::oC_Order, CypherReader::read),
                optional(ctx, CypherParser.OC_ProjectionBodyContext::oC_Skip, CypherReader::read),
                optional(ctx, CypherParser.OC_ProjectionBodyContext::oC_Limit, CypherReader::read));
    }

    /**
     * Parse a limit context into a Limit.
     *
     * @param ctx the ANTLR limit context
     * @return the parsed Limit
     */
    private static Limit read(CypherParser.OC_LimitContext ctx) {
        return new Limit(required(ctx, CypherParser.OC_LimitContext::oC_Expression, CypherReader::read));
    }

    /**
     * Parse a skip context into a Skip.
     *
     * @param ctx the ANTLR skip context
     * @return the parsed Skip
     */
    private static Skip read(CypherParser.OC_SkipContext ctx) {
        return new Skip(required(ctx, CypherParser.OC_SkipContext::oC_Expression, CypherReader::read));
    }

    /**
     * Parse an order context into an Order.
     *
     * @param ctx the ANTLR order context
     * @return the parsed Order
     */
    private static Order read(CypherParser.OC_OrderContext ctx) {
        return new Order(list(ctx, CypherParser.OC_OrderContext::oC_SortItem, CypherReader::read));
    }

    /**
     * Parse a sort item context into a SortItem.
     *
     * @param ctx the ANTLR sort item context
     * @return the parsed SortItem
     */
    private static SortItem read(CypherParser.OC_SortItemContext ctx) {
        return new SortItem(
                required(ctx, CypherParser.OC_SortItemContext::oC_Expression, CypherReader::read),
                optional(ctx, CypherParser.OC_SortItemContext::oC_SortOrder, CypherReader::read));
    }

    /**
     * Parse a sort order context into a SortOrder.
     *
     * @param ctx the ANTLR sort order context
     * @return the parsed SortOrder
     */
    private static SortOrder read(CypherParser.OC_SortOrderContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_SortOrderContext::oC_SortOrder_Ascending, new SortOrder.Ascending(false)),
                matchCase(CypherParser.OC_SortOrderContext::oC_SortOrder_Descending, new SortOrder.Descending(false)));
    }

    /**
     * Parse projection items context into ProjectionItems.
     *
     * @param ctx the ANTLR projection items context
     * @return the parsed ProjectionItems
     */
    private static ProjectionItems read(CypherParser.OC_ProjectionItemsContext ctx) {
        return new ProjectionItems(
                ctx.getText().startsWith("*"),
                list(ctx, CypherParser.OC_ProjectionItemsContext::oC_ProjectionItem, CypherReader::read));
    }

    /**
     * Parse a projection item context into a ProjectionItem.
     *
     * @param ctx the ANTLR projection item context
     * @return the parsed ProjectionItem
     */
    private static ProjectionItem read(CypherParser.OC_ProjectionItemContext ctx) {
        return new ProjectionItem(
                required(ctx, CypherParser.OC_ProjectionItemContext::oC_Expression, CypherReader::read),
                optional(ctx, CypherParser.OC_ProjectionItemContext::oC_Variable, CypherReader::read));
    }

    /**
     * Parse an updating clause context into an UpdatingClause.
     *
     * @param ctx the ANTLR updating clause context
     * @return the parsed UpdatingClause
     */
    private static UpdatingClause read(CypherParser.OC_UpdatingClauseContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_UpdatingClauseContext::oC_Create, CypherReader::read,
                        UpdatingClause.Create::new),
                matchCase(CypherParser.OC_UpdatingClauseContext::oC_Merge, CypherReader::read,
                        UpdatingClause.Merge::new),
                matchCase(CypherParser.OC_UpdatingClauseContext::oC_Delete, CypherReader::read,
                        UpdatingClause.Delete::new),
                matchCase(CypherParser.OC_UpdatingClauseContext::oC_Set, CypherReader::read,
                        UpdatingClause.Set::new),
                matchCase(CypherParser.OC_UpdatingClauseContext::oC_Remove, CypherReader::read,
                        UpdatingClause.Remove::new));
    }

    /**
     * Parse a remove context into a Remove.
     *
     * @param ctx the ANTLR remove context
     * @return the parsed Remove
     */
    private static Remove read(CypherParser.OC_RemoveContext ctx) {
        return new Remove(nonemptyList(ctx, CypherParser.OC_RemoveContext::oC_RemoveItem, CypherReader::read));
    }

    /**
     * Parse a remove item context into a RemoveItem.
     *
     * @param ctx the ANTLR remove item context
     * @return the parsed RemoveItem
     */
    private static RemoveItem read(CypherParser.OC_RemoveItemContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_RemoveItemContext::oC_VariableAndNodeLabels, CypherReader::read,
                        RemoveItem.VariableLabels::new),
                matchCase(CypherParser.OC_RemoveItemContext::oC_PropertyExpression, CypherReader::read,
                        RemoveItem.Property::new));
    }

    /**
     * Parse a variable and node labels context into a VariableAndNodeLabels.
     *
     * @param ctx the ANTLR variable and node labels context
     * @return the parsed VariableAndNodeLabels
     */
    private static VariableAndNodeLabels read(CypherParser.OC_VariableAndNodeLabelsContext ctx) {
        return new VariableAndNodeLabels(
                required(ctx, CypherParser.OC_VariableAndNodeLabelsContext::oC_Variable, CypherReader::read),
                required(ctx, CypherParser.OC_VariableAndNodeLabelsContext::oC_NodeLabels, CypherReader::read));
    }

    /**
     * Parse a set context into a Set.
     *
     * @param ctx the ANTLR set context
     * @return the parsed Set
     */
    private static Set read(CypherParser.OC_SetContext ctx) {
        return new Set(list(ctx, CypherParser.OC_SetContext::oC_SetItem, CypherReader::read));
    }

    /**
     * Parse a set item context into a SetItem.
     *
     * @param ctx the ANTLR set item context
     * @return the parsed SetItem
     */
    private static SetItem read(CypherParser.OC_SetItemContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_SetItemContext::oC_SetItem_Property, CypherReader::read,
                        SetItem.Property::new),
                matchCase(CypherParser.OC_SetItemContext::oC_SetItem_Equal, CypherReader::read,
                        SetItem.VariableEqual::new),
                matchCase(CypherParser.OC_SetItemContext::oC_SetItem_PlusEqual, CypherReader::read,
                        SetItem.VariablePlusEqual::new),
                matchCase(CypherParser.OC_SetItemContext::oC_SetItem_NodeLabels, CypherReader::read,
                        SetItem.VariableLabels::new));
    }

    /**
     * Parse a set-item node labels context into a VariableAndNodeLabels.
     *
     * @param ctx the ANTLR set-item node labels context
     * @return the parsed VariableAndNodeLabels
     */
    private static VariableAndNodeLabels read(CypherParser.OC_SetItem_NodeLabelsContext ctx) {
        return new VariableAndNodeLabels(
                required(ctx, CypherParser.OC_SetItem_NodeLabelsContext::oC_Variable, CypherReader::read),
                required(ctx, CypherParser.OC_SetItem_NodeLabelsContext::oC_NodeLabels, CypherReader::read));
    }

    /**
     * Parse a variable plus-equals context into a VariablePlusEquals.
     *
     * @param ctx the ANTLR variable plus-equals context
     * @return the parsed VariablePlusEquals
     */
    private static VariablePlusEquals read(CypherParser.OC_SetItem_PlusEqualContext ctx) {
        return new VariablePlusEquals(
                required(ctx, CypherParser.OC_SetItem_PlusEqualContext::oC_Variable, CypherReader::read),
                required(ctx, CypherParser.OC_SetItem_PlusEqualContext::oC_Expression, CypherReader::read));
    }

    /**
     * Parse a variable equals context into a VariableEquals.
     *
     * @param ctx the ANTLR variable equals context
     * @return the parsed VariableEquals
     */
    private static VariableEquals read(CypherParser.OC_SetItem_EqualContext ctx) {
        return new VariableEquals(
                required(ctx, CypherParser.OC_SetItem_EqualContext::oC_Variable, CypherReader::read),
                required(ctx, CypherParser.OC_SetItem_EqualContext::oC_Expression, CypherReader::read));
    }

    /**
     * Parse a property equals context into a PropertyEquals.
     *
     * @param ctx the ANTLR property equals context
     * @return the parsed PropertyEquals
     */
    private static PropertyEquals read(CypherParser.OC_SetItem_PropertyContext ctx) {
        return new PropertyEquals(
                required(ctx, CypherParser.OC_SetItem_PropertyContext::oC_PropertyExpression, CypherReader::read),
                required(ctx, CypherParser.OC_SetItem_PropertyContext::oC_Expression, CypherReader::read));
    }

    /**
     * Parse a property expression context into a PropertyExpression.
     *
     * @param ctx the ANTLR property expression context
     * @return the parsed PropertyExpression
     */
    private static PropertyExpression read(CypherParser.OC_PropertyExpressionContext ctx) {
        return new PropertyExpression(
                required(ctx, CypherParser.OC_PropertyExpressionContext::oC_Atom, CypherReader::read),
                nonemptyList(ctx, CypherParser.OC_PropertyExpressionContext::oC_PropertyLookup, CypherReader::read));
    }

    /**
     * Parse a delete context into a Delete.
     *
     * @param ctx the ANTLR delete context
     * @return the parsed Delete
     */
    private static Delete read(CypherParser.OC_DeleteContext ctx) {
        return new Delete(
                ctx.DETACH() != null,
                list(ctx, CypherParser.OC_DeleteContext::oC_Expression, CypherReader::read));
    }

    /**
     * Parse a merge context into a Merge.
     *
     * @param ctx the ANTLR merge context
     * @return the parsed Merge
     */
    private static Merge read(CypherParser.OC_MergeContext ctx) {
        return new Merge(
                required(ctx, CypherParser.OC_MergeContext::oC_PatternPart, CypherReader::read),
                list(ctx, CypherParser.OC_MergeContext::oC_MergeAction, CypherReader::read));
    }

    /**
     * Parse a merge action context into a MergeAction.
     *
     * @param ctx the ANTLR merge action context
     * @return the parsed MergeAction
     */
    private static MergeAction read(CypherParser.OC_MergeActionContext ctx) {
        return new MergeAction(
                required(ctx, CypherParser.OC_MergeActionContext::oC_MatchOrCreate, CypherReader::read),
                required(ctx, CypherParser.OC_MergeActionContext::oC_Set, CypherReader::read));
    }

    /**
     * Parse a match-or-create context into a MatchOrCreate.
     *
     * @param ctx the ANTLR match-or-create context
     * @return the parsed MatchOrCreate
     */
    private static MatchOrCreate read(CypherParser.OC_MatchOrCreateContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_MatchOrCreateContext::MATCH, new MatchOrCreate.Match(false)),
                matchCase(CypherParser.OC_MatchOrCreateContext::CREATE, new MatchOrCreate.Create(false)));
    }

    /**
     * Parse a create context into a Create.
     *
     * @param ctx the ANTLR create context
     * @return the parsed Create
     */
    private static Create read(CypherParser.OC_CreateContext ctx) {
        return new Create(required(ctx, CypherParser.OC_CreateContext::oC_Pattern, CypherReader::read));
    }

    /**
     * Parse a single query context into a SingleQuery.
     *
     * @param ctx the ANTLR single query context
     * @return the parsed SingleQuery
     */
    private static SingleQuery read(CypherParser.OC_SingleQueryContext ctx) {
        return match(ctx,
                matchCase(CypherParser.OC_SingleQueryContext::oC_SinglePartQuery, CypherReader::read,
                        SingleQuery.SinglePart::new),
                matchCase(CypherParser.OC_SingleQueryContext::oC_MultiPartQuery, CypherReader::read,
                        SingleQuery.MultiPart::new));
    }

    /**
     * Parse a union context into a Union.
     *
     * @param ctx the ANTLR union context
     * @return the parsed Union
     */
    private static Union read(CypherParser.OC_UnionContext ctx) {
        return new Union(
                ctx.ALL() != null,
                required(ctx, CypherParser.OC_UnionContext::oC_SingleQuery, CypherReader::read));
    }

    /**
     * Parse a variable context into a Variable.
     *
     * @param ctx the ANTLR variable context
     * @return the parsed Variable
     */
    private static Variable read(CypherParser.OC_VariableContext ctx) {
        return new Variable(ctx.getText());
    }

    /**
     * Parse a where context into a Where.
     *
     * @param ctx the ANTLR where context
     * @return the parsed Where
     */
    private static Where read(CypherParser.OC_WhereContext ctx) {
        return new Where(
                required(ctx, CypherParser.OC_WhereContext::oC_Expression, CypherReader::read));
    }

    /**
     * Parse a with context into a With.
     *
     * @param ctx the ANTLR with context
     * @return the parsed With
     */
    private static With read(CypherParser.OC_WithContext ctx) {
        return new With(
                required(ctx, CypherParser.OC_WithContext::oC_ProjectionBody, CypherReader::read),
                optional(ctx, CypherParser.OC_WithContext::oC_Where, CypherReader::read));
    }

    /**
     * Unescape a single-quoted string literal.
     *
     * @param input the single-quoted string literal
     * @return the unescaped string
     */
    // TODO: this may not match Neo4j's behavior exactly
    private static String unescapeSingleQuotedString(String input) {
        String mid = input.substring(1, input.length() - 1);
        String tempEscaped = mid.replace("\\'", "_TEMP_");
        String unescaped = StringEscapeUtils.unescapeJava(tempEscaped);
        return unescaped.replace("_TEMP_", "'");
    }
}
