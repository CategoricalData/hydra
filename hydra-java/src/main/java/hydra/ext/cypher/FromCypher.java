package hydra.ext.cypher;

import hydra.ext.cypher.openCypher.AddOrSubtractExpression;
import hydra.ext.cypher.openCypher.AddOrSubtractRightHandSide;
import hydra.ext.cypher.openCypher.AndExpression;
import hydra.ext.cypher.openCypher.Atom;
import hydra.ext.cypher.openCypher.ComparisonExpression;
import hydra.ext.cypher.openCypher.ListOperatorExpressionOrPropertyLookup;
import hydra.ext.cypher.openCypher.Match;
import hydra.ext.cypher.openCypher.MultiplyDivideModuloExpression;
import hydra.ext.cypher.openCypher.MultiplyDivideModuloRightHandSide;
import hydra.ext.cypher.openCypher.NodeLabel;
import hydra.ext.cypher.openCypher.NodePattern;
import hydra.ext.cypher.openCypher.NodePatternChain;
import hydra.ext.cypher.openCypher.NonArithmeticOperatorExpression;
import hydra.ext.cypher.openCypher.NotExpression;
import hydra.ext.cypher.openCypher.OrExpression;
import hydra.ext.cypher.openCypher.PartialComparisonExpression;
import hydra.ext.cypher.openCypher.PatternElement;
import hydra.ext.cypher.openCypher.PatternElementChain;
import hydra.ext.cypher.openCypher.PatternPart;
import hydra.ext.cypher.openCypher.PowerOfExpression;
import hydra.ext.cypher.openCypher.ProjectionBody;
import hydra.ext.cypher.openCypher.ProjectionItem;
import hydra.ext.cypher.openCypher.ProjectionItems;
import hydra.ext.cypher.openCypher.Properties;
import hydra.ext.cypher.openCypher.PropertyKeyName;
import hydra.ext.cypher.openCypher.ReadingClause;
import hydra.ext.cypher.openCypher.RegularQuery;
import hydra.ext.cypher.openCypher.RelTypeName;
import hydra.ext.cypher.openCypher.RelationshipDetail;
import hydra.ext.cypher.openCypher.RelationshipPattern;
import hydra.ext.cypher.openCypher.SinglePartQuery;
import hydra.ext.cypher.openCypher.SingleQuery;
import hydra.ext.cypher.openCypher.StringListNullPredicateExpression;
import hydra.ext.cypher.openCypher.StringListNullPredicateRightHandSide;
import hydra.ext.cypher.openCypher.UnaryAddOrSubtractExpression;
import hydra.ext.cypher.openCypher.UpdatingClause;
import hydra.ext.cypher.openCypher.XorExpression;
import hydra.pg.model.Direction;
import hydra.pg.model.EdgeLabel;
import hydra.pg.model.PropertyKey;
import hydra.pg.model.VertexLabel;
import hydra.pg.query.AssociativeExpression;
import hydra.pg.query.BinaryBooleanOperator;
import hydra.pg.query.BinaryExpression;
import hydra.pg.query.BinaryOperator;
import hydra.pg.query.ComparisonOperator;
import hydra.pg.query.EdgeProjectionPattern;
import hydra.pg.query.Expression;
import hydra.pg.query.MatchQuery;
import hydra.pg.query.Projection;
import hydra.pg.query.Projections;
import hydra.pg.query.PropertyPattern;
import hydra.pg.query.PropertyProjection;
import hydra.pg.query.Query;
import hydra.pg.query.SelectQuery;
import hydra.pg.query.UnaryExpression;
import hydra.pg.query.UnaryOperator;
import hydra.pg.query.Variable;
import hydra.pg.query.VertexPattern;
import hydra.tools.MapperBase;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import hydra.util.Opt;

import static hydra.pg.dsl.Queries.apply;
import static hydra.pg.dsl.Queries.query;

/**
 * Mapper from Cypher (OpenCypher) query model to property graph query model.
 * Converts Cypher query structures to corresponding property graph query structures.
 */
public class FromCypher extends MapperBase {

    /**
     * Convert a Cypher add-or-subtract expression to a property graph Expression.
     *
     * @param cypher the Cypher add-or-subtract expression
     * @return the converted property graph Expression
     */
    public static Expression from(AddOrSubtractExpression cypher) {
        Expression cur = from(cypher.left);

        for (AddOrSubtractRightHandSide rhs : cypher.right) {
            return unsupported();
        }

        return cur;
    }

    /**
     * Convert a Cypher atom to a property graph Expression.
     *
     * @param cypher the Cypher atom
     * @return the converted property graph Expression
     */
    public static Expression from(Atom cypher) {
      return cypher.accept(new Atom.Visitor<Expression>() {
          @Override
          public Expression visit(Atom.Literal instance) {
              return unsupported();
          }

          @Override
          public Expression visit(Atom.Parameter instance) {
              return unsupported();
          }

          @Override
          public Expression visit(Atom.Case instance) {
              return unsupported();
          }

          @Override
          public Expression visit(Atom.CountStar instance) {
              return unsupported();
          }

          @Override
          public Expression visit(Atom.ListComprehension instance) {
              return unsupported();
          }

          @Override
          public Expression visit(Atom.PatternComprehension instance) {
              return unsupported();
          }

          @Override
          public Expression visit(Atom.Quantifier instance) {
              return unsupported();
          }

          @Override
          public Expression visit(Atom.PatternPredicate instance) {
              return unsupported();
          }

          @Override
          public Expression visit(Atom.Parenthesized instance) {
              return unsupported();
          }

          @Override
          public Expression visit(Atom.FunctionInvocation instance) {
              return unsupported();
          }

          @Override
          public Expression visit(Atom.ExistentialSubquery instance) {
              return unsupported();
          }

          @Override
          public Expression visit(Atom.Variable instance) {
              return new Expression.Variable(from(instance.value));
          }
      });
    }

    /**
     * Convert a Cypher comparison operator to a property graph ComparisonOperator.
     *
     * @param cypher the Cypher comparison operator
     * @return the converted property graph ComparisonOperator
     */
    public static ComparisonOperator from(hydra.ext.cypher.openCypher.ComparisonOperator cypher) {
        return cypher.accept(new hydra.ext.cypher.openCypher.ComparisonOperator.Visitor<ComparisonOperator>() {
            @Override
            public ComparisonOperator visit(hydra.ext.cypher.openCypher.ComparisonOperator.Eq instance) {
                return new ComparisonOperator.Eq(false);
            }

            @Override
            public ComparisonOperator visit(hydra.ext.cypher.openCypher.ComparisonOperator.Neq instance) {
                return new ComparisonOperator.Neq(false);
            }

            @Override
            public ComparisonOperator visit(hydra.ext.cypher.openCypher.ComparisonOperator.Lt instance) {
                return new ComparisonOperator.Lt(false);
            }

            @Override
            public ComparisonOperator visit(hydra.ext.cypher.openCypher.ComparisonOperator.Gt instance) {
                return new ComparisonOperator.Gt(false);
            }

            @Override
            public ComparisonOperator visit(hydra.ext.cypher.openCypher.ComparisonOperator.Lte instance) {
                return new ComparisonOperator.Lte(false);
            }

            @Override
            public ComparisonOperator visit(hydra.ext.cypher.openCypher.ComparisonOperator.Gte instance) {
                return new ComparisonOperator.Gte(false);
            }
        });
    }

    /**
     * Convert a Cypher expression to a property graph Expression.
     *
     * @param cypher the Cypher expression
     * @return the converted property graph Expression
     */
    public static Expression from(hydra.ext.cypher.openCypher.Expression cypher) {
        return from(cypher.value);
    }

    /**
     * Convert a Cypher OR expression to a property graph Expression.
     *
     * @param cypher the Cypher OR expression
     * @return the converted property graph Expression
     */
    public static Expression from(OrExpression cypher) {
        return oneOrMany(cypher.value, FromCypher::from, exps -> new Expression.Associative(new AssociativeExpression(
                new BinaryOperator.Boolean_(new BinaryBooleanOperator.Or(false)),
                map(exps, FromCypher::from))));
    }

    /**
     * Convert a Cypher XOR expression to a property graph Expression.
     *
     * @param cypher the Cypher XOR expression
     * @return the converted property graph Expression
     */
    public static Expression from(XorExpression cypher) {
        return oneOrMany(cypher.value, FromCypher::from, exps -> new Expression.Associative(new AssociativeExpression(
                new BinaryOperator.Boolean_(new BinaryBooleanOperator.Xor(false)),
                map(exps, FromCypher::from))));
    }

    /**
     * Convert a Cypher AND expression to a property graph Expression.
     *
     * @param cypher the Cypher AND expression
     * @return the converted property graph Expression
     */
    public static Expression from(AndExpression cypher) {
        return oneOrMany(cypher.value, FromCypher::from, exps -> new Expression.Associative(new AssociativeExpression(
                new BinaryOperator.Boolean_(new BinaryBooleanOperator.And(false)),
                map(exps, FromCypher::from))));
    }

    /**
     * Convert a Cypher NOT expression to a property graph Expression.
     *
     * @param cypher the Cypher NOT expression
     * @return the converted property graph Expression
     */
    public static Expression from(NotExpression cypher) {
        return cypher.not
                ? new Expression.Unary(new UnaryExpression(new UnaryOperator.Negate(false), from(cypher.expression)))
                : from(cypher.expression);
    }

    /**
     * Convert a Cypher comparison expression to a property graph Expression.
     *
     * @param cypher the Cypher comparison expression
     * @return the converted property graph Expression
     */
    public static Expression from(ComparisonExpression cypher) {
        Expression cur = from(cypher.left);
        for (PartialComparisonExpression pce : cypher.right) {
            cur = new Expression.Binary(
                    new BinaryExpression(cur, new BinaryOperator.Comparison(from(pce.operator)), from(pce.right)));
        }
        return cur;
    }

    /**
     * Convert a Cypher match to a property graph MatchQuery.
     *
     * @param cypher the Cypher match
     * @return the converted property graph MatchQuery
     */
    public static MatchQuery from(Match cypher) {
        return new MatchQuery(
                cypher.optional,
                map(cypher.pattern.value, FromCypher::from),
                cypher.where.map(w -> from(w.value)));
    }

    /**
     * Convert a Cypher multiply/divide/modulo expression to a property graph Expression.
     *
     * @param cypher the Cypher multiply/divide/modulo expression
     * @return the converted property graph Expression
     */
    public static Expression from(MultiplyDivideModuloExpression cypher) {
        Expression cur = from(cypher.left);

        for (MultiplyDivideModuloRightHandSide rhs : cypher.right) {
            return unsupported();
        }

        return cur;
    }

    /**
     * Convert a Cypher node pattern to a property graph VertexPattern.
     *
     * @param cypher the Cypher node pattern
     * @return the converted property graph VertexPattern
     */
    public static VertexPattern from(NodePattern cypher) {
        return new VertexPattern(
                cypher.variable.map(FromCypher::from),
                labelOf(cypher),
                cypher.properties.isPresent()
                        ? from(cypher.properties.get()) : Collections.emptyList(),
                Collections.emptyList());
    }

    /**
     * Convert a Cypher node pattern chain to a property graph VertexPattern.
     *
     * @param cypher the Cypher node pattern chain
     * @return the converted property graph VertexPattern
     */
    public static VertexPattern from(NodePatternChain cypher) {
        VertexPattern vq = from(cypher.nodePattern);
        return cypher.chain.size() > 0
                ? vq.withEdges(Collections.singletonList(from(cypher.chain)))
                : vq;
    }

    /**
     * Convert a Cypher non-arithmetic operator expression to a property graph Expression.
     *
     * @param cypher the Cypher non-arithmetic operator expression
     * @return the converted property graph Expression
     */
    public static Expression from(NonArithmeticOperatorExpression cypher) {
        if (cypher.labels.isPresent()) {
            return unsupported();
        }

        Expression cur = from(cypher.atom);

        for (ListOperatorExpressionOrPropertyLookup l : cypher.listsAndLookups) {
            final Expression c = cur;
            cur = l.accept(new ListOperatorExpressionOrPropertyLookup.Visitor<Expression>() {
                @Override
                public Expression visit(ListOperatorExpressionOrPropertyLookup.List instance) {
                    return unsupported();
                }

                @Override
                public Expression visit(ListOperatorExpressionOrPropertyLookup.Property instance) {
                    return new Expression.Property(
                            new PropertyProjection(c, from(instance.value.value)));
                }
            });
        }

        return cur;
    }

    /**
     * Convert a list of Cypher pattern element chains to a property graph EdgeProjectionPattern.
     *
     * @param cypher the list of Cypher pattern element chains
     * @return the converted property graph EdgeProjectionPattern
     */
    public static EdgeProjectionPattern from(List<PatternElementChain> cypher) {
        EdgeProjectionPattern cur = null;
        for (int i = cypher.size() - 1; i >= 0; i--) {
            PatternElementChain chain = cypher.get(i);
            VertexPattern vp = from(chain.node);

            chain.relationship.detail.ifPresent(FromCypher::checkRelationshipDetail);

            vp = null == cur
                    ? vp
                    : vp.withEdges(Collections.singletonList(cur));
            Direction dir = directionOf(chain.relationship);
            Opt<EdgeLabel> label = labelOf(chain.relationship);
            Opt<Properties> ps = chain.relationship.detail.flatMap(r -> r.properties);
            List<PropertyPattern> props = ps.isPresent()
                    ? from(ps.get())
                    : Collections.emptyList();
            cur = new EdgeProjectionPattern(dir, label, props, Opt.of(vp));
        }

        return cur;
    }

    /**
     * Convert Cypher properties to a list of property graph PropertyPattern.
     *
     * @param cypher the Cypher properties
     * @return the converted list of property graph PropertyPattern
     */
    public static List<PropertyPattern> from(Properties cypher) {
        return unsupported();
    }

    /**
     * Convert a Cypher pattern element to a property graph Expression.
     *
     * @param cypher the Cypher pattern element
     * @return the converted property graph Expression
     */
    public static Expression from(PatternElement cypher) {
        return cypher.accept(new PatternElement.Visitor<Expression>() {
            @Override
            public Expression visit(PatternElement.Chained instance) {
                return new Expression.Vertex(from(instance.value));
            }

            @Override
            public Expression visit(PatternElement.Parenthesized instance) {
                return from(instance.value);
            }
        });
    }

    /**
     * Convert a Cypher pattern part to a property graph Projection.
     *
     * @param cypher the Cypher pattern part
     * @return the converted property graph Projection
     */
    public static Projection from(PatternPart cypher) {
        return new Projection(
                from(cypher.pattern.value),
                cypher.variable.map(FromCypher::from));
    }

    /**
     * Convert a Cypher power-of expression to a property graph Expression.
     *
     * @param cypher the Cypher power-of expression
     * @return the converted property graph Expression
     */
    public static Expression from(PowerOfExpression cypher) {
        return oneOrMany(cypher.value, FromCypher::from, exps -> new Expression.Associative(new AssociativeExpression(
                new BinaryOperator.Power(false),
                map(exps, FromCypher::from))));
    }

    /**
     * Convert a Cypher query to a property graph Query.
     *
     * @param cypher the Cypher query
     * @return the converted property graph Query
     */
    public static Query from(hydra.ext.cypher.openCypher.Query cypher) {
        return cypher.accept(new hydra.ext.cypher.openCypher.Query.Visitor<Query>() {
            @Override
            public Query visit(hydra.ext.cypher.openCypher.Query.Regular instance) {
                return from(instance.value);
            }

            @Override
            public Query visit(hydra.ext.cypher.openCypher.Query.Standalone instance) {
                return unsupported();
            }
        });
    }

    /**
     * Convert a Cypher projection body to a property graph SelectQuery.
     *
     * @param cypher the Cypher projection body
     * @return the converted property graph SelectQuery
     */
    public static SelectQuery from(ProjectionBody cypher) {
        return new SelectQuery(cypher.distinct, from(cypher.projectionItems));
    }

    /**
     * Convert a Cypher projection item to a property graph Projection.
     *
     * @param cypher the Cypher projection item
     * @return the converted property graph Projection
     */
    public static Projection from(ProjectionItem cypher) {
        return new Projection(
                from(cypher.expression),
                cypher.variable.map(FromCypher::from));
    }

    /**
     * Convert Cypher projection items to property graph Projections.
     *
     * @param cypher the Cypher projection items
     * @return the converted property graph Projections
     */
    public static Projections from(ProjectionItems cypher) {
        return new Projections(cypher.star, map(cypher.explicit, FromCypher::from));
    }

    /**
     * Convert a Cypher property key name to a property graph PropertyKey.
     *
     * @param cypher the Cypher property key name
     * @return the converted property graph PropertyKey
     */
    public static PropertyKey from(PropertyKeyName cypher) {
        return new PropertyKey(cypher.value);
    }

    /**
     * Convert a Cypher reading clause to a property graph Query.
     *
     * @param cypher the Cypher reading clause
     * @return the converted property graph Query
     */
    public static Query from(ReadingClause cypher) {
        return cypher.accept(new ReadingClause.Visitor<Query>() {
            @Override
            public Query visit(ReadingClause.Match instance) {
                return new Query.Match(from(instance.value));
            }

            @Override
            public Query visit(ReadingClause.Unwind instance) {
                return unsupported();
            }

            @Override
            public Query visit(ReadingClause.InQueryCall instance) {
                return unsupported();
            }
        });
    }

    /**
     * Convert a Cypher regular query to a property graph Query.
     *
     * @param cypher the Cypher regular query
     * @return the converted property graph Query
     */
    public static Query from(RegularQuery cypher) {
        if (cypher.rest.size() > 0) {
            return unsupported();
        }

        return from(cypher.head);
    }

    /**
     * Convert a Cypher single query to a property graph Query.
     *
     * @param cypher the Cypher single query
     * @return the converted property graph Query
     */
    public static Query from(SingleQuery cypher) {
        return cypher.accept(new hydra.ext.cypher.openCypher.SingleQuery.Visitor<Query>() {
            @Override
            public Query visit(hydra.ext.cypher.openCypher.SingleQuery.SinglePart instance) {
                return from(instance.value);
            }

            @Override
            public Query visit(hydra.ext.cypher.openCypher.SingleQuery.MultiPart instance) {
                return unsupported();
            }
        });
    }

    /**
     * Convert a Cypher single-part query to a property graph Query.
     *
     * @param cypher the Cypher single-part query
     * @return the converted property graph Query
     */
    public static Query from(SinglePartQuery cypher) {
        List<Query> subqueries = new ArrayList<>();
        for (ReadingClause c : cypher.reading) {
            subqueries.add(from(c));
        }
        for (UpdatingClause c : cypher.updating) {
            subqueries.add(from(c));
        }

        cypher.return_.ifPresent(r -> subqueries.add(query(from(r.value))));

        return apply(subqueries);
    }

    /**
     * Convert a Cypher string/list/null predicate expression to a property graph Expression.
     *
     * @param cypher the Cypher string/list/null predicate expression
     * @return the converted property graph Expression
     */
    public static Expression from(StringListNullPredicateExpression cypher) {
        Expression cur = from(cypher.left);

        for (StringListNullPredicateRightHandSide rhs : cypher.right) {
            return unsupported();
        }

        return cur;
    }

    /**
     * Convert a Cypher updating clause to a property graph Query.
     *
     * @param cypher the Cypher updating clause
     * @return the converted property graph Query
     */
    public static Query from(UpdatingClause cypher) {
        return unsupported();
    }

    /**
     * Convert a Cypher unary add-or-subtract expression to a property graph Expression.
     *
     * @param cypher the Cypher unary add-or-subtract expression
     * @return the converted property graph Expression
     */
    public static Expression from(UnaryAddOrSubtractExpression cypher) {
        return cypher.operator.isPresent()
                ? unsupported()
                : from(cypher.expression);
    }

    /**
     * Convert a Cypher variable to a property graph Variable.
     *
     * @param cypher the Cypher variable
     * @return the converted property graph Variable
     */
    public static Variable from(hydra.ext.cypher.openCypher.Variable cypher) {
        return new Variable(cypher.value);
    }

    ////////////////////////////////////////////////////////////////////////////

    /**
     * Check if a relationship detail is supported.
     *
     * @param cypher the Cypher relationship detail
     * @throws MapperException if the relationship detail is not supported
     */
    private static void checkRelationshipDetail(RelationshipDetail cypher) {
        if (cypher.range.isPresent()) {
            throw new MapperException("not yet supported");
        }
        if (cypher.variable.isPresent()) {
            throw new MapperException("not yet supported");
        }
    }

    /**
     * Determine the direction of a relationship pattern.
     *
     * @param cypher the Cypher relationship pattern
     * @return the property graph Direction
     */
    private static Direction directionOf(RelationshipPattern cypher) {
        return cypher.leftArrow
                ? (cypher.rightArrow ? new Direction.Both(false) : new Direction.In(false))
                : (cypher.rightArrow ? new Direction.Out(false) : new Direction.Undirected(false));
    }

    /**
     * Extract the vertex label from a node pattern.
     *
     * @param cypher the Cypher node pattern
     * @return the optional property graph VertexLabel
     */
    private static Opt<VertexLabel> labelOf(NodePattern cypher) {
        if (cypher.labels.isPresent()) {
            List<NodeLabel> labels = cypher.labels.get().value;
            if (labels.size() > 0) {
                if (labels.size() > 1) {
                    return unsupported("multiple vertex labels per pattern not yet supported");
                }

                return Opt.of(new VertexLabel(labels.get(0).value));
            }
        }

        return Opt.empty();
    }

    /**
     * Extract the edge label from a relationship pattern.
     *
     * @param cypher the Cypher relationship pattern
     * @return the optional property graph EdgeLabel
     */
    private static Opt<EdgeLabel> labelOf(RelationshipPattern cypher) {
        if (cypher.detail.isPresent()) {
            RelationshipDetail detail = cypher.detail.get();
            if (detail.types.isPresent()) {
                List<RelTypeName> types = detail.types.get().value;
                if (types.size() > 0) {
                    if (types.size() > 1) {
                        return unsupported("multiple edge labels per pattern not yet supported");
                    }

                    return Opt.of(new EdgeLabel(types.get(0).value));
                }
            }
        }

        return Opt.empty();
    }
}
