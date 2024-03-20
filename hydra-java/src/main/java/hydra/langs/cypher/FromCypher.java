package hydra.langs.cypher;

import hydra.langs.cypher.openCypher.AddOrSubtractExpression;
import hydra.langs.cypher.openCypher.AddOrSubtractRightHandSide;
import hydra.langs.cypher.openCypher.AndExpression;
import hydra.langs.cypher.openCypher.Atom;
import hydra.langs.cypher.openCypher.ComparisonExpression;
import hydra.langs.cypher.openCypher.ListOperatorExpressionOrPropertyLookup;
import hydra.langs.cypher.openCypher.Match;
import hydra.langs.cypher.openCypher.MultiplyDivideModuloExpression;
import hydra.langs.cypher.openCypher.MultiplyDivideModuloRightHandSide;
import hydra.langs.cypher.openCypher.NodeLabel;
import hydra.langs.cypher.openCypher.NodePattern;
import hydra.langs.cypher.openCypher.NodePatternChain;
import hydra.langs.cypher.openCypher.NonArithmeticOperatorExpression;
import hydra.langs.cypher.openCypher.NotExpression;
import hydra.langs.cypher.openCypher.OrExpression;
import hydra.langs.cypher.openCypher.PartialComparisonExpression;
import hydra.langs.cypher.openCypher.PatternElement;
import hydra.langs.cypher.openCypher.PatternElementChain;
import hydra.langs.cypher.openCypher.PatternPart;
import hydra.langs.cypher.openCypher.PowerOfExpression;
import hydra.langs.cypher.openCypher.ProjectionBody;
import hydra.langs.cypher.openCypher.ProjectionItem;
import hydra.langs.cypher.openCypher.ProjectionItems;
import hydra.langs.cypher.openCypher.Properties;
import hydra.langs.cypher.openCypher.PropertyKeyName;
import hydra.langs.cypher.openCypher.ReadingClause;
import hydra.langs.cypher.openCypher.RegularQuery;
import hydra.langs.cypher.openCypher.RelTypeName;
import hydra.langs.cypher.openCypher.RelationshipDetail;
import hydra.langs.cypher.openCypher.RelationshipPattern;
import hydra.langs.cypher.openCypher.SinglePartQuery;
import hydra.langs.cypher.openCypher.SingleQuery;
import hydra.langs.cypher.openCypher.StringListNullPredicateExpression;
import hydra.langs.cypher.openCypher.StringListNullPredicateRightHandSide;
import hydra.langs.cypher.openCypher.UnaryAddOrSubtractExpression;
import hydra.langs.cypher.openCypher.UpdatingClause;
import hydra.langs.cypher.openCypher.XorExpression;
import hydra.langs.tinkerpop.propertyGraph.Direction;
import hydra.langs.tinkerpop.propertyGraph.EdgeLabel;
import hydra.langs.tinkerpop.propertyGraph.PropertyKey;
import hydra.langs.tinkerpop.propertyGraph.VertexLabel;
import hydra.langs.tinkerpop.queries.AssociativeExpression;
import hydra.langs.tinkerpop.queries.BinaryBooleanOperator;
import hydra.langs.tinkerpop.queries.BinaryExpression;
import hydra.langs.tinkerpop.queries.BinaryOperator;
import hydra.langs.tinkerpop.queries.ComparisonOperator;
import hydra.langs.tinkerpop.queries.EdgeProjectionPattern;
import hydra.langs.tinkerpop.queries.Expression;
import hydra.langs.tinkerpop.queries.MatchQuery;
import hydra.langs.tinkerpop.queries.Projection;
import hydra.langs.tinkerpop.queries.Projections;
import hydra.langs.tinkerpop.queries.PropertyPattern;
import hydra.langs.tinkerpop.queries.PropertyProjection;
import hydra.langs.tinkerpop.queries.Query;
import hydra.langs.tinkerpop.queries.SelectQuery;
import hydra.langs.tinkerpop.queries.UnaryExpression;
import hydra.langs.tinkerpop.queries.UnaryOperator;
import hydra.langs.tinkerpop.queries.Variable;
import hydra.langs.tinkerpop.queries.VertexPattern;
import hydra.tools.MapperBase;

import java.util.ArrayList;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

import static hydra.langs.tinkerpop.dsl.Queries.apply;
import static hydra.langs.tinkerpop.dsl.Queries.query;

public class FromCypher extends MapperBase {

    public static Expression from(AddOrSubtractExpression cypher) {
        Expression cur = from(cypher.left);

        for (AddOrSubtractRightHandSide rhs : cypher.right) {
            return unsupported();
        }

        return cur;
    }

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

    public static ComparisonOperator from(hydra.langs.cypher.openCypher.ComparisonOperator cypher) {
        return cypher.accept(new hydra.langs.cypher.openCypher.ComparisonOperator.Visitor<>() {
            @Override
            public ComparisonOperator visit(hydra.langs.cypher.openCypher.ComparisonOperator.Eq instance) {
                return new ComparisonOperator.Eq();
            }

            @Override
            public ComparisonOperator visit(hydra.langs.cypher.openCypher.ComparisonOperator.Neq instance) {
                return new ComparisonOperator.Neq();
            }

            @Override
            public ComparisonOperator visit(hydra.langs.cypher.openCypher.ComparisonOperator.Lt instance) {
                return new ComparisonOperator.Lt();
            }

            @Override
            public ComparisonOperator visit(hydra.langs.cypher.openCypher.ComparisonOperator.Gt instance) {
                return new ComparisonOperator.Gt();
            }

            @Override
            public ComparisonOperator visit(hydra.langs.cypher.openCypher.ComparisonOperator.Lte instance) {
                return new ComparisonOperator.Lte();
            }

            @Override
            public ComparisonOperator visit(hydra.langs.cypher.openCypher.ComparisonOperator.Gte instance) {
                return new ComparisonOperator.Gte();
            }
        });
    }

    public static Expression from(hydra.langs.cypher.openCypher.Expression cypher) {
        return from(cypher.value);
    }

    public static Expression from(OrExpression cypher) {
        return oneOrMany(cypher.value, FromCypher::from, exps -> new Expression.Associative(new AssociativeExpression(
                new BinaryOperator.Boolean_(new BinaryBooleanOperator.Or()),
                map(exps, FromCypher::from))));
    }

    public static Expression from(XorExpression cypher) {
        return oneOrMany(cypher.value, FromCypher::from, exps -> new Expression.Associative(new AssociativeExpression(
                new BinaryOperator.Boolean_(new BinaryBooleanOperator.Xor()),
                map(exps, FromCypher::from))));
    }

    public static Expression from(AndExpression cypher) {
        return oneOrMany(cypher.value, FromCypher::from, exps -> new Expression.Associative(new AssociativeExpression(
                new BinaryOperator.Boolean_(new BinaryBooleanOperator.And()),
                map(exps, FromCypher::from))));
    }

    public static Expression from(NotExpression cypher) {
        return cypher.not
                ? new Expression.Unary(new UnaryExpression(new UnaryOperator.Negate(), from(cypher.expression)))
                : from(cypher.expression);
    }

    public static Expression from(ComparisonExpression cypher) {
        Expression cur = from(cypher.left);
        for (PartialComparisonExpression pce : cypher.right) {
            cur = new Expression.Binary(
                    new BinaryExpression(cur, new BinaryOperator.Comparison(from(pce.operator)), from(pce.right)));
        }
        return cur;
    }

    public static MatchQuery from(Match cypher) {
        return new MatchQuery(
                cypher.optional,
                map(cypher.pattern.value, FromCypher::from),
                cypher.where.map(w -> from(w.value)));
    }

    public static Expression from(MultiplyDivideModuloExpression cypher) {
        Expression cur = from(cypher.left);

        for (MultiplyDivideModuloRightHandSide rhs : cypher.right) {
            return unsupported();
        }

        return cur;
    }

    public static VertexPattern from(NodePattern cypher) {
        return new VertexPattern(
                cypher.variable.map(FromCypher::from),
                labelOf(cypher),
                cypher.properties.isPresent()
                        ? from(cypher.properties.get()) : Collections.emptyList(),
                Collections.emptyList());
    }

    public static VertexPattern from(NodePatternChain cypher) {
        VertexPattern vq = from(cypher.nodePattern);
        return cypher.chain.size() > 0
                ? vq.withEdges(Collections.singletonList(from(cypher.chain)))
                : vq;
    }

    public static Expression from(NonArithmeticOperatorExpression cypher) {
        if (cypher.labels.isPresent()) {
            return unsupported();
        }

        Expression cur = from(cypher.atom);

        for (ListOperatorExpressionOrPropertyLookup l : cypher.listsAndLookups) {
            final Expression c = cur;
            cur = l.accept(new ListOperatorExpressionOrPropertyLookup.Visitor<>() {
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
            Optional<EdgeLabel> label = labelOf(chain.relationship);
            Optional<Properties> ps = chain.relationship.detail.flatMap(r -> r.properties);
            List<PropertyPattern> props = ps.isPresent()
                    ? from(ps.get())
                    : Collections.emptyList();
            cur = new EdgeProjectionPattern(dir, label, props, Optional.of(vp));
        }

        return cur;
    }

    public static List<PropertyPattern> from(Properties cypher) {
        return unsupported();
    }

    public static Expression from(PatternElement cypher) {
        return cypher.accept(new PatternElement.Visitor<>() {
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

    public static Projection from(PatternPart cypher) {
        return new Projection(
                from(cypher.pattern.value),
                cypher.variable.map(FromCypher::from));
    }

    public static Expression from(PowerOfExpression cypher) {
        return oneOrMany(cypher.value, FromCypher::from, exps -> new Expression.Associative(new AssociativeExpression(
                new BinaryOperator.Power(),
                map(exps, FromCypher::from))));
    }

    public static Query from(hydra.langs.cypher.openCypher.Query cypher) {
        return cypher.accept(new hydra.langs.cypher.openCypher.Query.Visitor<>() {
            @Override
            public Query visit(hydra.langs.cypher.openCypher.Query.Regular instance) {
                return from(instance.value);
            }

            @Override
            public Query visit(hydra.langs.cypher.openCypher.Query.Standalone instance) {
                return unsupported();
            }
        });
    }

    public static SelectQuery from(ProjectionBody cypher) {
        return new SelectQuery(cypher.distinct, from(cypher.projectionItems));
    }

    public static Projection from(ProjectionItem cypher) {
        return new Projection(
                from(cypher.expression),
                cypher.variable.map(FromCypher::from));
    }

    public static Projections from(ProjectionItems cypher) {
        return new Projections(cypher.star, map(cypher.explicit, FromCypher::from));
    }

    public static PropertyKey from(PropertyKeyName cypher) {
        return new PropertyKey(cypher.value);
    }

    public static Query from(ReadingClause cypher) {
        return cypher.accept(new ReadingClause.Visitor<>() {
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

    public static Query from(RegularQuery cypher) {
        if (cypher.rest.size() > 0) {
            return unsupported();
        }

        return from(cypher.head);
    }

    public static Query from(SingleQuery cypher) {
        return cypher.accept(new hydra.langs.cypher.openCypher.SingleQuery.Visitor<>() {
            @Override
            public Query visit(hydra.langs.cypher.openCypher.SingleQuery.SinglePart instance) {
                return from(instance.value);
            }

            @Override
            public Query visit(hydra.langs.cypher.openCypher.SingleQuery.MultiPart instance) {
                return unsupported();
            }
        });
    }

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

    public static Expression from(StringListNullPredicateExpression cypher) {
        Expression cur = from(cypher.left);

        for (StringListNullPredicateRightHandSide rhs : cypher.right) {
            return unsupported();
        }

        return cur;
    }

    public static Query from(UpdatingClause cypher) {
        return unsupported();
    }

    public static Expression from(UnaryAddOrSubtractExpression cypher) {
        return cypher.operator.isPresent()
                ? unsupported()
                : from(cypher.expression);
    }

    public static Variable from(hydra.langs.cypher.openCypher.Variable cypher) {
        return new Variable(cypher.value);
    }

    ////////////////////////////////////////////////////////////////////////////

    private static void checkRelationshipDetail(RelationshipDetail cypher) {
        if (cypher.range.isPresent()) {
            throw new MapperException("not yet supported");
        }
        if (cypher.variable.isPresent()) {
            throw new MapperException("not yet supported");
        }
    }

    private static Direction directionOf(RelationshipPattern cypher) {
        return cypher.leftArrow
                ? (cypher.rightArrow ? new Direction.Both() : new Direction.In())
                : (cypher.rightArrow ? new Direction.Out() : new Direction.Undirected());
    }

    private static Optional<VertexLabel> labelOf(NodePattern cypher) {
        if (cypher.labels.isPresent()) {
            List<NodeLabel> labels = cypher.labels.get().value;
            if (labels.size() > 0) {
                if (labels.size() > 1) {
                    return unsupported("multiple vertex labels per pattern not yet supported");
                }

                return Optional.of(new VertexLabel(labels.get(0).value));
            }
        }

        return Optional.empty();
    }

    private static Optional<EdgeLabel> labelOf(RelationshipPattern cypher) {
        if (cypher.detail.isPresent()) {
            RelationshipDetail detail = cypher.detail.get();
            if (detail.types.isPresent()) {
                List<RelTypeName> types = detail.types.get().value;
                if (types.size() > 0) {
                    if (types.size() > 1) {
                        return unsupported("multiple edge labels per pattern not yet supported");
                    }

                    return Optional.of(new EdgeLabel(types.get(0).value));
                }
            }
        }

        return Optional.empty();
    }
}
