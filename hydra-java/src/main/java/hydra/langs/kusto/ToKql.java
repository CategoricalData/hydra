package hydra.langs.kusto;

import hydra.langs.kusto.kql.ColumnName;
import hydra.langs.kusto.kql.Command;
import hydra.langs.kusto.kql.PipelineExpression;
import hydra.langs.kusto.kql.TableName;
import hydra.langs.kusto.kql.TabularExpression;
import hydra.langs.tinkerpop.propertyGraph.PropertyKey;
import hydra.langs.tinkerpop.propertyGraph.VertexLabel;
import hydra.langs.tinkerpop.queries.Expression;
import hydra.langs.tinkerpop.queries.MatchQuery;
import hydra.langs.tinkerpop.queries.Projection;
import hydra.langs.tinkerpop.queries.Projections;
import hydra.langs.tinkerpop.queries.PropertyProjection;
import hydra.langs.tinkerpop.queries.Query;
import hydra.langs.tinkerpop.queries.SelectQuery;
import hydra.langs.tinkerpop.queries.VertexPattern;
import hydra.tools.MapperBase;

public class ToKql extends MapperBase {

    public static hydra.langs.kusto.kql.Expression toKql(Expression pg) {
        return pg.accept(new Expression.Visitor<hydra.langs.kusto.kql.Expression>() {
            @Override
            public hydra.langs.kusto.kql.Expression visit(Expression.Associative instance) {
                return unsupported();
            }

            @Override
            public hydra.langs.kusto.kql.Expression visit(Expression.Binary instance) {
                return unsupported();
            }

            @Override
            public hydra.langs.kusto.kql.Expression visit(Expression.Property instance) {
                PropertyProjection p = instance.value;

                // TODO: use the base
                return toKql(p.key);
            }

            @Override
            public hydra.langs.kusto.kql.Expression visit(Expression.Unary instance) {
                return unsupported();
            }

            @Override
            public hydra.langs.kusto.kql.Expression visit(Expression.Variable instance) {
                return unsupported();
            }

            @Override
            public hydra.langs.kusto.kql.Expression visit(Expression.Vertex instance) {
                return unsupported();
            }
        });
    }

    public static hydra.langs.kusto.kql.Expression toKql(PropertyKey pg) {
        return new hydra.langs.kusto.kql.Expression.Column(new ColumnName(pg.value));
    }

    public static TabularExpression toKql(SelectQuery pg) {
        if (pg.distinct) {
            return unsupported();
        }

        Projections p = pg.projection;
        if (p.all) {
            return unsupported();
        }

        return new TabularExpression.Command(
                new Command.Project(map(pg.projection.explicit, ToKql::toKql)));
    }

    public static hydra.langs.kusto.kql.Projection toKql(Projection pg) {
        return new hydra.langs.kusto.kql.Projection(toKql(pg.value), pg.as.map(v -> new ColumnName(v.value)));
    }

    public static TabularExpression toKql(MatchQuery pg) {
        if (pg.where.isPresent() && pg.optional) {
            return unsupported();
        }

        if (pg.pattern.size() != 1) {
            return unsupported();
        }

        Projection p = pg.pattern.get(0);
        if (p.as.isPresent()) {
            return unsupported();
        }

        return toTabularExpression(p.value);
    }

    public static TabularExpression toKql(Query pg) {
        return pg.accept(new Query.Visitor<TabularExpression>() {
            @Override
            public TabularExpression visit(Query.Application instance) {
                return new TabularExpression.Pipeline(
                        new PipelineExpression(map(instance.value.value, ToKql::toKql)));
            }

            @Override
            public TabularExpression visit(Query.Aggregate instance) {
                return unsupported();
            }

            @Override
            public TabularExpression visit(Query.LetQuery instance) {
                return unsupported();
            }

            @Override
            public TabularExpression visit(Query.Match instance) {
                return toKql(instance.value);
            }

            @Override
            public TabularExpression visit(Query.Select instance) {
                return toKql(instance.value);
            }

            @Override
            public TabularExpression visit(Query.Value instance) {
                return unsupported();
            }
        });
    }

    public static TableName toKql(VertexLabel pg) {
        return new TableName(pg.value);
    }

    public static TabularExpression toTabularExpression(Expression pg) {
        return pg.accept(new Expression.PartialVisitor<TabularExpression>() {
            @Override
            public TabularExpression otherwise(Expression instance) {
                return unsupported();
            }

            @Override
            public TabularExpression visit(Expression.Vertex instance) {
                VertexPattern vp = instance.value;
                if (!vp.label.isPresent()) {
                    return unsupported();
                }

                if (vp.edges.size() > 0) {
                    return unsupported();
                }

                if (vp.properties.size() > 0) {
                    return unsupported();
                }

                // Note: vp.variable is ignored for now

                VertexLabel l = vp.label.get();
                return new TabularExpression.Table(toKql(l));
            }
        });
    }
}
