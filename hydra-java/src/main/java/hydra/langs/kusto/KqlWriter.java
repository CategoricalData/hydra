package hydra.langs.kusto;

import hydra.langs.kusto.kql.BetweenExpression;
import hydra.langs.kusto.kql.BinaryExpression;
import hydra.langs.kusto.kql.BinaryOperator;
import hydra.langs.kusto.kql.ColumnAlias;
import hydra.langs.kusto.kql.ColumnAssignment;
import hydra.langs.kusto.kql.ColumnName;
import hydra.langs.kusto.kql.Command;
import hydra.langs.kusto.kql.Datetime;
import hydra.langs.kusto.kql.Duration;
import hydra.langs.kusto.kql.DurationUnit;
import hydra.langs.kusto.kql.Expression;
import hydra.langs.kusto.kql.IndexExpression;
import hydra.langs.kusto.kql.JoinCommand;
import hydra.langs.kusto.kql.JoinKind;
import hydra.langs.kusto.kql.KeyValuePair;
import hydra.langs.kusto.kql.LetBinding;
import hydra.langs.kusto.kql.LetExpression;
import hydra.langs.kusto.kql.Literal;
import hydra.langs.kusto.kql.Order;
import hydra.langs.kusto.kql.Parameter;
import hydra.langs.kusto.kql.ParseCommand;
import hydra.langs.kusto.kql.PipelineExpression;
import hydra.langs.kusto.kql.PrintCommand;
import hydra.langs.kusto.kql.Projection;
import hydra.langs.kusto.kql.PropertyExpression;
import hydra.langs.kusto.kql.Query;
import hydra.langs.kusto.kql.SearchCommand;
import hydra.langs.kusto.kql.SortBy;
import hydra.langs.kusto.kql.SummarizeCommand;
import hydra.langs.kusto.kql.TableName;
import hydra.langs.kusto.kql.TabularExpression;
import hydra.langs.kusto.kql.TopCommand;
import hydra.langs.kusto.kql.UnaryExpression;
import hydra.langs.kusto.kql.UnaryOperator;
import hydra.langs.kusto.kql.UnionCommand;
import hydra.langs.kusto.kql.UnionKind;
import hydra.tools.MapperBase;

import java.util.List;
import java.util.function.Function;
import java.util.stream.Collectors;

/**
 * Simple serializer for Kusto Query Language (KQL) queries
 */
public class KqlWriter extends MapperBase {

    private static <T> String commaSep(List<T> parts, Function<T, String> toString) {
        return sep(", ", parts, toString);
    }

    private static <T> String sep(String delimiter, List<T> parts, Function<T, String> toString) {
        return parts.stream().map(toString).collect(Collectors.joining(delimiter));
    }

    private static String write(boolean b) {
        return b ? "true" : "false";
    }

    private static String write(double d) {
        return Double.toString(d);
    }

    private static String write(int i) {
        return Integer.toString(i);
    }

    private static String write(long l) {
        return Long.toString(l);
    }

    private static String write(String s) {
        return "\"" + s + "\""; // TODO: quoting
    }

    private static String write(BinaryOperator o) {
        return o.accept(new BinaryOperator.Visitor<String>() {
            @Override
            public String visit(BinaryOperator.CaseInsensitiveEqual instance) {
                return "=~";
            }

            @Override
            public String visit(BinaryOperator.Contains instance) {
                return "contains";
            }

            @Override
            public String visit(BinaryOperator.Divide instance) {
                return "/";
            }

            @Override
            public String visit(BinaryOperator.EndsWith instance) {
                return "endswith";
            }

            @Override
            public String visit(BinaryOperator.Equal instance) {
                return "==";
            }

            @Override
            public String visit(BinaryOperator.Greater instance) {
                return ">";
            }

            @Override
            public String visit(BinaryOperator.GreaterOrEqual instance) {
                return ">=";
            }

            @Override
            public String visit(BinaryOperator.Has instance) {
                return "has";
            }

            @Override
            public String visit(BinaryOperator.HasPrefix instance) {
                return "hasprefix";
            }

            @Override
            public String visit(BinaryOperator.HasSuffix instance) {
                return "hassuffix";
            }

            @Override
            public String visit(BinaryOperator.Less instance) {
                return "<";
            }

            @Override
            public String visit(BinaryOperator.LessOrEqual instance) {
                return "<=";
            }

            @Override
            public String visit(BinaryOperator.MatchesRegex instance) {
                return "matches regex";
            }

            @Override
            public String visit(BinaryOperator.Minus instance) {
                return "-";
            }

            @Override
            public String visit(BinaryOperator.NotEqual instance) {
                return "!=";
            }

            @Override
            public String visit(BinaryOperator.Plus instance) {
                return "+";
            }

            @Override
            public String visit(BinaryOperator.StartsWith instance) {
                return "startswith";
            }

            @Override
            public String visit(BinaryOperator.Times instance) {
                return "*";
            }
        });
    }

    private static String write(ColumnAlias a) {
        return write(a.column) + " = " + write(a.alias);
    }

    private static String write(ColumnAssignment a) {
        return write(a.column) + " = " + write(a.expression);
    }

    private static String write(ColumnName n) {
        return n.value;
    }

    private static String write(Command c) {
        return c.accept(new Command.Visitor<String>() {
            @Override
            public String visit(Command.Count instance) {
                return "count";
            }

            @Override
            public String visit(Command.Distinct instance) {
                return "distinct " + commaSep(instance.value, KqlWriter::write);
            }

            @Override
            public String visit(Command.Extend instance) {
                return "extend " + commaSep(instance.value, KqlWriter::write);
            }

            @Override
            public String visit(Command.Join instance) {
                JoinCommand c = instance.value;
                return "join kind="
                        + write(c.kind)
                        + " (" + write(c.expression) + ") on "
                        + write(c.on);
            }

            @Override
            public String visit(Command.Limit instance) {
                return "limit " + instance.value;
            }

            @Override
            public String visit(Command.Mvexpand instance) {
                return "mvexpand " + write(instance.value);
            }

            @Override
            public String visit(Command.OrderBy instance) {
                return "order by " + commaSep(instance.value, KqlWriter::write);
            }

            @Override
            public String visit(Command.Parse instance) {
                ParseCommand c = instance.value;
                return "parse " + c.column + " with " + sep("\n\t", c.pairs, KqlWriter::write);
            }

            @Override
            public String visit(Command.Print instance) {
                PrintCommand c = instance.value;
                return "print " + (c.column.map(columnName -> write(columnName) + "=").orElse(""))
                        + write(c.expression);
            }

            @Override
            public String visit(Command.Project instance) {
                return "project " + sep("\n\t, ", instance.value, KqlWriter::write);
            }

            @Override
            public String visit(Command.ProjectAway instance) {
                return "project-away " + sep("\n\t, ", instance.value, KqlWriter::write);
            }

            @Override
            public String visit(Command.ProjectRename instance) {
                return "project-rename " + sep("\n\t, ", instance.value, KqlWriter::write);
            }

            @Override
            public String visit(Command.Render instance) {
                return "render " + instance.value;
            }

            @Override
            public String visit(Command.Search instance) {
                SearchCommand c = instance.value;
                return "search "
                        + (c.datasets.size() > 0 ? "in (" + commaSep(c.datasets, KqlWriter::write) + ") " : "")
                        + write(c.pattern);
            }

            @Override
            public String visit(Command.SortBy instance) {
                return "sort by " + commaSep(instance.value, KqlWriter::write);
            }

            @Override
            public String visit(Command.Summarize instance) {
                SummarizeCommand c = instance.value;
                return "summarize " + sep("\n\t, ", c.columns, KqlWriter::write)
                        + (c.by.size() > 0 ? " by " + sep(", ", c.by, KqlWriter::write) : "");
            }

            @Override
            public String visit(Command.Take instance) {
                return "take " + instance.value;
            }

            @Override
            public String visit(Command.Top instance) {
                TopCommand c = instance.value;
                return "top " + c.count
                        + (c.sort.size() > 0 ? " by " + sep(", ", c.sort, KqlWriter::write) : "");
            }

            @Override
            public String visit(Command.Union instance) {
                UnionCommand c = instance.value;
                return "union "
                        + (c.parameters.size() > 0 ? sep(", ", c.parameters, KqlWriter::write) + " " : "")
                        + (c.kind.map(k -> "kind=" + write(k) + " ").orElse(""))
                        + (c.withSource.map(c1 -> "withsource=" + write(c1) + " ").orElse(""))
                        + (c.isFuzzy.map(f -> "isfuzzy=" + write(f) + " ").orElse(""))
                        + sep(", ", c.tables, KqlWriter::write);
            }

            @Override
            public String visit(Command.Where instance) {
                return "where " + write(instance.value);
            }
        });
    }

    private static String write(Datetime d) {
        return d.value;
    }

    private static String write(Duration d) {
        return d.value + write(d.unit);
    }

    private static String write(DurationUnit u) {
        return u.accept(new DurationUnit.Visitor<String>(){
            @Override
            public String visit(DurationUnit.Second instance) {
                return "s";
            }

            @Override
            public String visit(DurationUnit.Minute instance) {
                return "m";
            }

            @Override
            public String visit(DurationUnit.Hour instance) {
                return "h";
            }
        });
    }

    private static String write(Expression e) {
        return e.accept(new Expression.Visitor<String>(){
            @Override
            public String visit(Expression.And instance) {
                return sep(" and ", instance.value, KqlWriter::write);
            }

            @Override
            public String visit(Expression.Any instance) {
                return "*";
            }

            @Override
            public String visit(Expression.Between instance) {
                BetweenExpression e = instance.value;
                return write(e.expression) + " "
                        + (e.not ? "!" : "")
                        + "between("
                        + write(e.lowerBound)
                        + " .. "
                        + write(e.upperBound)
                        + ")";
            }

            @Override
            public String visit(Expression.Binary instance) {
                BinaryExpression e = instance.value;
                return write(e.left) + " " + write(e.operator) + " " + write(e.right);
            }

            @Override
            public String visit(Expression.Braces instance) {
                return "{" + write(instance.value) + "}";
            }

            @Override
            public String visit(Expression.Column instance) {
                return write(instance.value);
            }

            @Override
            public String visit(Expression.Dataset instance) {
                return write(instance.value);
            }

            @Override
            public String visit(Expression.Index instance) {
                IndexExpression e = instance.value;
                return write(e.expression) + "[" + write(e.index) + "]";
            }

            @Override
            public String visit(Expression.List instance) {
                return "(" + commaSep(instance.value, KqlWriter::write) + ")";
            }

            @Override
            public String visit(Expression.Literal instance) {
                return write(instance.value);
            }

            @Override
            public String visit(Expression.Or instance) {
                return sep(" or ", instance.value, KqlWriter::write);
            }

            @Override
            public String visit(Expression.Parentheses instance) {
                return "(" + write(instance.value) + ")";
            }

            @Override
            public String visit(Expression.Property instance) {
                PropertyExpression e = instance.value;
                return write(e.expression) + "." + write(e.property);
            }

            @Override
            public String visit(Expression.Unary instance) {
                UnaryExpression e = instance.value;
                return write(e.operator) + " " + write(e.expression);
            }
        });
    }

    private static String write(JoinKind k) {
        return k.accept(new JoinKind.Visitor<String>() {
            @Override
            public String visit(JoinKind.Leftouter instance) {
                return "leftouter";
            }

            @Override
            public String visit(JoinKind.Leftsemi instance) {
                return "leftsemi";
            }

            @Override
            public String visit(JoinKind.Leftanti instance) {
                return "leftanti";
            }

            @Override
            public String visit(JoinKind.Fullouter instance) {
                return "fullouter";
            }

            @Override
            public String visit(JoinKind.Inner instance) {
                return "inner";
            }

            @Override
            public String visit(JoinKind.Innerunique instance) {
                return "innerunique";
            }

            @Override
            public String visit(JoinKind.Rightouter instance) {
                return "rightouter";
            }

            @Override
            public String visit(JoinKind.Rightsemi instance) {
                return "rightsemi";
            }

            @Override
            public String visit(JoinKind.Rightanti instance) {
                return "rightanti";
            }
        });
    }

    private static String write(KeyValuePair p) {
        return p.key + " " + write(p.value);
    }

    private static String write(LetBinding b) {
        return write(b.name) + " = " + write(b.expression) + ";";
    }

    private static String write(LetExpression l) {
        return sep("\n", l.bindings, KqlWriter::write) + "\n" + write(l.expression);
    }

    private static String write(Literal l) {
        return l.accept(new Literal.Visitor<String>(){
            @Override
            public String visit(Literal.Duration instance) {
                return write(instance.value);
            }

            @Override
            public String visit(Literal.Datetime instance) {
                return write(instance.value);
            }

            @Override
            public String visit(Literal.String_ instance) {
                return write(instance.value);
            }

            @Override
            public String visit(Literal.Int instance) {
                return write(instance.value);
            }

            @Override
            public String visit(Literal.Long_ instance) {
                return write(instance.value);
            }

            @Override
            public String visit(Literal.Double_ instance) {
                return write(instance.value);
            }

            @Override
            public String visit(Literal.Boolean_ instance) {
                return write(instance.value);
            }
        });
    }

    private static String write(Order o) {
        return o.accept(new Order.Visitor<String>() {
            @Override
            public String visit(Order.Ascending instance) {
                return "asc";
            }

            @Override
            public String visit(Order.Descending instance) {
                return "desc";
            }
        });
    }

    private static String write(Parameter p) {
        return p.key + "=" + write(p.value);
    }

    private static String write(PipelineExpression p) {
        return String.join("\n\t| ", map(p.value, KqlWriter::write));
    }

    private static String write(Projection p) {
        return write(p.expression) + (p.alias.map(columnName -> " = " + write(columnName)).orElse(""));
    }

    public static String write(Query query) {
        return write(query.value);
    }

    private static String write(SortBy s) {
        return write(s.column) + (s.order.map(order -> " by " + write(order)).orElse(""));
    }

    private static String write(TableName n) {
        return n.value;
    }

    private static String write(TabularExpression e) {
        return e.accept(new TabularExpression.Visitor<String>() {
            @Override
            public String visit(TabularExpression.Command instance) {
                return write(instance.value);
            }

            @Override
            public String visit(TabularExpression.Pipeline instance) {
                return write(instance.value);
            }

            @Override
            public String visit(TabularExpression.Let instance) {
                return write(instance.value);
            }

            @Override
            public String visit(TabularExpression.Table instance) {
                return write(instance.value);
            }
        });
    }

    private static String write(UnaryOperator o) {
        return o.accept(new UnaryOperator.Visitor<String>(){
            @Override
            public String visit(UnaryOperator.Not instance) {
                return "not";
            }
        });
    }

    private static String write(UnionKind k) {
        return k.accept(new UnionKind.Visitor<String>() {
            @Override
            public String visit(UnionKind.Inner instance) {
                return "inner";
            }

            @Override
            public String visit(UnionKind.Outer instance) {
                return "outer";
            }
        });
    }
}
