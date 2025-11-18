package hydra.tools;

import hydra.core.AnnotatedTerm;
import hydra.core.FloatValue;
import hydra.core.Function;
import hydra.core.IntegerValue;
import hydra.core.Lambda;
import hydra.core.Literal;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.WrappedTerm;
import hydra.lib.literals.ShowString;
import hydra.util.Maybe;

import java.util.Arrays;
import java.util.List;
import java.util.function.Consumer;
import java.util.stream.Collectors;


/**
 * A temporary pretty-printer for terms, for the sake of tests and debugging. In the future, JSON should be used.
 */
public class PrettyPrinter {
    private PrettyPrinter() {
    }

    /**
     * Serialize a term to a string.
     *
     * @param t the term to serialize
     * @return the string representation of the term
     */
    public static  String printTerm(Term t) {
        StringBuilder sb = new StringBuilder();
        term(t).accept(sb);
        return sb.toString();
    }

    /**
     * Create a consumer that renders a comma-separated list of items within brackets.
     *
     * @param open the opening bracket string
     * @param close the closing bracket string
     * @param consumers the list of consumers to render between the brackets
     * @return a consumer that appends the bracketed list to a StringBuilder
     */
    private static Consumer<StringBuilder> brackets(String open, String close,
        List<Consumer<StringBuilder>> consumers) {
        return sb -> {
            sb.append(open);
            boolean first = true;
            for (Consumer<StringBuilder> c : consumers) {
                if (first) {
                    first = false;
                } else {
                    sb.append(", ");
                }
                c.accept(sb);
            }
            sb.append(close);
        };
    }

    /**
     * Create a consumer that renders a float value.
     *
     * @param f the float value to render
     * @return a consumer that appends the float value to a StringBuilder
     */
    private static Consumer<StringBuilder> floatValue(FloatValue f) {
        return f.accept(new FloatValue.Visitor<Consumer<StringBuilder>>() {
            @Override
            public Consumer<StringBuilder> visit(FloatValue.Bigfloat instance) {
                return sb -> sb.append(instance.value);
            }

            @Override
            public Consumer<StringBuilder> visit(FloatValue.Float32 instance) {
                return sb -> sb.append(instance.value);
            }

            @Override
            public Consumer<StringBuilder> visit(FloatValue.Float64 instance) {
                return sb -> sb.append(instance.value);
            }
        });
    }

    /**
     * Create a consumer that renders a function.
     *
     * @param function the function to render
     * @return a consumer that appends the function to a StringBuilder
     */
    private static  Consumer<StringBuilder> function(hydra.core.Function function) {
        return function.accept(new Function.Visitor<Consumer<StringBuilder>>() {
            @Override
            public Consumer<StringBuilder> visit(Function.Elimination instance) {
                return notImplemented("elimination");
            }

            @Override
            public Consumer<StringBuilder> visit(Function.Lambda instance) {
                Lambda lam = instance.value;
                return sb -> {
                    sb.append("\"").append(shortName(lam.parameter)).append(" -> ");
                    term(lam.body).accept(sb);
                };
            }

            @Override
            public Consumer<StringBuilder> visit(Function.Primitive instance) {
                return sb -> sb.append(shortName(instance.value)).append("!");
            }
        });
    }

    /**
     * Create a consumer that renders an integer value.
     *
     * @param i the integer value to render
     * @return a consumer that appends the integer value to a StringBuilder
     */
    private static Consumer<StringBuilder> integerValue(IntegerValue i) {
        return i.accept(new IntegerValue.Visitor<Consumer<StringBuilder>>() {
            @Override
            public Consumer<StringBuilder> visit(IntegerValue.Bigint instance) {
                return sb -> sb.append(instance.value);
            }

            @Override
            public Consumer<StringBuilder> visit(IntegerValue.Int16 instance) {
                return sb -> sb.append(instance.value);
            }

            @Override
            public Consumer<StringBuilder> visit(IntegerValue.Int32 instance) {
                return sb -> sb.append(instance.value);
            }

            @Override
            public Consumer<StringBuilder> visit(IntegerValue.Int64 instance) {
                return sb -> sb.append(instance.value);
            }

            @Override
            public Consumer<StringBuilder> visit(IntegerValue.Int8 instance) {
                return sb -> sb.append(instance.value);
            }

            @Override
            public Consumer<StringBuilder> visit(IntegerValue.Uint16 instance) {
                return sb -> sb.append(instance.value);
            }

            @Override
            public Consumer<StringBuilder> visit(IntegerValue.Uint32 instance) {
                return sb -> sb.append(instance.value);
            }

            @Override
            public Consumer<StringBuilder> visit(IntegerValue.Uint64 instance) {
                return sb -> sb.append(instance.value);
            }

            @Override
            public Consumer<StringBuilder> visit(IntegerValue.Uint8 instance) {
                return sb -> sb.append(instance.value);
            }
        });
    }

    /**
     * Create a consumer that renders a literal value.
     *
     * @param l the literal to render
     * @return a consumer that appends the literal to a StringBuilder
     */
    private static Consumer<StringBuilder> literal(Literal l) {
        return l.accept(new Literal.Visitor<Consumer<StringBuilder>>() {
            @Override
            public Consumer<StringBuilder> visit(Literal.Binary instance) {
                return sb -> sb.append(instance.value);
            }

            @Override
            public Consumer<StringBuilder> visit(Literal.Boolean_ instance) {
                return sb -> sb.append(instance.value);
            }

            @Override
            public Consumer<StringBuilder> visit(Literal.Float_ instance) {
                return floatValue(instance.value);
            }

            @Override
            public Consumer<StringBuilder> visit(Literal.Integer_ instance) {
                return integerValue(instance.value);
            }

            @Override
            public Consumer<StringBuilder> visit(Literal.String_ instance) {
                return sb -> sb.append(ShowString.apply(instance.value));
            }
        });
    }

    /**
     * Create a consumer that renders a placeholder for not-yet-implemented features.
     *
     * @param name the name of the feature
     * @return a consumer that appends a placeholder to a StringBuilder
     */
    private static Consumer<StringBuilder> notImplemented(String name) {
        return sb -> sb.append("(").append(name).append(")");
    }

    /**
     * Create a consumer that renders items within parentheses.
     *
     * @param consumers the items to render
     * @return a consumer that appends the parenthesized items to a StringBuilder
     */
    private static Consumer<StringBuilder> parens(Consumer<StringBuilder>... consumers) {
        return parens(Arrays.asList(consumers));
    }

    /**
     * Create a consumer that renders items within parentheses.
     *
     * @param consumers the list of items to render
     * @return a consumer that appends the parenthesized items to a StringBuilder
     */
    private static Consumer<StringBuilder> parens(List<Consumer<StringBuilder>> consumers) {
        return brackets("(", ")", consumers);
    }

    /**
     * Extract the short name from a fully qualified name by taking the part after the last dot.
     *
     * @param name the fully qualified name
     * @return the short name (part after the last dot, or the entire name if no dot is present)
     */
    private static String shortName(Name name) {
        String s = name.value;
        int i = s.lastIndexOf('.');
        return i < 0 ? s : s.substring(i + 1);
    }

    /**
     * Create a consumer that renders items within square brackets.
     *
     * @param consumers the list of items to render
     * @return a consumer that appends the square-bracketed items to a StringBuilder
     */
    private static Consumer<StringBuilder> squareBrackets(List<Consumer<StringBuilder>> consumers) {
        return brackets("[", "]", consumers);
    }

    /**
     * Create a consumer that renders a term.
     *
     * @param t the term to render
     * @return a consumer that appends the term to a StringBuilder
     */
    private static Consumer<StringBuilder> term(Term t) {
        return sb -> {
            Consumer<StringBuilder> c = t.accept(new Term.Visitor<Consumer<StringBuilder>>() {
                @Override
                public Consumer<StringBuilder> visit(Term.Annotated instance) {
                    AnnotatedTerm ann = instance.value;
                    Consumer<StringBuilder> ac = sb1 -> sb1.append(ann.annotation.toString());
                    return var("annot", term(ann.body), ac);
                }

                @Override
                public Consumer<StringBuilder> visit(Term.Application instance) {
                    return var("apply", term(instance.value.function), term(instance.value.argument));
                }

                @Override
                public Consumer<StringBuilder> visit(Term.Function instance) {
                    return function(instance.value);
                }

                @Override
                public Consumer<StringBuilder> visit(Term.Let instance) {
                    return notImplemented("let");
                }

                @Override
                public Consumer<StringBuilder> visit(Term.List instance) {
                    List<Term> list = instance.value;
                    List<Consumer<StringBuilder>> cs =
                        list.stream().map(PrettyPrinter::term).collect(Collectors.toList());
                    return squareBrackets(cs);
                }

                @Override
                public Consumer<StringBuilder> visit(Term.Literal instance) {
                    return literal(instance.value);
                }

                @Override
                public Consumer<StringBuilder> visit(Term.Map instance) {
                    return notImplemented("map");
                }

                @Override
                public Consumer<StringBuilder> visit(Term.Maybe instance) {
                    Maybe<Term> opt = instance.value;
                    return sb -> {
                        if (opt.isJust()) {
                            var("just", term(opt.fromJust())).accept(sb);
                        } else {
                            sb.append("nothing");
                        }
                    };
                }

                @Override
                public Consumer<StringBuilder> visit(Term.Pair instance) {
                    return notImplemented("pair");
                }

                @Override
                public Consumer<StringBuilder> visit(Term.Either instance) {
                    return notImplemented("either");
                }

                @Override
                public Consumer<StringBuilder> visit(Term.Product instance) {
                    return notImplemented("product");
                }

                @Override
                public Consumer<StringBuilder> visit(Term.Record instance) {
                    return notImplemented("record");
                }

                @Override
                public Consumer<StringBuilder> visit(Term.Set instance) {
                    return notImplemented("set");
                }

                @Override
                public Consumer<StringBuilder> visit(Term.Sum instance) {
                    return notImplemented("sum");
                }

                @Override
                public Consumer<StringBuilder> visit(Term.TypeLambda instance) {
                    return notImplemented("typeLambda");
                }

                @Override
                public Consumer<StringBuilder> visit(Term.TypeApplication instance) {
                    return notImplemented("typeApplication");
                }

                @Override
                public Consumer<StringBuilder> visit(Term.Unit instance) {
                    return sb -> sb.append("unit");
                }

                @Override
                public Consumer<StringBuilder> visit(Term.Union instance) {
                    return notImplemented("union");
                }

                @Override
                public Consumer<StringBuilder> visit(Term.Variable instance) {
                    return sb -> sb.append("?").append(shortName(instance.value));
                }

                @Override
                public Consumer<StringBuilder> visit(Term.Wrap instance) {
                    WrappedTerm nom = instance.value;
                    return var(shortName(nom.typeName), term(nom.body));
                }
            });

            if (c != null) {
                c.accept(sb);
            }
        };
    }

    /**
     * Create a consumer that renders a variable or constructor with arguments.
     *
     * @param name the name of the variable or constructor
     * @param consumers the arguments to render
     * @return a consumer that appends the variable/constructor with arguments to a StringBuilder
     */
    private static Consumer<StringBuilder> var(String name, Consumer<StringBuilder>... consumers) {
        return sb -> {
            sb.append(name);
            parens(consumers).accept(sb);
        };
    }
}
