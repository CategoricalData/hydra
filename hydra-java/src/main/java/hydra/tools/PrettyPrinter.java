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
import hydra.util.Opt;

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
     */
    public static  String printTerm(Term t) {
        StringBuilder sb = new StringBuilder();
        term(t).accept(sb);
        return sb.toString();
    }

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

    private static Consumer<StringBuilder> notImplemented(String name) {
        return sb -> sb.append("(").append(name).append(")");
    }

    private static Consumer<StringBuilder> parens(Consumer<StringBuilder>... consumers) {
        return parens(Arrays.asList(consumers));
    }

    private static Consumer<StringBuilder> parens(List<Consumer<StringBuilder>> consumers) {
        return brackets("(", ")", consumers);
    }

    private static String shortName(Name name) {
        String s = name.value;
        int i = s.lastIndexOf('.');
        return i < 0 ? s : s.substring(i + 1);
    }

    private static Consumer<StringBuilder> squareBrackets(List<Consumer<StringBuilder>> consumers) {
        return brackets("[", "]", consumers);
    }

    private static Consumer<StringBuilder> term(Term t) {
        return sb -> {
            Consumer<StringBuilder> c = t.accept(new Term.Visitor<Consumer<StringBuilder>>() {
                @Override
                public Consumer<StringBuilder> visit(Term.Annotated instance) {
                    AnnotatedTerm ann = instance.value;
                    Consumer<StringBuilder> ac = sb1 -> sb1.append(ann.annotation.toString());
                    return var("annot", term(ann.subject), ac);
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
                public Consumer<StringBuilder> visit(Term.Optional instance) {
                    Opt<Term> opt = instance.value;
                    return sb -> {
                        if (opt.isPresent()) {
                            var("just", term(opt.get())).accept(sb);
                        } else {
                            sb.append("nothing");
                        }
                    };
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
                public Consumer<StringBuilder> visit(Term.TypeAbstraction instance) {
                    return notImplemented("typeAbstraction");
                }

                @Override
                public Consumer<StringBuilder> visit(Term.TypeApplication instance) {
                    return notImplemented("typeApplication");
                }

                @Override
                public Consumer<StringBuilder> visit(Term.Typed instance) {
                    return notImplemented("typed");
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
                    return var(shortName(nom.typeName), term(nom.object));
                }
            });

            if (c != null) {
                c.accept(sb);
            }
        };
    }

    private static Consumer<StringBuilder> var(String name, Consumer<StringBuilder>... consumers) {
        return sb -> {
            sb.append(name);
            parens(consumers).accept(sb);
        };
    }
}
