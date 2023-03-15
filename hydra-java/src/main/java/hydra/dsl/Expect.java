package hydra.dsl;

import hydra.Reduction;
import hydra.compute.Flow;
import hydra.core.FloatValue;
import hydra.core.IntegerValue;
import hydra.core.Literal;
import hydra.core.Term;
import hydra.graph.Graph;
import java.math.BigInteger;
import java.util.List;
import java.util.Optional;
import java.util.Set;
import java.util.function.Function;

import static hydra.Flows.*;


public interface Expect {
    static <S, A> Flow<S, Double> bigfloat(final Term<A> term) {
        return bind(float_(term), floatValue -> floatValue.accept(new FloatValue.PartialVisitor<Flow<S, Double>>() {
            @Override
            public Flow<S, Double> otherwise(FloatValue instance) {
                return unexpected("bigfloat", instance);
            }

            @Override
            public Flow<S, Double> visit(FloatValue.Bigfloat instance) {
                return pure(instance.value);
            }
        }));
    }

    static <S, A> Flow<S, BigInteger> bigint(final Term<A> term) {
        return bind(integer(term), integerValue -> integerValue.accept(new IntegerValue.PartialVisitor<Flow<S, BigInteger>>() {
            @Override
            public Flow<S, BigInteger> otherwise(IntegerValue instance) {
                return unexpected("bigint", instance);
            }

            @Override
            public Flow<S, BigInteger> visit(IntegerValue.Bigint instance) {
                return pure(instance.value);
            }
        }));
    }

    static <S, A> Flow<S, String> binary(final Term<A> term) {
        return bind(literal(term), literal -> literal.accept(new Literal.PartialVisitor<Flow<S, String>>() {
            @Override
            public Flow<S, String> otherwise(Literal instance) {
                return unexpected("binary", instance);
            }

            @Override
            public Flow<S, String> visit(Literal.Binary instance) {
                return pure(instance.value);
            }
        }));
    }

    static <S, A> Flow<S, Boolean> boolean_(final Term<A> term) {
        return bind(literal(term), literal -> literal.accept(new Literal.PartialVisitor<Flow<S, Boolean>>() {
            @Override
            public Flow<S, Boolean> otherwise(Literal instance) {
                return unexpected("boolean", instance);
            }

            @Override
            public Flow<S, Boolean> visit(Literal.Boolean_ instance) {
                return pure(instance.value);
            }
        }));
    }

    static <S, A> Flow<S, FloatValue> float_(final Term<A> term) {
        return bind(literal(term), literal -> literal.accept(new Literal.PartialVisitor<Flow<S, FloatValue>>() {
            @Override
            public Flow<S, FloatValue> otherwise(Literal instance) {
                return unexpected("float", instance);
            }

            @Override
            public Flow<S, FloatValue> visit(Literal.Float_ instance) {
                return pure(instance.value);
            }
        }));
    }

    static <S, A> Flow<S, Float> float32(final Term<A> term) {
        return bind(float_(term), floatValue -> floatValue.accept(new FloatValue.PartialVisitor<Flow<S, Float>>() {
            @Override
            public Flow<S, Float> otherwise(FloatValue instance) {
                return unexpected("float32", instance);
            }

            @Override
            public Flow<S, Float> visit(FloatValue.Float32 instance) {
                return pure(instance.value);
            }
        }));
    }

    static <S, A> Flow<S, Double> float64(final Term<A> term) {
        return bind(float_(term), floatValue -> floatValue.accept(new FloatValue.PartialVisitor<Flow<S, Double>>() {
            @Override
            public Flow<S, Double> otherwise(FloatValue instance) {
                return unexpected("float64", instance);
            }

            @Override
            public Flow<S, Double> visit(FloatValue.Float64 instance) {
                return pure(instance.value);
            }
        }));
    }

    static <A, X, Y> Function<X, Flow<Graph<A>, Y>> function(
        final Function<X, Term<A>> fin,
        final Function<Term<A>, Flow<Graph<A>, Y>> fout,
        final Term<A> func) {
        return x -> bind(Reduction.reduceLazy(Terms.apply(func, fin.apply(x))), fout);
    }

    static <S, A> Flow<S, Short> int8(final Term<A> term) {
        return bind(integer(term), integerValue -> integerValue.accept(new IntegerValue.PartialVisitor<Flow<S, Short>>() {
            @Override
            public Flow<S, Short> otherwise(IntegerValue instance) {
                return unexpected("int8", instance);
            }

            @Override
            public Flow<S, Short> visit(IntegerValue.Int8 instance) {
                return pure(instance.value);
            }
        }));
    }

    static <S, A> Flow<S, Short> int16(final Term<A> term) {
        return bind(integer(term), integerValue -> integerValue.accept(new IntegerValue.PartialVisitor<Flow<S, Short>>() {
            @Override
            public Flow<S, Short> otherwise(IntegerValue instance) {
                return unexpected("int16", instance);
            }

            @Override
            public Flow<S, Short> visit(IntegerValue.Int16 instance) {
                return pure(instance.value);
            }
        }));
    }

    static <S, A> Flow<S, Integer> int32(final Term<A> term) {
        return bind(integer(term), integerValue -> integerValue.accept(new IntegerValue.PartialVisitor<Flow<S, Integer>>() {
            @Override
            public Flow<S, Integer> otherwise(IntegerValue instance) {
                return unexpected("int32", instance);
            }

            @Override
            public Flow<S, Integer> visit(IntegerValue.Int32 instance) {
                return pure(instance.value);
            }
        }));
    }

    static <S, A> Flow<S, Long> int64(final Term<A> term) {
        return bind(integer(term), integerValue -> integerValue.accept(new IntegerValue.PartialVisitor<Flow<S, Long>>() {
            @Override
            public Flow<S, Long> otherwise(IntegerValue instance) {
                return unexpected("int64", instance);
            }

            @Override
            public Flow<S, Long> visit(IntegerValue.Int64 instance) {
                return pure(instance.value);
            }
        }));
    }

    static <S, A> Flow<S, IntegerValue> integer(final Term<A> term) {
        return bind(literal(term), literal -> literal.accept(new Literal.PartialVisitor<Flow<S, IntegerValue>>() {
            @Override
            public Flow<S, IntegerValue> otherwise(Literal instance) {
                return unexpected("float", instance);
            }

            @Override
            public Flow<S, IntegerValue> visit(Literal.Integer_ instance) {
                return pure(instance.value);
            }
        }));
    }

    static <S, A, X> Flow<S, List<X>> list(final Function<Term<A>, Flow<S, X>> elems, final Term<A> term) {
        return term.accept(new Term.PartialVisitor<Flow<S, List<X>>>() {
            @Override
            public Flow<S, List<X>> otherwise(Term instance) {
                return unexpected("list", instance);
            }

            @Override
            public Flow<S, List<X>> visit(Term.List instance) {
                return mapM(((Term.List<A>) instance).value, elems);
            }
        });
    }

    static <S, A> Flow<S, Literal> literal(final Term<A> term) {
        return term.accept(new Term.PartialVisitor<Flow<S, Literal>>() {
            @Override
            public Flow<S, Literal> otherwise(Term instance) {
                return unexpected("literal", instance);
            }

            @Override
            public Flow<S, Literal> visit(Term.Literal instance) {
                return pure(instance.value);
            }
        });
    }

    static <S, A, X> Flow<S, Optional<X>> optional(final Function<Term<A>, Flow<S, X>> elems, final Term<A> term) {
        return term.accept(new Term.PartialVisitor<Flow<S, Optional<X>>>() {
            @Override
            public Flow<S, Optional<X>> otherwise(Term instance) {
                return unexpected("optional", instance);
            }

            @Override
            public Flow<S, Optional<X>> visit(Term.Optional instance) {
                return instance.value.isPresent() ? map(elems.apply((Term<A>) instance.value.get()), Optional::of)
                    : pure(Optional.empty());
            }
        });
    }

    static <S, A, X> Flow<S, Set<X>> set(final Function<Term<A>, Flow<S, X>> elems, final Term<A> term) {
        return term.accept(new Term.PartialVisitor<Flow<S, Set<X>>>() {
            @Override
            public Flow<S, Set<X>> otherwise(Term instance) {
                return unexpected("set", instance);
            }

            @Override
            public Flow<S, Set<X>> visit(Term.Set instance) {
                return mapM(((Term.Set<A>) instance).value, elems);
            }
        });
    }

    static <S, A> Flow<S, String> string(final Term<A> term) {
        return bind(literal(term), literal -> literal.accept(new Literal.PartialVisitor<Flow<S, String>>() {
            @Override
            public Flow<S, String> otherwise(Literal instance) {
                return unexpected("string", instance);
            }

            @Override
            public Flow<S, String> visit(Literal.String_ instance) {
                return pure(instance.value);
            }
        }));
    }

    static <S, A> Flow<S, Byte> uint8(final Term<A> term) {
        return bind(integer(term), integerValue -> integerValue.accept(new IntegerValue.PartialVisitor<Flow<S, Byte>>() {
            @Override
            public Flow<S, Byte> otherwise(IntegerValue instance) {
                return unexpected("uint8", instance);
            }

            @Override
            public Flow<S, Byte> visit(IntegerValue.Uint8 instance) {
                return pure(instance.value);
            }
        }));
    }

    static <S, A> Flow<S, Character> uint16(final Term<A> term) {
        return bind(integer(term), integerValue -> integerValue.accept(new IntegerValue.PartialVisitor<Flow<S, Character>>() {
            @Override
            public Flow<S, Character> otherwise(IntegerValue instance) {
                return unexpected("uint16", instance);
            }

            @Override
            public Flow<S, Character> visit(IntegerValue.Uint16 instance) {
                return pure(instance.value);
            }
        }));
    }

    static <S, A> Flow<S, Long> uint32(final Term<A> term) {
        return bind(integer(term), integerValue -> integerValue.accept(new IntegerValue.PartialVisitor<Flow<S, Long>>() {
            @Override
            public Flow<S, Long> otherwise(IntegerValue instance) {
                return unexpected("uint32", instance);
            }

            @Override
            public Flow<S, Long> visit(IntegerValue.Uint32 instance) {
                return pure(instance.value);
            }
        }));
    }

    static <S, A> Flow<S, BigInteger> uint64(final Term<A> term) {
        return bind(integer(term), integerValue -> integerValue.accept(new IntegerValue.PartialVisitor<Flow<S, BigInteger>>() {
            @Override
            public Flow<S, BigInteger> otherwise(IntegerValue instance) {
                return unexpected("uint64", instance);
            }

            @Override
            public Flow<S, BigInteger> visit(IntegerValue.Uint64 instance) {
                return pure(instance.value);
            }
        }));
    }
}
