package hydra;

import hydra.compute.Flow;
import hydra.core.Application;
import hydra.core.CaseStatement;
import hydra.core.Elimination;
import hydra.core.Field;
import hydra.core.Injection;
import hydra.core.Lambda;
import hydra.core.Name;
import hydra.core.Projection;
import hydra.core.Record;
import hydra.core.Term;
import hydra.dsl.Terms;
import hydra.graph.Graph;
import hydra.graph.Primitive;
import hydra.tools.LList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.function.Function;

import static hydra.Common.*;
import static hydra.Flows.*;
import static hydra.basics.Basics.*;


public class Reduction {
    private Reduction() {
    }

    public static <A> Flow<Graph<A>, Term<A>> reduce(boolean eager, Term<A> term) {
        return reduce(eager, term, new HashMap<>(), null);
    }

    private static <A> Flow<Graph<A>, Term<A>> reduceArg(boolean eager, Term<A> arg) {
        // Reduce an argument only if evaluation is lazy (i.e. the argument may not already have been reduced)
        return eager ? pure(arg) : reduce(false, arg);
    }

    private static <A> Flow<Graph<A>, Term<A>> applyIfNullary(boolean eager, Term<A> original, LList<Term<A>> args) {
        return stripTerm(original).accept(new Term.PartialVisitor<Flow<Graph<A>, Term<A>>>() {
            @Override
            public Flow<Graph<A>, Term<A>> otherwise(Term instance) {
                return pure(applyToArguments(original, args));
            }

            @Override
            public Flow<Graph<A>, Term<A>> visit(Term.Application instance) {
                Application<A> app = instance.value;
                return applyIfNullary(eager, app.function, LList.push(app.argument, args));
            }

            @Override
            public Flow<Graph<A>, Term<A>> visit(Term.Function instance) {
                hydra.core.Function<A> fun = instance.value;
                return fun.accept(new hydra.core.Function.Visitor<Flow<Graph<A>, Term<A>>>() {
                    @Override
                    public Flow<Graph<A>, Term<A>> visit(hydra.core.Function.Elimination instance) {
                        if (LList.isEmpty(args)) {
                            return pure(original);
                        } else {
                            Elimination<A> elm = instance.value;
                            Term<A> arg = args.first;
                            LList<Term<A>> remainingArgs = args.rest;
                            // Reduce the argument prior to application, regardless of laziness
                            return bind(reduceArg(eager, stripTerm(arg)),
                                new Function<Term<A>, Flow<Graph<A>, Term<A>>>() {
                                    @Override
                                    public Flow<Graph<A>, Term<A>> apply(Term<A> reducedArg) {
                                        return bind(applyElimination(elm, reducedArg),
                                            new Function<Term<A>, Flow<Graph<A>, Term<A>>>() {
                                                @Override
                                                public Flow<Graph<A>, Term<A>> apply(Term<A> result) {
                                                    return bind(reduce(eager, result),
                                                        reducedResult -> applyIfNullary(eager, reducedResult,
                                                            remainingArgs));
                                                }
                                            });
                                    }
                                });
                        }
                    }

                    @Override
                    public Flow<Graph<A>, Term<A>> visit(hydra.core.Function.Lambda instance) {
                        if (LList.isEmpty(args)) {
                            return pure(original);
                        } else {
                            Lambda<A> lam = instance.value;
                            Term<A> arg = args.first;
                            LList<Term<A>> remainingArgs = args.rest;
                            return bind(reduce(eager, stripTerm(arg)),
                                new Function<Term<A>, Flow<Graph<A>, Term<A>>>() {
                                    @Override
                                    public Flow<Graph<A>, Term<A>> apply(Term<A> reducedArg) {
                                        Term<A> result = replaceFreeName(lam.parameter, reducedArg, lam.body);
                                        return bind(reduce(eager, result),
                                            new Function<Term<A>, Flow<Graph<A>, Term<A>>>() {
                                                @Override
                                                public Flow<Graph<A>, Term<A>> apply(Term<A> reducedResult) {
                                                    return applyIfNullary(eager, reducedResult, remainingArgs);
                                                }
                                            });
                                    }
                                });
                        }
                    }

                    @Override
                    public Flow<Graph<A>, Term<A>> visit(hydra.core.Function.Primitive instance) {
                        return bind(Lexical.requirePrimitive(instance.value), prim -> {
                            int arity = primitiveArity(prim);
                            if (arity <= LList.length(args)) {
                                List<Term<A>> argList = LList.take(arity, args);
                                Flow<Graph<A>, List<Term<A>>> reducedArgs = mapM(argList, a -> reduceArg(eager, a));
                                LList<Term<A>> remainingArgs = LList.drop(arity, args);
                                return bind(reducedArgs, rargs -> bind(prim.implementation.apply(rargs),
                                    result -> bind(reduce(eager, result),
                                        reducedResult -> applyIfNullary(eager, reducedResult, remainingArgs))));
                            } else {
                                // Not enough arguments available; back out
                                return pure(applyToArguments(original, args));
                            }
                        });
                    }
                });
            }

            @Override
            public Flow<Graph<A>, Term<A>> visit(Term.Variable instance) {
                // TODO: dereference variables
                return pure(applyToArguments(original, args));
            }
        });
    }

    private static <A> Flow<Graph<A>, Term<A>> applyElimination(Elimination<A> elm, Term<A> reducedArg) {
        return elm.accept(new Elimination.Visitor<>() {
            @Override
            public Flow<Graph<A>, Term<A>> visit(Elimination.Element instance) {
                throw new UnsupportedOperationException();
            }

            @Override
            public Flow<Graph<A>, Term<A>> visit(Elimination.List instance) {
                throw new UnsupportedOperationException();
            }

            @Override
            public Flow<Graph<A>, Term<A>> visit(Elimination.Optional instance) {
                throw new UnsupportedOperationException();
            }

            @Override
            public Flow<Graph<A>, Term<A>> visit(Elimination.Record elim) {
                return reducedArg.accept(new Term.PartialVisitor<>() {
                    @Override
                    public Flow<Graph<A>, Term<A>> otherwise(Term instance) {
                        return unexpected("record", instance);
                    }

                    @Override
                    public Flow<Graph<A>, Term<A>> visit(Term.Record instance) {
                        Record<A> record = instance.value;
                        Projection proj = elim.value;
                        if (record.typeName.equals(proj.typeName)) {
                            for (Field<A> field : record.fields) {
                                if (field.name.equals(proj.field)) {
                                    return pure(field.term);
                                }
                            }
                            return fail("no such field: " + proj.field + " in " + record.typeName + " record");
                        } else {
                            return fail("tried to project a " + proj.typeName + " field out of a " + record.typeName
                                + " record");
                        }
                    }
                });
            }

            @Override
            public Flow<Graph<A>, Term<A>> visit(Elimination.Union elim) {
                return reducedArg.accept(new Term.PartialVisitor<>() {
                    @Override
                    public Flow<Graph<A>, Term<A>> otherwise(Term instance) {
                        return unexpected("injection", instance);
                    }

                    @Override
                    public Flow<Graph<A>, Term<A>> visit(Term.Union instance) {
                        CaseStatement<A> cases = elim.value;
                        Injection<A> inj = instance.value;
                        if (cases.typeName.equals(inj.typeName)) {
                            for (Field<A> field : cases.cases) {
                                if (field.name.equals(inj.field.name)) {
                                    return pure(Terms.apply(field.term, inj.field.term));
                                }
                            }
                            return cases.default_.isPresent() ? pure(cases.default_.get())
                                : fail("no such field " + inj.field.name + " in " + cases.typeName + " case statement");
                        } else {
                            return fail("tried to match a " + inj.typeName + " injection as " + cases.typeName);
                        }
                    }
                });
            }

            @Override
            public Flow<Graph<A>, Term<A>> visit(Elimination.Wrap instance) {
                return reducedArg.accept(new Term.PartialVisitor<>() {
                    @Override
                    public Flow<Graph<A>, Term<A>> otherwise(Term instance) {
                        return unexpected("wrapped term", instance);
                    }

                    @Override
                    public Flow<Graph<A>, Term<A>> visit(Term.Wrap wrapped) {
                        Name fname = instance.value;
                        Name aname = wrapped.value.typeName;
                        return fname.equals(aname) ? pure(((Term.Wrap<A>) wrapped).value.object)
                            : fail("tried to unwrap an instance of " + aname + " as an instance of " + fname);
                    }
                });
            }
        });
    }

    private static <A> Term<A> applyToArguments(Term<A> function, LList<Term<A>> args) {
        Term<A> tcur = function;
        LList<Term<A>> lcur = args;
        while (lcur != null) {
            tcur = Terms.apply(tcur, args.first);
            lcur = lcur.rest;
        }
        return tcur;
    }

    private static <A> boolean doRecurse(boolean eager, Term<A> term) {
        boolean isLambda = term.accept(new Term.PartialVisitor<Boolean>() {
            @Override
            public Boolean otherwise(Term instance) {
                return false;
            }

            @Override
            public Boolean visit(Term.Function instance) {
                hydra.core.Function<A> fun = instance.value;
                return fun.accept(new hydra.core.Function.PartialVisitor<Boolean>() {
                    @Override
                    public Boolean otherwise(hydra.core.Function instance) {
                        return false;
                    }

                    @Override
                    public Boolean visit(hydra.core.Function.Lambda instance) {
                        return true;
                    }
                });
            }
        });

        return eager && !isLambda;
    }

    private static <A> Flow<Graph<A>, Term<A>> reduce(boolean eager, Term<A> original, Map<Name, Term<A>> env,
        LList<Term<A>> args) {
        return Rewriting.rewriteTermM(
            new Function<Function<Term<A>, Flow<Graph<A>, Term<A>>>, Function<Term<A>, Flow<Graph<A>, Term<A>>>>() {
                @Override
                public Function<Term<A>, Flow<Graph<A>, Term<A>>> apply(
                    Function<Term<A>, Flow<Graph<A>, Term<A>>> recurse) {
                    return new Function<Term<A>, Flow<Graph<A>, Term<A>>>() {
                        @Override
                        public Flow<Graph<A>, Term<A>> apply(Term<A> mid) {
                            // Do not recurse into lambda expressions, which will cause unexpected free variables to be encountered
                            Flow<Graph<A>, Term<A>> ready = doRecurse(eager, mid) ? recurse.apply(mid) : pure(mid);
                            return bind(ready, new Function<Term<A>, Flow<Graph<A>, Term<A>>>() {
                                @Override
                                public Flow<Graph<A>, Term<A>> apply(Term<A> inner) {
                                    return applyIfNullary(eager, inner, null);
                                }
                            });
                        }
                    };
                }
            }, Flows::pure, original);
    }

    public static <A> Term<A> replaceFreeName(Name toReplace, Term<A> replacement, Term<A> body) {
        return Rewriting.rewriteTerm(recurse -> inner -> inner.accept(new Term.PartialVisitor<Term<A>>() {
            @Override
            public Term<A> otherwise(Term instance) {
                return recurse.apply(instance);
            }

            @Override
            public Term<A> visit(Term.Function instance) {
                hydra.core.Function<A> fun = instance.value;
                return fun.accept(new hydra.core.Function.PartialVisitor<>() {
                    @Override
                    public Term<A> otherwise(hydra.core.Function instance) {
                        return recurse.apply(inner);
                    }

                    @Override
                    public Term<A> visit(hydra.core.Function.Lambda instance) {
                        Lambda<A> lam = instance.value;
                        return lam.parameter.equals(toReplace) ? inner : recurse.apply(inner);
                    }
                });
            }

            @Override
            public Term<A> visit(Term.Variable instance) {
                return instance.value.equals(toReplace) ? replacement : instance;
            }
        }), a -> a, body);
    }
}