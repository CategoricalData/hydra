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

    public static <A> Flow<Graph<A>, Term<A>> reduce(boolean eager, Term<A> outer) {
        return reduce(eager, outer, new HashMap<>(), null);
    }

    public static <A> Flow<Graph<A>, Term<A>> reduceEager(Term<A> outer) {
        return reduce(true, outer);
    }

    public static <A> Flow<Graph<A>, Term<A>> reduceLazy(Term<A> outer) {
        return reduce(false, outer);
    }

    private static <A> Flow<Graph<A>, Term<A>> reduce(boolean eager, Term<A> original, Map<Name, Term<A>> env,
        LList<Term<A>> args) {
        return Rewriting.rewriteTermM(new Function<>() {
            @Override
            public Function<Term<A>, Flow<Graph<A>, Term<A>>> apply(Function<Term<A>, Flow<Graph<A>, Term<A>>> recurse) {
                return new Function<>() {
                    @Override
                    public Flow<Graph<A>, Term<A>> apply(Term<A> mid) {
                        Flow<Graph<A>, Term<A>> withReducedSubterms = eager ? recurse.apply(mid) : pure(mid);
                        return bind(withReducedSubterms, new Function<Term<A>, Flow<Graph<A>, Term<A>>>() {
                            @Override
                            public Flow<Graph<A>, Term<A>> apply(Term<A> inner) {
                                return stripTerm(inner).accept(new Term.PartialVisitor<Flow<Graph<A>, Term<A>>>() {
                                    @Override
                                    public Flow<Graph<A>, Term<A>> otherwise(Term instance) {
                                        // If evaluation is eager, subterms have already been reduced.
                                        // If it is lazy, reduction stops here regardless of subterms.
                                        return pure(instance);
                                    }

                                    @Override
                                    public Flow<Graph<A>, Term<A>> visit(Term.Application instance) {
                                        Application<A> app = instance.value;
                                        return reduce(eager, app.function, env, LList.push(app.argument, args));
                                    }

                                    @Override
                                    public Flow<Graph<A>, Term<A>> visit(Term.Function instance) {
                                        hydra.core.Function<A> fun = instance.value;
                                        return fun.accept(new hydra.core.Function.Visitor<>() {
                                            @Override
                                            public Flow<Graph<A>, Term<A>> visit(hydra.core.Function.Elimination instance) {
                                                if (!LList.isEmpty(args)) {
                                                    Elimination<A> elm = instance.value;
                                                    return bind(applyElimination(eager, elm, args.first, env), result -> reduce(eager, result, env, args.rest));
                                                } else {
                                                    return pure(applyToArguments(inner, args));
                                                }
                                            }

                                            @Override
                                            public Flow<Graph<A>, Term<A>> visit(hydra.core.Function.Lambda instance) {
                                                if (LList.isEmpty(args)) {
                                                    return pure(inner);
                                                } else {
                                                    Lambda<A> lam = instance.value;
                                                    Term<A> replaced = replaceFreeName(lam.parameter, args.first, lam.body);
                                                    return reduce(eager, replaced, env, args.rest);
                                                }
                                            }

                                            @Override
                                            public Flow<Graph<A>, Term<A>> visit(hydra.core.Function.Primitive instance) {
                                                return bind(getState(), graph -> {
                                                    Primitive<A> prim = graph.primitives.get(instance.value);
                                                    if (prim == null) {
                                                        return fail("no such primitive function: " + instance.value.value);
                                                    } else {
                                                        int arity = primitiveArity(prim);
                                                        if (arity <= LList.length(args)) {
                                                            List<Term<A>> argList = LList.take(arity, args);
                                                            Flow<Graph<A>, List<Term<A>>> reducedArgs = eager ? pure(argList)
                                                                : mapM(argList, t -> reduce(eager, t, env, null));

                                                            return bind(reducedArgs, rargs -> bind(prim.implementation.apply(rargs),
                                                                result -> reduce(eager, result, env, LList.drop(arity, args))));
                                                        } else {
                                                            // Not enough arguments available; abort reduction
                                                            return pure(applyToArguments(inner, args));
                                                        }
                                                    }
                                                });
                                            }
                                        });
                                    }
                                });
                            }
                        });
                    }
                };
            }
        }, Flows::pure, original);
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

    private static <A> Flow<Graph<A>, Term<A>> applyElimination(boolean eager, Elimination<A> elm, Term<A> arg,
        Map<Name, Term<A>> env) {
        return bind(reduce(eager, stripTerm(arg), env, null), new Function<>() {
            @Override
            public Flow<Graph<A>, Term<A>> apply(Term<A> reducedArg) {
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
                                    return fail(
                                        "tried to project a " + proj.typeName + " field out of a " + record.typeName
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
                                    return cases.default_.isPresent() ? pure(cases.default_.get()) : fail(
                                        "no such field " + inj.field.name + " in " + cases.typeName
                                            + " case statement");
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
        });
    }
}