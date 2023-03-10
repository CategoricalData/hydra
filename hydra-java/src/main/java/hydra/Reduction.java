package hydra;

import hydra.compute.Flow;
import hydra.core.Term;
import hydra.graph.Graph;

public interface Reduction {

    static <A> Flow<Graph<A>, Term<A>> betaReduceTerm(Term<A> term) {
        return term.accept(new Term.Visitor<Flow<Graph<A>, Term<A>>>() {
            @Override
            public Flow<Graph<A>, Term<A>> visit(Term.Annotated instance) {
                return null;
            }

            @Override
            public Flow<Graph<A>, Term<A>> visit(Term.Application instance) {
                return null;
            }

            @Override
            public Flow<Graph<A>, Term<A>> visit(Term.Element instance) {
                return null;
            }

            @Override
            public Flow<Graph<A>, Term<A>> visit(Term.Function instance) {
                return null;
            }

            @Override
            public Flow<Graph<A>, Term<A>> visit(Term.Let instance) {
                return null;
            }

            @Override
            public Flow<Graph<A>, Term<A>> visit(Term.List instance) {
                return null;
            }

            @Override
            public Flow<Graph<A>, Term<A>> visit(Term.Literal instance) {
                return null;
            }

            @Override
            public Flow<Graph<A>, Term<A>> visit(Term.Map instance) {
                return null;
            }

            @Override
            public Flow<Graph<A>, Term<A>> visit(Term.Optional instance) {
                return null;
            }

            @Override
            public Flow<Graph<A>, Term<A>> visit(Term.Product instance) {
                return null;
            }

            @Override
            public Flow<Graph<A>, Term<A>> visit(Term.Record instance) {
                return null;
            }

            @Override
            public Flow<Graph<A>, Term<A>> visit(Term.Set instance) {
                return null;
            }

            @Override
            public Flow<Graph<A>, Term<A>> visit(Term.Stream instance) {
                return null;
            }

            @Override
            public Flow<Graph<A>, Term<A>> visit(Term.Sum instance) {
                return null;
            }

            @Override
            public Flow<Graph<A>, Term<A>> visit(Term.Union instance) {
                return null;
            }

            @Override
            public Flow<Graph<A>, Term<A>> visit(Term.Variable instance) {
                return null;
            }

            @Override
            public Flow<Graph<A>, Term<A>> visit(Term.Wrap instance) {
                return null;
            }
        });
    }
}