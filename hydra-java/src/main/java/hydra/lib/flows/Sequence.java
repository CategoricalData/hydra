package hydra.lib.flows;

import hydra.dsl.Flows;
import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Flows.bind;
import static hydra.dsl.Flows.pure;


/**
 * Sequences a list of flows.
 */
public class Sequence extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.flows.sequence");
    }

    @Override
    public TypeScheme type() {
        return Types.scheme("s", "x",
                Types.function(
                        Types.list(Types.flow("s", "x")),
                        Types.flow("s", Types.list("x"))));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        Term bindTerm = (new Bind()).term();
        Term pureTerm = (new Pure()).term();
        Term consTerm = (new hydra.lib.lists.Cons()).term();

        return args -> bind(Expect.list(Flows::pure, args.get(0)), flowTerms -> {
            // Build up the sequencing using fold
            // Start with pure([])
            Term empty = Terms.list(new ArrayList<>());
            Term initialFlow = Terms.apply(pureTerm, empty);

            // For each flow, bind it and cons the result onto the list
            Flow<Graph, Term> result = Flows.pure(initialFlow);
            for (Term flowTerm : flowTerms) {
                result = Flows.map(result, accFlow ->
                    Terms.apply(Terms.apply(bindTerm, flowTerm),
                        Terms.lambda("x",
                            Terms.apply(Terms.apply(bindTerm, accFlow),
                                Terms.lambda("xs",
                                    Terms.apply(pureTerm,
                                        Terms.apply(Terms.apply(consTerm, Terms.variable("x")),
                                            Terms.variable("xs"))))))));
            }
            return result;
        });
    }

    /**
     * Combines flows into a flow of list.
     * @param <S> the state type
     * @param <X> the element type
     * @param flows the flows
     * @return the flow of list
     */
    public static <S, X> Flow<S, List<X>> apply(List<Flow<S, X>> flows) {
        return Flows.sequence(flows);
    }
}
