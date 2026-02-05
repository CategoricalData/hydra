package hydra.lib.lists;

import hydra.compute.Flow;
import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Expect;
import hydra.dsl.Flows;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Flows.pure;
import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;


/**
 * Transposes a matrix (list of lists).
 */
public class Transpose extends PrimitiveFunction {
    public Name name() {
        return new Name("hydra.lib.lists.transpose");
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(list(list("a")), list(list("a"))));
    }

    @Override
    protected Function<List<Term>, Flow<Graph, Term>> implementation() {
        return args -> Flows.bind(Expect.list(Flows::pure, args.get(0)), outerList -> {
            // Parse each inner list
            Flow<Graph, List<List<Term>>> matrixFlow = pure(new ArrayList<>());
            for (Term inner : outerList) {
                matrixFlow = Flows.bind(matrixFlow, acc ->
                    Flows.map(Expect.list(Flows::pure, inner), row -> {
                        List<List<Term>> newAcc = new ArrayList<>(acc);
                        newAcc.add(row);
                        return newAcc;
                    }));
            }
            return Flows.map(matrixFlow, matrix -> {
                List<List<Term>> transposed = apply(matrix);
                List<Term> result = new ArrayList<>();
                for (List<Term> row : transposed) {
                    result.add(Terms.list(row));
                }
                return Terms.list(result);
            });
        });
    }

    /**
     * Transposes rows and columns.
     * @param <X> the element type
     * @param matrix the matrix to transpose
     * @return the transposed matrix
     */
    public static <X> List<List<X>> apply(List<List<X>> matrix) {
        if (matrix.isEmpty()) {
            return List.of();
        }
        int maxCols = 0;
        for (List<X> row : matrix) {
            maxCols = Math.max(maxCols, row.size());
        }
        List<List<X>> result = new ArrayList<>();
        for (int col = 0; col < maxCols; col++) {
            List<X> newRow = new ArrayList<>();
            for (List<X> row : matrix) {
                if (col < row.size()) {
                    newRow.add(row.get(col));
                }
            }
            if (!newRow.isEmpty()) {
                result.add(newRow);
            }
        }
        return result;
    }
}
