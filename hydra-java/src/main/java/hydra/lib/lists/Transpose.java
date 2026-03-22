package hydra.lib.lists;

import hydra.core.Name;
import hydra.core.Term;
import hydra.core.TypeScheme;
import hydra.dsl.Terms;
import hydra.dsl.Types;
import hydra.graph.Graph;
import hydra.tools.PrimitiveFunction;

import hydra.util.ConsList;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static hydra.dsl.Types.function;
import static hydra.dsl.Types.list;
import static hydra.dsl.Types.scheme;
import hydra.context.Context;
import hydra.context.InContext;
import hydra.errors.Error_;
import hydra.util.Either;


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
    protected Function<List<Term>, Function<Context, Function<Graph, Either<InContext<Error_>, Term>>>> implementation() {
        return args -> cx -> graph -> hydra.lib.eithers.Bind.apply(hydra.extract.Core.list(cx, graph, args.get(0)), outerList -> {
            // Parse each inner list
            Either<InContext<Error_>, List<List<Term>>> matrixFlow = Either.right(new ArrayList<>());
            for (Term inner : outerList) {
                matrixFlow = hydra.lib.eithers.Bind.apply(matrixFlow, acc ->
                    hydra.lib.eithers.Map.apply(row -> {
                        List<List<Term>> newAcc = new ArrayList<>(acc);
                        newAcc.add(row);
                        return newAcc;
                    }, hydra.extract.Core.list(cx, graph, inner)));
            }
            return hydra.lib.eithers.Map.apply(matrix -> {
                List<List<Term>> transposed = transposeRaw(matrix);
                List<Term> result = new ArrayList<>();
                for (List<Term> row : transposed) {
                    result.add(Terms.list(row));
                }
                return Terms.list(result);
            }, matrixFlow);
        });
    }

    /**
     * Internal helper for transposing raw lists (used by implementation()).
     */
    private static <X> List<List<X>> transposeRaw(List<List<X>> matrix) {
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

    /**
     * Transposes rows and columns.
     * @param <X> the element type
     * @param matrix the matrix to transpose
     * @return the transposed matrix
     */
    public static <X> ConsList<ConsList<X>> apply(ConsList<ConsList<X>> matrix) {
        if (matrix.isEmpty()) {
            return ConsList.empty();
        }
        // Convert to ArrayList for indexed access
        ArrayList<ArrayList<X>> rows = new ArrayList<>();
        for (ConsList<X> row : matrix) {
            rows.add(new ArrayList<>(row));
        }
        int maxCols = 0;
        for (ArrayList<X> row : rows) {
            maxCols = Math.max(maxCols, row.size());
        }
        ArrayList<ConsList<X>> result = new ArrayList<>();
        for (int col = 0; col < maxCols; col++) {
            ArrayList<X> newRow = new ArrayList<>();
            for (ArrayList<X> row : rows) {
                if (col < row.size()) {
                    newRow.add(row.get(col));
                }
            }
            if (!newRow.isEmpty()) {
                result.add(ConsList.fromList(newRow));
            }
        }
        return ConsList.fromList(result);
    }
}
