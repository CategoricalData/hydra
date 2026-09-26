package hydra.core.overlay.java.lib.lists;

import hydra.core.model.Name;
import hydra.core.model.Term;
import hydra.core.model.TypeScheme;
import hydra.core.overlay.java.dsl.Terms;
import hydra.core.overlay.java.dsl.Types;
import hydra.core.graph.Graph;
import hydra.core.overlay.java.tools.PrimitiveFunction;

import java.util.ArrayList;
import java.util.List;
import java.util.function.Function;

import static hydra.core.overlay.java.dsl.Types.function;
import static hydra.core.overlay.java.dsl.Types.list;
import static hydra.core.overlay.java.dsl.Types.scheme;
import hydra.core.errors.Error_;
import hydra.core.overlay.java.util.ConsList;
import hydra.core.overlay.java.util.Either;


/**
 * Transposes a matrix (list of lists).
 */
public class Transpose extends PrimitiveFunction {
    public Name name() {
        return hydra.core.lib.Lists.transpose().name;
    }

    @Override
    public TypeScheme type() {
        return scheme("a", function(list(list("a")), list(list("a"))));
    }

    @Override
    protected Function<List<Term>, Function<Graph, Either<Error_, Term>>> implementation() {
        return args -> graph -> hydra.core.overlay.java.lib.eithers.Bind.apply(hydra.core.extract.Model.list(graph, args.get(0)), outerList -> {
            // Parse each inner list, accumulating into a ConsList in reverse, then reverse once.
            ConsList<List<Term>> matrixRev = ConsList.empty();
            for (Term inner : outerList) {
                Either<Error_, List<Term>> rowE = hydra.core.extract.Model.list(graph, inner);
                if (rowE.isLeft()) return (Either) rowE;
                matrixRev = ConsList.cons(((Either.Right<Error_, List<Term>>) rowE).value, matrixRev);
            }
            List<List<Term>> matrix = matrixRev.reverse();
            List<List<Term>> transposed = apply(matrix);
            ConsList<Term> resultRev = ConsList.empty();
            for (List<Term> row : transposed) {
                resultRev = ConsList.cons(Terms.list(row), resultRev);
            }
            return Either.right(Terms.list(resultRev.reverse()));
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
            return ConsList.empty();
        }
        // Snapshot rows into ArrayList scratch buffers for fast random access during transposition.
        ArrayList<ArrayList<X>> rows = new ArrayList<>(matrix.size());
        int maxCols = 0;
        for (List<X> row : matrix) {
            ArrayList<X> snap = new ArrayList<>(row);
            maxCols = Math.max(maxCols, snap.size());
            rows.add(snap);
        }
        ConsList<List<X>> resultRev = ConsList.empty();
        for (int col = 0; col < maxCols; col++) {
            ConsList<X> newRowRev = ConsList.empty();
            boolean any = false;
            for (ArrayList<X> row : rows) {
                if (col < row.size()) {
                    newRowRev = ConsList.cons(row.get(col), newRowRev);
                    any = true;
                }
            }
            if (any) {
                resultRev = ConsList.cons(newRowRev.reverse(), resultRev);
            }
        }
        return resultRev.reverse();
    }
}
