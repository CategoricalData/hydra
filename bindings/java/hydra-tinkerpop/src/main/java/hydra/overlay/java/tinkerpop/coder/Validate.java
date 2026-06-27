package hydra.overlay.java.tinkerpop.coder;

import hydra.core.Literal;
import hydra.core.LiteralType;
import hydra.error.pg.InvalidGraphError;
import hydra.overlay.java.util.Optional;
import hydra.pg.model.Graph;
import hydra.pg.model.GraphSchema;
import hydra.validate.Pg;
import hydra.validation.ValidationResult;

import org.apache.tinkerpop.gremlin.process.traversal.dsl.graph.GraphTraversalSource;

import java.util.List;

/**
 * Convenience methods for validating TinkerPop graphs against a Hydra GraphSchema.
 */
public class Validate {

    private Validate() {
    }

    /**
     * The result of validating a graph against a schema.
     *
     * <p>Errors are typed ({@link InvalidGraphError}) rather than bare strings;
     * the result retains the typed value alongside a human-readable rendering.
     */
    public static class Result {
        private final Optional<InvalidGraphError<Literal>> error;

        private Result(Optional<InvalidGraphError<Literal>> error) {
            this.error = error;
        }

        /** Returns true if the graph is valid. */
        public boolean isValid() {
            return !error.isGiven();
        }

        /** Returns the first typed error, or {@link Optional#none()} if valid. */
        public Optional<InvalidGraphError<Literal>> getError() {
            return error;
        }

        @Override
        public String toString() {
            return error.isGiven() ? "INVALID - " + error.fromGiven() : "VALID";
        }
    }

    /**
     * Runs Hydra's PG validation and reduces the result to the first error, if any.
     *
     * <p>{@link Pg#validateGraph} returns a {@link ValidationResult} accumulating
     * all findings; this collapses it to first-error semantics to match {@link Result}.
     */
    private static Optional<InvalidGraphError<Literal>> firstError(
            GraphSchema<LiteralType> schema, Graph<Literal> hydraGraph) {
        ValidationResult<InvalidGraphError<Literal>> result = Pg.validateGraph(
                Pg.defaultPgProfile(),
                new ValidationResult<>(List.of(), List.of()),
                HydraGremlinBridge::checkLiteral,
                schema,
                hydraGraph);
        return result.errors.isEmpty()
                ? Optional.none()
                : Optional.given(result.errors.get(0));
    }

    /**
     * Validates a TinkerPop graph against a Hydra GraphSchema.
     *
     * <p>Converts the TinkerPop graph to a Hydra graph using
     * {@link HydraGremlinBridge#objectToLiteral} and validates it using
     * {@link HydraGremlinBridge#checkLiteral}. Returns at most one error
     * (the first encountered).
     *
     * @param schema the graph schema to validate against
     * @param gremlinGraph the TinkerPop graph to validate
     * @return a {@link Result} whose {@code toString()} is either "VALID" or "INVALID - ..."
     */
    public static Result validate(
            GraphSchema<LiteralType> schema,
            org.apache.tinkerpop.gremlin.structure.Graph gremlinGraph) {
        Graph<Literal> hydraGraph = HydraGremlinBridge.gremlinToHydra(
                gremlinGraph, HydraGremlinBridge::objectToLiteral);
        return new Result(firstError(schema, hydraGraph));
    }

    /**
     * Validates a TinkerPop graph via a traversal source against a Hydra GraphSchema.
     * Works with both local and remote graph connections.
     *
     * @param schema the graph schema to validate against
     * @param g the traversal source to read the graph from
     * @return a {@link Result} whose {@code toString()} is either "VALID" or "INVALID - ..."
     */
    public static Result validate(
            GraphSchema<LiteralType> schema,
            GraphTraversalSource g) {
        Graph<Literal> hydraGraph = HydraGremlinBridge.gremlinToHydra(
                g, HydraGremlinBridge::objectToLiteral);
        return new Result(firstError(schema, hydraGraph));
    }
}
