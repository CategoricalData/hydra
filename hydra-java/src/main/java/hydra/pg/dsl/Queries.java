package hydra.pg.dsl;

import hydra.pg.query.ApplicationQuery;
import hydra.pg.query.Query;
import hydra.pg.query.SelectQuery;

import java.util.Arrays;
import java.util.List;

/**
 * DSL for constructing property graph queries.
 */
public interface Queries {
    /**
     * Creates an application query from a list of subqueries.
     * If the list contains exactly one query, that query is returned directly.
     * Otherwise, an application query is created that applies the subqueries in sequence.
     *
     * @param subqueries the list of subqueries to apply
     * @return a query representing the application of the subqueries
     * @throws IllegalArgumentException if the subqueries list is empty
     */
    static Query apply(List<Query> subqueries) {
        if (subqueries.size() == 0) {
            throw new IllegalArgumentException();
        } else if (subqueries.size() == 1) {
            return subqueries.get(0);
        } else {
            return new Query.Application(new ApplicationQuery(subqueries));
        }
    }

    /**
     * Creates an application query from a varargs array of subqueries.
     * If the array contains exactly one query, that query is returned directly.
     * Otherwise, an application query is created that applies the subqueries in sequence.
     *
     * @param subqueries the subqueries to apply
     * @return a query representing the application of the subqueries
     * @throws IllegalArgumentException if no subqueries are provided
     */
    static Query apply(Query... subqueries) {
        return apply(Arrays.asList(subqueries));
    }

    /**
     * Creates a query from a select query.
     *
     * @param q the select query
     * @return a query wrapping the select query
     */
    static Query query(SelectQuery q) {
        return new Query.Select(q);
    }
}
