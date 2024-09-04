package hydra.pg.dsl;

import hydra.pg.query.ApplicationQuery;
import hydra.pg.query.Query;
import hydra.pg.query.SelectQuery;

import java.util.Arrays;
import java.util.List;

public interface Queries {
    static Query apply(List<Query> subqueries) {
        if (subqueries.size() == 0) {
            throw new IllegalArgumentException();
        } else if (subqueries.size() == 1) {
            return subqueries.get(0);
        } else {
            return new Query.Application(new ApplicationQuery(subqueries));
        }
    }

    static Query apply(Query... subqueries) {
        return apply(Arrays.asList(subqueries));
    }

    static Query query(SelectQuery q) {
        return new Query.Select(q);
    }
}
