package hydra.langs.cypher;

import hydra.langs.cypher.openCypher.Query;
import hydra.tools.AntlrReaderBase;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;


/**
 * Tests for CypherReader
 *
 * Sources of test cases:
 *   QUERY_BASIC_xx    -- https://neo4j.com/docs/cypher-manual/current/queries/basic/
 *   QUERY_CASE_xx     -- https://neo4j.com/docs/cypher-manual/current/queries/case/
 *   QUERY_CLAUSE_xx   -- https://neo4j.com/docs/cypher-manual/current/clauses/clause_composition/
 *   QUERY_MATCH_xx    -- https://neo4j.com/docs/cypher-manual/current/clauses/match/
 *   QUERY_OPTIONAL_xx -- https://neo4j.com/docs/cypher-manual/current/clauses/optional-match/
 */
public class CypherReaderTest {
    private static final String QUERY_1 = "MATCH (n) RETURN *";

    private static final String QUERY_BASIC_1
            = "MATCH (keanu:Person {name:'Keanu Reeves'})\n"
            + "RETURN keanu.name AS name, keanu.born AS born";

    private static final String QUERY_BASIC_2
            = "MATCH (people:Person)\n"
            + "RETURN people\n"
            + "LIMIT 5";

    private static final String QUERY_BASIC_3
            = "MATCH (bornInEighties:Person)\n"
            + "WHERE bornInEighties.born >= 1980 AND bornInEighties.born < 1990\n"
            + "RETURN bornInEighties.name as name, bornInEighties.born as born\n"
            + "ORDER BY born DESC";

    private static final String QUERY_BASIC_4
            = "MATCH (m:Movie {title: 'The Matrix'})<-[d:DIRECTED]-(p:Person)\n"
            + "RETURN p.name as director";

    private static final String QUERY_BASIC_5
            = "MATCH (tom:Person {name:'Tom Hanks'})-[r]->(m:Movie)\n"
            + "RETURN type(r) AS type, m.title AS movie";

    private static final String QUERY_BASIC_6
            = "MATCH (:Person {name:'Tom Hanks'})-[r:!ACTED_IN]->(m:Movie)\n"
            + "Return type(r) AS type, m.title AS movies";

    private static final String QUERY_BASIC_7_UNSUPPORTED
            = "MATCH (tom:Person {name:'Tom Hanks'})--{2}(colleagues:Person)\n"
            + "RETURN DISTINCT colleagues.name AS name, colleagues.born AS bornIn\n"
            + "ORDER BY bornIn\n"
            + "LIMIT 5";

    private static final String QUERY_BASIC_8_UNSUPPORTED
            = "MATCH (p:Person {name:'Tom Hanks'})--{1,4}(colleagues:Person)\n"
            + "RETURN DISTINCT colleagues.name AS name, colleagues.born AS bornIn\n"
            + "ORDER BY bornIn, name\n"
            + "LIMIT 5";

    private static final String QUERY_BASIC_9_UNSUPPORTED
            = "MATCH p=shortestPath(\n"
            + "(:Person {name:\"Keanu Reeves\"})-[*]-(:Person {name:\"Tom Hanks\"})\n"
            + ")\n"
            + "RETURN p";

    private static final String QUERY_BASIC_10
            = "MATCH (keanu:Person {name:'Keanu Reeves'})-[:ACTED_IN]->(m:Movie)<-[:ACTED_IN]-(coActors:Person),\n"
            + "  (coActors:Person)-[:ACTED_IN]->(m2:Movie)<-[:ACTED_IN]-(cocoActors:Person)\n"
            + "WHERE NOT (keanu)-[:ACTED_IN]->()<-[:ACTED_IN]-(cocoActors) AND keanu <> cocoActors\n"
            + "RETURN cocoActors.name AS recommended, count(cocoActors) AS strength\n"
            + "ORDER BY strength DESC\n"
            + "LIMIT 7";

    private static final String QUERY_BASIC_11
            = "MATCH (:Person {name: 'Keanu Reeves'})-[:ACTED_IN]->(:Movie)<-[:ACTED_IN]-(coActor:Person),\n"
            + "  (coActor)-[:ACTED_IN]->(:Movie)<-[:ACTED_IN]-(:Person {name:'Tom Hanks'})\n"
            + "RETURN DISTINCT coActor.name AS coActor";

    private static final String QUERY_BASIC_12
            = "MATCH (n)\n"
            + "DETACH DELETE n";

    private static final String QUERY_CASE_1_UNSUPPORTED
            = "CREATE\n"
            + "  (alice:Person {name:'Alice', age: 38, eyes: 'brown'}),\n"
            + "  (bob:Person {name: 'Bob', age: 25, eyes: 'blue'}),\n"
            + "  (charlie:Person {name: 'Charlie', age: 53, eyes: 'green'}),\n"
            + "  (daniel:Person {name: 'Daniel', eyes: 'brown'}),\n"
            + "  (eskil:Person {name: 'Eskil', age: 41, eyes: 'blue'}),\n"
            + "  (alice)-[:KNOWS]->(bob),\n"
            + "  (alice)-[:KNOWS]->(charlie),\n"
            + "  (bob)-[:KNOWS]->(daniel),\n"
            + "  (charlie)-[:KNOWS]->(daniel),\n"
            + "  (bob)-[:MARRIED]->(eskil)";

    private static final String QUERY_CASE_2_UNSUPPORTED
            = "MATCH (n:Person)\n"
            + "RETURN\n"
            + "CASE n.eyes\n"
            + "  WHEN 'blue'  THEN 1\n"
            + "  WHEN 'brown', 'hazel' THEN 2\n"
            + "  ELSE 3\n"
            + "END AS result, n.eyes";

    private static final String QUERY_CASE_3_UNSUPPORTED
            = "MATCH (n:Person)\n"
            + "RETURN n.name,\n"
            + "CASE n.age\n"
            + "  WHEN IS NULL, IS NOT TYPED INTEGER | FLOAT THEN \"Unknown\"\n"
            + "  WHEN = 0, = 1, = 2 THEN \"Baby\"\n"
            + "  WHEN <= 13 THEN \"Child\"\n"
            + "  WHEN < 20 THEN \"Teenager\"\n"
            + "  WHEN < 30 THEN \"Young Adult\"\n"
            + "  WHEN > 1000 THEN \"Immortal\"\n"
            + "  ELSE \"Adult\"\n"
            + "END AS result";

    private static final String QUERY_CASE_4
            = "MATCH (n:Person)\n"
            + "RETURN\n"
            + "CASE\n"
            + "  WHEN n.eyes = 'blue' THEN 1\n"
            + "  WHEN n.age < 40      THEN 2\n"
            + "  ELSE 3\n"
            + "END AS result, n.eyes, n.age";

    private static final String QUERY_CASE_5
            = "MATCH (n:Person)\n"
            + "RETURN n.name,\n"
            + "CASE n.age\n"
            + "  WHEN null THEN -1\n"
            + "  ELSE n.age - 10\n"
            + "END AS age_10_years_ago";

    private static final String QUERY_CASE_6
            = "MATCH (n:Person)\n"
            + "RETURN n.name,\n"
            + "CASE\n"
            + "  WHEN n.age IS NULL THEN -1\n"
            + "  ELSE n.age - 10\n"
            + "END AS age_10_years_ago";

    private static final String QUERY_CASE_7
            = "MATCH (n:Person)\n"
            + "WITH n,\n"
            + "CASE n.eyes\n"
            + "  WHEN 'blue'  THEN 1\n"
            + "  WHEN 'brown' THEN 2\n"
            + "  ELSE 3\n"
            + "END AS colorCode\n"
            + "SET n.colorCode = colorCode\n"
            + "RETURN n.name, n.colorCode";

    private static final String QUERY_CLAUSE_1
            = "MATCH (john:Person {name: 'John'})\n"
            + "MATCH (john)-[:FRIEND]->(friend)\n"
            + "RETURN friend.name AS friendName";

    private static final String QUERY_CLAUSE_2
            = "MATCH (j:Person) WHERE j.name STARTS WITH \"J\"\n"
            + "CREATE (j)-[:FRIEND]->(jj:Person {name: \"Jay-jay\"})";

    private static final String QUERY_CLAUSE_3
            = "CREATE (jj:Person {name: \"Jay-jay\"})\n"
            + "RETURN count(*) AS count\n"
            + "  UNION\n"
            + "MATCH (j:Person) WHERE j.name STARTS WITH \"J\"\n"
            + "RETURN count(*) AS count";

    private static final String QUERY_CLAUSE_4_UNSUPPORTED
            = "MATCH (john:Person {name: 'John'})\n"
            + "SET john.friends = []\n"
            + "WITH john\n"
            + "MATCH (john)-[:FRIEND]->(friend)\n"
            + "WITH john, friend\n"
            + "CALL {\n"
            + "  WITH john, friend\n"
            + "  WITH *, john.friends AS friends\n"
            + "  SET john.friends = friends + friend.name\n"
            + "}";

    private static final String QUERY_MATCH_1
            = "MATCH (n)\n"
            + "RETURN n";

    private static final String QUERY_MATCH_2
            = "MATCH (movie:Movie)\n"
            + "RETURN movie.title";

    private static final String QUERY_MATCH_3
            = "MATCH (director {name: 'Oliver Stone'})--(movie)\n"
            + "RETURN movie.title";

    private static final String QUERY_MATCH_4
            = "MATCH (:Person {name: 'Oliver Stone'})--(movie:Movie)\n"
            + "RETURN movie.title";

    private static final String QUERY_MATCH_5_UNSUPPORTED
            = "MATCH (n:Movie|Person)\n"
            + "RETURN n.name AS name, n.title AS title";

    private static final String QUERY_MATCH_6
            = "MATCH (:Person {name: 'Oliver Stone'})-->(movie)\n"
            + "RETURN movie.title";

    private static final String QUERY_MATCH_7
            = "MATCH (:Person {name: 'Oliver Stone'})-[r]->(movie)\n"
            + "RETURN type(r)";

    private static final String QUERY_MATCH_8
            = "MATCH (a)-[:ACTED_IN {role: 'Bud Fox'}]-(b)\n"
            + "RETURN a, b";

    private static final String QUERY_MATCH_9
            = "MATCH (wallstreet:Movie {title: 'Wall Street'})<-[:ACTED_IN]-(actor)\n"
            + "RETURN actor.name";

    private static final String QUERY_MATCH_10
            = "MATCH (wallstreet {title: 'Wall Street'})<-[:ACTED_IN|DIRECTED]-(person)\n"
            + "RETURN person.name";

    private static final String QUERY_MATCH_11
            = "MATCH (wallstreet {title: 'Wall Street'})<-[r:ACTED_IN]-(actor)\n"
            + "RETURN r.role";

    private static final String QUERY_MATCH_12
            = "MATCH\n"
            + "  (martin:Person {name: 'Martin Sheen'}),\n"
            + "  (rob:Person {name: 'Rob Reiner'})\n"
            + "CREATE (rob)-[:`OLD FRIENDS`]->(martin)";

    private static final String QUERY_MATCH_13
            = "MATCH (n {name: 'Rob Reiner'})-[r:`OLD FRIENDS`]->()\n"
            + "RETURN type(r)";

    private static final String QUERY_MATCH_14
            = "MATCH (charlie {name: 'Charlie Sheen'})-[:ACTED_IN]->(movie)<-[:DIRECTED]-(director)\n"
            + "RETURN movie.title, director.name";

    private static final String QUERY_OPTIONAL_1
            = "MATCH (a:Person {name: 'Martin Sheen'})\n"
            + "MATCH (a)-[r:DIRECTED]->()\n"
            + "RETURN a.name, r";

    private static final String QUERY_OPTIONAL_2
            = "MATCH (p:Person {name: 'Martin Sheen'})\n"
            + "OPTIONAL MATCH (p)-[r:DIRECTED]->()\n"
            + "RETURN p.name, r";

    private static final String QUERY_OPTIONAL_3
            = "MATCH (a:Movie {title: 'Wall Street'})\n"
            + "OPTIONAL MATCH (a)-->(x)\n"
            + "RETURN x";

    private static final String QUERY_OPTIONAL_4
            = "MATCH (a:Person {name: 'Charlie Sheen'})\n"
            + "OPTIONAL MATCH (a)-->(x)\n"
            + "RETURN x";

    private static final String QUERY_OPTIONAL_5
            = "MATCH (a:Movie {title: 'Wall Street'})\n"
            + "OPTIONAL MATCH (a)-->(x)\n"
            + "RETURN x, x.name";

    private static final String QUERY_OPTIONAL_6
            = "MATCH (a:Person {name: 'Martin Sheen'})\n"
            + "OPTIONAL MATCH (a)-->(x)\n"
            + "RETURN x, x.name";

    private static final String QUERY_OPTIONAL_7
            = "MATCH (a:Movie {title: 'Wall Street'})\n"
            + "OPTIONAL MATCH (a)-[r:ACTED_IN]->()\n"
            + "RETURN a.title, r";

    private static final String QUERY_OPTIONAL_8
            = "MATCH (a:Movie {title: 'Wall Street'})\n"
            + "OPTIONAL MATCH (x)-[r:ACTED_IN]->(a)\n"
            + "RETURN a.title, x.name, type(r)";

    private static final String QUERY_INVALID_1 = "MATCH oops";

    @ParameterizedTest
    @ValueSource(strings = {
            QUERY_1,
            QUERY_BASIC_1, QUERY_BASIC_2, QUERY_BASIC_3, QUERY_BASIC_4, QUERY_BASIC_5, QUERY_BASIC_6,
            QUERY_BASIC_10, QUERY_BASIC_11, QUERY_BASIC_12,
            QUERY_CASE_4, QUERY_CASE_5, QUERY_CASE_6, QUERY_CASE_7,
            QUERY_CLAUSE_1, QUERY_CLAUSE_2, QUERY_CLAUSE_3,
            QUERY_MATCH_1, QUERY_MATCH_2, QUERY_MATCH_3, QUERY_MATCH_4, QUERY_MATCH_6, QUERY_MATCH_7, QUERY_MATCH_8, QUERY_MATCH_9, QUERY_MATCH_10, QUERY_MATCH_11, QUERY_MATCH_12, QUERY_MATCH_13, QUERY_MATCH_14,
            QUERY_OPTIONAL_1, QUERY_OPTIONAL_2, QUERY_OPTIONAL_3, QUERY_OPTIONAL_4, QUERY_OPTIONAL_5, QUERY_OPTIONAL_6, QUERY_OPTIONAL_7, QUERY_OPTIONAL_8})
    public void testValidQueriesAreAccepted(String queryStr) {
        Query query = CypherReader.read(queryStr);
        assertNotNull(query);
    }

    /**
     * Invalid queries are not expected to be accepted by the parser
     */
    @ParameterizedTest
    @ValueSource(strings = {QUERY_INVALID_1})
    public void testInvalidQueriesAreRejected(String queryStr) {
        assertThrows(AntlrReaderBase.AntlrReaderException.class, () -> {
            CypherReader.read(queryStr);
        });
    }

    /**
     * Unsupported queries are valid Cypher, yet are not accepted by the OpenCypher ANTLR parser.
     * It is not known in all cases whether these queries *should* be accepted by the parser.
     */
    @ParameterizedTest
    @ValueSource(strings = {
            QUERY_BASIC_7_UNSUPPORTED, QUERY_BASIC_8_UNSUPPORTED, QUERY_BASIC_9_UNSUPPORTED,
            QUERY_CASE_1_UNSUPPORTED, QUERY_CASE_2_UNSUPPORTED, QUERY_CASE_3_UNSUPPORTED,
            QUERY_CLAUSE_4_UNSUPPORTED,
            QUERY_MATCH_5_UNSUPPORTED})
    public void testUnsupportedQueriesAreRejected(String queryStr) {
        assertThrows(AntlrReaderBase.AntlrReaderException.class, () -> {
            CypherReader.read(queryStr);
        });
    }

    @Test
    public void testSingleQueryForDebugging() {
//        CypherReader.read(QUERY_OPTIONAL_8);
    }
}
