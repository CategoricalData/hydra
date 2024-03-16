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
 *   QUERY_BASIC_xx -- https://neo4j.com/docs/cypher-manual/current/queries/basic/
 *   QUERY_CASE_xx  -- https://neo4j.com/docs/cypher-manual/current/queries/case/
 */
public class CypherReaderTest {
    private static final String QUERY_1 = "MATCH (n) RETURN *";

    private static final String QUERY_2 = "MATCH (n) RETURN n";

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

//    private static final String QUERY_CASE_2_UNSUPPORTED
//            = "MATCH (n:Person)\n"
//            + "RETURN\n"
//            + "CASE n.eyes\n"
//            + "  WHEN 'blue'  THEN 1\n"
//            + "  WHEN 'brown', 'hazel' THEN 2\n"
//            + "  ELSE 3\n"
//            + "END AS result, n.eyes";

    private static final String QUERY_INVALID_1 = "MATCH oops";

    @ParameterizedTest
    @ValueSource(strings = {
            QUERY_1, QUERY_2,
            QUERY_BASIC_1, QUERY_BASIC_2, QUERY_BASIC_3, QUERY_BASIC_4, QUERY_BASIC_5, QUERY_BASIC_6,
            QUERY_BASIC_10, QUERY_BASIC_11, QUERY_BASIC_12})
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
            QUERY_CASE_1_UNSUPPORTED})
    public void testUnsupportedQueriesAreRejected(String queryStr) {
        assertThrows(AntlrReaderBase.AntlrReaderException.class, () -> {
            CypherReader.read(queryStr);
        });
    }

    @Test
    public void testSingleQueryForDebugging() {
//        CypherReader.read(QUERY_BASIC_6);
        //CypherReader.read(QUERY_CASE_2_UNSUPPORTED);
    }
}
