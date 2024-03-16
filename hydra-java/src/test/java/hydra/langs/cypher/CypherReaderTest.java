package hydra.langs.cypher;

import hydra.langs.cypher.openCypher.Query;
import hydra.tools.AntlrReaderBase;
import org.junit.jupiter.api.Test;
import org.junit.jupiter.params.ParameterizedTest;
import org.junit.jupiter.params.provider.ValueSource;

import static org.junit.jupiter.api.Assertions.assertNotNull;
import static org.junit.jupiter.api.Assertions.assertThrows;


public class CypherReaderTest {
    private static final String QUERY_1 = "MATCH (n) RETURN *";

    private static final String QUERY_2 = "MATCH (n) RETURN n";

    private static final String QUERY_3
            = "MATCH (keanu:Person {name:'Keanu Reeves'})\n"
            + "RETURN keanu.name AS name, keanu.born AS born";

    private static final String QUERY_4
            = "MATCH (people:Person)\n"
            + "RETURN people\n"
            + "LIMIT 5";

    private static final String QUERY_INVALID_1 = "MATCH oops";

    @ParameterizedTest
    @ValueSource(strings = {QUERY_1, QUERY_2, QUERY_3, QUERY_4})
    public void testValidQueriesAreAccepted(String queryStr) {
        Query query = CypherReader.read(queryStr);
        assertNotNull(query);
    }

    @ParameterizedTest
    @ValueSource(strings = {QUERY_INVALID_1})
    public void testInvalidQueriesAreRejected(String queryStr) {
        assertThrows(AntlrReaderBase.AntlrReaderException.class, () -> {
            CypherReader.read(queryStr);
        });
    }

    @Test
    public void testSingleQueryForDebugging() {
        CypherReader.read(QUERY_4);
    }
}
