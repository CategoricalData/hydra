import hydra.langs.cypher.CypherReader;
import hydra.langs.cypher.CypherReaderTest;
import hydra.langs.cypher.FromCypher;
import hydra.langs.kusto.KqlWriter;
import hydra.langs.kusto.ToKql;
import hydra.langs.tinkerpop.queries.Query;
import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.assertNotNull;

public class CypherToKqlTest extends CypherReaderTest {
    private static final String QUERY_0 = "MATCH (n:Person) RETURN n.name, n.email";

    private static final String QUERY_1 = "MATCH (n) RETURN *";

    private static final String QUERY_2 = "MATCH (n:Person) RETURN COUNT(n)";

    private static final String QUERY_3 = "MATCH (p1:Person)-[:knows]->(p2:Person) RETURN p2";

    @Test
    public void testSingleQueryForDebugging() {
        String cypherString = QUERY_0;

        hydra.langs.cypher.openCypher.Query cypher = CypherReader.read(cypherString);
        assertNotNull(cypher);

        Query pg = FromCypher.from(cypher);
        assertNotNull(pg);

        hydra.langs.kusto.kql.Query kql = new hydra.langs.kusto.kql.Query(ToKql.toKql(pg));
        assertNotNull(kql);

        String kqlString = KqlWriter.write(kql);

        System.out.println(
                "----------------------------------------\n" +
                        "[Cypher]\n" +
                        cypherString + "\n" +
                        "\n" +
                        "[KQL]\n" +
                        kqlString + "\n" +
                        "----------------------------------------\n");
    }
}
