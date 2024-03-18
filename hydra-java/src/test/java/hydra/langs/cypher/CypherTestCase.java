package hydra.langs.cypher;

import org.apache.commons.csv.CSVFormat;
import org.apache.commons.csv.CSVParser;
import org.apache.commons.csv.CSVRecord;

import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.List;

/**
 * A test case drawn from Neo4j's Cypher manual (https://neo4j.com/docs/cypher-manual)
 *
 * The test cases are broken down by section and subsection of the manual,
 * and given a numeric id within each subsection.
 * Certain sections and subsections (e.g. on loading from CSVs, database management, etc.) have been omitted
 * as they are less relevant to query transformation.
 */
class CypherTestCase {
    public final String section;
    public final String subsection;
    public final int id;
    public final boolean supported;
    public final String query;

    public CypherTestCase(String section,
                          String subsection,
                          int id,
                          boolean supported,
                          String query) {
        this.section = section;
        this.subsection = subsection;
        this.id = id;
        this.supported = supported;
        this.query = query;
    }

    public String getName() {
        return (section + "_" + subsection + "_" + id).replaceAll("[ \\t]", "_");
    }

    public static List<CypherTestCase> loadCases() throws IOException {
        InputStream inputStream = CypherReaderTest.class.getResourceAsStream("/Cypher_test_cases.csv");
        assert inputStream != null;
        InputStreamReader reader = new InputStreamReader(inputStream);

        CSVParser parser = CSVFormat.DEFAULT.withHeader().parse(reader);
        List<CypherTestCase> cases = new ArrayList<>();
        for (CSVRecord record : parser) {
            String[] values = record.values();
            if (values.length != 5) {
                throw new IOException("Invalid test case record: " + record);
            }
            String section = values[0];
            String subsection = values[1];
            int id = Integer.parseInt(values[2]);
            String supportedStr = values[3].toLowerCase();
            String query = values[4];

            boolean supported;
            if (supportedStr.equals("true") || supportedStr.equals("yes")) {
                supported = true;
            } else if (supportedStr.equals("false") || supportedStr.equals("no")) {
                supported = false;
            } else {
                throw new IOException("Invalid supported value: " + supportedStr);
            }

            if (section.isEmpty() || subsection.isEmpty() || query.isEmpty()) {
                throw new IOException("Empty value(s) in test case record: " + record);
            }

            CypherTestCase tc = new CypherTestCase(section, subsection, id, supported, query);

            cases.add(tc);
        }

        return cases;
    }
}
