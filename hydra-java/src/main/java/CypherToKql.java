public class CypherToKql {
    public static hydra.langs.kusto.kql.Query toKql(hydra.langs.cypher.openCypher.Query cypher) {
        return unsupported();
    }


    private static <T> T unsupported() {
        throw new UnsupportedOperationException();
    }
}
