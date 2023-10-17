package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class StringListNullPredicateExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.StringListNullPredicateExpression");
  
  public final hydra.langs.cypher.openCypher.AddOrSubtractExpression prefix;
  
  public final java.util.List<hydra.langs.cypher.openCypher.StringListNullPredicateSuffix> suffixes;
  
  public StringListNullPredicateExpression (hydra.langs.cypher.openCypher.AddOrSubtractExpression prefix, java.util.List<hydra.langs.cypher.openCypher.StringListNullPredicateSuffix> suffixes) {
    this.prefix = prefix;
    this.suffixes = suffixes;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof StringListNullPredicateExpression)) {
      return false;
    }
    StringListNullPredicateExpression o = (StringListNullPredicateExpression) (other);
    return prefix.equals(o.prefix) && suffixes.equals(o.suffixes);
  }
  
  @Override
  public int hashCode() {
    return 2 * prefix.hashCode() + 3 * suffixes.hashCode();
  }
  
  public StringListNullPredicateExpression withPrefix(hydra.langs.cypher.openCypher.AddOrSubtractExpression prefix) {
    return new StringListNullPredicateExpression(prefix, suffixes);
  }
  
  public StringListNullPredicateExpression withSuffixes(java.util.List<hydra.langs.cypher.openCypher.StringListNullPredicateSuffix> suffixes) {
    return new StringListNullPredicateExpression(prefix, suffixes);
  }
}