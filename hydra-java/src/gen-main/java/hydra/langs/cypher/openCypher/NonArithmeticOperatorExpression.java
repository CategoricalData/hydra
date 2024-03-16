package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class NonArithmeticOperatorExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.NonArithmeticOperatorExpression");
  
  public final hydra.langs.cypher.openCypher.Atom atom;
  
  public final java.util.List<hydra.langs.cypher.openCypher.ListOperatorExpressionOrPropertyLookup> listsAndLookups;
  
  public final java.util.Optional<hydra.langs.cypher.openCypher.NodeLabels> labels;
  
  public NonArithmeticOperatorExpression (hydra.langs.cypher.openCypher.Atom atom, java.util.List<hydra.langs.cypher.openCypher.ListOperatorExpressionOrPropertyLookup> listsAndLookups, java.util.Optional<hydra.langs.cypher.openCypher.NodeLabels> labels) {
    this.atom = atom;
    this.listsAndLookups = listsAndLookups;
    this.labels = labels;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NonArithmeticOperatorExpression)) {
      return false;
    }
    NonArithmeticOperatorExpression o = (NonArithmeticOperatorExpression) (other);
    return atom.equals(o.atom) && listsAndLookups.equals(o.listsAndLookups) && labels.equals(o.labels);
  }
  
  @Override
  public int hashCode() {
    return 2 * atom.hashCode() + 3 * listsAndLookups.hashCode() + 5 * labels.hashCode();
  }
  
  public NonArithmeticOperatorExpression withAtom(hydra.langs.cypher.openCypher.Atom atom) {
    return new NonArithmeticOperatorExpression(atom, listsAndLookups, labels);
  }
  
  public NonArithmeticOperatorExpression withListsAndLookups(java.util.List<hydra.langs.cypher.openCypher.ListOperatorExpressionOrPropertyLookup> listsAndLookups) {
    return new NonArithmeticOperatorExpression(atom, listsAndLookups, labels);
  }
  
  public NonArithmeticOperatorExpression withLabels(java.util.Optional<hydra.langs.cypher.openCypher.NodeLabels> labels) {
    return new NonArithmeticOperatorExpression(atom, listsAndLookups, labels);
  }
}