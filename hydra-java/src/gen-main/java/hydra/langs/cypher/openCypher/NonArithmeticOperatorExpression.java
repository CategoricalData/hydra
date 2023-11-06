package hydra.langs.cypher.openCypher;

import java.io.Serializable;

public class NonArithmeticOperatorExpression implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/cypher/openCypher.NonArithmeticOperatorExpression");
  
  public final hydra.langs.cypher.openCypher.Atom atom;
  
  public final java.util.List<hydra.langs.cypher.openCypher.NonArithmeticOperatorInfix> suffixes;
  
  public final java.util.List<hydra.langs.cypher.openCypher.NodeLabel> labels;
  
  public NonArithmeticOperatorExpression (hydra.langs.cypher.openCypher.Atom atom, java.util.List<hydra.langs.cypher.openCypher.NonArithmeticOperatorInfix> suffixes, java.util.List<hydra.langs.cypher.openCypher.NodeLabel> labels) {
    this.atom = atom;
    this.suffixes = suffixes;
    this.labels = labels;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof NonArithmeticOperatorExpression)) {
      return false;
    }
    NonArithmeticOperatorExpression o = (NonArithmeticOperatorExpression) (other);
    return atom.equals(o.atom) && suffixes.equals(o.suffixes) && labels.equals(o.labels);
  }
  
  @Override
  public int hashCode() {
    return 2 * atom.hashCode() + 3 * suffixes.hashCode() + 5 * labels.hashCode();
  }
  
  public NonArithmeticOperatorExpression withAtom(hydra.langs.cypher.openCypher.Atom atom) {
    return new NonArithmeticOperatorExpression(atom, suffixes, labels);
  }
  
  public NonArithmeticOperatorExpression withSuffixes(java.util.List<hydra.langs.cypher.openCypher.NonArithmeticOperatorInfix> suffixes) {
    return new NonArithmeticOperatorExpression(atom, suffixes, labels);
  }
  
  public NonArithmeticOperatorExpression withLabels(java.util.List<hydra.langs.cypher.openCypher.NodeLabel> labels) {
    return new NonArithmeticOperatorExpression(atom, suffixes, labels);
  }
}