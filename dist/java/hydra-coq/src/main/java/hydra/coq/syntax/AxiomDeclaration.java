// Note: this is an automatically generated file. Do not edit.

package hydra.coq.syntax;

import java.io.Serializable;

/**
 * An Axiom declaration: `Axiom &lt;name&gt; : &lt;type&gt;.`
 */
public class AxiomDeclaration implements Serializable, Comparable<AxiomDeclaration> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.coq.syntax.AxiomDeclaration");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TYPE = new hydra.core.Name("type");

  public final hydra.coq.syntax.Ident name;

  public final hydra.coq.syntax.Type type;

  public AxiomDeclaration (hydra.coq.syntax.Ident name, hydra.coq.syntax.Type type) {
    this.name = name;
    this.type = type;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AxiomDeclaration)) {
      return false;
    }
    AxiomDeclaration o = (AxiomDeclaration) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.type,
      o.type);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(type);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(AxiomDeclaration other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      type,
      other.type);
  }

  public AxiomDeclaration withName(hydra.coq.syntax.Ident name) {
    return new AxiomDeclaration(name, type);
  }

  public AxiomDeclaration withType(hydra.coq.syntax.Type type) {
    return new AxiomDeclaration(name, type);
  }
}
