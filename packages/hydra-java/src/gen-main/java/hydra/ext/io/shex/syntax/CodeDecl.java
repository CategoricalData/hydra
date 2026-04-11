// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class CodeDecl implements Serializable, Comparable<CodeDecl> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.io.shex.syntax.CodeDecl");

  public static final hydra.core.Name IRI = new hydra.core.Name("Iri");

  public static final hydra.core.Name ALTS = new hydra.core.Name("alts");

  public final hydra.ext.io.shex.syntax.Iri Iri;

  public final hydra.ext.io.shex.syntax.CodeDecl_Alts alts;

  public CodeDecl (hydra.ext.io.shex.syntax.Iri Iri, hydra.ext.io.shex.syntax.CodeDecl_Alts alts) {
    this.Iri = Iri;
    this.alts = alts;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CodeDecl)) {
      return false;
    }
    CodeDecl o = (CodeDecl) other;
    return java.util.Objects.equals(
      this.Iri,
      o.Iri) && java.util.Objects.equals(
      this.alts,
      o.alts);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(Iri) + 3 * java.util.Objects.hashCode(alts);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(CodeDecl other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      Iri,
      other.Iri);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      alts,
      other.alts);
  }

  public CodeDecl withIri(hydra.ext.io.shex.syntax.Iri Iri) {
    return new CodeDecl(Iri, alts);
  }

  public CodeDecl withAlts(hydra.ext.io.shex.syntax.CodeDecl_Alts alts) {
    return new CodeDecl(Iri, alts);
  }
}
