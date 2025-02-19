// Note: this is an automatically generated file. Do not edit.

package hydra.ext.io.shex.syntax;

import java.io.Serializable;

public class CodeDecl implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.io.shex.syntax.CodeDecl");
  
  public static final hydra.core.Name FIELD_NAME_IRI = new hydra.core.Name("iri");
  
  public static final hydra.core.Name FIELD_NAME_ALTS = new hydra.core.Name("alts");
  
  public final hydra.ext.io.shex.syntax.Iri iri;
  
  public final hydra.ext.io.shex.syntax.CodeDecl_Alts alts;
  
  public CodeDecl (hydra.ext.io.shex.syntax.Iri iri, hydra.ext.io.shex.syntax.CodeDecl_Alts alts) {
    java.util.Objects.requireNonNull((iri));
    java.util.Objects.requireNonNull((alts));
    this.iri = iri;
    this.alts = alts;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof CodeDecl)) {
      return false;
    }
    CodeDecl o = (CodeDecl) (other);
    return iri.equals(o.iri) && alts.equals(o.alts);
  }
  
  @Override
  public int hashCode() {
    return 2 * iri.hashCode() + 3 * alts.hashCode();
  }
  
  public CodeDecl withIri(hydra.ext.io.shex.syntax.Iri iri) {
    java.util.Objects.requireNonNull((iri));
    return new CodeDecl(iri, alts);
  }
  
  public CodeDecl withAlts(hydra.ext.io.shex.syntax.CodeDecl_Alts alts) {
    java.util.Objects.requireNonNull((alts));
    return new CodeDecl(iri, alts);
  }
}