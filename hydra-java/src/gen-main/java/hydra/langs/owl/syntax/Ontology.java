// Note: this is an automatically generated file. Do not edit.

package hydra.langs.owl.syntax;

import java.io.Serializable;

public class Ontology implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/owl/syntax.Ontology");
  
  public final java.util.List<hydra.langs.owl.syntax.Ontology> directImports;
  
  public final java.util.List<hydra.langs.owl.syntax.Annotation> annotations;
  
  public final java.util.List<hydra.langs.owl.syntax.Axiom> axioms;
  
  public Ontology (java.util.List<hydra.langs.owl.syntax.Ontology> directImports, java.util.List<hydra.langs.owl.syntax.Annotation> annotations, java.util.List<hydra.langs.owl.syntax.Axiom> axioms) {
    if (directImports == null) {
      throw new IllegalArgumentException("null value for 'directImports' argument");
    }
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    if (axioms == null) {
      throw new IllegalArgumentException("null value for 'axioms' argument");
    }
    this.directImports = directImports;
    this.annotations = annotations;
    this.axioms = axioms;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Ontology)) {
      return false;
    }
    Ontology o = (Ontology) (other);
    return directImports.equals(o.directImports) && annotations.equals(o.annotations) && axioms.equals(o.axioms);
  }
  
  @Override
  public int hashCode() {
    return 2 * directImports.hashCode() + 3 * annotations.hashCode() + 5 * axioms.hashCode();
  }
  
  public Ontology withDirectImports(java.util.List<hydra.langs.owl.syntax.Ontology> directImports) {
    if (directImports == null) {
      throw new IllegalArgumentException("null value for 'directImports' argument");
    }
    return new Ontology(directImports, annotations, axioms);
  }
  
  public Ontology withAnnotations(java.util.List<hydra.langs.owl.syntax.Annotation> annotations) {
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    return new Ontology(directImports, annotations, axioms);
  }
  
  public Ontology withAxioms(java.util.List<hydra.langs.owl.syntax.Axiom> axioms) {
    if (axioms == null) {
      throw new IllegalArgumentException("null value for 'axioms' argument");
    }
    return new Ontology(directImports, annotations, axioms);
  }
}