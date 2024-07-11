// Note: this is an automatically generated file. Do not edit.

package hydra.langs.owl.syntax;

import java.io.Serializable;

public class Ontology implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/owl/syntax.Ontology");
  
  public final java.util.List<hydra.langs.owl.syntax.Ontology> directImports;
  
  public final java.util.List<hydra.langs.owl.syntax.Annotation> annotations;
  
  public final java.util.List<hydra.langs.owl.syntax.Axiom> axioms;
  
  public Ontology (java.util.List<hydra.langs.owl.syntax.Ontology> directImports, java.util.List<hydra.langs.owl.syntax.Annotation> annotations, java.util.List<hydra.langs.owl.syntax.Axiom> axioms) {
    java.util.Objects.requireNonNull((directImports));
    java.util.Objects.requireNonNull((annotations));
    java.util.Objects.requireNonNull((axioms));
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
    java.util.Objects.requireNonNull((directImports));
    return new Ontology(directImports, annotations, axioms);
  }
  
  public Ontology withAnnotations(java.util.List<hydra.langs.owl.syntax.Annotation> annotations) {
    java.util.Objects.requireNonNull((annotations));
    return new Ontology(directImports, annotations, axioms);
  }
  
  public Ontology withAxioms(java.util.List<hydra.langs.owl.syntax.Axiom> axioms) {
    java.util.Objects.requireNonNull((axioms));
    return new Ontology(directImports, annotations, axioms);
  }
}