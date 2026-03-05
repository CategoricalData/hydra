// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.owl.syntax;

import java.io.Serializable;

public class Ontology implements Serializable, Comparable<Ontology> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.owl.syntax.Ontology");
  
  public static final hydra.core.Name DIRECT_IMPORTS = new hydra.core.Name("directImports");
  
  public static final hydra.core.Name ANNOTATIONS = new hydra.core.Name("annotations");
  
  public static final hydra.core.Name AXIOMS = new hydra.core.Name("axioms");
  
  public final java.util.List<hydra.ext.org.w3.owl.syntax.Ontology> directImports;
  
  public final java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations;
  
  public final java.util.List<hydra.ext.org.w3.owl.syntax.Axiom> axioms;
  
  public Ontology (java.util.List<hydra.ext.org.w3.owl.syntax.Ontology> directImports, java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations, java.util.List<hydra.ext.org.w3.owl.syntax.Axiom> axioms) {
    this.directImports = directImports;
    this.annotations = annotations;
    this.axioms = axioms;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Ontology)) {
      return false;
    }
    Ontology o = (Ontology) other;
    return java.util.Objects.equals(
      this.directImports,
      o.directImports) && java.util.Objects.equals(
      this.annotations,
      o.annotations) && java.util.Objects.equals(
      this.axioms,
      o.axioms);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(directImports) + 3 * java.util.Objects.hashCode(annotations) + 5 * java.util.Objects.hashCode(axioms);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Ontology other) {
    int cmp = 0;
    cmp = Integer.compare(
      directImports.hashCode(),
      other.directImports.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    cmp = Integer.compare(
      annotations.hashCode(),
      other.annotations.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return Integer.compare(
      axioms.hashCode(),
      other.axioms.hashCode());
  }
  
  public Ontology withDirectImports(java.util.List<hydra.ext.org.w3.owl.syntax.Ontology> directImports) {
    return new Ontology(directImports, annotations, axioms);
  }
  
  public Ontology withAnnotations(java.util.List<hydra.ext.org.w3.owl.syntax.Annotation> annotations) {
    return new Ontology(directImports, annotations, axioms);
  }
  
  public Ontology withAxioms(java.util.List<hydra.ext.org.w3.owl.syntax.Axiom> axioms) {
    return new Ontology(directImports, annotations, axioms);
  }
}
