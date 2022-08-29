package hydra.ext.rdf.syntax;

/**
 * A value such as a string, number, or date
 */
public class Literal {
  /**
   * a Unicode string, which should be in Normal Form C
   */
  public final String lexicalForm;
  
  /**
   * an IRI identifying a datatype that determines how the lexical form maps to a literal value
   */
  public final hydra.ext.rdf.syntax.Iri datatypeIri;
  
  /**
   * An optional language tag, present if and only if the datatype IRI is http://www.w3.org/1999/02/22-rdf-syntax-ns#langString
   */
  public final java.util.Optional<hydra.ext.rdf.syntax.LanguageTag> languageTag;
  
  public Literal (String lexicalForm, hydra.ext.rdf.syntax.Iri datatypeIri, java.util.Optional<hydra.ext.rdf.syntax.LanguageTag> languageTag) {
    this.lexicalForm = lexicalForm;
    this.datatypeIri = datatypeIri;
    this.languageTag = languageTag;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Literal)) {
      return false;
    }
    Literal o = (Literal) (other);
    return lexicalForm.equals(o.lexicalForm) && datatypeIri.equals(o.datatypeIri) && languageTag.equals(o.languageTag);
  }
  
  @Override
  public int hashCode() {
    return 2 * lexicalForm.hashCode() + 3 * datatypeIri.hashCode() + 5 * languageTag.hashCode();
  }
  
  public Literal withLexicalForm(String lexicalForm) {
    return new Literal(lexicalForm, datatypeIri, languageTag);
  }
  
  public Literal withDatatypeIri(hydra.ext.rdf.syntax.Iri datatypeIri) {
    return new Literal(lexicalForm, datatypeIri, languageTag);
  }
  
  public Literal withLanguageTag(java.util.Optional<hydra.ext.rdf.syntax.LanguageTag> languageTag) {
    return new Literal(lexicalForm, datatypeIri, languageTag);
  }
}