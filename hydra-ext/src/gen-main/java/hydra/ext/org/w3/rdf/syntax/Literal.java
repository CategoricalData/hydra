// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.rdf.syntax;

import java.io.Serializable;

/**
 * A value such as a string, number, or date
 */
public class Literal implements Serializable, Comparable<Literal> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.org.w3.rdf.syntax.Literal");
  
  public static final hydra.core.Name LEXICAL_FORM = new hydra.core.Name("lexicalForm");
  
  public static final hydra.core.Name DATATYPE_IRI = new hydra.core.Name("datatypeIri");
  
  public static final hydra.core.Name LANGUAGE_TAG = new hydra.core.Name("languageTag");
  
  /**
   * a Unicode string, which should be in Normal Form C
   */
  public final String lexicalForm;
  
  /**
   * an IRI identifying a datatype that determines how the lexical form maps to a literal value
   */
  public final hydra.ext.org.w3.rdf.syntax.Iri datatypeIri;
  
  /**
   * An optional language tag, present if and only if the datatype IRI is http://www.w3.org/1999/02/22-rdf-syntax-ns#langString
   */
  public final hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag> languageTag;
  
  public Literal (String lexicalForm, hydra.ext.org.w3.rdf.syntax.Iri datatypeIri, hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag> languageTag) {
    this.lexicalForm = lexicalForm;
    this.datatypeIri = datatypeIri;
    this.languageTag = languageTag;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof Literal)) {
      return false;
    }
    Literal o = (Literal) other;
    return java.util.Objects.equals(
      this.lexicalForm,
      o.lexicalForm) && java.util.Objects.equals(
      this.datatypeIri,
      o.datatypeIri) && java.util.Objects.equals(
      this.languageTag,
      o.languageTag);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(lexicalForm) + 3 * java.util.Objects.hashCode(datatypeIri) + 5 * java.util.Objects.hashCode(languageTag);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(Literal other) {
    int cmp = 0;
    cmp = ((Comparable) lexicalForm).compareTo(other.lexicalForm);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) datatypeIri).compareTo(other.datatypeIri);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) languageTag).compareTo(other.languageTag);
  }
  
  public Literal withLexicalForm(String lexicalForm) {
    return new Literal(lexicalForm, datatypeIri, languageTag);
  }
  
  public Literal withDatatypeIri(hydra.ext.org.w3.rdf.syntax.Iri datatypeIri) {
    return new Literal(lexicalForm, datatypeIri, languageTag);
  }
  
  public Literal withLanguageTag(hydra.util.Maybe<hydra.ext.org.w3.rdf.syntax.LanguageTag> languageTag) {
    return new Literal(lexicalForm, datatypeIri, languageTag);
  }
}
