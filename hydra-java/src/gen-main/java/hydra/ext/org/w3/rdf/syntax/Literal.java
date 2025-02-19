// Note: this is an automatically generated file. Do not edit.

package hydra.ext.org.w3.rdf.syntax;

import java.io.Serializable;

/**
 * A value such as a string, number, or date
 */
public class Literal implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.org.w3.rdf.syntax.Literal");
  
  public static final hydra.core.Name FIELD_NAME_LEXICAL_FORM = new hydra.core.Name("lexicalForm");
  
  public static final hydra.core.Name FIELD_NAME_DATATYPE_IRI = new hydra.core.Name("datatypeIri");
  
  public static final hydra.core.Name FIELD_NAME_LANGUAGE_TAG = new hydra.core.Name("languageTag");
  
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
  public final hydra.util.Opt<hydra.ext.org.w3.rdf.syntax.LanguageTag> languageTag;
  
  public Literal (String lexicalForm, hydra.ext.org.w3.rdf.syntax.Iri datatypeIri, hydra.util.Opt<hydra.ext.org.w3.rdf.syntax.LanguageTag> languageTag) {
    java.util.Objects.requireNonNull((lexicalForm));
    java.util.Objects.requireNonNull((datatypeIri));
    java.util.Objects.requireNonNull((languageTag));
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
    java.util.Objects.requireNonNull((lexicalForm));
    return new Literal(lexicalForm, datatypeIri, languageTag);
  }
  
  public Literal withDatatypeIri(hydra.ext.org.w3.rdf.syntax.Iri datatypeIri) {
    java.util.Objects.requireNonNull((datatypeIri));
    return new Literal(lexicalForm, datatypeIri, languageTag);
  }
  
  public Literal withLanguageTag(hydra.util.Opt<hydra.ext.org.w3.rdf.syntax.LanguageTag> languageTag) {
    java.util.Objects.requireNonNull((languageTag));
    return new Literal(lexicalForm, datatypeIri, languageTag);
  }
}