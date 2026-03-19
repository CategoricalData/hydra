// Note: this is an automatically generated file. Do not edit.

package hydra.ext.lisp.syntax;

import java.io.Serializable;

/**
 * A record/struct type definition. Serializes as (defrecord Name [fields]) in Clojure, (cl-defstruct name fields) in Emacs Lisp, (defstruct name fields) in Common Lisp, (define-record-type &lt;Name&gt; ...) in Scheme
 */
public class RecordTypeDefinition implements Serializable, Comparable<RecordTypeDefinition> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.lisp.syntax.RecordTypeDefinition");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name FIELDS = new hydra.core.Name("fields");

  public static final hydra.core.Name DOC = new hydra.core.Name("doc");

  /**
   * The record type name
   */
  public final hydra.ext.lisp.syntax.Symbol name;

  /**
   * The field definitions
   */
  public final hydra.util.ConsList<hydra.ext.lisp.syntax.FieldDefinition> fields;

  /**
   * Optional docstring
   */
  public final hydra.util.Maybe<hydra.ext.lisp.syntax.Docstring> doc;

  public RecordTypeDefinition (hydra.ext.lisp.syntax.Symbol name, hydra.util.ConsList<hydra.ext.lisp.syntax.FieldDefinition> fields, hydra.util.Maybe<hydra.ext.lisp.syntax.Docstring> doc) {
    this.name = name;
    this.fields = fields;
    this.doc = doc;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RecordTypeDefinition)) {
      return false;
    }
    RecordTypeDefinition o = (RecordTypeDefinition) other;
    return java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.fields,
      o.fields) && java.util.Objects.equals(
      this.doc,
      o.doc);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(name) + 3 * java.util.Objects.hashCode(fields) + 5 * java.util.Objects.hashCode(doc);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(RecordTypeDefinition other) {
    int cmp = 0;
    cmp = ((Comparable) name).compareTo(other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = ((Comparable) fields).compareTo(other.fields);
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) doc).compareTo(other.doc);
  }

  public RecordTypeDefinition withName(hydra.ext.lisp.syntax.Symbol name) {
    return new RecordTypeDefinition(name, fields, doc);
  }

  public RecordTypeDefinition withFields(hydra.util.ConsList<hydra.ext.lisp.syntax.FieldDefinition> fields) {
    return new RecordTypeDefinition(name, fields, doc);
  }

  public RecordTypeDefinition withDoc(hydra.util.Maybe<hydra.ext.lisp.syntax.Docstring> doc) {
    return new RecordTypeDefinition(name, fields, doc);
  }
}
