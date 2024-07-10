// Note: this is an automatically generated file. Do not edit.

package hydra.langs.owl.syntax;

import java.io.Serializable;

public class DataPropertyRange implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/langs/owl/syntax.DataPropertyRange");
  
  public final java.util.List<hydra.langs.owl.syntax.Annotation> annotations;
  
  public final hydra.langs.owl.syntax.DataPropertyExpression property;
  
  public final hydra.langs.owl.syntax.ClassExpression range;
  
  public DataPropertyRange (java.util.List<hydra.langs.owl.syntax.Annotation> annotations, hydra.langs.owl.syntax.DataPropertyExpression property, hydra.langs.owl.syntax.ClassExpression range) {
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    if (property == null) {
      throw new IllegalArgumentException("null value for 'property' argument");
    }
    if (range == null) {
      throw new IllegalArgumentException("null value for 'range' argument");
    }
    this.annotations = annotations;
    this.property = property;
    this.range = range;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof DataPropertyRange)) {
      return false;
    }
    DataPropertyRange o = (DataPropertyRange) (other);
    return annotations.equals(o.annotations) && property.equals(o.property) && range.equals(o.range);
  }
  
  @Override
  public int hashCode() {
    return 2 * annotations.hashCode() + 3 * property.hashCode() + 5 * range.hashCode();
  }
  
  public DataPropertyRange withAnnotations(java.util.List<hydra.langs.owl.syntax.Annotation> annotations) {
    if (annotations == null) {
      throw new IllegalArgumentException("null value for 'annotations' argument");
    }
    return new DataPropertyRange(annotations, property, range);
  }
  
  public DataPropertyRange withProperty(hydra.langs.owl.syntax.DataPropertyExpression property) {
    if (property == null) {
      throw new IllegalArgumentException("null value for 'property' argument");
    }
    return new DataPropertyRange(annotations, property, range);
  }
  
  public DataPropertyRange withRange(hydra.langs.owl.syntax.ClassExpression range) {
    if (range == null) {
      throw new IllegalArgumentException("null value for 'range' argument");
    }
    return new DataPropertyRange(annotations, property, range);
  }
}