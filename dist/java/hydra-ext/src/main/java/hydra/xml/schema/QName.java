// Note: this is an automatically generated file. Do not edit.

package hydra.xml.schema;

import java.io.Serializable;

public class QName implements Serializable, Comparable<QName> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.xml.schema.QName");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String value;

  public QName (String value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof QName)) {
      return false;
    }
    QName o = (QName) other;
    return java.util.Objects.equals(
      this.value,
      o.value);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(value);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(QName other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
