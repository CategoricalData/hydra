// Note: this is an automatically generated file. Do not edit.

package hydra.packaging;

import java.io.Serializable;

/**
 * A file extension (without the dot), e.g. "json" or "py"
 */
public class FileExtension implements Serializable, Comparable<FileExtension> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.packaging.FileExtension");

  public static final hydra.core.Name VALUE = new hydra.core.Name("value");

  public final String value;

  public FileExtension (String value) {
    this.value = value;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FileExtension)) {
      return false;
    }
    FileExtension o = (FileExtension) other;
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
  public int compareTo(FileExtension other) {
    return hydra.util.Comparing.compare(
      value,
      other.value);
  }
}
