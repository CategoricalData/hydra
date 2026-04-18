// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class AbsoluteDirectoryAndSchema implements Serializable, Comparable<AbsoluteDirectoryAndSchema> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.AbsoluteDirectoryAndSchema");

  public static final hydra.core.Name DIRECTORY_PATH = new hydra.core.Name("directoryPath");

  public static final hydra.core.Name SCHEMA_NAME = new hydra.core.Name("schemaName");

  public final hydra.util.Maybe<java.util.List<String>> directoryPath;

  public final String schemaName;

  public AbsoluteDirectoryAndSchema (hydra.util.Maybe<java.util.List<String>> directoryPath, String schemaName) {
    this.directoryPath = directoryPath;
    this.schemaName = schemaName;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof AbsoluteDirectoryAndSchema)) {
      return false;
    }
    AbsoluteDirectoryAndSchema o = (AbsoluteDirectoryAndSchema) other;
    return java.util.Objects.equals(
      this.directoryPath,
      o.directoryPath) && java.util.Objects.equals(
      this.schemaName,
      o.schemaName);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(directoryPath) + 3 * java.util.Objects.hashCode(schemaName);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(AbsoluteDirectoryAndSchema other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      directoryPath,
      other.directoryPath);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      schemaName,
      other.schemaName);
  }

  public AbsoluteDirectoryAndSchema withDirectoryPath(hydra.util.Maybe<java.util.List<String>> directoryPath) {
    return new AbsoluteDirectoryAndSchema(directoryPath, schemaName);
  }

  public AbsoluteDirectoryAndSchema withSchemaName(String schemaName) {
    return new AbsoluteDirectoryAndSchema(directoryPath, schemaName);
  }
}
