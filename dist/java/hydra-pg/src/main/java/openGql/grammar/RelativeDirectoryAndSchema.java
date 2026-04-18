// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class RelativeDirectoryAndSchema implements Serializable, Comparable<RelativeDirectoryAndSchema> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.RelativeDirectoryAndSchema");

  public static final hydra.core.Name DIRECTORY_PATH = new hydra.core.Name("directoryPath");

  public static final hydra.core.Name SCHEMA_NAME = new hydra.core.Name("schemaName");

  public final openGql.grammar.RelativeDirectoryPath directoryPath;

  public final String schemaName;

  public RelativeDirectoryAndSchema (openGql.grammar.RelativeDirectoryPath directoryPath, String schemaName) {
    this.directoryPath = directoryPath;
    this.schemaName = schemaName;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof RelativeDirectoryAndSchema)) {
      return false;
    }
    RelativeDirectoryAndSchema o = (RelativeDirectoryAndSchema) other;
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
  public int compareTo(RelativeDirectoryAndSchema other) {
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

  public RelativeDirectoryAndSchema withDirectoryPath(openGql.grammar.RelativeDirectoryPath directoryPath) {
    return new RelativeDirectoryAndSchema(directoryPath, schemaName);
  }

  public RelativeDirectoryAndSchema withSchemaName(String schemaName) {
    return new RelativeDirectoryAndSchema(directoryPath, schemaName);
  }
}
