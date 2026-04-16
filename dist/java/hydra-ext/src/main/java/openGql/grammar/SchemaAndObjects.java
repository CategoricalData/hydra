// Note: this is an automatically generated file. Do not edit.

package openGql.grammar;

import java.io.Serializable;

public class SchemaAndObjects implements Serializable, Comparable<SchemaAndObjects> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("openGql.grammar.SchemaAndObjects");

  public static final hydra.core.Name SCHEMA_REFERENCE = new hydra.core.Name("schemaReference");

  public static final hydra.core.Name OBJECTS = new hydra.core.Name("objects");

  public final openGql.grammar.SchemaReference schemaReference;

  public final java.util.List<String> objects;

  public SchemaAndObjects (openGql.grammar.SchemaReference schemaReference, java.util.List<String> objects) {
    this.schemaReference = schemaReference;
    this.objects = objects;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof SchemaAndObjects)) {
      return false;
    }
    SchemaAndObjects o = (SchemaAndObjects) other;
    return java.util.Objects.equals(
      this.schemaReference,
      o.schemaReference) && java.util.Objects.equals(
      this.objects,
      o.objects);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(schemaReference) + 3 * java.util.Objects.hashCode(objects);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(SchemaAndObjects other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      schemaReference,
      other.schemaReference);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      objects,
      other.objects);
  }

  public SchemaAndObjects withSchemaReference(openGql.grammar.SchemaReference schemaReference) {
    return new SchemaAndObjects(schemaReference, objects);
  }

  public SchemaAndObjects withObjects(java.util.List<String> objects) {
    return new SchemaAndObjects(schemaReference, objects);
  }
}
