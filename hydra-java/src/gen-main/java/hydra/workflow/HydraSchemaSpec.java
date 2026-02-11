// Note: this is an automatically generated file. Do not edit.

package hydra.workflow;

import java.io.Serializable;

/**
 * The specification of a Hydra schema, provided as a set of modules and a distinguished type
 */
public class HydraSchemaSpec implements Serializable, Comparable<HydraSchemaSpec> {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.workflow.HydraSchemaSpec");
  
  public static final hydra.core.Name FIELD_NAME_MODULES = new hydra.core.Name("modules");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_NAME = new hydra.core.Name("typeName");
  
  /**
   * The modules to include in the schema graph
   */
  public final java.util.List<hydra.module.Module> modules;
  
  /**
   * The name of the top-level type; all data which passes through the workflow will be instances of this type
   */
  public final hydra.core.Name typeName;
  
  public HydraSchemaSpec (java.util.List<hydra.module.Module> modules, hydra.core.Name typeName) {
    this.modules = modules;
    this.typeName = typeName;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof HydraSchemaSpec)) {
      return false;
    }
    HydraSchemaSpec o = (HydraSchemaSpec) other;
    return java.util.Objects.equals(
      this.modules,
      o.modules) && java.util.Objects.equals(
      this.typeName,
      o.typeName);
  }
  
  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(modules) + 3 * java.util.Objects.hashCode(typeName);
  }
  
  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(HydraSchemaSpec other) {
    int cmp = 0;
    cmp = Integer.compare(
      modules.hashCode(),
      other.modules.hashCode());
    if (cmp != 0) {
      return cmp;
    }
    return ((Comparable) typeName).compareTo(other.typeName);
  }
  
  public HydraSchemaSpec withModules(java.util.List<hydra.module.Module> modules) {
    return new HydraSchemaSpec(modules, typeName);
  }
  
  public HydraSchemaSpec withTypeName(hydra.core.Name typeName) {
    return new HydraSchemaSpec(modules, typeName);
  }
}
