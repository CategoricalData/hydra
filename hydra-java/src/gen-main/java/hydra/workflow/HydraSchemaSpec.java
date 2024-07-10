// Note: this is an automatically generated file. Do not edit.

package hydra.workflow;

import java.io.Serializable;

/**
 * The specification of a Hydra schema, provided as a set of modules and a distinguished type
 */
public class HydraSchemaSpec implements Serializable {
  public static final hydra.core.Name NAME = new hydra.core.Name("hydra/workflow.HydraSchemaSpec");
  
  /**
   * The modules to include in the schema graph
   */
  public final java.util.List<hydra.module.Module<hydra.compute.Kv>> modules;
  
  /**
   * The name of the top-level type; all data which passes through the workflow will be instances of this type
   */
  public final hydra.core.Name typeName;
  
  public HydraSchemaSpec (java.util.List<hydra.module.Module<hydra.compute.Kv>> modules, hydra.core.Name typeName) {
    if (modules == null) {
      throw new IllegalArgumentException("null value for 'modules' argument");
    }
    if (typeName == null) {
      throw new IllegalArgumentException("null value for 'typeName' argument");
    }
    this.modules = modules;
    this.typeName = typeName;
  }
  
  @Override
  public boolean equals(Object other) {
    if (!(other instanceof HydraSchemaSpec)) {
      return false;
    }
    HydraSchemaSpec o = (HydraSchemaSpec) (other);
    return modules.equals(o.modules) && typeName.equals(o.typeName);
  }
  
  @Override
  public int hashCode() {
    return 2 * modules.hashCode() + 3 * typeName.hashCode();
  }
  
  public HydraSchemaSpec withModules(java.util.List<hydra.module.Module<hydra.compute.Kv>> modules) {
    if (modules == null) {
      throw new IllegalArgumentException("null value for 'modules' argument");
    }
    return new HydraSchemaSpec(modules, typeName);
  }
  
  public HydraSchemaSpec withTypeName(hydra.core.Name typeName) {
    if (typeName == null) {
      throw new IllegalArgumentException("null value for 'typeName' argument");
    }
    return new HydraSchemaSpec(modules, typeName);
  }
}