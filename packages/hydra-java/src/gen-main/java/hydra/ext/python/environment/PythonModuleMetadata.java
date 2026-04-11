// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.environment;

import java.io.Serializable;

/**
 * Temporary metadata used to create the header section of a Python file
 */
public class PythonModuleMetadata implements Serializable, Comparable<PythonModuleMetadata> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.python.environment.PythonModuleMetadata");

  public static final hydra.core.Name NAMESPACES = new hydra.core.Name("namespaces");

  public static final hydra.core.Name TYPE_VARIABLES = new hydra.core.Name("typeVariables");

  public static final hydra.core.Name USES_ANNOTATED = new hydra.core.Name("usesAnnotated");

  public static final hydra.core.Name USES_CALLABLE = new hydra.core.Name("usesCallable");

  public static final hydra.core.Name USES_CAST = new hydra.core.Name("usesCast");

  public static final hydra.core.Name USES_LRU_CACHE = new hydra.core.Name("usesLruCache");

  public static final hydra.core.Name USES_TYPE_ALIAS = new hydra.core.Name("usesTypeAlias");

  public static final hydra.core.Name USES_DATACLASS = new hydra.core.Name("usesDataclass");

  public static final hydra.core.Name USES_DECIMAL = new hydra.core.Name("usesDecimal");

  public static final hydra.core.Name USES_EITHER = new hydra.core.Name("usesEither");

  public static final hydra.core.Name USES_ENUM = new hydra.core.Name("usesEnum");

  public static final hydra.core.Name USES_FROZEN_DICT = new hydra.core.Name("usesFrozenDict");

  public static final hydra.core.Name USES_FROZEN_LIST = new hydra.core.Name("usesFrozenList");

  public static final hydra.core.Name USES_GENERIC = new hydra.core.Name("usesGeneric");

  public static final hydra.core.Name USES_JUST = new hydra.core.Name("usesJust");

  public static final hydra.core.Name USES_LEFT = new hydra.core.Name("usesLeft");

  public static final hydra.core.Name USES_MAYBE = new hydra.core.Name("usesMaybe");

  public static final hydra.core.Name USES_NAME = new hydra.core.Name("usesName");

  public static final hydra.core.Name USES_NODE = new hydra.core.Name("usesNode");

  public static final hydra.core.Name USES_NOTHING = new hydra.core.Name("usesNothing");

  public static final hydra.core.Name USES_RIGHT = new hydra.core.Name("usesRight");

  public static final hydra.core.Name USES_TYPE_VAR = new hydra.core.Name("usesTypeVar");

  /**
   * Namespace mapping for imports
   */
  public final hydra.packaging.Namespaces<hydra.ext.python.syntax.DottedName> namespaces;

  /**
   * Type variables used in the module
   */
  public final java.util.Set<hydra.core.Name> typeVariables;

  public final Boolean usesAnnotated;

  public final Boolean usesCallable;

  public final Boolean usesCast;

  public final Boolean usesLruCache;

  public final Boolean usesTypeAlias;

  public final Boolean usesDataclass;

  public final Boolean usesDecimal;

  public final Boolean usesEither;

  public final Boolean usesEnum;

  public final Boolean usesFrozenDict;

  public final Boolean usesFrozenList;

  public final Boolean usesGeneric;

  public final Boolean usesJust;

  public final Boolean usesLeft;

  public final Boolean usesMaybe;

  public final Boolean usesName;

  public final Boolean usesNode;

  public final Boolean usesNothing;

  public final Boolean usesRight;

  public final Boolean usesTypeVar;

  public PythonModuleMetadata (hydra.packaging.Namespaces<hydra.ext.python.syntax.DottedName> namespaces, java.util.Set<hydra.core.Name> typeVariables, Boolean usesAnnotated, Boolean usesCallable, Boolean usesCast, Boolean usesLruCache, Boolean usesTypeAlias, Boolean usesDataclass, Boolean usesDecimal, Boolean usesEither, Boolean usesEnum, Boolean usesFrozenDict, Boolean usesFrozenList, Boolean usesGeneric, Boolean usesJust, Boolean usesLeft, Boolean usesMaybe, Boolean usesName, Boolean usesNode, Boolean usesNothing, Boolean usesRight, Boolean usesTypeVar) {
    this.namespaces = namespaces;
    this.typeVariables = typeVariables;
    this.usesAnnotated = usesAnnotated;
    this.usesCallable = usesCallable;
    this.usesCast = usesCast;
    this.usesLruCache = usesLruCache;
    this.usesTypeAlias = usesTypeAlias;
    this.usesDataclass = usesDataclass;
    this.usesDecimal = usesDecimal;
    this.usesEither = usesEither;
    this.usesEnum = usesEnum;
    this.usesFrozenDict = usesFrozenDict;
    this.usesFrozenList = usesFrozenList;
    this.usesGeneric = usesGeneric;
    this.usesJust = usesJust;
    this.usesLeft = usesLeft;
    this.usesMaybe = usesMaybe;
    this.usesName = usesName;
    this.usesNode = usesNode;
    this.usesNothing = usesNothing;
    this.usesRight = usesRight;
    this.usesTypeVar = usesTypeVar;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof PythonModuleMetadata)) {
      return false;
    }
    PythonModuleMetadata o = (PythonModuleMetadata) other;
    return java.util.Objects.equals(
      this.namespaces,
      o.namespaces) && java.util.Objects.equals(
      this.typeVariables,
      o.typeVariables) && java.util.Objects.equals(
      this.usesAnnotated,
      o.usesAnnotated) && java.util.Objects.equals(
      this.usesCallable,
      o.usesCallable) && java.util.Objects.equals(
      this.usesCast,
      o.usesCast) && java.util.Objects.equals(
      this.usesLruCache,
      o.usesLruCache) && java.util.Objects.equals(
      this.usesTypeAlias,
      o.usesTypeAlias) && java.util.Objects.equals(
      this.usesDataclass,
      o.usesDataclass) && java.util.Objects.equals(
      this.usesDecimal,
      o.usesDecimal) && java.util.Objects.equals(
      this.usesEither,
      o.usesEither) && java.util.Objects.equals(
      this.usesEnum,
      o.usesEnum) && java.util.Objects.equals(
      this.usesFrozenDict,
      o.usesFrozenDict) && java.util.Objects.equals(
      this.usesFrozenList,
      o.usesFrozenList) && java.util.Objects.equals(
      this.usesGeneric,
      o.usesGeneric) && java.util.Objects.equals(
      this.usesJust,
      o.usesJust) && java.util.Objects.equals(
      this.usesLeft,
      o.usesLeft) && java.util.Objects.equals(
      this.usesMaybe,
      o.usesMaybe) && java.util.Objects.equals(
      this.usesName,
      o.usesName) && java.util.Objects.equals(
      this.usesNode,
      o.usesNode) && java.util.Objects.equals(
      this.usesNothing,
      o.usesNothing) && java.util.Objects.equals(
      this.usesRight,
      o.usesRight) && java.util.Objects.equals(
      this.usesTypeVar,
      o.usesTypeVar);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(namespaces) + 3 * java.util.Objects.hashCode(typeVariables) + 5 * java.util.Objects.hashCode(usesAnnotated) + 7 * java.util.Objects.hashCode(usesCallable) + 11 * java.util.Objects.hashCode(usesCast) + 13 * java.util.Objects.hashCode(usesLruCache) + 17 * java.util.Objects.hashCode(usesTypeAlias) + 19 * java.util.Objects.hashCode(usesDataclass) + 23 * java.util.Objects.hashCode(usesDecimal) + 29 * java.util.Objects.hashCode(usesEither) + 31 * java.util.Objects.hashCode(usesEnum) + 37 * java.util.Objects.hashCode(usesFrozenDict) + 41 * java.util.Objects.hashCode(usesFrozenList) + 43 * java.util.Objects.hashCode(usesGeneric) + 47 * java.util.Objects.hashCode(usesJust) + 53 * java.util.Objects.hashCode(usesLeft) + 59 * java.util.Objects.hashCode(usesMaybe) + 61 * java.util.Objects.hashCode(usesName) + 67 * java.util.Objects.hashCode(usesNode) + 71 * java.util.Objects.hashCode(usesNothing);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(PythonModuleMetadata other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      namespaces,
      other.namespaces);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      typeVariables,
      other.typeVariables);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      usesAnnotated,
      other.usesAnnotated);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      usesCallable,
      other.usesCallable);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      usesCast,
      other.usesCast);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      usesLruCache,
      other.usesLruCache);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      usesTypeAlias,
      other.usesTypeAlias);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      usesDataclass,
      other.usesDataclass);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      usesDecimal,
      other.usesDecimal);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      usesEither,
      other.usesEither);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      usesEnum,
      other.usesEnum);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      usesFrozenDict,
      other.usesFrozenDict);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      usesFrozenList,
      other.usesFrozenList);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      usesGeneric,
      other.usesGeneric);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      usesJust,
      other.usesJust);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      usesLeft,
      other.usesLeft);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      usesMaybe,
      other.usesMaybe);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      usesName,
      other.usesName);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      usesNode,
      other.usesNode);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      usesNothing,
      other.usesNothing);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      usesRight,
      other.usesRight);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      usesTypeVar,
      other.usesTypeVar);
  }

  public PythonModuleMetadata withNamespaces(hydra.packaging.Namespaces<hydra.ext.python.syntax.DottedName> namespaces) {
    return new PythonModuleMetadata(namespaces, typeVariables, usesAnnotated, usesCallable, usesCast, usesLruCache, usesTypeAlias, usesDataclass, usesDecimal, usesEither, usesEnum, usesFrozenDict, usesFrozenList, usesGeneric, usesJust, usesLeft, usesMaybe, usesName, usesNode, usesNothing, usesRight, usesTypeVar);
  }

  public PythonModuleMetadata withTypeVariables(java.util.Set<hydra.core.Name> typeVariables) {
    return new PythonModuleMetadata(namespaces, typeVariables, usesAnnotated, usesCallable, usesCast, usesLruCache, usesTypeAlias, usesDataclass, usesDecimal, usesEither, usesEnum, usesFrozenDict, usesFrozenList, usesGeneric, usesJust, usesLeft, usesMaybe, usesName, usesNode, usesNothing, usesRight, usesTypeVar);
  }

  public PythonModuleMetadata withUsesAnnotated(Boolean usesAnnotated) {
    return new PythonModuleMetadata(namespaces, typeVariables, usesAnnotated, usesCallable, usesCast, usesLruCache, usesTypeAlias, usesDataclass, usesDecimal, usesEither, usesEnum, usesFrozenDict, usesFrozenList, usesGeneric, usesJust, usesLeft, usesMaybe, usesName, usesNode, usesNothing, usesRight, usesTypeVar);
  }

  public PythonModuleMetadata withUsesCallable(Boolean usesCallable) {
    return new PythonModuleMetadata(namespaces, typeVariables, usesAnnotated, usesCallable, usesCast, usesLruCache, usesTypeAlias, usesDataclass, usesDecimal, usesEither, usesEnum, usesFrozenDict, usesFrozenList, usesGeneric, usesJust, usesLeft, usesMaybe, usesName, usesNode, usesNothing, usesRight, usesTypeVar);
  }

  public PythonModuleMetadata withUsesCast(Boolean usesCast) {
    return new PythonModuleMetadata(namespaces, typeVariables, usesAnnotated, usesCallable, usesCast, usesLruCache, usesTypeAlias, usesDataclass, usesDecimal, usesEither, usesEnum, usesFrozenDict, usesFrozenList, usesGeneric, usesJust, usesLeft, usesMaybe, usesName, usesNode, usesNothing, usesRight, usesTypeVar);
  }

  public PythonModuleMetadata withUsesLruCache(Boolean usesLruCache) {
    return new PythonModuleMetadata(namespaces, typeVariables, usesAnnotated, usesCallable, usesCast, usesLruCache, usesTypeAlias, usesDataclass, usesDecimal, usesEither, usesEnum, usesFrozenDict, usesFrozenList, usesGeneric, usesJust, usesLeft, usesMaybe, usesName, usesNode, usesNothing, usesRight, usesTypeVar);
  }

  public PythonModuleMetadata withUsesTypeAlias(Boolean usesTypeAlias) {
    return new PythonModuleMetadata(namespaces, typeVariables, usesAnnotated, usesCallable, usesCast, usesLruCache, usesTypeAlias, usesDataclass, usesDecimal, usesEither, usesEnum, usesFrozenDict, usesFrozenList, usesGeneric, usesJust, usesLeft, usesMaybe, usesName, usesNode, usesNothing, usesRight, usesTypeVar);
  }

  public PythonModuleMetadata withUsesDataclass(Boolean usesDataclass) {
    return new PythonModuleMetadata(namespaces, typeVariables, usesAnnotated, usesCallable, usesCast, usesLruCache, usesTypeAlias, usesDataclass, usesDecimal, usesEither, usesEnum, usesFrozenDict, usesFrozenList, usesGeneric, usesJust, usesLeft, usesMaybe, usesName, usesNode, usesNothing, usesRight, usesTypeVar);
  }

  public PythonModuleMetadata withUsesDecimal(Boolean usesDecimal) {
    return new PythonModuleMetadata(namespaces, typeVariables, usesAnnotated, usesCallable, usesCast, usesLruCache, usesTypeAlias, usesDataclass, usesDecimal, usesEither, usesEnum, usesFrozenDict, usesFrozenList, usesGeneric, usesJust, usesLeft, usesMaybe, usesName, usesNode, usesNothing, usesRight, usesTypeVar);
  }

  public PythonModuleMetadata withUsesEither(Boolean usesEither) {
    return new PythonModuleMetadata(namespaces, typeVariables, usesAnnotated, usesCallable, usesCast, usesLruCache, usesTypeAlias, usesDataclass, usesDecimal, usesEither, usesEnum, usesFrozenDict, usesFrozenList, usesGeneric, usesJust, usesLeft, usesMaybe, usesName, usesNode, usesNothing, usesRight, usesTypeVar);
  }

  public PythonModuleMetadata withUsesEnum(Boolean usesEnum) {
    return new PythonModuleMetadata(namespaces, typeVariables, usesAnnotated, usesCallable, usesCast, usesLruCache, usesTypeAlias, usesDataclass, usesDecimal, usesEither, usesEnum, usesFrozenDict, usesFrozenList, usesGeneric, usesJust, usesLeft, usesMaybe, usesName, usesNode, usesNothing, usesRight, usesTypeVar);
  }

  public PythonModuleMetadata withUsesFrozenDict(Boolean usesFrozenDict) {
    return new PythonModuleMetadata(namespaces, typeVariables, usesAnnotated, usesCallable, usesCast, usesLruCache, usesTypeAlias, usesDataclass, usesDecimal, usesEither, usesEnum, usesFrozenDict, usesFrozenList, usesGeneric, usesJust, usesLeft, usesMaybe, usesName, usesNode, usesNothing, usesRight, usesTypeVar);
  }

  public PythonModuleMetadata withUsesFrozenList(Boolean usesFrozenList) {
    return new PythonModuleMetadata(namespaces, typeVariables, usesAnnotated, usesCallable, usesCast, usesLruCache, usesTypeAlias, usesDataclass, usesDecimal, usesEither, usesEnum, usesFrozenDict, usesFrozenList, usesGeneric, usesJust, usesLeft, usesMaybe, usesName, usesNode, usesNothing, usesRight, usesTypeVar);
  }

  public PythonModuleMetadata withUsesGeneric(Boolean usesGeneric) {
    return new PythonModuleMetadata(namespaces, typeVariables, usesAnnotated, usesCallable, usesCast, usesLruCache, usesTypeAlias, usesDataclass, usesDecimal, usesEither, usesEnum, usesFrozenDict, usesFrozenList, usesGeneric, usesJust, usesLeft, usesMaybe, usesName, usesNode, usesNothing, usesRight, usesTypeVar);
  }

  public PythonModuleMetadata withUsesJust(Boolean usesJust) {
    return new PythonModuleMetadata(namespaces, typeVariables, usesAnnotated, usesCallable, usesCast, usesLruCache, usesTypeAlias, usesDataclass, usesDecimal, usesEither, usesEnum, usesFrozenDict, usesFrozenList, usesGeneric, usesJust, usesLeft, usesMaybe, usesName, usesNode, usesNothing, usesRight, usesTypeVar);
  }

  public PythonModuleMetadata withUsesLeft(Boolean usesLeft) {
    return new PythonModuleMetadata(namespaces, typeVariables, usesAnnotated, usesCallable, usesCast, usesLruCache, usesTypeAlias, usesDataclass, usesDecimal, usesEither, usesEnum, usesFrozenDict, usesFrozenList, usesGeneric, usesJust, usesLeft, usesMaybe, usesName, usesNode, usesNothing, usesRight, usesTypeVar);
  }

  public PythonModuleMetadata withUsesMaybe(Boolean usesMaybe) {
    return new PythonModuleMetadata(namespaces, typeVariables, usesAnnotated, usesCallable, usesCast, usesLruCache, usesTypeAlias, usesDataclass, usesDecimal, usesEither, usesEnum, usesFrozenDict, usesFrozenList, usesGeneric, usesJust, usesLeft, usesMaybe, usesName, usesNode, usesNothing, usesRight, usesTypeVar);
  }

  public PythonModuleMetadata withUsesName(Boolean usesName) {
    return new PythonModuleMetadata(namespaces, typeVariables, usesAnnotated, usesCallable, usesCast, usesLruCache, usesTypeAlias, usesDataclass, usesDecimal, usesEither, usesEnum, usesFrozenDict, usesFrozenList, usesGeneric, usesJust, usesLeft, usesMaybe, usesName, usesNode, usesNothing, usesRight, usesTypeVar);
  }

  public PythonModuleMetadata withUsesNode(Boolean usesNode) {
    return new PythonModuleMetadata(namespaces, typeVariables, usesAnnotated, usesCallable, usesCast, usesLruCache, usesTypeAlias, usesDataclass, usesDecimal, usesEither, usesEnum, usesFrozenDict, usesFrozenList, usesGeneric, usesJust, usesLeft, usesMaybe, usesName, usesNode, usesNothing, usesRight, usesTypeVar);
  }

  public PythonModuleMetadata withUsesNothing(Boolean usesNothing) {
    return new PythonModuleMetadata(namespaces, typeVariables, usesAnnotated, usesCallable, usesCast, usesLruCache, usesTypeAlias, usesDataclass, usesDecimal, usesEither, usesEnum, usesFrozenDict, usesFrozenList, usesGeneric, usesJust, usesLeft, usesMaybe, usesName, usesNode, usesNothing, usesRight, usesTypeVar);
  }

  public PythonModuleMetadata withUsesRight(Boolean usesRight) {
    return new PythonModuleMetadata(namespaces, typeVariables, usesAnnotated, usesCallable, usesCast, usesLruCache, usesTypeAlias, usesDataclass, usesDecimal, usesEither, usesEnum, usesFrozenDict, usesFrozenList, usesGeneric, usesJust, usesLeft, usesMaybe, usesName, usesNode, usesNothing, usesRight, usesTypeVar);
  }

  public PythonModuleMetadata withUsesTypeVar(Boolean usesTypeVar) {
    return new PythonModuleMetadata(namespaces, typeVariables, usesAnnotated, usesCallable, usesCast, usesLruCache, usesTypeAlias, usesDataclass, usesDecimal, usesEither, usesEnum, usesFrozenDict, usesFrozenList, usesGeneric, usesJust, usesLeft, usesMaybe, usesName, usesNode, usesNothing, usesRight, usesTypeVar);
  }
}
