// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class FunctionDefRaw implements Serializable, Comparable<FunctionDefRaw> {
  public static final hydra.core.Name TYPE_ = new hydra.core.Name("hydra.ext.python.syntax.FunctionDefRaw");

  public static final hydra.core.Name ASYNC = new hydra.core.Name("async");

  public static final hydra.core.Name NAME = new hydra.core.Name("name");

  public static final hydra.core.Name TYPE_PARAMS = new hydra.core.Name("typeParams");

  public static final hydra.core.Name PARAMS = new hydra.core.Name("params");

  public static final hydra.core.Name RETURN_TYPE = new hydra.core.Name("returnType");

  public static final hydra.core.Name FUNC_TYPE_COMMENT = new hydra.core.Name("funcTypeComment");

  public static final hydra.core.Name BLOCK = new hydra.core.Name("block");

  public final Boolean async;

  public final hydra.ext.python.syntax.Name name;

  public final java.util.List<hydra.ext.python.syntax.TypeParameter> typeParams;

  public final hydra.util.Maybe<hydra.ext.python.syntax.Parameters> params;

  public final hydra.util.Maybe<hydra.ext.python.syntax.Expression> returnType;

  public final hydra.util.Maybe<hydra.ext.python.syntax.FuncTypeComment> funcTypeComment;

  public final hydra.ext.python.syntax.Block block;

  public FunctionDefRaw (Boolean async, hydra.ext.python.syntax.Name name, java.util.List<hydra.ext.python.syntax.TypeParameter> typeParams, hydra.util.Maybe<hydra.ext.python.syntax.Parameters> params, hydra.util.Maybe<hydra.ext.python.syntax.Expression> returnType, hydra.util.Maybe<hydra.ext.python.syntax.FuncTypeComment> funcTypeComment, hydra.ext.python.syntax.Block block) {
    this.async = async;
    this.name = name;
    this.typeParams = typeParams;
    this.params = params;
    this.returnType = returnType;
    this.funcTypeComment = funcTypeComment;
    this.block = block;
  }

  @Override
  public boolean equals(Object other) {
    if (!(other instanceof FunctionDefRaw)) {
      return false;
    }
    FunctionDefRaw o = (FunctionDefRaw) other;
    return java.util.Objects.equals(
      this.async,
      o.async) && java.util.Objects.equals(
      this.name,
      o.name) && java.util.Objects.equals(
      this.typeParams,
      o.typeParams) && java.util.Objects.equals(
      this.params,
      o.params) && java.util.Objects.equals(
      this.returnType,
      o.returnType) && java.util.Objects.equals(
      this.funcTypeComment,
      o.funcTypeComment) && java.util.Objects.equals(
      this.block,
      o.block);
  }

  @Override
  public int hashCode() {
    return 2 * java.util.Objects.hashCode(async) + 3 * java.util.Objects.hashCode(name) + 5 * java.util.Objects.hashCode(typeParams) + 7 * java.util.Objects.hashCode(params) + 11 * java.util.Objects.hashCode(returnType) + 13 * java.util.Objects.hashCode(funcTypeComment) + 17 * java.util.Objects.hashCode(block);
  }

  @Override
  @SuppressWarnings("unchecked")
  public int compareTo(FunctionDefRaw other) {
    int cmp = 0;
    cmp = hydra.util.Comparing.compare(
      async,
      other.async);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      name,
      other.name);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      typeParams,
      other.typeParams);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      params,
      other.params);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      returnType,
      other.returnType);
    if (cmp != 0) {
      return cmp;
    }
    cmp = hydra.util.Comparing.compare(
      funcTypeComment,
      other.funcTypeComment);
    if (cmp != 0) {
      return cmp;
    }
    return hydra.util.Comparing.compare(
      block,
      other.block);
  }

  public FunctionDefRaw withAsync(Boolean async) {
    return new FunctionDefRaw(async, name, typeParams, params, returnType, funcTypeComment, block);
  }

  public FunctionDefRaw withName(hydra.ext.python.syntax.Name name) {
    return new FunctionDefRaw(async, name, typeParams, params, returnType, funcTypeComment, block);
  }

  public FunctionDefRaw withTypeParams(java.util.List<hydra.ext.python.syntax.TypeParameter> typeParams) {
    return new FunctionDefRaw(async, name, typeParams, params, returnType, funcTypeComment, block);
  }

  public FunctionDefRaw withParams(hydra.util.Maybe<hydra.ext.python.syntax.Parameters> params) {
    return new FunctionDefRaw(async, name, typeParams, params, returnType, funcTypeComment, block);
  }

  public FunctionDefRaw withReturnType(hydra.util.Maybe<hydra.ext.python.syntax.Expression> returnType) {
    return new FunctionDefRaw(async, name, typeParams, params, returnType, funcTypeComment, block);
  }

  public FunctionDefRaw withFuncTypeComment(hydra.util.Maybe<hydra.ext.python.syntax.FuncTypeComment> funcTypeComment) {
    return new FunctionDefRaw(async, name, typeParams, params, returnType, funcTypeComment, block);
  }

  public FunctionDefRaw withBlock(hydra.ext.python.syntax.Block block) {
    return new FunctionDefRaw(async, name, typeParams, params, returnType, funcTypeComment, block);
  }
}
