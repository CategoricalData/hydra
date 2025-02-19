// Note: this is an automatically generated file. Do not edit.

package hydra.ext.python.syntax;

import java.io.Serializable;

public class FunctionDefRaw implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.python.syntax.FunctionDefRaw");
  
  public static final hydra.core.Name FIELD_NAME_ASYNC = new hydra.core.Name("async");
  
  public static final hydra.core.Name FIELD_NAME_NAME = new hydra.core.Name("name");
  
  public static final hydra.core.Name FIELD_NAME_TYPE_PARAMS = new hydra.core.Name("typeParams");
  
  public static final hydra.core.Name FIELD_NAME_PARAMS = new hydra.core.Name("params");
  
  public static final hydra.core.Name FIELD_NAME_RETURN_TYPE = new hydra.core.Name("returnType");
  
  public static final hydra.core.Name FIELD_NAME_FUNC_TYPE_COMMENT = new hydra.core.Name("funcTypeComment");
  
  public static final hydra.core.Name FIELD_NAME_BLOCK = new hydra.core.Name("block");
  
  public final Boolean async;
  
  public final hydra.ext.python.syntax.Name name;
  
  public final java.util.List<hydra.ext.python.syntax.TypeParameter> typeParams;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.Parameters> params;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.Expression> returnType;
  
  public final hydra.util.Opt<hydra.ext.python.syntax.FuncTypeComment> funcTypeComment;
  
  public final hydra.ext.python.syntax.Block block;
  
  public FunctionDefRaw (Boolean async, hydra.ext.python.syntax.Name name, java.util.List<hydra.ext.python.syntax.TypeParameter> typeParams, hydra.util.Opt<hydra.ext.python.syntax.Parameters> params, hydra.util.Opt<hydra.ext.python.syntax.Expression> returnType, hydra.util.Opt<hydra.ext.python.syntax.FuncTypeComment> funcTypeComment, hydra.ext.python.syntax.Block block) {
    java.util.Objects.requireNonNull((async));
    java.util.Objects.requireNonNull((name));
    java.util.Objects.requireNonNull((typeParams));
    java.util.Objects.requireNonNull((params));
    java.util.Objects.requireNonNull((returnType));
    java.util.Objects.requireNonNull((funcTypeComment));
    java.util.Objects.requireNonNull((block));
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
    FunctionDefRaw o = (FunctionDefRaw) (other);
    return async.equals(o.async) && name.equals(o.name) && typeParams.equals(o.typeParams) && params.equals(o.params) && returnType.equals(o.returnType) && funcTypeComment.equals(o.funcTypeComment) && block.equals(o.block);
  }
  
  @Override
  public int hashCode() {
    return 2 * async.hashCode() + 3 * name.hashCode() + 5 * typeParams.hashCode() + 7 * params.hashCode() + 11 * returnType.hashCode() + 13 * funcTypeComment.hashCode() + 17 * block.hashCode();
  }
  
  public FunctionDefRaw withAsync(Boolean async) {
    java.util.Objects.requireNonNull((async));
    return new FunctionDefRaw(async, name, typeParams, params, returnType, funcTypeComment, block);
  }
  
  public FunctionDefRaw withName(hydra.ext.python.syntax.Name name) {
    java.util.Objects.requireNonNull((name));
    return new FunctionDefRaw(async, name, typeParams, params, returnType, funcTypeComment, block);
  }
  
  public FunctionDefRaw withTypeParams(java.util.List<hydra.ext.python.syntax.TypeParameter> typeParams) {
    java.util.Objects.requireNonNull((typeParams));
    return new FunctionDefRaw(async, name, typeParams, params, returnType, funcTypeComment, block);
  }
  
  public FunctionDefRaw withParams(hydra.util.Opt<hydra.ext.python.syntax.Parameters> params) {
    java.util.Objects.requireNonNull((params));
    return new FunctionDefRaw(async, name, typeParams, params, returnType, funcTypeComment, block);
  }
  
  public FunctionDefRaw withReturnType(hydra.util.Opt<hydra.ext.python.syntax.Expression> returnType) {
    java.util.Objects.requireNonNull((returnType));
    return new FunctionDefRaw(async, name, typeParams, params, returnType, funcTypeComment, block);
  }
  
  public FunctionDefRaw withFuncTypeComment(hydra.util.Opt<hydra.ext.python.syntax.FuncTypeComment> funcTypeComment) {
    java.util.Objects.requireNonNull((funcTypeComment));
    return new FunctionDefRaw(async, name, typeParams, params, returnType, funcTypeComment, block);
  }
  
  public FunctionDefRaw withBlock(hydra.ext.python.syntax.Block block) {
    java.util.Objects.requireNonNull((block));
    return new FunctionDefRaw(async, name, typeParams, params, returnType, funcTypeComment, block);
  }
}