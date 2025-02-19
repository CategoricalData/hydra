// Note: this is an automatically generated file. Do not edit.

package hydra.ext.fr.inria.coq.syntax;

import java.io.Serializable;

public abstract class TypeCastOperator implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra.ext.fr.inria.coq.syntax.TypeCastOperator");
  
  public static final hydra.core.Name FIELD_NAME_NORMAL = new hydra.core.Name("normal");
  
  public static final hydra.core.Name FIELD_NAME_VM_COMPUTE = new hydra.core.Name("vmCompute");
  
  public static final hydra.core.Name FIELD_NAME_NATIVE_COMPUTE = new hydra.core.Name("nativeCompute");
  
  private TypeCastOperator () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(Normal instance) ;
    
    R visit(VmCompute instance) ;
    
    R visit(NativeCompute instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TypeCastOperator instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(Normal instance) {
      return otherwise((instance));
    }
    
    default R visit(VmCompute instance) {
      return otherwise((instance));
    }
    
    default R visit(NativeCompute instance) {
      return otherwise((instance));
    }
  }
  
  /**
   * The expression term10 : type is a type cast expression. It enforces the type of term10 to be type.
   */
  public static final class Normal extends hydra.ext.fr.inria.coq.syntax.TypeCastOperator implements Serializable {
    public Normal () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Normal)) {
        return false;
      }
      Normal o = (Normal) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * term10 &lt;: type specifies that the virtual machine will be used to type check that term10 has type type (see vm_compute).
   */
  public static final class VmCompute extends hydra.ext.fr.inria.coq.syntax.TypeCastOperator implements Serializable {
    public VmCompute () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof VmCompute)) {
        return false;
      }
      VmCompute o = (VmCompute) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * term10 &lt;&lt;: type specifies that compilation to OCaml will be used to type check that term10 has type type (see native_compute).
   */
  public static final class NativeCompute extends hydra.ext.fr.inria.coq.syntax.TypeCastOperator implements Serializable {
    public NativeCompute () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NativeCompute)) {
        return false;
      }
      NativeCompute o = (NativeCompute) (other);
      return true;
    }
    
    @Override
    public int hashCode() {
      return 0;
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}