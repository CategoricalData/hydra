package hydra.ext.coq.syntax;

public abstract class TypeCastOperator {
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
  public static final class Normal extends TypeCastOperator {
    /**
     * The expression term10 : type is a type cast expression. It enforces the type of term10 to be type.
     */
    public final java.lang.Void value;
    
    public Normal (java.lang.Void value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof Normal)) {
        return false;
      }
      Normal o = (Normal) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * term10 <: type specifies that the virtual machine will be used to type check that term10 has type type (see vm_compute).
   */
  public static final class VmCompute extends TypeCastOperator {
    /**
     * term10 <: type specifies that the virtual machine will be used to type check that term10 has type type (see vm_compute).
     */
    public final java.lang.Void value;
    
    public VmCompute (java.lang.Void value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof VmCompute)) {
        return false;
      }
      VmCompute o = (VmCompute) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
  
  /**
   * term10 <<: type specifies that compilation to OCaml will be used to type check that term10 has type type (see native_compute).
   */
  public static final class NativeCompute extends TypeCastOperator {
    /**
     * term10 <<: type specifies that compilation to OCaml will be used to type check that term10 has type type (see native_compute).
     */
    public final java.lang.Void value;
    
    public NativeCompute (java.lang.Void value) {
      this.value = value;
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof NativeCompute)) {
        return false;
      }
      NativeCompute o = (NativeCompute) (other);
      return value.equals(o.value);
    }
    
    @Override
    public int hashCode() {
      return 2 * value.hashCode();
    }
    
    @Override
    public <R> R accept(Visitor<R> visitor) {
      return visitor.visit(this);
    }
  }
}