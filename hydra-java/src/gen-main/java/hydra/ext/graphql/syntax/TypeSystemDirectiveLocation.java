// Note: this is an automatically generated file. Do not edit.

package hydra.ext.graphql.syntax;

import java.io.Serializable;

public abstract class TypeSystemDirectiveLocation implements Serializable {
  public static final hydra.core.Name TYPE_NAME = new hydra.core.Name("hydra/ext/graphql/syntax.TypeSystemDirectiveLocation");
  
  public static final hydra.core.Name FIELD_NAME_S_C_H_E_M_A = new hydra.core.Name("sCHEMA");
  
  public static final hydra.core.Name FIELD_NAME_S_C_A_L_A_R = new hydra.core.Name("sCALAR");
  
  public static final hydra.core.Name FIELD_NAME_O_B_J_E_C_T = new hydra.core.Name("oBJECT");
  
  public static final hydra.core.Name FIELD_NAME_F_I_E_L_D_LOWBAR_D_E_F_I_N_I_T_I_O_N = new hydra.core.Name("fIELDLowbarDEFINITION");
  
  public static final hydra.core.Name FIELD_NAME_A_R_G_U_M_E_N_T_LOWBAR_D_E_F_I_N_I_T_I_O_N = new hydra.core.Name("aRGUMENTLowbarDEFINITION");
  
  public static final hydra.core.Name FIELD_NAME_I_N_T_E_R_F_A_C_E = new hydra.core.Name("iNTERFACE");
  
  public static final hydra.core.Name FIELD_NAME_U_N_I_O_N = new hydra.core.Name("uNION");
  
  public static final hydra.core.Name FIELD_NAME_E_N_U_M = new hydra.core.Name("eNUM");
  
  public static final hydra.core.Name FIELD_NAME_E_N_U_M_LOWBAR_V_A_L_U_E = new hydra.core.Name("eNUMLowbarVALUE");
  
  public static final hydra.core.Name FIELD_NAME_I_N_P_U_T_LOWBAR_O_B_J_E_C_T = new hydra.core.Name("iNPUTLowbarOBJECT");
  
  public static final hydra.core.Name FIELD_NAME_I_N_P_U_T_LOWBAR_F_I_E_L_D_LOWBAR_D_E_F_I_N_I_T_I_O_N = new hydra.core.Name("iNPUTLowbarFIELDLowbarDEFINITION");
  
  private TypeSystemDirectiveLocation () {
  
  }
  
  public abstract <R> R accept(Visitor<R> visitor) ;
  
  public interface Visitor<R> {
    R visit(SCHEMA instance) ;
    
    R visit(SCALAR instance) ;
    
    R visit(OBJECT instance) ;
    
    R visit(FIELDLowbarDEFINITION instance) ;
    
    R visit(ARGUMENTLowbarDEFINITION instance) ;
    
    R visit(INTERFACE instance) ;
    
    R visit(UNION instance) ;
    
    R visit(ENUM instance) ;
    
    R visit(ENUMLowbarVALUE instance) ;
    
    R visit(INPUTLowbarOBJECT instance) ;
    
    R visit(INPUTLowbarFIELDLowbarDEFINITION instance) ;
  }
  
  public interface PartialVisitor<R> extends Visitor<R> {
    default R otherwise(TypeSystemDirectiveLocation instance) {
      throw new IllegalStateException("Non-exhaustive patterns when matching: " + (instance));
    }
    
    default R visit(SCHEMA instance) {
      return otherwise((instance));
    }
    
    default R visit(SCALAR instance) {
      return otherwise((instance));
    }
    
    default R visit(OBJECT instance) {
      return otherwise((instance));
    }
    
    default R visit(FIELDLowbarDEFINITION instance) {
      return otherwise((instance));
    }
    
    default R visit(ARGUMENTLowbarDEFINITION instance) {
      return otherwise((instance));
    }
    
    default R visit(INTERFACE instance) {
      return otherwise((instance));
    }
    
    default R visit(UNION instance) {
      return otherwise((instance));
    }
    
    default R visit(ENUM instance) {
      return otherwise((instance));
    }
    
    default R visit(ENUMLowbarVALUE instance) {
      return otherwise((instance));
    }
    
    default R visit(INPUTLowbarOBJECT instance) {
      return otherwise((instance));
    }
    
    default R visit(INPUTLowbarFIELDLowbarDEFINITION instance) {
      return otherwise((instance));
    }
  }
  
  public static final class SCHEMA extends hydra.ext.graphql.syntax.TypeSystemDirectiveLocation implements Serializable {
    public SCHEMA () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SCHEMA)) {
        return false;
      }
      SCHEMA o = (SCHEMA) (other);
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
  
  public static final class SCALAR extends hydra.ext.graphql.syntax.TypeSystemDirectiveLocation implements Serializable {
    public SCALAR () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof SCALAR)) {
        return false;
      }
      SCALAR o = (SCALAR) (other);
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
  
  public static final class OBJECT extends hydra.ext.graphql.syntax.TypeSystemDirectiveLocation implements Serializable {
    public OBJECT () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof OBJECT)) {
        return false;
      }
      OBJECT o = (OBJECT) (other);
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
  
  public static final class FIELDLowbarDEFINITION extends hydra.ext.graphql.syntax.TypeSystemDirectiveLocation implements Serializable {
    public FIELDLowbarDEFINITION () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof FIELDLowbarDEFINITION)) {
        return false;
      }
      FIELDLowbarDEFINITION o = (FIELDLowbarDEFINITION) (other);
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
  
  public static final class ARGUMENTLowbarDEFINITION extends hydra.ext.graphql.syntax.TypeSystemDirectiveLocation implements Serializable {
    public ARGUMENTLowbarDEFINITION () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ARGUMENTLowbarDEFINITION)) {
        return false;
      }
      ARGUMENTLowbarDEFINITION o = (ARGUMENTLowbarDEFINITION) (other);
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
  
  public static final class INTERFACE extends hydra.ext.graphql.syntax.TypeSystemDirectiveLocation implements Serializable {
    public INTERFACE () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof INTERFACE)) {
        return false;
      }
      INTERFACE o = (INTERFACE) (other);
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
  
  public static final class UNION extends hydra.ext.graphql.syntax.TypeSystemDirectiveLocation implements Serializable {
    public UNION () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof UNION)) {
        return false;
      }
      UNION o = (UNION) (other);
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
  
  public static final class ENUM extends hydra.ext.graphql.syntax.TypeSystemDirectiveLocation implements Serializable {
    public ENUM () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ENUM)) {
        return false;
      }
      ENUM o = (ENUM) (other);
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
  
  public static final class ENUMLowbarVALUE extends hydra.ext.graphql.syntax.TypeSystemDirectiveLocation implements Serializable {
    public ENUMLowbarVALUE () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof ENUMLowbarVALUE)) {
        return false;
      }
      ENUMLowbarVALUE o = (ENUMLowbarVALUE) (other);
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
  
  public static final class INPUTLowbarOBJECT extends hydra.ext.graphql.syntax.TypeSystemDirectiveLocation implements Serializable {
    public INPUTLowbarOBJECT () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof INPUTLowbarOBJECT)) {
        return false;
      }
      INPUTLowbarOBJECT o = (INPUTLowbarOBJECT) (other);
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
  
  public static final class INPUTLowbarFIELDLowbarDEFINITION extends hydra.ext.graphql.syntax.TypeSystemDirectiveLocation implements Serializable {
    public INPUTLowbarFIELDLowbarDEFINITION () {
    
    }
    
    @Override
    public boolean equals(Object other) {
      if (!(other instanceof INPUTLowbarFIELDLowbarDEFINITION)) {
        return false;
      }
      INPUTLowbarFIELDLowbarDEFINITION o = (INPUTLowbarFIELDLowbarDEFINITION) (other);
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
