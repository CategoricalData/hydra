package hydra.encode.errors

import hydra.core.*

import hydra.errors.*

def decodingError(x: hydra.errors.DecodingError): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.errors.DecodingError", hydra.core.Term.literal(hydra.core.Literal.string(x))))

def error(v1: hydra.errors.Error): hydra.core.Term =
  v1 match
  case hydra.errors.Error.checking(v_Error_checking_y) => hydra.core.Term.union(hydra.core.Injection("hydra.errors.Error",
     hydra.core.Field("checking", hydra.encode.error.checking.checkingError(v_Error_checking_y))))
  case hydra.errors.Error.decoding(v_Error_decoding_y) => hydra.core.Term.union(hydra.core.Injection("hydra.errors.Error",
     hydra.core.Field("decoding", hydra.encode.errors.decodingError(v_Error_decoding_y))))
  case hydra.errors.Error.duplicateBinding(v_Error_duplicateBinding_y) => hydra.core.Term.union(hydra.core.Injection("hydra.errors.Error",
     hydra.core.Field("duplicateBinding", hydra.encode.error.core.duplicateBindingError(v_Error_duplicateBinding_y))))
  case hydra.errors.Error.duplicateField(v_Error_duplicateField_y) => hydra.core.Term.union(hydra.core.Injection("hydra.errors.Error",
     hydra.core.Field("duplicateField", hydra.encode.error.core.duplicateFieldError(v_Error_duplicateField_y))))
  case hydra.errors.Error.other(v_Error_other_y) => hydra.core.Term.union(hydra.core.Injection("hydra.errors.Error",
     hydra.core.Field("other", hydra.encode.errors.otherError(v_Error_other_y))))
  case hydra.errors.Error.undefinedField(v_Error_undefinedField_y) => hydra.core.Term.union(hydra.core.Injection("hydra.errors.Error",
     hydra.core.Field("undefinedField", hydra.encode.error.core.undefinedFieldError(v_Error_undefinedField_y))))
  case hydra.errors.Error.undefinedTerm(v_Error_undefinedTerm_y) => hydra.core.Term.union(hydra.core.Injection("hydra.errors.Error",
     hydra.core.Field("undefinedTerm", hydra.encode.error.core.undefinedTermError(v_Error_undefinedTerm_y))))
  case hydra.errors.Error.undefinedType(v_Error_undefinedType_y) => hydra.core.Term.union(hydra.core.Injection("hydra.errors.Error",
     hydra.core.Field("undefinedType", hydra.encode.error.core.undefinedTypeError(v_Error_undefinedType_y))))
  case hydra.errors.Error.unexpectedTermVariant(v_Error_unexpectedTermVariant_y) => hydra.core.Term.union(hydra.core.Injection("hydra.errors.Error",
     hydra.core.Field("unexpectedTermVariant", hydra.encode.error.core.unexpectedTermVariantError(v_Error_unexpectedTermVariant_y))))
  case hydra.errors.Error.unexpectedTypeVariant(v_Error_unexpectedTypeVariant_y) => hydra.core.Term.union(hydra.core.Injection("hydra.errors.Error",
     hydra.core.Field("unexpectedTypeVariant", hydra.encode.error.core.unexpectedTypeVariantError(v_Error_unexpectedTypeVariant_y))))
  case hydra.errors.Error.unification(v_Error_unification_y) => hydra.core.Term.union(hydra.core.Injection("hydra.errors.Error",
     hydra.core.Field("unification", hydra.encode.errors.unificationError(v_Error_unification_y))))

def otherError(x: hydra.errors.OtherError): hydra.core.Term =
  hydra.core.Term.wrap(hydra.core.WrappedTerm("hydra.errors.OtherError", hydra.core.Term.literal(hydra.core.Literal.string(x))))

def unificationError(x: hydra.errors.UnificationError): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record("hydra.errors.UnificationError", Seq(hydra.core.Field("leftType",
     hydra.encode.core.`type`(x.leftType)), hydra.core.Field("rightType", hydra.encode.core.`type`(x.rightType)),
     hydra.core.Field("message", hydra.core.Term.literal(hydra.core.Literal.string(x.message))))))
