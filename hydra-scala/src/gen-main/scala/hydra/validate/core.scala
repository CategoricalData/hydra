package hydra.validate.core

import hydra.core.*

import hydra.error.core.*

import hydra.graph.*

import hydra.paths.*

import hydra.lib.equality

import hydra.lib.lists

import hydra.lib.logic

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.pairs

import hydra.lib.sets

def checkDuplicateBindings(path: hydra.paths.SubtermPath)(bindings: Seq[hydra.core.Binding]): Option[hydra.error.core.InvalidTermError] =
  {
  lazy val names: Seq[hydra.core.Name] = hydra.lib.lists.map[hydra.core.Binding, hydra.core.Name]((x: hydra.core.Binding) => (x.name))(bindings)
  lazy val dup: Option[hydra.core.Name] = hydra.validate.core.findDuplicate(names)
  hydra.lib.maybes.map[hydra.core.Name, hydra.error.core.InvalidTermError]((name: hydra.core.Name) =>
    hydra.error.core.InvalidTermError.duplicateBinding(hydra.error.core.DuplicateBindingError(path, name)))(dup)
}

def checkDuplicateFieldTypes[T0](fields: Seq[hydra.core.FieldType])(mkError: (hydra.core.Name => Option[T0])): Option[T0] =
  {
  lazy val names: Seq[hydra.core.Name] = hydra.lib.lists.map[hydra.core.FieldType, hydra.core.Name]((x: hydra.core.FieldType) => (x.name))(fields)
  lazy val dup: Option[hydra.core.Name] = hydra.validate.core.findDuplicateFieldType(names)
  hydra.lib.maybes.cases[hydra.core.Name, Option[T0]](dup)(None)((name: hydra.core.Name) => mkError(name))
}

def checkDuplicateFields(path: hydra.paths.SubtermPath)(names: Seq[hydra.core.Name]): Option[hydra.error.core.InvalidTermError] =
  {
  lazy val dup: Option[hydra.core.Name] = hydra.validate.core.findDuplicate(names)
  hydra.lib.maybes.map[hydra.core.Name, hydra.error.core.InvalidTermError]((name: hydra.core.Name) =>
    hydra.error.core.InvalidTermError.duplicateField(hydra.error.core.DuplicateFieldError(path, name)))(dup)
}

def checkShadowing(path: hydra.paths.SubtermPath)(cx: hydra.graph.Graph)(names: Seq[hydra.core.Name]): Option[hydra.error.core.InvalidTermError] =
  {
  lazy val result: Option[hydra.error.core.InvalidTermError] = hydra.lib.lists.foldl[Option[hydra.error.core.InvalidTermError],
     hydra.core.Name]((acc: Option[hydra.error.core.InvalidTermError]) =>
    (name: hydra.core.Name) =>
    hydra.lib.maybes.cases[hydra.error.core.InvalidTermError, Option[hydra.error.core.InvalidTermError]](acc)(hydra.lib.logic.ifElse[Option[hydra.error.core.InvalidTermError]](hydra.lib.logic.or(hydra.lib.maybes.isJust[hydra.core.Term](hydra.lib.maps.lookup[hydra.core.Name,
       hydra.core.Term](name)(cx.boundTerms)))(hydra.lib.sets.member[hydra.core.Name](name)(cx.lambdaVariables)))(Some(hydra.error.core.InvalidTermError.termVariableShadowing(hydra.error.core.TermVariableShadowingError(path,
       name))))(None))((_x: hydra.error.core.InvalidTermError) => acc))(None)(names)
  result
}

def checkTerm(typed: Boolean)(path: hydra.paths.SubtermPath)(cx: hydra.graph.Graph)(term: hydra.core.Term): Option[hydra.error.core.InvalidTermError] =
  term match
  case hydra.core.Term.annotated(v_Term_annotated_ann) => {
    lazy val body: hydra.core.Term = (v_Term_annotated_ann.body)
    {
      lazy val annMap: Map[hydra.core.Name, hydra.core.Term] = (v_Term_annotated_ann.annotation)
      hydra.validate.core.firstError(Seq(hydra.lib.logic.ifElse[Option[hydra.error.core.InvalidTermError]](hydra.lib.maps.`null`[hydra.core.Name,
         hydra.core.Term](annMap))(Some(hydra.error.core.InvalidTermError.emptyTermAnnotation(hydra.error.core.EmptyTermAnnotationError(path))))(None),
         body match
        case hydra.core.Term.annotated(v_Term_annotated__) => Some(hydra.error.core.InvalidTermError.nestedTermAnnotation(hydra.error.core.NestedTermAnnotationError(path)))
        case _ => None))
    }
  }
  case hydra.core.Term.application(v_Term_application_app) => {
    lazy val fun: hydra.core.Term = (v_Term_application_app.function)
    {
      lazy val arg: hydra.core.Term = (v_Term_application_app.argument)
      hydra.validate.core.firstError(Seq(fun match
        case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
          case hydra.core.Function.primitive(v_Function_primitive_primName) => hydra.lib.logic.ifElse[Option[hydra.error.core.InvalidTermError]](hydra.lib.equality.equal[scala.Predef.String](v_Function_primitive_primName)("hydra.lib.logic.ifElse"))(arg match
            case hydra.core.Term.literal(v_Term_literal_lit) => v_Term_literal_lit match
              case hydra.core.Literal.boolean(v_Literal_boolean_boolVal) => Some(hydra.error.core.InvalidTermError.constantCondition(hydra.error.core.ConstantConditionError(path,
                 v_Literal_boolean_boolVal)))
              case _ => None
            case _ => None)(None)
          case _ => None
        case _ => None, fun match
        case hydra.core.Term.variable(v_Term_variable_funName) => arg match
          case hydra.core.Term.variable(v_Term_variable_argName) => hydra.lib.logic.ifElse[Option[hydra.error.core.InvalidTermError]](hydra.lib.equality.equal[hydra.core.Name](v_Term_variable_funName)(v_Term_variable_argName))(Some(hydra.error.core.InvalidTermError.selfApplication(hydra.error.core.SelfApplicationError(path,
             v_Term_variable_funName))))(None)
          case _ => None
        case _ => None, fun match
        case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
          case hydra.core.Function.lambda(v_Function_lambda_lam) => {
            lazy val param: hydra.core.Name = (v_Function_lambda_lam.parameter)
            {
              lazy val body: hydra.core.Term = (v_Function_lambda_lam.body)
              body match
                case hydra.core.Term.variable(v_Term_variable_bodyVar) => hydra.lib.logic.ifElse[Option[hydra.error.core.InvalidTermError]](hydra.lib.equality.equal[hydra.core.Name](param)(v_Term_variable_bodyVar))(Some(hydra.error.core.InvalidTermError.unnecessaryIdentityApplication(hydra.error.core.UnnecessaryIdentityApplicationError(path))))(None)
                case _ => None
            }
          }
          case _ => None
        case _ => None, fun match
        case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
          case hydra.core.Function.elimination(v_Function_elimination_elim) => v_Function_elimination_elim match
            case hydra.core.Elimination.wrap(v_Elimination_wrap_unwrapName) => arg match
              case hydra.core.Term.wrap(v_Term_wrap_wt) => {
                lazy val wrapName: hydra.core.Name = (v_Term_wrap_wt.typeName)
                hydra.lib.logic.ifElse[Option[hydra.error.core.InvalidTermError]](hydra.lib.equality.equal[hydra.core.Name](v_Elimination_wrap_unwrapName)(wrapName))(Some(hydra.error.core.InvalidTermError.redundantWrapUnwrap(hydra.error.core.RedundantWrapUnwrapError(path,
                   v_Elimination_wrap_unwrapName))))(None)
              }
              case _ => None
            case _ => None
          case _ => None
        case _ => None))
    }
  }
  case hydra.core.Term.record(v_Term_record_rec) => {
    lazy val tname: hydra.core.Name = (v_Term_record_rec.typeName)
    {
      lazy val flds: Seq[hydra.core.Field] = (v_Term_record_rec.fields)
      hydra.validate.core.firstError(Seq(hydra.lib.logic.ifElse[Option[hydra.error.core.InvalidTermError]](hydra.lib.equality.equal[scala.Predef.String](tname)(""))(Some(hydra.error.core.InvalidTermError.emptyTypeNameInTerm(hydra.error.core.EmptyTypeNameInTermError(path))))(None),
         hydra.validate.core.checkDuplicateFields(path)(hydra.lib.lists.map[hydra.core.Field, hydra.core.Name]((x: hydra.core.Field) => (x.name))(flds))))
    }
  }
  case hydra.core.Term.let(v_Term_let_lt) => {
    lazy val bindings: Seq[hydra.core.Binding] = (v_Term_let_lt.bindings)
    {
      lazy val names: Seq[hydra.core.Name] = hydra.lib.lists.map[hydra.core.Binding, hydra.core.Name]((x: hydra.core.Binding) => (x.name))(bindings)
      hydra.validate.core.firstError(Seq(hydra.lib.logic.ifElse[Option[hydra.error.core.InvalidTermError]](hydra.lib.lists.`null`[hydra.core.Binding](bindings))(Some(hydra.error.core.InvalidTermError.emptyLetBindings(hydra.error.core.EmptyLetBindingsError(path))))(None),
         hydra.validate.core.checkDuplicateBindings(path)(bindings), None, hydra.validate.core.firstError(hydra.lib.lists.map[hydra.core.Name,
         Option[hydra.error.core.InvalidTermError]]((bname: hydra.core.Name) =>
        hydra.lib.logic.ifElse[Option[hydra.error.core.InvalidTermError]](hydra.validate.core.isValidName(bname))(None)(Some(hydra.error.core.InvalidTermError.invalidLetBindingName(hydra.error.core.InvalidLetBindingNameError(path,
           bname)))))(names)), hydra.lib.logic.ifElse[Option[hydra.error.core.InvalidTermError]](typed)(hydra.validate.core.firstError(hydra.lib.lists.map[hydra.core.Binding,
           Option[hydra.error.core.InvalidTermError]]((b: hydra.core.Binding) =>
        hydra.lib.maybes.cases[hydra.core.TypeScheme, Option[hydra.error.core.InvalidTermError]](b.`type`)(None)((ts: hydra.core.TypeScheme) =>
        hydra.validate.core.checkUndefinedTypeVariablesInTypeScheme(path)(cx)(ts)((uvName: hydra.core.Name) =>
        Some(hydra.error.core.InvalidTermError.undefinedTypeVariableInBindingType(hydra.error.core.UndefinedTypeVariableInBindingTypeError(path,
           uvName))))))(bindings)))(None)))
    }
  }
  case hydra.core.Term.union(v_Term_union_inj) => {
    lazy val tname: hydra.core.Name = (v_Term_union_inj.typeName)
    hydra.lib.logic.ifElse[Option[hydra.error.core.InvalidTermError]](hydra.lib.equality.equal[scala.Predef.String](tname)(""))(Some(hydra.error.core.InvalidTermError.emptyTypeNameInTerm(hydra.error.core.EmptyTypeNameInTermError(path))))(None)
  }
  case hydra.core.Term.function(v_Term_function_fun) => v_Term_function_fun match
    case hydra.core.Function.lambda(v_Function_lambda_lam) => {
      lazy val paramName: hydra.core.Name = (v_Function_lambda_lam.parameter)
      hydra.validate.core.firstError(Seq(hydra.lib.logic.ifElse[Option[hydra.error.core.InvalidTermError]](hydra.lib.maybes.isJust[hydra.core.Term](hydra.lib.maps.lookup[hydra.core.Name,
         hydra.core.Term](paramName)(cx.boundTerms)))(Some(hydra.error.core.InvalidTermError.termVariableShadowing(hydra.error.core.TermVariableShadowingError(path,
         paramName))))(None), hydra.lib.logic.ifElse[Option[hydra.error.core.InvalidTermError]](hydra.validate.core.isValidName(paramName))(None)(Some(hydra.error.core.InvalidTermError.invalidLambdaParameterName(hydra.error.core.InvalidLambdaParameterNameError(path,
         paramName)))), hydra.lib.logic.ifElse[Option[hydra.error.core.InvalidTermError]](typed)(hydra.lib.maybes.cases[hydra.core.Type,
         Option[hydra.error.core.InvalidTermError]](v_Function_lambda_lam.domain)(None)((dom: hydra.core.Type) =>
        hydra.validate.core.checkUndefinedTypeVariablesInType(path)(cx)(dom)((uvName: hydra.core.Name) =>
        Some(hydra.error.core.InvalidTermError.undefinedTypeVariableInLambdaDomain(hydra.error.core.UndefinedTypeVariableInLambdaDomainError(path,
           uvName))))))(None)))
    }
    case hydra.core.Function.primitive(v_Function_primitive_primName) => hydra.lib.logic.ifElse[Option[hydra.error.core.InvalidTermError]](hydra.lib.maybes.isJust[hydra.graph.Primitive](hydra.lib.maps.lookup[hydra.core.Name,
       hydra.graph.Primitive](v_Function_primitive_primName)(cx.primitives)))(None)(Some(hydra.error.core.InvalidTermError.unknownPrimitiveName(hydra.error.core.UnknownPrimitiveNameError(path,
       v_Function_primitive_primName))))
    case hydra.core.Function.elimination(v_Function_elimination_elim) => v_Function_elimination_elim match
      case hydra.core.Elimination.record(v_Elimination_record_proj) => {
        lazy val tname: hydra.core.Name = (v_Elimination_record_proj.typeName)
        hydra.lib.logic.ifElse[Option[hydra.error.core.InvalidTermError]](hydra.lib.equality.equal[scala.Predef.String](tname)(""))(Some(hydra.error.core.InvalidTermError.emptyTypeNameInTerm(hydra.error.core.EmptyTypeNameInTermError(path))))(None)
      }
      case hydra.core.Elimination.union(v_Elimination_union_cs) => {
        lazy val tname: hydra.core.Name = (v_Elimination_union_cs.typeName)
        {
          lazy val csDefault: Option[hydra.core.Term] = (v_Elimination_union_cs.default)
          {
            lazy val csCases: Seq[hydra.core.Field] = (v_Elimination_union_cs.cases)
            hydra.validate.core.firstError(Seq(hydra.lib.logic.ifElse[Option[hydra.error.core.InvalidTermError]](hydra.lib.equality.equal[scala.Predef.String](tname)(""))(Some(hydra.error.core.InvalidTermError.emptyTypeNameInTerm(hydra.error.core.EmptyTypeNameInTermError(path))))(None),
               hydra.lib.logic.ifElse[Option[hydra.error.core.InvalidTermError]](hydra.lib.logic.and(hydra.lib.lists.`null`[hydra.core.Field](csCases))(hydra.lib.maybes.isNothing[hydra.core.Term](csDefault)))(Some(hydra.error.core.InvalidTermError.emptyCaseStatement(hydra.error.core.EmptyCaseStatementError(path,
               tname))))(None), hydra.validate.core.checkDuplicateFields(path)(hydra.lib.lists.map[hydra.core.Field,
               hydra.core.Name]((x: hydra.core.Field) => (x.name))(csCases))))
          }
        }
      }
      case _ => None
    case _ => None
  case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => hydra.lib.logic.ifElse[Option[hydra.error.core.InvalidTermError]](typed)(hydra.validate.core.checkUndefinedTypeVariablesInType(path)(cx)(v_Term_typeApplication_ta.`type`)((uvName: hydra.core.Name) =>
    Some(hydra.error.core.InvalidTermError.undefinedTypeVariableInTypeApplication(hydra.error.core.UndefinedTypeVariableInTypeApplicationError(path,
       uvName)))))(None)
  case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => {
    lazy val tvName: hydra.core.Name = (v_Term_typeLambda_tl.parameter)
    hydra.validate.core.firstError(Seq(hydra.lib.logic.ifElse[Option[hydra.error.core.InvalidTermError]](hydra.lib.sets.member[hydra.core.Name](tvName)(hydra.lib.sets.delete[hydra.core.Name](tvName)(cx.typeVariables)))(Some(hydra.error.core.InvalidTermError.typeVariableShadowingInTypeLambda(hydra.error.core.TypeVariableShadowingInTypeLambdaError(path,
       tvName))))(None), hydra.lib.logic.ifElse[Option[hydra.error.core.InvalidTermError]](hydra.validate.core.isValidName(tvName))(None)(Some(hydra.error.core.InvalidTermError.invalidTypeLambdaParameterName(hydra.error.core.InvalidTypeLambdaParameterNameError(path,
       tvName))))))
  }
  case hydra.core.Term.variable(v_Term_variable_varName) => hydra.lib.logic.ifElse[Option[hydra.error.core.InvalidTermError]](hydra.lib.logic.or(hydra.lib.maybes.isJust[hydra.core.Term](hydra.lib.maps.lookup[hydra.core.Name,
     hydra.core.Term](v_Term_variable_varName)(cx.boundTerms)))(hydra.lib.logic.or(hydra.lib.sets.member[hydra.core.Name](v_Term_variable_varName)(cx.lambdaVariables))(hydra.lib.maybes.isJust[hydra.graph.Primitive](hydra.lib.maps.lookup[hydra.core.Name,
     hydra.graph.Primitive](v_Term_variable_varName)(cx.primitives)))))(None)(Some(hydra.error.core.InvalidTermError.undefinedTermVariable(hydra.error.core.UndefinedTermVariableError(path,
     v_Term_variable_varName))))
  case hydra.core.Term.wrap(v_Term_wrap_wt) => {
    lazy val tname: hydra.core.Name = (v_Term_wrap_wt.typeName)
    hydra.lib.logic.ifElse[Option[hydra.error.core.InvalidTermError]](hydra.lib.equality.equal[scala.Predef.String](tname)(""))(Some(hydra.error.core.InvalidTermError.emptyTypeNameInTerm(hydra.error.core.EmptyTypeNameInTermError(path))))(None)
  }
  case _ => None

def checkUndefinedTypeVariablesInType[T0, T1](path: T0)(cx: hydra.graph.Graph)(typ: hydra.core.Type)(mkError: (hydra.core.Name => Option[T1])): Option[T1] =
  {
  lazy val freeVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.variables.freeVariablesInType(typ)
  lazy val undefined: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.difference[hydra.core.Name](freeVars)(cx.typeVariables)
  hydra.lib.logic.ifElse[Option[T1]](hydra.lib.sets.`null`[hydra.core.Name](undefined))(None)({
    lazy val firstUndefined: hydra.core.Name = hydra.lib.lists.head[hydra.core.Name](hydra.lib.sets.toList[hydra.core.Name](undefined))
    mkError(firstUndefined)
  })
}

def checkUndefinedTypeVariablesInTypeScheme[T0, T1](path: T0)(cx: hydra.graph.Graph)(ts: hydra.core.TypeScheme)(mkError: (hydra.core.Name => Option[T1])): Option[T1] =
  {
  lazy val freeVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.variables.freeVariablesInTypeScheme(ts)
  lazy val undefined: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.difference[hydra.core.Name](freeVars)(cx.typeVariables)
  hydra.lib.logic.ifElse[Option[T1]](hydra.lib.sets.`null`[hydra.core.Name](undefined))(None)({
    lazy val firstUndefined: hydra.core.Name = hydra.lib.lists.head[hydra.core.Name](hydra.lib.sets.toList[hydra.core.Name](undefined))
    mkError(firstUndefined)
  })
}

def checkVoid(typ: hydra.core.Type): Option[hydra.error.core.InvalidTypeError] =
  typ match
  case hydra.core.Type.void => Some(hydra.error.core.InvalidTypeError.voidInNonBottomPosition(hydra.error.core.VoidInNonBottomPositionError(Seq())))
  case _ => None

def findDuplicate[T0](names: Seq[T0]): Option[T0] =
  {
  lazy val result: Tuple2[scala.collection.immutable.Set[T0], Option[T0]] = hydra.lib.lists.foldl[Tuple2[scala.collection.immutable.Set[T0],
     Option[T0]], T0]((acc: Tuple2[scala.collection.immutable.Set[T0], Option[T0]]) =>
    (name: T0) =>
    {
    lazy val seen: scala.collection.immutable.Set[T0] = hydra.lib.pairs.first[scala.collection.immutable.Set[T0], Option[T0]](acc)
    {
      lazy val dup: Option[T0] = hydra.lib.pairs.second[scala.collection.immutable.Set[T0], Option[T0]](acc)
      hydra.lib.maybes.cases[T0, Tuple2[scala.collection.immutable.Set[T0], Option[T0]]](dup)(hydra.lib.logic.ifElse[Tuple2[scala.collection.immutable.Set[T0],
         Option[T0]]](hydra.lib.sets.member[T0](name)(seen))(Tuple2(seen, Some(name)))(Tuple2(hydra.lib.sets.insert[T0](name)(seen),
         None)))((_x: T0) => acc)
    }
  })(Tuple2(hydra.lib.sets.empty[T0], None))(names)
  hydra.lib.pairs.second[scala.collection.immutable.Set[T0], Option[T0]](result)
}

def findDuplicateFieldType[T0](names: Seq[T0]): Option[T0] =
  {
  lazy val result: Tuple2[scala.collection.immutable.Set[T0], Option[T0]] = hydra.lib.lists.foldl[Tuple2[scala.collection.immutable.Set[T0],
     Option[T0]], T0]((acc: Tuple2[scala.collection.immutable.Set[T0], Option[T0]]) =>
    (name: T0) =>
    {
    lazy val seen: scala.collection.immutable.Set[T0] = hydra.lib.pairs.first[scala.collection.immutable.Set[T0], Option[T0]](acc)
    {
      lazy val dup: Option[T0] = hydra.lib.pairs.second[scala.collection.immutable.Set[T0], Option[T0]](acc)
      hydra.lib.maybes.cases[T0, Tuple2[scala.collection.immutable.Set[T0], Option[T0]]](dup)(hydra.lib.logic.ifElse[Tuple2[scala.collection.immutable.Set[T0],
         Option[T0]]](hydra.lib.sets.member[T0](name)(seen))(Tuple2(seen, Some(name)))(Tuple2(hydra.lib.sets.insert[T0](name)(seen),
         None)))((_x: T0) => acc)
    }
  })(Tuple2(hydra.lib.sets.empty[T0], None))(names)
  hydra.lib.pairs.second[scala.collection.immutable.Set[T0], Option[T0]](result)
}

def firstError[T0](checks: Seq[Option[T0]]): Option[T0] =
  hydra.lib.lists.foldl[Option[T0], Option[T0]]((acc: Option[T0]) =>
  (check: Option[T0]) =>
  hydra.lib.maybes.cases[T0, Option[T0]](acc)(check)((_x: T0) => acc))(None)(checks)

def firstTypeError[T0](checks: Seq[Option[T0]]): Option[T0] =
  hydra.lib.lists.foldl[Option[T0], Option[T0]]((acc: Option[T0]) =>
  (check: Option[T0]) =>
  hydra.lib.maybes.cases[T0, Option[T0]](acc)(check)((_x: T0) => acc))(None)(checks)

def isValidName(name: hydra.core.Name): Boolean = hydra.lib.logic.not(hydra.lib.equality.equal[scala.Predef.String](name)(""))

def term(typed: Boolean)(g: hydra.graph.Graph)(t: hydra.core.Term): Option[hydra.error.core.InvalidTermError] =
  hydra.rewriting.foldTermWithGraphAndPath((recurse: (Option[hydra.error.core.InvalidTermError] => hydra.core.Term => Option[hydra.error.core.InvalidTermError])) =>
  (path: Seq[hydra.paths.SubtermStep]) =>
  (cx: hydra.graph.Graph) =>
  (acc: Option[hydra.error.core.InvalidTermError]) =>
  (trm: hydra.core.Term) =>
  hydra.lib.maybes.cases[hydra.error.core.InvalidTermError, Option[hydra.error.core.InvalidTermError]](acc)({
  lazy val checkResult: Option[hydra.error.core.InvalidTermError] = hydra.validate.core.checkTerm(typed)(path)(cx)(trm)
  hydra.lib.maybes.cases[hydra.error.core.InvalidTermError, Option[hydra.error.core.InvalidTermError]](checkResult)(recurse(None)(trm))((err: hydra.error.core.InvalidTermError) => Some(err))
})((_x: hydra.error.core.InvalidTermError) => acc))(g)(None)(t)

def `type`(boundVars: scala.collection.immutable.Set[hydra.core.Name])(typ: hydra.core.Type): Option[hydra.error.core.InvalidTypeError] =
  {
  lazy val checkResult: Option[hydra.error.core.InvalidTypeError] = hydra.validate.core.validateTypeNode(boundVars)(typ)
  hydra.lib.maybes.cases[hydra.error.core.InvalidTypeError, Option[hydra.error.core.InvalidTypeError]](checkResult)(typ match
    case hydra.core.Type.forall(v_Type_forall_ft) => {
      lazy val newBound: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.insert[hydra.core.Name](v_Type_forall_ft.parameter)(boundVars)
      hydra.validate.core.`type`(newBound)(v_Type_forall_ft.body)
    }
    case hydra.core.Type.annotated(v_Type_annotated_ann) => hydra.validate.core.`type`(boundVars)(v_Type_annotated_ann.body)
    case hydra.core.Type.application(v_Type_application_at) => hydra.validate.core.firstTypeError(Seq(hydra.validate.core.`type`(boundVars)(v_Type_application_at.function),
       hydra.validate.core.`type`(boundVars)(v_Type_application_at.argument)))
    case hydra.core.Type.either(v_Type_either_et) => hydra.validate.core.firstTypeError(Seq(hydra.validate.core.`type`(boundVars)(v_Type_either_et.left),
       hydra.validate.core.`type`(boundVars)(v_Type_either_et.right)))
    case hydra.core.Type.function(v_Type_function_ft) => hydra.validate.core.firstTypeError(Seq(hydra.validate.core.`type`(boundVars)(v_Type_function_ft.domain),
       hydra.validate.core.`type`(boundVars)(v_Type_function_ft.codomain)))
    case hydra.core.Type.list(v_Type_list_lt) => hydra.validate.core.`type`(boundVars)(v_Type_list_lt)
    case hydra.core.Type.map(v_Type_map_mt) => hydra.validate.core.firstTypeError(Seq(hydra.validate.core.`type`(boundVars)(v_Type_map_mt.keys),
       hydra.validate.core.`type`(boundVars)(v_Type_map_mt.values)))
    case hydra.core.Type.maybe(v_Type_maybe_mt) => hydra.validate.core.`type`(boundVars)(v_Type_maybe_mt)
    case hydra.core.Type.pair(v_Type_pair_pt) => hydra.validate.core.firstTypeError(Seq(hydra.validate.core.`type`(boundVars)(v_Type_pair_pt.first),
       hydra.validate.core.`type`(boundVars)(v_Type_pair_pt.second)))
    case hydra.core.Type.record(v_Type_record_fields) => hydra.validate.core.firstTypeError(hydra.lib.lists.map[hydra.core.FieldType,
       Option[hydra.error.core.InvalidTypeError]]((f: hydra.core.FieldType) => hydra.validate.core.`type`(boundVars)(f.`type`))(v_Type_record_fields))
    case hydra.core.Type.set(v_Type_set_st) => hydra.validate.core.`type`(boundVars)(v_Type_set_st)
    case hydra.core.Type.union(v_Type_union_fields) => hydra.validate.core.firstTypeError(hydra.lib.lists.map[hydra.core.FieldType,
       Option[hydra.error.core.InvalidTypeError]]((f: hydra.core.FieldType) => hydra.validate.core.`type`(boundVars)(f.`type`))(v_Type_union_fields))
    case hydra.core.Type.wrap(v_Type_wrap_wt) => hydra.validate.core.`type`(boundVars)(v_Type_wrap_wt)
    case _ => None)((err: hydra.error.core.InvalidTypeError) => Some(err))
}

def validateTypeNode(boundVars: scala.collection.immutable.Set[hydra.core.Name])(typ: hydra.core.Type): Option[hydra.error.core.InvalidTypeError] =
  typ match
  case hydra.core.Type.annotated(v_Type_annotated_ann) => {
    lazy val body: hydra.core.Type = (v_Type_annotated_ann.body)
    {
      lazy val annMap: Map[hydra.core.Name, hydra.core.Term] = (v_Type_annotated_ann.annotation)
      hydra.validate.core.firstTypeError(Seq(hydra.lib.logic.ifElse[Option[hydra.error.core.InvalidTypeError]](hydra.lib.maps.`null`[hydra.core.Name,
         hydra.core.Term](annMap))(Some(hydra.error.core.InvalidTypeError.emptyTypeAnnotation(hydra.error.core.EmptyTypeAnnotationError(Seq()))))(None),
         body match
        case hydra.core.Type.annotated(v_Type_annotated__) => Some(hydra.error.core.InvalidTypeError.nestedTypeAnnotation(hydra.error.core.NestedTypeAnnotationError(Seq())))
        case _ => None))
    }
  }
  case hydra.core.Type.either(v_Type_either_et) => hydra.validate.core.firstTypeError(Seq(hydra.validate.core.checkVoid(v_Type_either_et.left),
     hydra.validate.core.checkVoid(v_Type_either_et.right)))
  case hydra.core.Type.forall(v_Type_forall_ft) => {
    lazy val paramName: hydra.core.Name = (v_Type_forall_ft.parameter)
    hydra.validate.core.firstTypeError(Seq(hydra.lib.logic.ifElse[Option[hydra.error.core.InvalidTypeError]](hydra.lib.sets.member[hydra.core.Name](paramName)(boundVars))(Some(hydra.error.core.InvalidTypeError.typeVariableShadowingInForall(hydra.error.core.TypeVariableShadowingInForallError(Seq(),
       paramName))))(None), hydra.lib.logic.ifElse[Option[hydra.error.core.InvalidTypeError]](hydra.validate.core.isValidName(paramName))(None)(Some(hydra.error.core.InvalidTypeError.invalidForallParameterName(hydra.error.core.InvalidForallParameterNameError(Seq(),
       paramName))))))
  }
  case hydra.core.Type.function(v_Type_function_ft) => hydra.validate.core.checkVoid(v_Type_function_ft.codomain)
  case hydra.core.Type.list(v_Type_list_lt) => hydra.validate.core.checkVoid(v_Type_list_lt)
  case hydra.core.Type.map(v_Type_map_mt) => {
    lazy val keyType: hydra.core.Type = (v_Type_map_mt.keys)
    hydra.validate.core.firstTypeError(Seq(keyType match
      case hydra.core.Type.function(v_Type_function__) => Some(hydra.error.core.InvalidTypeError.nonComparableMapKeyType(hydra.error.core.NonComparableMapKeyTypeError(Seq(),
         keyType)))
      case _ => None, hydra.validate.core.checkVoid(keyType), hydra.validate.core.checkVoid(v_Type_map_mt.values)))
  }
  case hydra.core.Type.pair(v_Type_pair_pt) => hydra.validate.core.firstTypeError(Seq(hydra.validate.core.checkVoid(v_Type_pair_pt.first),
     hydra.validate.core.checkVoid(v_Type_pair_pt.second)))
  case hydra.core.Type.record(v_Type_record_fields) => hydra.validate.core.firstTypeError(Seq(hydra.lib.logic.ifElse[Option[hydra.error.core.InvalidTypeError]](hydra.lib.lists.`null`[hydra.core.FieldType](v_Type_record_fields))(Some(hydra.error.core.InvalidTypeError.emptyRecordType(hydra.error.core.EmptyRecordTypeError(Seq()))))(None),
     hydra.validate.core.checkDuplicateFieldTypes(v_Type_record_fields)((dupName: hydra.core.Name) =>
    Some(hydra.error.core.InvalidTypeError.duplicateRecordTypeFieldNames(hydra.error.core.DuplicateRecordTypeFieldNamesError(Seq(),
       dupName)))), hydra.validate.core.firstTypeError(hydra.lib.lists.map[hydra.core.FieldType, Option[hydra.error.core.InvalidTypeError]]((f: hydra.core.FieldType) => hydra.validate.core.checkVoid(f.`type`))(v_Type_record_fields))))
  case hydra.core.Type.set(v_Type_set_elemType) => hydra.validate.core.firstTypeError(Seq(v_Type_set_elemType match
    case hydra.core.Type.function(v_Type_function__) => Some(hydra.error.core.InvalidTypeError.nonComparableSetElementType(hydra.error.core.NonComparableSetElementTypeError(Seq(),
       v_Type_set_elemType)))
    case _ => None, hydra.validate.core.checkVoid(v_Type_set_elemType)))
  case hydra.core.Type.union(v_Type_union_fields) => hydra.validate.core.firstTypeError(Seq(hydra.lib.logic.ifElse[Option[hydra.error.core.InvalidTypeError]](hydra.lib.lists.`null`[hydra.core.FieldType](v_Type_union_fields))(Some(hydra.error.core.InvalidTypeError.emptyUnionType(hydra.error.core.EmptyUnionTypeError(Seq()))))(None),
     hydra.lib.logic.ifElse[Option[hydra.error.core.InvalidTypeError]](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.FieldType](v_Type_union_fields))(1))({
    lazy val singleField: hydra.core.FieldType = hydra.lib.lists.head[hydra.core.FieldType](v_Type_union_fields)
    Some(hydra.error.core.InvalidTypeError.singleVariantUnion(hydra.error.core.SingleVariantUnionError(Seq(), (singleField.name))))
  })(None), hydra.validate.core.checkDuplicateFieldTypes(v_Type_union_fields)((dupName: hydra.core.Name) =>
    Some(hydra.error.core.InvalidTypeError.duplicateUnionTypeFieldNames(hydra.error.core.DuplicateUnionTypeFieldNamesError(Seq(),
       dupName)))), hydra.validate.core.firstTypeError(hydra.lib.lists.map[hydra.core.FieldType, Option[hydra.error.core.InvalidTypeError]]((f: hydra.core.FieldType) => hydra.validate.core.checkVoid(f.`type`))(v_Type_union_fields))))
  case hydra.core.Type.variable(v_Type_variable_varName) => hydra.lib.logic.ifElse[Option[hydra.error.core.InvalidTypeError]](hydra.lib.sets.member[hydra.core.Name](v_Type_variable_varName)(boundVars))(None)(Some(hydra.error.core.InvalidTypeError.undefinedTypeVariable(hydra.error.core.UndefinedTypeVariableError(Seq(),
     v_Type_variable_varName))))
  case _ => None
