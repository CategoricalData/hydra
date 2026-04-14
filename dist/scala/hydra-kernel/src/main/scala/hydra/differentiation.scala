package hydra.differentiation

import hydra.core.*

def differentiateBinary(bfname: hydra.core.Name)(a: hydra.core.Term)(b: hydra.core.Term)(da: hydra.core.Term)(db: hydra.core.Term): hydra.core.Term =
  hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.logic.or(hydra.lib.equality.equal[hydra.core.Name](bfname)("hydra.lib.math.add"))(hydra.lib.equality.equal[hydra.core.Name](bfname)("hydra.lib.math.addFloat64")))(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.addFloat64"),
     da)), db)))(hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.logic.or(hydra.lib.equality.equal[hydra.core.Name](bfname)("hydra.lib.math.sub"))(hydra.lib.equality.equal[hydra.core.Name](bfname)("hydra.lib.math.subFloat64")))(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.subFloat64"),
     da)), db)))(hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.logic.or(hydra.lib.equality.equal[hydra.core.Name](bfname)("hydra.lib.math.mul"))(hydra.lib.equality.equal[hydra.core.Name](bfname)("hydra.lib.math.mulFloat64")))(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.addFloat64"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.mulFloat64"),
     a)), db)))), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.mulFloat64"),
     b)), da)))))(hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Name](bfname)("hydra.lib.math.pow"))(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.mulFloat64"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.pow"),
     a)), b)))), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.addFloat64"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.mulFloat64"),
     db)), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.log"),
     a)))))), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.mulFloat64"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.mulFloat64"),
     b)), da)))), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.pow"),
     a)), hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(-1.0))))))))))))(hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Name](bfname)("hydra.lib.math.atan2"))(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.mulFloat64"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.subFloat64"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.mulFloat64"),
     b)), da)))), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.mulFloat64"),
     a)), db)))))), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.pow"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.addFloat64"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.mulFloat64"),
     a)), a)))), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.mulFloat64"),
     b)), b)))))), hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(-1.0))))))))(hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Name](bfname)("hydra.lib.math.logBase"))(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.mulFloat64"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.subFloat64"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.mulFloat64"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.log"),
     a)))), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.mulFloat64"),
     db)), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.pow"),
     b)), hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(-1.0))))))))))),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.mulFloat64"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.log"),
     b)))), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.mulFloat64"),
     da)), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.pow"),
     a)), hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(-1.0))))))))))))),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.pow"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.mulFloat64"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.log"),
     a)))), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.log"),
     a)))))), hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(-1.0))))))))(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(0.0)))))))))

def differentiateFunction(term: hydra.core.Term): hydra.core.Term =
  term match
  case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.differentiation.differentiateFunction(v_Term_annotated_at.body)
  case hydra.core.Term.lambda(v_Term_lambda_l) => {
    lazy val paramName: hydra.core.Name = (v_Term_lambda_l.parameter)
    {
      lazy val body: hydra.core.Term = (v_Term_lambda_l.body)
      hydra.core.Term.lambda(hydra.core.Lambda(paramName, (v_Term_lambda_l.domain), hydra.differentiation.differentiateTerm(paramName)(body)))
    }
  }
  case _ => term

def differentiateTerm(dx: hydra.core.Name)(term: hydra.core.Term): hydra.core.Term =
  term match
  case hydra.core.Term.variable(v_Term_variable_v) => hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Name](v_Term_variable_v)(dx))(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(1.0))))(hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(0.0))))
  case hydra.core.Term.literal(v_Term_literal__) => hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(0.0)))
  case hydra.core.Term.application(v_Term_application_app) => {
    lazy val func: hydra.core.Term = (v_Term_application_app.function)
    {
      lazy val arg: hydra.core.Term = (v_Term_application_app.argument)
      func match
        case hydra.core.Term.variable(v_Term_variable_fname) => hydra.lib.maybes.maybe[hydra.core.Term,
           hydra.core.Term](hydra.differentiation.differentiateTerm(dx)(hydra.core.Term.application(hydra.core.Application(func,
           arg))))((derivTerm: hydra.core.Term) =>
          hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.mulFloat64"),
             hydra.core.Term.application(hydra.core.Application(derivTerm, arg)))), hydra.differentiation.differentiateTerm(dx)(arg))))(hydra.differentiation.primitiveDerivative(v_Term_variable_fname))
        case hydra.core.Term.application(v_Term_application_innerApp) => {
          lazy val innerFunc: hydra.core.Term = (v_Term_application_innerApp.function)
          {
            lazy val innerArg: hydra.core.Term = (v_Term_application_innerApp.argument)
            innerFunc match
              case hydra.core.Term.variable(v_Term_variable_bfname) => hydra.differentiation.differentiateBinary(v_Term_variable_bfname)(innerArg)(arg)(hydra.differentiation.differentiateTerm(dx)(innerArg))(hydra.differentiation.differentiateTerm(dx)(arg))
              case _ => hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.mulFloat64"),
                 hydra.differentiation.differentiateTerm(dx)(hydra.core.Term.application(hydra.core.Application(func,
                 arg))))), hydra.differentiation.differentiateTerm(dx)(arg)))
          }
        }
        case _ => hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.mulFloat64"),
           hydra.differentiation.differentiateTerm(dx)(hydra.core.Term.application(hydra.core.Application(func,
           arg))))), hydra.differentiation.differentiateTerm(dx)(arg)))
    }
  }
  case hydra.core.Term.lambda(v_Term_lambda_l) => hydra.lib.logic.ifElse[hydra.core.Term](hydra.lib.equality.equal[hydra.core.Name](v_Term_lambda_l.parameter)(dx))(hydra.core.Term.lambda(hydra.core.Lambda(v_Term_lambda_l.parameter,
     (v_Term_lambda_l.domain), hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(0.0))))))(hydra.core.Term.lambda(hydra.core.Lambda(v_Term_lambda_l.parameter,
     (v_Term_lambda_l.domain), hydra.differentiation.differentiateTerm(dx)(v_Term_lambda_l.body))))
  case hydra.core.Term.cases(v_Term_cases__) => hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(0.0)))
  case hydra.core.Term.project(v_Term_project__) => hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(0.0)))
  case hydra.core.Term.unwrap(v_Term_unwrap__) => hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(0.0)))
  case hydra.core.Term.let(v_Term_let_l) => hydra.core.Term.let(hydra.core.Let(hydra.lib.lists.map[hydra.core.Binding,
     hydra.core.Binding]((b: hydra.core.Binding) =>
    hydra.core.Binding(b.name, hydra.differentiation.differentiateTerm(dx)(b.term), None))(v_Term_let_l.bindings),
       hydra.differentiation.differentiateTerm(dx)(v_Term_let_l.body)))
  case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.differentiation.differentiateTerm(dx)(v_Term_annotated_at.body)
  case hydra.core.Term.list(v_Term_list_elems) => hydra.core.Term.list(hydra.lib.lists.map[hydra.core.Term,
     hydra.core.Term]((v1: hydra.core.Term) => hydra.differentiation.differentiateTerm(dx)(v1))(v_Term_list_elems))
  case hydra.core.Term.pair(v_Term_pair_p) => hydra.core.Term.pair(Tuple2(hydra.differentiation.differentiateTerm(dx)(hydra.lib.pairs.first[hydra.core.Term,
     hydra.core.Term](v_Term_pair_p)), hydra.differentiation.differentiateTerm(dx)(hydra.lib.pairs.second[hydra.core.Term,
     hydra.core.Term](v_Term_pair_p))))
  case hydra.core.Term.record(v_Term_record_r) => hydra.core.Term.record(hydra.core.Record(v_Term_record_r.typeName,
     hydra.lib.lists.map[hydra.core.Field, hydra.core.Field]((fld: hydra.core.Field) =>
    hydra.core.Field(fld.name, hydra.differentiation.differentiateTerm(dx)(fld.term)))(v_Term_record_r.fields)))
  case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => hydra.differentiation.differentiateTerm(dx)(v_Term_typeApplication_ta.body)
  case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.differentiation.differentiateTerm(dx)(v_Term_typeLambda_tl.body)
  case hydra.core.Term.unit => hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(0.0)))
  case hydra.core.Term.set(v_Term_set__) => hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(0.0)))
  case hydra.core.Term.map(v_Term_map__) => hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(0.0)))
  case hydra.core.Term.either(v_Term_either__) => hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(0.0)))
  case hydra.core.Term.maybe(v_Term_maybe__) => hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(0.0)))
  case hydra.core.Term.inject(v_Term_inject__) => hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(0.0)))
  case hydra.core.Term.wrap(v_Term_wrap__) => hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(0.0)))

def gradient(typeName: hydra.core.Name)(vars: Seq[hydra.core.Name])(term: hydra.core.Term): hydra.core.Term =
  hydra.core.Term.record(hydra.core.Record(typeName, hydra.lib.lists.map[hydra.core.Name, hydra.core.Field]((v: hydra.core.Name) =>
  hydra.core.Field(v, hydra.differentiation.differentiateTerm(v)(term)))(vars)))

def primitiveDerivative(name: hydra.core.Name): Option[hydra.core.Term] =
  hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.math.sin"))(Some(hydra.core.Term.variable("hydra.lib.math.cos")))(hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.math.cos"))(Some(hydra.core.Term.lambda(hydra.core.Lambda("_x",
     None, hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.negateFloat64"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.sin"),
     hydra.core.Term.variable("_x")))))))))(hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.math.tan"))(Some(hydra.core.Term.lambda(hydra.core.Lambda("_x",
     None, hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.pow"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.cos"),
     hydra.core.Term.variable("_x"))))), hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(-2.0)))))))))(hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.math.exp"))(Some(hydra.core.Term.variable("hydra.lib.math.exp")))(hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.math.log"))(Some(hydra.core.Term.lambda(hydra.core.Lambda("_x",
     None, hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.pow"),
     hydra.core.Term.variable("_x"))), hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(-1.0)))))))))(hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.math.sqrt"))(Some(hydra.core.Term.lambda(hydra.core.Lambda("_x",
     None, hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.mulFloat64"),
     hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(0.5))))), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.pow"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.sqrt"),
     hydra.core.Term.variable("_x"))))), hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(-1.0)))))))))))(hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.math.asin"))(Some(hydra.core.Term.lambda(hydra.core.Lambda("_x",
     None, hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.pow"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.sqrt"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.subFloat64"),
     hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(1.0))))), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.mulFloat64"),
     hydra.core.Term.variable("_x"))), hydra.core.Term.variable("_x"))))))))), hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(-1.0)))))))))(hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.math.acos"))(Some(hydra.core.Term.lambda(hydra.core.Lambda("_x",
     None, hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.negateFloat64"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.pow"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.sqrt"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.subFloat64"),
     hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(1.0))))), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.mulFloat64"),
     hydra.core.Term.variable("_x"))), hydra.core.Term.variable("_x"))))))))), hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(-1.0)))))))))))(hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.math.atan"))(Some(hydra.core.Term.lambda(hydra.core.Lambda("_x",
     None, hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.pow"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.addFloat64"),
     hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(1.0))))), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.mulFloat64"),
     hydra.core.Term.variable("_x"))), hydra.core.Term.variable("_x"))))))), hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(-1.0)))))))))(hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.math.sinh"))(Some(hydra.core.Term.variable("hydra.lib.math.cosh")))(hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.math.cosh"))(Some(hydra.core.Term.variable("hydra.lib.math.sinh")))(hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.math.tanh"))(Some(hydra.core.Term.lambda(hydra.core.Lambda("_x",
     None, hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.subFloat64"),
     hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(1.0))))), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.mulFloat64"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.tanh"),
     hydra.core.Term.variable("_x"))))), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.tanh"),
     hydra.core.Term.variable("_x")))))))))))(hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.math.asinh"))(Some(hydra.core.Term.lambda(hydra.core.Lambda("_x",
     None, hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.pow"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.sqrt"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.addFloat64"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.mulFloat64"),
     hydra.core.Term.variable("_x"))), hydra.core.Term.variable("_x"))))), hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(1.0))))))))),
     hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(-1.0)))))))))(hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.math.acosh"))(Some(hydra.core.Term.lambda(hydra.core.Lambda("_x",
     None, hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.pow"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.sqrt"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.subFloat64"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.mulFloat64"),
     hydra.core.Term.variable("_x"))), hydra.core.Term.variable("_x"))))), hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(1.0))))))))),
     hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(-1.0)))))))))(hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.math.atanh"))(Some(hydra.core.Term.lambda(hydra.core.Lambda("_x",
     None, hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.pow"),
     hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.subFloat64"),
     hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(1.0))))), hydra.core.Term.application(hydra.core.Application(hydra.core.Term.application(hydra.core.Application(hydra.core.Term.variable("hydra.lib.math.mulFloat64"),
     hydra.core.Term.variable("_x"))), hydra.core.Term.variable("_x"))))))), hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(-1.0)))))))))(hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.math.negate"))(Some(hydra.core.Term.lambda(hydra.core.Lambda("_x",
     None, hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(-1.0)))))))(hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.math.abs"))(Some(hydra.core.Term.variable("hydra.lib.math.signum")))(hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.math.ceiling"))(Some(hydra.core.Term.lambda(hydra.core.Lambda("_x",
     None, hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(0.0)))))))(hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.math.floor"))(Some(hydra.core.Term.lambda(hydra.core.Lambda("_x",
     None, hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(0.0)))))))(hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.math.round"))(Some(hydra.core.Term.lambda(hydra.core.Lambda("_x",
     None, hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(0.0)))))))(hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.math.truncate"))(Some(hydra.core.Term.lambda(hydra.core.Lambda("_x",
     None, hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(0.0)))))))(hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.lib.equality.equal[hydra.core.Name](name)("hydra.lib.math.signum"))(Some(hydra.core.Term.lambda(hydra.core.Lambda("_x",
     None, hydra.core.Term.literal(hydra.core.Literal.float(hydra.core.FloatValue.float64(0.0)))))))(None))))))))))))))))))))))
