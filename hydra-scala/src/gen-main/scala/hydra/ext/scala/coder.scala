package hydra.ext.scala.coder

import hydra.context.*

import hydra.core.*

import hydra.errors.*

import hydra.ext.scala.syntax.*

import hydra.graph.*

import hydra.packaging.*

import hydra.typing.*

import hydra.util.*

def applyVar(fterm: hydra.core.Term)(avar: hydra.core.Name): hydra.core.Term =
  {
  lazy val v: scala.Predef.String = avar
  hydra.strip.deannotateAndDetypeTerm(fterm) match
    case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
      case hydra.core.Function.lambda(v_Function_lambda_lam) => {
        lazy val lamParam: hydra.core.Name = (v_Function_lambda_lam.parameter)
        lazy val lamBody: hydra.core.Term = (v_Function_lambda_lam.body)
        hydra.lib.logic.ifElse[hydra.core.Term](hydra.variables.isFreeVariableInTerm(lamParam)(lamBody))(lamBody)(hydra.variables.substituteVariable(lamParam)(avar)(lamBody))
      }
      case _ => hydra.core.Term.application(hydra.core.Application(fterm, hydra.core.Term.variable(avar)))
    case _ => hydra.core.Term.application(hydra.core.Application(fterm, hydra.core.Term.variable(avar)))
}

def constructModule(cx: hydra.context.Context)(g: hydra.graph.Graph)(mod: hydra.packaging.Module)(defs: Seq[hydra.packaging.Definition]): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.ext.scala.syntax.Pkg] =
  {
  lazy val partitioned: Tuple2[Seq[hydra.packaging.TypeDefinition], Seq[hydra.packaging.TermDefinition]] = hydra.environment.partitionDefinitions(defs)
  lazy val typeDefs: Seq[hydra.packaging.TypeDefinition] = hydra.lib.pairs.first[Seq[hydra.packaging.TypeDefinition],
     Seq[hydra.packaging.TermDefinition]](partitioned)
  lazy val termDefs: Seq[hydra.packaging.TermDefinition] = hydra.lib.pairs.second[Seq[hydra.packaging.TypeDefinition],
     Seq[hydra.packaging.TermDefinition]](partitioned)
  lazy val nsName: scala.Predef.String = (mod.namespace)
  lazy val pname: hydra.ext.scala.syntax.Data_Name = hydra.ext.scala.syntax.Data_Name(hydra.lib.strings.intercalate(".")(hydra.lib.strings.splitOn(".")(nsName)))
  lazy val pref: hydra.ext.scala.syntax.Data_Ref = hydra.ext.scala.syntax.Data_Ref.name(pname)
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.scala.syntax.Stat],
     hydra.ext.scala.syntax.Pkg](hydra.lib.eithers.mapList[hydra.packaging.TypeDefinition, hydra.ext.scala.syntax.Stat,
     hydra.context.InContext[hydra.errors.Error]]((td: hydra.packaging.TypeDefinition) => hydra.ext.scala.coder.encodeTypeDefinition(cx)(g)(td))(typeDefs))((typeDeclStats: Seq[hydra.ext.scala.syntax.Stat]) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.scala.syntax.Stat],
       hydra.ext.scala.syntax.Pkg](hydra.lib.eithers.mapList[hydra.packaging.TermDefinition, hydra.ext.scala.syntax.Stat,
       hydra.context.InContext[hydra.errors.Error]]((td: hydra.packaging.TermDefinition) => hydra.ext.scala.coder.encodeTermDefinition(cx)(g)(td))(termDefs))((termDeclStats: Seq[hydra.ext.scala.syntax.Stat]) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.scala.syntax.Stat],
       hydra.ext.scala.syntax.Pkg](hydra.ext.scala.coder.findImports(cx)(g)(mod))((imports: Seq[hydra.ext.scala.syntax.Stat]) =>
    Right(hydra.ext.scala.syntax.Pkg(pname, pref, hydra.lib.lists.concat[hydra.ext.scala.syntax.Stat](Seq(imports, typeDeclStats, termDeclStats)))))))
}

def dropDomains(n: Int)(t: hydra.core.Type): hydra.core.Type =
  hydra.lib.logic.ifElse[hydra.core.Type](hydra.lib.equality.lte[Int](n)(0))(t)(hydra.strip.deannotateType(t) match
  case hydra.core.Type.function(v_Type_function_ft) => hydra.ext.scala.coder.dropDomains(hydra.lib.math.sub(n)(1))(v_Type_function_ft.codomain)
  case hydra.core.Type.forall(v_Type_forall_fa) => hydra.ext.scala.coder.dropDomains(n)(v_Type_forall_fa.body)
  case _ => t)

def encodeCase(cx: hydra.context.Context)(g: hydra.graph.Graph)(ftypes: Map[hydra.core.Name, hydra.core.Type])(sn: Option[hydra.core.Name])(f: hydra.core.Field): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.ext.scala.syntax.Case] =
  {
  lazy val fname: hydra.core.Name = (f.name)
  lazy val fterm: hydra.core.Term = (f.term)
  lazy val isUnit: Boolean = hydra.lib.maybes.maybe[Boolean, hydra.core.Type](hydra.strip.deannotateAndDetypeTerm(fterm) match
    case hydra.core.Term.function(v_Term_function_fn) => v_Term_function_fn match
      case hydra.core.Function.lambda(v_Function_lambda_lam) => {
        lazy val lamParam: hydra.core.Name = (v_Function_lambda_lam.parameter)
        lazy val lamBody: hydra.core.Term = (v_Function_lambda_lam.body)
        lazy val domIsUnit: Boolean = hydra.lib.maybes.maybe[Boolean, hydra.core.Type](false)((dom: hydra.core.Type) =>
          hydra.lib.equality.equal[hydra.core.Type](dom)(hydra.core.Type.unit))(v_Function_lambda_lam.domain)
        lazy val bodyIgnoresParam: Boolean = hydra.variables.isFreeVariableInTerm(lamParam)(lamBody)
        hydra.lib.logic.or(domIsUnit)(bodyIgnoresParam)
      }
      case _ => false
    case hydra.core.Term.record(v_Term_record_r) => hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Field](v_Term_record_r.fields))(0)
    case hydra.core.Term.unit => true
    case _ => false)((dom: hydra.core.Type) =>
    hydra.strip.deannotateType(dom) match
    case hydra.core.Type.unit => true
    case hydra.core.Type.record(v_Type_record_rt) => hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.FieldType](v_Type_record_rt))(0)
    case _ => false)(hydra.lib.maps.lookup[hydra.core.Name, hydra.core.Type](fname)(ftypes))
  lazy val shortTypeName: scala.Predef.String = hydra.lib.lists.last[scala.Predef.String](hydra.lib.strings.splitOn(".")(hydra.lib.maybes.maybe[scala.Predef.String,
     hydra.core.Name]("x")((n: hydra.core.Name) => n)(sn)))
  lazy val lamParamSuffix: scala.Predef.String = hydra.strip.deannotateAndDetypeTerm(fterm) match
    case hydra.core.Term.function(v_Term_function_fn) => v_Term_function_fn match
      case hydra.core.Function.lambda(v_Function_lambda_lam) => {
        lazy val rawName: scala.Predef.String = (v_Function_lambda_lam.parameter)
        lazy val safeName: scala.Predef.String = hydra.lib.strings.fromList(hydra.lib.lists.map[Int, Int]((c: Int) =>
          hydra.lib.logic.ifElse[Int](hydra.lib.equality.equal[Int](c)(39))(95)(c))(hydra.lib.strings.toList(rawName)))
        hydra.lib.strings.cat2("_")(safeName)
      }
      case _ => ""
    case _ => ""
  lazy val v: hydra.core.Name = hydra.lib.strings.cat(Seq("v_", shortTypeName, "_", fname, lamParamSuffix))
  lazy val domainIsUnit: Boolean = hydra.strip.deannotateAndDetypeTerm(fterm) match
    case hydra.core.Term.function(v_Term_function_fn) => v_Term_function_fn match
      case hydra.core.Function.lambda(v_Function_lambda_lam) => hydra.lib.maybes.maybe[Boolean, hydra.core.Type](true)((dom: hydra.core.Type) =>
        hydra.lib.equality.equal[hydra.core.Type](dom)(hydra.core.Type.unit))(v_Function_lambda_lam.domain)
      case _ => true
    case _ => true
  lazy val patArgs: Seq[hydra.ext.scala.syntax.Pat] = hydra.lib.logic.ifElse[Seq[hydra.ext.scala.syntax.Pat]](isUnit)(hydra.lib.logic.ifElse[Seq[hydra.ext.scala.syntax.Pat]](domainIsUnit)(Seq())(Seq(hydra.ext.scala.syntax.Pat.wildcard)))(Seq(hydra.ext.scala.utils.svar(v)))
  lazy val pat: hydra.ext.scala.syntax.Pat = hydra.ext.scala.syntax.Pat.extract(hydra.ext.scala.syntax.Pat_Extract(hydra.ext.scala.utils.sname(hydra.ext.scala.utils.qualifyUnionFieldName("MATCHED.")(sn)(fname)),
     patArgs))
  lazy val applied: hydra.core.Term = hydra.ext.scala.coder.applyVar(fterm)(v)
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data, hydra.ext.scala.syntax.Case](hydra.ext.scala.coder.encodeTerm(cx)(g)(applied))((body: hydra.ext.scala.syntax.Data) => Right(hydra.ext.scala.syntax.Case(pat,
     None, body)))
}

def encodeComplexTermDef(cx: hydra.context.Context)(g: hydra.graph.Graph)(lname: scala.Predef.String)(term: hydra.core.Term)(typ: hydra.core.Type): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.ext.scala.syntax.Stat] =
  {
  lazy val doms: Seq[hydra.core.Type] = hydra.ext.scala.coder.extractDomains(typ)
  lazy val paramNames: Seq[hydra.core.Name] = hydra.ext.scala.coder.extractParams(term)
  lazy val paramCount: Int = hydra.lib.math.min(hydra.lib.lists.length[hydra.core.Name](paramNames))(hydra.lib.lists.length[hydra.core.Type](doms))
  lazy val cod: hydra.core.Type = hydra.ext.scala.coder.dropDomains(paramCount)(typ)
  lazy val zippedParams: Seq[Tuple2[hydra.core.Name, hydra.core.Type]] = hydra.lib.lists.zip[hydra.core.Name,
     hydra.core.Type](hydra.lib.lists.take[hydra.core.Name](paramCount)(paramNames))(hydra.lib.lists.take[hydra.core.Type](paramCount)(doms))
  lazy val freeTypeVars: Seq[hydra.core.Name] = hydra.lib.lists.filter[hydra.core.Name]((v: hydra.core.Name) =>
    hydra.lib.logic.not(hydra.lib.lists.elem[Int](46)(hydra.lib.strings.toList(v))))(hydra.lib.sets.toList[hydra.core.Name](hydra.variables.freeVariablesInType(typ)))
  lazy val tparams: Seq[hydra.ext.scala.syntax.Type_Param] = hydra.lib.lists.map[hydra.core.Name, hydra.ext.scala.syntax.Type_Param]((tv: hydra.core.Name) => hydra.ext.scala.utils.stparam(tv))(freeTypeVars)
  lazy val letBindings: Seq[hydra.core.Binding] = hydra.ext.scala.coder.extractLetBindings(term)
  lazy val gWithTypeVars: hydra.graph.Graph = hydra.graph.Graph(g.boundTerms, (g.boundTypes), (g.classConstraints),
     (g.lambdaVariables), (g.metadata), (g.primitives), (g.schemaTypes), hydra.lib.sets.union[hydra.core.Name](hydra.lib.sets.fromList[hydra.core.Name](freeTypeVars))(g.typeVariables))
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.scala.syntax.Data_Param],
     hydra.ext.scala.syntax.Stat](hydra.lib.eithers.mapList[Tuple2[hydra.core.Name, hydra.core.Type],
     hydra.ext.scala.syntax.Data_Param, hydra.context.InContext[hydra.errors.Error]]((v1: Tuple2[hydra.core.Name,
     hydra.core.Type]) =>
    hydra.ext.scala.coder.encodeTypedParam(cx)(gWithTypeVars)(v1))(zippedParams))((sparams: Seq[hydra.ext.scala.syntax.Data_Param]) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data, hydra.ext.scala.syntax.Stat](hydra.ext.scala.coder.encodeTerm(cx)(gWithTypeVars)(hydra.ext.scala.coder.extractBody(term)))((sbody: hydra.ext.scala.syntax.Data) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.Stat](hydra.ext.scala.coder.encodeType(cx)(g)(cod))((scod: hydra.ext.scala.syntax.Type) =>
    {
    lazy val gForLets: hydra.graph.Graph = hydra.lib.logic.ifElse[hydra.graph.Graph](hydra.lib.lists.`null`[hydra.core.Binding](letBindings))(gWithTypeVars)(hydra.scoping.extendGraphForLet((g2: hydra.graph.Graph) =>
      (b: hydra.core.Binding) =>
      hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.predicates.isComplexBinding(g2)(b))(Some(hydra.core.Term.literal(hydra.core.Literal.boolean(true))))(None))(gWithTypeVars)(hydra.core.Let(letBindings,
         hydra.core.Term.variable("dummy"))))
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.scala.syntax.Stat],
       hydra.ext.scala.syntax.Stat](hydra.lib.eithers.mapList[hydra.core.Binding, hydra.ext.scala.syntax.Stat,
       hydra.context.InContext[hydra.errors.Error]]((v1: hydra.core.Binding) =>
      hydra.ext.scala.coder.encodeLetBinding(cx)(gForLets)(hydra.lib.sets.fromList[hydra.core.Name](freeTypeVars))(v1))(letBindings))((sbindings: Seq[hydra.ext.scala.syntax.Stat]) =>
      {
      lazy val defBody: hydra.ext.scala.syntax.Data = hydra.lib.logic.ifElse[hydra.ext.scala.syntax.Data](hydra.lib.lists.`null`[hydra.ext.scala.syntax.Stat](sbindings))(sbody)(hydra.ext.scala.syntax.Data.block(hydra.ext.scala.syntax.Data_Block(hydra.lib.lists.concat2[hydra.ext.scala.syntax.Stat](sbindings)(Seq(hydra.ext.scala.syntax.Stat.term(sbody))))))
      Right(hydra.ext.scala.syntax.Stat.defn(hydra.ext.scala.syntax.Defn.`def`(hydra.ext.scala.syntax.Defn_Def(Seq(),
         hydra.ext.scala.syntax.Data_Name(lname), tparams, hydra.lib.lists.map[hydra.ext.scala.syntax.Data_Param,
         Seq[hydra.ext.scala.syntax.Data_Param]]((p: hydra.ext.scala.syntax.Data_Param) => Seq(p))(sparams),
         Some(scod), defBody))))
    })
  })))
}

def encodeFunction(cx: hydra.context.Context)(g: hydra.graph.Graph)(meta: Map[hydra.core.Name, hydra.core.Term])(fun: hydra.core.Function)(arg: Option[hydra.core.Term]): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.ext.scala.syntax.Data] =
  fun match
  case hydra.core.Function.lambda(v_Function_lambda_lam) => {
    lazy val param: hydra.core.Name = (v_Function_lambda_lam.parameter)
    lazy val v: scala.Predef.String = hydra.ext.scala.utils.scalaEscapeName(param)
    lazy val body: hydra.core.Term = (v_Function_lambda_lam.body)
    lazy val rawMdom: Option[hydra.core.Type] = (v_Function_lambda_lam.domain)
    lazy val mdom: Option[hydra.core.Type] = hydra.lib.maybes.bind[hydra.core.Type, hydra.core.Type](rawMdom)((dom: hydra.core.Type) =>
      {
      lazy val freeVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.variables.freeVariablesInType(dom)
      lazy val unqualifiedFreeVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.fromList[hydra.core.Name](hydra.lib.lists.filter[hydra.core.Name]((n: hydra.core.Name) =>
        hydra.lib.logic.not(hydra.lib.lists.elem[Int](46)(hydra.lib.strings.toList(n))))(hydra.lib.sets.toList[hydra.core.Name](freeVars)))
      lazy val unresolvedVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.difference[hydra.core.Name](unqualifiedFreeVars)(g.typeVariables)
      hydra.lib.logic.ifElse[Option[hydra.core.Type]](hydra.lib.sets.`null`[hydra.core.Name](unresolvedVars))(Some(dom))(None)
    })
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data, hydra.ext.scala.syntax.Data](hydra.ext.scala.coder.encodeTerm(cx)(g)(body))((sbody: hydra.ext.scala.syntax.Data) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Option[hydra.ext.scala.syntax.Type],
         hydra.ext.scala.syntax.Data](hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error],
         Option[hydra.ext.scala.syntax.Type]], hydra.core.Type](hydra.ext.scala.coder.findSdom(cx)(g)(meta))((dom: hydra.core.Type) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Type,
         Option[hydra.ext.scala.syntax.Type]](hydra.ext.scala.coder.encodeType(cx)(g)(dom))((sdom: hydra.ext.scala.syntax.Type) => Right(Some(sdom))))(mdom))((sdom: Option[hydra.ext.scala.syntax.Type]) => Right(hydra.ext.scala.utils.slambda(v)(sbody)(sdom))))
  }
  case hydra.core.Function.primitive(v_Function_primitive_name) => Right(hydra.ext.scala.utils.sprim(v_Function_primitive_name))
  case hydra.core.Function.elimination(v_Function_elimination_e) => v_Function_elimination_e match
    case hydra.core.Elimination.wrap(v_Elimination_wrap_name) => hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error],
       hydra.ext.scala.syntax.Data], hydra.core.Term](hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error],
       Option[hydra.ext.scala.syntax.Type], hydra.ext.scala.syntax.Data](hydra.ext.scala.coder.findSdom(cx)(g)(meta))((sdom: Option[hydra.ext.scala.syntax.Type]) =>
      Right(hydra.ext.scala.utils.slambda("x")(hydra.ext.scala.utils.sname("x"))(sdom))))((a: hydra.core.Term) => hydra.ext.scala.coder.encodeTerm(cx)(g)(a))(arg)
    case hydra.core.Elimination.record(v_Elimination_record_proj) => {
      lazy val fname: scala.Predef.String = hydra.ext.scala.utils.scalaEscapeName(v_Elimination_record_proj.field)
      lazy val typeName: hydra.core.Name = (v_Elimination_record_proj.typeName)
      lazy val pv: scala.Predef.String = "x"
      hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data],
         hydra.core.Term](hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Option[hydra.ext.scala.syntax.Type],
         hydra.ext.scala.syntax.Data](hydra.lib.eithers.either[hydra.context.InContext[hydra.errors.Error],
         Option[hydra.ext.scala.syntax.Type], Either[hydra.context.InContext[hydra.errors.Error], Option[hydra.ext.scala.syntax.Type]]]((_x: hydra.context.InContext[hydra.errors.Error]) =>
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Type,
           Option[hydra.ext.scala.syntax.Type]](hydra.ext.scala.coder.encodeType(cx)(g)(hydra.core.Type.variable(typeName)))((st: hydra.ext.scala.syntax.Type) => Right(Some(st))))((msdom: Option[hydra.ext.scala.syntax.Type]) =>
        hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], Option[hydra.ext.scala.syntax.Type]],
           hydra.ext.scala.syntax.Type](hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error],
           hydra.ext.scala.syntax.Type, Option[hydra.ext.scala.syntax.Type]](hydra.ext.scala.coder.encodeType(cx)(g)(hydra.core.Type.variable(typeName)))((st: hydra.ext.scala.syntax.Type) => Right(Some(st))))((sdom: hydra.ext.scala.syntax.Type) => Right(Some(sdom)))(msdom))(hydra.ext.scala.coder.findSdom(cx)(g)(meta)))((msdom: Option[hydra.ext.scala.syntax.Type]) =>
        Right(hydra.ext.scala.utils.slambda(pv)(hydra.ext.scala.syntax.Data.ref(hydra.ext.scala.syntax.Data_Ref.select(hydra.ext.scala.syntax.Data_Select(hydra.ext.scala.utils.sname(pv),
           hydra.ext.scala.syntax.Data_Name(fname)))))(msdom))))((a: hydra.core.Term) =>
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data,
           hydra.ext.scala.syntax.Data](hydra.ext.scala.coder.encodeTerm(cx)(g)(a))((sa: hydra.ext.scala.syntax.Data) =>
        Right(hydra.ext.scala.syntax.Data.ref(hydra.ext.scala.syntax.Data_Ref.select(hydra.ext.scala.syntax.Data_Select(sa,
           hydra.ext.scala.syntax.Data_Name(fname)))))))(arg)
    }
    case hydra.core.Elimination.union(v_Elimination_union_cs) => {
      lazy val v: scala.Predef.String = "v"
      lazy val tname: hydra.core.Name = (v_Elimination_union_cs.typeName)
      lazy val dom: hydra.core.Type = hydra.core.Type.variable(tname)
      lazy val sn: Option[hydra.core.Name] = hydra.ext.scala.utils.nameOfType(g)(dom)
      lazy val cases: Seq[hydra.core.Field] = (v_Elimination_union_cs.cases)
      lazy val dflt: Option[hydra.core.Term] = (v_Elimination_union_cs.default)
      lazy val ftypes: Map[hydra.core.Name, hydra.core.Type] = hydra.lib.eithers.either[hydra.context.InContext[hydra.errors.Error],
         Map[hydra.core.Name, hydra.core.Type], Map[hydra.core.Name, hydra.core.Type]]((_x: hydra.context.InContext[hydra.errors.Error]) => hydra.lib.maps.empty[hydra.core.Name,
         hydra.core.Type])((`x_`: Map[hydra.core.Name, hydra.core.Type]) => `x_`)(hydra.resolution.fieldTypes(cx)(g)(dom))
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.scala.syntax.Case],
         hydra.ext.scala.syntax.Data](hydra.lib.eithers.mapList[hydra.core.Field, hydra.ext.scala.syntax.Case,
         hydra.context.InContext[hydra.errors.Error]]((f: hydra.core.Field) => hydra.ext.scala.coder.encodeCase(cx)(g)(ftypes)(sn)(f))(cases))((fieldCases: Seq[hydra.ext.scala.syntax.Case]) =>
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.scala.syntax.Case],
           hydra.ext.scala.syntax.Data](hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error],
           Seq[hydra.ext.scala.syntax.Case]], hydra.core.Term](Right(fieldCases))((dfltTerm: hydra.core.Term) =>
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data,
           Seq[hydra.ext.scala.syntax.Case]](hydra.ext.scala.coder.encodeTerm(cx)(g)(dfltTerm))((sdflt: hydra.ext.scala.syntax.Data) =>
        Right(hydra.lib.lists.concat2[hydra.ext.scala.syntax.Case](fieldCases)(Seq(hydra.ext.scala.syntax.Case(hydra.ext.scala.syntax.Pat.wildcard,
           None, sdflt))))))(dflt))((scases: Seq[hydra.ext.scala.syntax.Case]) =>
        hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data],
           hydra.core.Term](hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Option[hydra.ext.scala.syntax.Type],
           hydra.ext.scala.syntax.Data](hydra.ext.scala.coder.findSdom(cx)(g)(meta))((sdom: Option[hydra.ext.scala.syntax.Type]) =>
        Right(hydra.ext.scala.utils.slambda(v)(hydra.ext.scala.syntax.Data.`match`(hydra.ext.scala.syntax.Data_Match(hydra.ext.scala.utils.sname(v),
           scases)))(sdom))))((a: hydra.core.Term) =>
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data,
           hydra.ext.scala.syntax.Data](hydra.ext.scala.coder.encodeTerm(cx)(g)(a))((sa: hydra.ext.scala.syntax.Data) =>
        Right(hydra.ext.scala.syntax.Data.`match`(hydra.ext.scala.syntax.Data_Match(sa, scases)))))(arg)))
    }
    case _ => Left(hydra.context.InContext(hydra.errors.Error.other("unsupported elimination"), cx))
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other("unsupported function"), cx))

def encodeLetBinding(cx: hydra.context.Context)(g: hydra.graph.Graph)(outerTypeVars: scala.collection.immutable.Set[hydra.core.Name])(b: hydra.core.Binding): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.ext.scala.syntax.Stat] =
  {
  lazy val bname: scala.Predef.String = hydra.ext.scala.utils.scalaEscapeName(b.name)
  lazy val bterm: hydra.core.Term = (b.term)
  lazy val mts: Option[hydra.core.TypeScheme] = hydra.lib.maybes.maybe[Option[hydra.core.TypeScheme],
     hydra.core.TypeScheme](hydra.lib.maps.lookup[hydra.core.Name, hydra.core.TypeScheme](b.name)(g.boundTypes))((ts: hydra.core.TypeScheme) => Some(ts))(b.`type`)
  lazy val isFn: Boolean = hydra.lib.maybes.maybe[Boolean, hydra.core.TypeScheme](false)((ts: hydra.core.TypeScheme) =>
    hydra.strip.deannotateType(ts.`type`) match
    case hydra.core.Type.function(v_Type_function__) => true
    case hydra.core.Type.forall(v_Type_forall_fa) => hydra.strip.deannotateType(v_Type_forall_fa.body) match
      case hydra.core.Type.function(v_Type_function__) => true
      case _ => false
    case _ => false)(mts)
  hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Stat],
     hydra.core.TypeScheme](hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data,
     hydra.ext.scala.syntax.Stat](hydra.ext.scala.coder.encodeTerm(cx)(g)(bterm))((srhs: hydra.ext.scala.syntax.Data) =>
    Right(hydra.ext.scala.syntax.Stat.defn(hydra.ext.scala.syntax.Defn.`val`(hydra.ext.scala.syntax.Defn_Val(Seq(hydra.ext.scala.syntax.Mod.`lazy`),
       Seq(hydra.ext.scala.syntax.Pat.`var`(hydra.ext.scala.syntax.Pat_Var(hydra.ext.scala.syntax.Data_Name(bname)))),
       None, srhs))))))((ts: hydra.core.TypeScheme) =>
    {
    lazy val newVars: Seq[hydra.core.Name] = hydra.lib.lists.filter[hydra.core.Name]((v: hydra.core.Name) =>
      hydra.lib.logic.not(hydra.lib.sets.member[hydra.core.Name](v)(outerTypeVars)))(ts.variables)
    lazy val useDef: Boolean = hydra.lib.logic.or(isFn)(hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.core.Name](newVars)))
    hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Stat]](useDef)(hydra.ext.scala.coder.encodeLocalDef(cx)(g)(outerTypeVars)(bname)(bterm)(ts.`type`))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error],
       hydra.ext.scala.syntax.Data, hydra.ext.scala.syntax.Stat](hydra.ext.scala.coder.encodeTerm(cx)(g)(bterm))((srhs: hydra.ext.scala.syntax.Data) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Type,
         hydra.ext.scala.syntax.Stat](hydra.ext.scala.coder.encodeType(cx)(g)(ts.`type`))((styp: hydra.ext.scala.syntax.Type) =>
      Right(hydra.ext.scala.syntax.Stat.defn(hydra.ext.scala.syntax.Defn.`val`(hydra.ext.scala.syntax.Defn_Val(Seq(hydra.ext.scala.syntax.Mod.`lazy`),
         Seq(hydra.ext.scala.syntax.Pat.`var`(hydra.ext.scala.syntax.Pat_Var(hydra.ext.scala.syntax.Data_Name(bname)))),
         Some(styp), srhs)))))))
  })(mts)
}

def encodeLiteral[T0](cx: hydra.context.Context)(g: T0)(av: hydra.core.Literal): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.ext.scala.syntax.Lit] =
  av match
  case hydra.core.Literal.binary(v_Literal_binary_b) => Right(hydra.ext.scala.syntax.Lit.bytes(hydra.lib.literals.binaryToBytes(v_Literal_binary_b)))
  case hydra.core.Literal.boolean(v_Literal_boolean_b) => Right(hydra.ext.scala.syntax.Lit.boolean(v_Literal_boolean_b))
  case hydra.core.Literal.float(v_Literal_float_fv) => v_Literal_float_fv match
    case hydra.core.FloatValue.bigfloat(v_FloatValue_bigfloat_bf) => Right(hydra.ext.scala.syntax.Lit.double(hydra.lib.literals.bigfloatToFloat64(v_FloatValue_bigfloat_bf)))
    case hydra.core.FloatValue.float32(v_FloatValue_float32_f) => Right(hydra.ext.scala.syntax.Lit.float(v_FloatValue_float32_f))
    case hydra.core.FloatValue.float64(v_FloatValue_float64_f) => Right(hydra.ext.scala.syntax.Lit.double(v_FloatValue_float64_f))
    case _ => Left(hydra.context.InContext(hydra.errors.Error.other("unexpected float value"), cx))
  case hydra.core.Literal.integer(v_Literal_integer_iv) => v_Literal_integer_iv match
    case hydra.core.IntegerValue.bigint(v_IntegerValue_bigint_i) => Right(hydra.ext.scala.syntax.Lit.long(hydra.lib.literals.bigintToInt64(v_IntegerValue_bigint_i)))
    case hydra.core.IntegerValue.int8(v_IntegerValue_int8_i) => Right(hydra.ext.scala.syntax.Lit.byte(v_IntegerValue_int8_i))
    case hydra.core.IntegerValue.int16(v_IntegerValue_int16_i) => Right(hydra.ext.scala.syntax.Lit.short(v_IntegerValue_int16_i))
    case hydra.core.IntegerValue.int32(v_IntegerValue_int32_i) => Right(hydra.ext.scala.syntax.Lit.int(v_IntegerValue_int32_i))
    case hydra.core.IntegerValue.int64(v_IntegerValue_int64_i) => Right(hydra.ext.scala.syntax.Lit.long(v_IntegerValue_int64_i))
    case hydra.core.IntegerValue.uint8(v_IntegerValue_uint8_i) => Right(hydra.ext.scala.syntax.Lit.byte(hydra.lib.literals.bigintToInt8(hydra.lib.literals.uint8ToBigint(v_IntegerValue_uint8_i))))
    case hydra.core.IntegerValue.uint16(v_IntegerValue_uint16_i) => Right(hydra.ext.scala.syntax.Lit.int(hydra.lib.literals.bigintToInt32(hydra.lib.literals.uint16ToBigint(v_IntegerValue_uint16_i))))
    case hydra.core.IntegerValue.uint32(v_IntegerValue_uint32_i) => Right(hydra.ext.scala.syntax.Lit.long(hydra.lib.literals.bigintToInt64(hydra.lib.literals.uint32ToBigint(v_IntegerValue_uint32_i))))
    case hydra.core.IntegerValue.uint64(v_IntegerValue_uint64_i) => Right(hydra.ext.scala.syntax.Lit.long(hydra.lib.literals.bigintToInt64(hydra.lib.literals.uint64ToBigint(v_IntegerValue_uint64_i))))
    case _ => Left(hydra.context.InContext(hydra.errors.Error.other("unexpected integer value"), cx))
  case hydra.core.Literal.string(v_Literal_string_s) => Right(hydra.ext.scala.syntax.Lit.string(v_Literal_string_s))
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other("unexpected literal"), cx))

def encodeLocalDef(cx: hydra.context.Context)(g: hydra.graph.Graph)(outerTypeVars: scala.collection.immutable.Set[hydra.core.Name])(lname: scala.Predef.String)(term: hydra.core.Term)(typ: hydra.core.Type): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.ext.scala.syntax.Stat] =
  {
  lazy val freeTypeVars: Seq[hydra.core.Name] = hydra.lib.lists.filter[hydra.core.Name]((v: hydra.core.Name) =>
    hydra.lib.logic.and(hydra.lib.logic.not(hydra.lib.lists.elem[Int](46)(hydra.lib.strings.toList(v))))(hydra.lib.logic.not(hydra.lib.sets.member[hydra.core.Name](v)(outerTypeVars))))(hydra.lib.sets.toList[hydra.core.Name](hydra.variables.freeVariablesInType(typ)))
  lazy val doms: Seq[hydra.core.Type] = hydra.ext.scala.coder.extractDomains(typ)
  lazy val paramNames: Seq[hydra.core.Name] = hydra.ext.scala.coder.extractParams(term)
  lazy val paramCount: Int = hydra.lib.math.min(hydra.lib.lists.length[hydra.core.Name](paramNames))(hydra.lib.lists.length[hydra.core.Type](doms))
  lazy val cod: hydra.core.Type = hydra.ext.scala.coder.dropDomains(paramCount)(typ)
  lazy val zippedParams: Seq[Tuple2[hydra.core.Name, hydra.core.Type]] = hydra.lib.lists.zip[hydra.core.Name,
     hydra.core.Type](hydra.lib.lists.take[hydra.core.Name](paramCount)(paramNames))(hydra.lib.lists.take[hydra.core.Type](paramCount)(doms))
  lazy val letBindings: Seq[hydra.core.Binding] = hydra.ext.scala.coder.extractLetBindings(term)
  lazy val tparams: Seq[hydra.ext.scala.syntax.Type_Param] = hydra.lib.lists.map[hydra.core.Name, hydra.ext.scala.syntax.Type_Param]((tv: hydra.core.Name) => hydra.ext.scala.utils.stparam(tv))(freeTypeVars)
  lazy val allTypeVars: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.sets.union[hydra.core.Name](outerTypeVars)(hydra.lib.sets.fromList[hydra.core.Name](freeTypeVars))
  lazy val gWithTypeVars: hydra.graph.Graph = hydra.graph.Graph(g.boundTerms, (g.boundTypes), (g.classConstraints),
     (g.lambdaVariables), (g.metadata), (g.primitives), (g.schemaTypes), hydra.lib.sets.union[hydra.core.Name](allTypeVars)(g.typeVariables))
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.scala.syntax.Data_Param],
     hydra.ext.scala.syntax.Stat](hydra.lib.eithers.mapList[Tuple2[hydra.core.Name, hydra.core.Type],
     hydra.ext.scala.syntax.Data_Param, hydra.context.InContext[hydra.errors.Error]]((v1: Tuple2[hydra.core.Name,
     hydra.core.Type]) =>
    hydra.ext.scala.coder.encodeTypedParam(cx)(gWithTypeVars)(v1))(zippedParams))((sparams: Seq[hydra.ext.scala.syntax.Data_Param]) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data, hydra.ext.scala.syntax.Stat](hydra.ext.scala.coder.encodeTerm(cx)(gWithTypeVars)(hydra.ext.scala.coder.extractBody(term)))((sbody: hydra.ext.scala.syntax.Data) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.Stat](hydra.ext.scala.coder.encodeType(cx)(gWithTypeVars)(cod))((scod: hydra.ext.scala.syntax.Type) =>
    {
    lazy val gForLets: hydra.graph.Graph = hydra.lib.logic.ifElse[hydra.graph.Graph](hydra.lib.lists.`null`[hydra.core.Binding](letBindings))(gWithTypeVars)(hydra.scoping.extendGraphForLet((g2: hydra.graph.Graph) =>
      (b: hydra.core.Binding) =>
      hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.predicates.isComplexBinding(g2)(b))(Some(hydra.core.Term.literal(hydra.core.Literal.boolean(true))))(None))(gWithTypeVars)(hydra.core.Let(letBindings,
         hydra.core.Term.variable("dummy"))))
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.scala.syntax.Stat],
       hydra.ext.scala.syntax.Stat](hydra.lib.eithers.mapList[hydra.core.Binding, hydra.ext.scala.syntax.Stat,
       hydra.context.InContext[hydra.errors.Error]]((v1: hydra.core.Binding) =>
      hydra.ext.scala.coder.encodeLetBinding(cx)(gForLets)(allTypeVars)(v1))(letBindings))((sbindings: Seq[hydra.ext.scala.syntax.Stat]) =>
      {
      lazy val defBody: hydra.ext.scala.syntax.Data = hydra.lib.logic.ifElse[hydra.ext.scala.syntax.Data](hydra.lib.lists.`null`[hydra.ext.scala.syntax.Stat](sbindings))(sbody)(hydra.ext.scala.syntax.Data.block(hydra.ext.scala.syntax.Data_Block(hydra.lib.lists.concat2[hydra.ext.scala.syntax.Stat](sbindings)(Seq(hydra.ext.scala.syntax.Stat.term(sbody))))))
      Right(hydra.ext.scala.syntax.Stat.defn(hydra.ext.scala.syntax.Defn.`def`(hydra.ext.scala.syntax.Defn_Def(Seq(),
         hydra.ext.scala.syntax.Data_Name(lname), tparams, hydra.lib.lists.map[hydra.ext.scala.syntax.Data_Param,
         Seq[hydra.ext.scala.syntax.Data_Param]]((p: hydra.ext.scala.syntax.Data_Param) => Seq(p))(sparams),
         Some(scod), defBody))))
    })
  })))
}

def encodeTerm(cx: hydra.context.Context)(g: hydra.graph.Graph)(term0: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.ext.scala.syntax.Data] =
  {
  lazy val term: hydra.core.Term = hydra.ext.scala.coder.stripWrapEliminations(term0)
  hydra.strip.deannotateTerm(term) match
    case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => {
      def collectTypeArgs(t: hydra.core.Term)(acc: Seq[hydra.core.Type]): Tuple2[Seq[hydra.core.Type], hydra.core.Term] =
        hydra.strip.deannotateTerm(t) match
        case hydra.core.Term.typeApplication(v_Term_typeApplication_ta2) => collectTypeArgs(v_Term_typeApplication_ta2.body)(hydra.lib.lists.cons[hydra.core.Type](v_Term_typeApplication_ta2.`type`)(acc))
        case _ => Tuple2(acc, t)
      lazy val collected: Tuple2[Seq[hydra.core.Type], hydra.core.Term] = collectTypeArgs(v_Term_typeApplication_ta.body)(Seq(v_Term_typeApplication_ta.`type`))
      lazy val typeArgs: Seq[hydra.core.Type] = hydra.lib.pairs.first[Seq[hydra.core.Type], hydra.core.Term](collected)
      lazy val innerTerm: hydra.core.Term = hydra.lib.pairs.second[Seq[hydra.core.Type], hydra.core.Term](collected)
      def collectTypeLambdas(t: hydra.core.Term)(acc: Seq[hydra.core.Name]): Tuple2[Seq[hydra.core.Name], hydra.core.Term] =
        hydra.strip.deannotateTerm(t) match
        case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => collectTypeLambdas(v_Term_typeLambda_tl.body)(hydra.lib.lists.cons[hydra.core.Name](v_Term_typeLambda_tl.parameter)(acc))
        case _ => Tuple2(acc, t)
      lazy val tlCollected: Tuple2[Seq[hydra.core.Name], hydra.core.Term] = collectTypeLambdas(innerTerm)(Seq())
      lazy val typeParams: Seq[hydra.core.Name] = hydra.lib.pairs.first[Seq[hydra.core.Name], hydra.core.Term](tlCollected)
      lazy val bodyAfterTypeLambdas: hydra.core.Term = hydra.lib.pairs.second[Seq[hydra.core.Name], hydra.core.Term](tlCollected)
      lazy val substitutedBody: hydra.core.Term = bodyAfterTypeLambdas
      hydra.strip.deannotateTerm(substitutedBody) match
        case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
          case hydra.core.Function.primitive(v_Function_primitive_pname) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error],
             Seq[hydra.ext.scala.syntax.Type], hydra.ext.scala.syntax.Data](hydra.lib.eithers.mapList[hydra.core.Type,
             hydra.ext.scala.syntax.Type, hydra.context.InContext[hydra.errors.Error]]((targ: hydra.core.Type) => hydra.ext.scala.coder.encodeType(cx)(g)(targ))(typeArgs))((stypeArgs: Seq[hydra.ext.scala.syntax.Type]) =>
            {
            lazy val inScopeTypeVarNames: scala.collection.immutable.Set[scala.Predef.String] = hydra.lib.sets.fromList[scala.Predef.String](hydra.lib.lists.map[hydra.core.Name,
               scala.Predef.String]((n: hydra.core.Name) => hydra.formatting.capitalize(n))(hydra.lib.sets.toList[hydra.core.Name](g.typeVariables)))
            lazy val hasForallResidual: Boolean = hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.ext.scala.syntax.Type](hydra.lib.lists.filter[hydra.ext.scala.syntax.Type]((st: hydra.ext.scala.syntax.Type) =>
              st match
              case hydra.ext.scala.syntax.Type.`var`(v_Type_var_tv) => {
                lazy val tvName: scala.Predef.String = (v_Type_var_tv.name.value)
                hydra.lib.logic.and(hydra.lib.logic.not(hydra.lib.lists.elem[Int](46)(hydra.lib.strings.toList(tvName))))(hydra.lib.logic.not(hydra.lib.sets.member[scala.Predef.String](tvName)(inScopeTypeVarNames)))
              }
              case _ => false)(stypeArgs)))
            hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data]](hasForallResidual)(Right(hydra.ext.scala.utils.sprim(v_Function_primitive_pname)))(Right(hydra.ext.scala.utils.sapplyTypes(hydra.ext.scala.utils.sprim(v_Function_primitive_pname))(stypeArgs)))
          })
          case hydra.core.Function.elimination(v_Function_elimination__) => hydra.ext.scala.coder.encodeTerm(cx)(g)(substitutedBody)
          case _ => hydra.ext.scala.coder.encodeTerm(cx)(g)(substitutedBody)
        case hydra.core.Term.variable(v_Term_variable_pname) => hydra.lib.maybes.cases[hydra.graph.Primitive,
           Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data]](hydra.lib.maps.lookup[hydra.core.Name,
           hydra.graph.Primitive](v_Term_variable_pname)(g.primitives))(hydra.ext.scala.coder.encodeTerm(cx)(g)(substitutedBody))((_prim: hydra.graph.Primitive) =>
          hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.scala.syntax.Type],
             hydra.ext.scala.syntax.Data](hydra.lib.eithers.mapList[hydra.core.Type, hydra.ext.scala.syntax.Type,
             hydra.context.InContext[hydra.errors.Error]]((targ: hydra.core.Type) => hydra.ext.scala.coder.encodeType(cx)(g)(targ))(typeArgs))((stypeArgs: Seq[hydra.ext.scala.syntax.Type]) =>
          {
          lazy val inScopeTypeVarNames: scala.collection.immutable.Set[scala.Predef.String] = hydra.lib.sets.fromList[scala.Predef.String](hydra.lib.lists.map[hydra.core.Name,
             scala.Predef.String]((n: hydra.core.Name) => hydra.formatting.capitalize(n))(hydra.lib.sets.toList[hydra.core.Name](g.typeVariables)))
          lazy val hasForallResidual: Boolean = hydra.lib.logic.not(hydra.lib.lists.`null`[hydra.ext.scala.syntax.Type](hydra.lib.lists.filter[hydra.ext.scala.syntax.Type]((st: hydra.ext.scala.syntax.Type) =>
            st match
            case hydra.ext.scala.syntax.Type.`var`(v_Type_var_tv) => {
              lazy val tvName: scala.Predef.String = (v_Type_var_tv.name.value)
              hydra.lib.logic.and(hydra.lib.logic.not(hydra.lib.lists.elem[Int](46)(hydra.lib.strings.toList(tvName))))(hydra.lib.logic.not(hydra.lib.sets.member[scala.Predef.String](tvName)(inScopeTypeVarNames)))
            }
            case _ => false)(stypeArgs)))
          hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data]](hasForallResidual)(Right(hydra.ext.scala.utils.sprim(v_Term_variable_pname)))(Right(hydra.ext.scala.utils.sapplyTypes(hydra.ext.scala.utils.sprim(v_Term_variable_pname))(stypeArgs)))
        }))
        case _ => hydra.ext.scala.coder.encodeTerm(cx)(g)(substitutedBody)
    }
    case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.ext.scala.coder.encodeTerm(cx)(hydra.scoping.extendGraphForTypeLambda(g)(v_Term_typeLambda_tl))(v_Term_typeLambda_tl.body)
    case hydra.core.Term.application(v_Term_application_app) => {
      lazy val fun: hydra.core.Term = (v_Term_application_app.function)
      lazy val arg: hydra.core.Term = (v_Term_application_app.argument)
      hydra.strip.deannotateAndDetypeTerm(fun) match
        case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
          case hydra.core.Function.lambda(v_Function_lambda_lam) => {
            lazy val lamBody: hydra.core.Term = (v_Function_lambda_lam.body)
            hydra.strip.deannotateAndDetypeTerm(lamBody) match
              case hydra.core.Term.application(v_Term_application_innerApp) => {
                lazy val innerFun: hydra.core.Term = (v_Term_application_innerApp.function)
                hydra.strip.deannotateAndDetypeTerm(innerFun) match
                  case hydra.core.Term.function(v_Term_function_innerF) => v_Term_function_innerF match
                    case hydra.core.Function.elimination(v_Function_elimination_innerE) => v_Function_elimination_innerE match
                      case hydra.core.Elimination.union(v_Elimination_union__) => hydra.ext.scala.coder.encodeFunction(cx)(g)(hydra.annotations.termAnnotationInternal(innerFun))(v_Term_function_innerF)(Some(arg))
                      case _ => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data,
                         hydra.ext.scala.syntax.Data](hydra.ext.scala.coder.encodeTerm(cx)(g)(fun))((sfun: hydra.ext.scala.syntax.Data) =>
                        hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data,
                           hydra.ext.scala.syntax.Data](hydra.ext.scala.coder.encodeTerm(cx)(g)(arg))((sarg: hydra.ext.scala.syntax.Data) => Right(hydra.ext.scala.utils.sapply(sfun)(Seq(sarg)))))
                    case _ => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data,
                       hydra.ext.scala.syntax.Data](hydra.ext.scala.coder.encodeTerm(cx)(g)(fun))((sfun: hydra.ext.scala.syntax.Data) =>
                      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data,
                         hydra.ext.scala.syntax.Data](hydra.ext.scala.coder.encodeTerm(cx)(g)(arg))((sarg: hydra.ext.scala.syntax.Data) => Right(hydra.ext.scala.utils.sapply(sfun)(Seq(sarg)))))
                  case _ => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data,
                     hydra.ext.scala.syntax.Data](hydra.ext.scala.coder.encodeTerm(cx)(g)(fun))((sfun: hydra.ext.scala.syntax.Data) =>
                    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data,
                       hydra.ext.scala.syntax.Data](hydra.ext.scala.coder.encodeTerm(cx)(g)(arg))((sarg: hydra.ext.scala.syntax.Data) => Right(hydra.ext.scala.utils.sapply(sfun)(Seq(sarg)))))
              }
              case _ => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data,
                 hydra.ext.scala.syntax.Data](hydra.ext.scala.coder.encodeTerm(cx)(g)(fun))((sfun: hydra.ext.scala.syntax.Data) =>
                hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data,
                   hydra.ext.scala.syntax.Data](hydra.ext.scala.coder.encodeTerm(cx)(g)(arg))((sarg: hydra.ext.scala.syntax.Data) => Right(hydra.ext.scala.utils.sapply(sfun)(Seq(sarg)))))
          }
          case hydra.core.Function.elimination(v_Function_elimination_e) => v_Function_elimination_e match
            case hydra.core.Elimination.record(v_Elimination_record_proj) => {
              lazy val fname: scala.Predef.String = hydra.ext.scala.utils.scalaEscapeName(v_Elimination_record_proj.field)
              hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data,
                 hydra.ext.scala.syntax.Data](hydra.ext.scala.coder.encodeTerm(cx)(g)(arg))((sarg: hydra.ext.scala.syntax.Data) =>
                Right(hydra.ext.scala.syntax.Data.ref(hydra.ext.scala.syntax.Data_Ref.select(hydra.ext.scala.syntax.Data_Select(sarg,
                   hydra.ext.scala.syntax.Data_Name(fname))))))
            }
            case hydra.core.Elimination.union(v_Elimination_union__) => hydra.ext.scala.coder.encodeFunction(cx)(g)(hydra.annotations.termAnnotationInternal(fun))(v_Term_function_f)(Some(arg))
            case _ => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data,
               hydra.ext.scala.syntax.Data](hydra.ext.scala.coder.encodeTerm(cx)(g)(fun))((sfun: hydra.ext.scala.syntax.Data) =>
              hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data,
                 hydra.ext.scala.syntax.Data](hydra.ext.scala.coder.encodeTerm(cx)(g)(arg))((sarg: hydra.ext.scala.syntax.Data) => Right(hydra.ext.scala.utils.sapply(sfun)(Seq(sarg)))))
          case _ => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data,
             hydra.ext.scala.syntax.Data](hydra.ext.scala.coder.encodeTerm(cx)(g)(fun))((sfun: hydra.ext.scala.syntax.Data) =>
            hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data,
               hydra.ext.scala.syntax.Data](hydra.ext.scala.coder.encodeTerm(cx)(g)(arg))((sarg: hydra.ext.scala.syntax.Data) => Right(hydra.ext.scala.utils.sapply(sfun)(Seq(sarg)))))
        case _ => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data,
           hydra.ext.scala.syntax.Data](hydra.ext.scala.coder.encodeTerm(cx)(g)(fun))((sfun: hydra.ext.scala.syntax.Data) =>
          hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data,
             hydra.ext.scala.syntax.Data](hydra.ext.scala.coder.encodeTerm(cx)(g)(arg))((sarg: hydra.ext.scala.syntax.Data) => Right(hydra.ext.scala.utils.sapply(sfun)(Seq(sarg)))))
    }
    case hydra.core.Term.function(v_Term_function_f) => hydra.ext.scala.coder.encodeFunction(cx)(g)(hydra.annotations.termAnnotationInternal(term))(v_Term_function_f)(None)
    case hydra.core.Term.list(v_Term_list_els) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error],
       Seq[hydra.ext.scala.syntax.Data], hydra.ext.scala.syntax.Data](hydra.lib.eithers.mapList[hydra.core.Term,
       hydra.ext.scala.syntax.Data, hydra.context.InContext[hydra.errors.Error]]((e: hydra.core.Term) => hydra.ext.scala.coder.encodeTerm(cx)(g)(e))(v_Term_list_els))((sels: Seq[hydra.ext.scala.syntax.Data]) =>
      Right(hydra.ext.scala.utils.sapply(hydra.ext.scala.utils.sname("Seq"))(sels)))
    case hydra.core.Term.literal(v_Term_literal_v) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error],
       hydra.ext.scala.syntax.Lit, hydra.ext.scala.syntax.Data](hydra.ext.scala.coder.encodeLiteral(cx)(g)(v_Term_literal_v))((slit: hydra.ext.scala.syntax.Lit) =>
      {
      lazy val litData: hydra.ext.scala.syntax.Data = hydra.ext.scala.syntax.Data.lit(slit)
      v_Term_literal_v match
        case hydra.core.Literal.integer(v_Literal_integer_iv) => v_Literal_integer_iv match
          case hydra.core.IntegerValue.bigint(v_IntegerValue_bigint_bi) => Right(hydra.ext.scala.utils.sapply(hydra.ext.scala.utils.sname("BigInt"))(Seq(hydra.ext.scala.syntax.Data.lit(hydra.ext.scala.syntax.Lit.string(hydra.lib.literals.showBigint(v_IntegerValue_bigint_bi))))))
          case hydra.core.IntegerValue.uint64(v_IntegerValue_uint64_ui) => Right(hydra.ext.scala.utils.sapply(hydra.ext.scala.utils.sname("BigInt"))(Seq(hydra.ext.scala.syntax.Data.lit(hydra.ext.scala.syntax.Lit.string(hydra.lib.literals.showBigint(hydra.lib.literals.uint64ToBigint(v_IntegerValue_uint64_ui)))))))
          case _ => Right(litData)
        case hydra.core.Literal.float(v_Literal_float_fv) => v_Literal_float_fv match
          case hydra.core.FloatValue.bigfloat(v_FloatValue_bigfloat__) => Right(hydra.ext.scala.utils.sapply(hydra.ext.scala.utils.sname("BigDecimal"))(Seq(litData)))
          case _ => Right(litData)
        case _ => Right(litData)
    })
    case hydra.core.Term.map(v_Term_map_m) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error],
       Seq[hydra.ext.scala.syntax.Data], hydra.ext.scala.syntax.Data](hydra.lib.eithers.mapList[Tuple2[hydra.core.Term,
       hydra.core.Term], hydra.ext.scala.syntax.Data, hydra.context.InContext[hydra.errors.Error]]((kv: Tuple2[hydra.core.Term,
       hydra.core.Term]) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data,
         hydra.ext.scala.syntax.Data](hydra.ext.scala.coder.encodeTerm(cx)(g)(hydra.lib.pairs.first[hydra.core.Term,
         hydra.core.Term](kv)))((sk: hydra.ext.scala.syntax.Data) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data,
         hydra.ext.scala.syntax.Data](hydra.ext.scala.coder.encodeTerm(cx)(g)(hydra.lib.pairs.second[hydra.core.Term,
         hydra.core.Term](kv)))((sv: hydra.ext.scala.syntax.Data) => Right(hydra.ext.scala.utils.sassign(sk)(sv)))))(hydra.lib.maps.toList[hydra.core.Term,
         hydra.core.Term](v_Term_map_m)))((spairs: Seq[hydra.ext.scala.syntax.Data]) =>
      Right(hydra.ext.scala.utils.sapply(hydra.ext.scala.utils.sname("Map"))(spairs)))
    case hydra.core.Term.wrap(v_Term_wrap_wt) => hydra.ext.scala.coder.encodeTerm(cx)(g)(v_Term_wrap_wt.body)
    case hydra.core.Term.maybe(v_Term_maybe_m) => hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error],
       hydra.ext.scala.syntax.Data], hydra.core.Term](Right(hydra.ext.scala.utils.sname("None")))((t: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data,
         hydra.ext.scala.syntax.Data](hydra.ext.scala.coder.encodeTerm(cx)(g)(t))((s: hydra.ext.scala.syntax.Data) =>
      Right(hydra.ext.scala.utils.sapply(hydra.ext.scala.utils.sname("Some"))(Seq(s)))))(v_Term_maybe_m)
    case hydra.core.Term.record(v_Term_record_rec) => {
      lazy val rname: hydra.core.Name = (v_Term_record_rec.typeName)
      lazy val fields: Seq[hydra.core.Field] = (v_Term_record_rec.fields)
      lazy val n: scala.Predef.String = hydra.ext.scala.utils.scalaTypeName(true)(rname)
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.scala.syntax.Data],
         hydra.ext.scala.syntax.Data](hydra.lib.eithers.mapList[hydra.core.Field, hydra.ext.scala.syntax.Data,
         hydra.context.InContext[hydra.errors.Error]]((f: hydra.core.Field) => hydra.ext.scala.coder.encodeTerm(cx)(g)(f.term))(fields))((args: Seq[hydra.ext.scala.syntax.Data]) =>
        Right(hydra.ext.scala.utils.sapply(hydra.ext.scala.utils.sname(n))(args)))
    }
    case hydra.core.Term.set(v_Term_set_s) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error],
       Seq[hydra.ext.scala.syntax.Data], hydra.ext.scala.syntax.Data](hydra.lib.eithers.mapList[hydra.core.Term,
       hydra.ext.scala.syntax.Data, hydra.context.InContext[hydra.errors.Error]]((e: hydra.core.Term) => hydra.ext.scala.coder.encodeTerm(cx)(g)(e))(hydra.lib.sets.toList[hydra.core.Term](v_Term_set_s)))((sels: Seq[hydra.ext.scala.syntax.Data]) =>
      Right(hydra.ext.scala.utils.sapply(hydra.ext.scala.utils.sname("scala.collection.immutable.Set"))(sels)))
    case hydra.core.Term.union(v_Term_union_inj) => {
      lazy val sn: hydra.core.Name = (v_Term_union_inj.typeName)
      lazy val fn: hydra.core.Name = (v_Term_union_inj.field.name)
      lazy val ft: hydra.core.Term = (v_Term_union_inj.field.term)
      lazy val lhs: hydra.ext.scala.syntax.Data = hydra.ext.scala.utils.sname(hydra.ext.scala.utils.qualifyUnionFieldName("UNION.")(Some(sn))(fn))
      lazy val unionFtypes: Map[hydra.core.Name, hydra.core.Type] = hydra.lib.eithers.either[hydra.context.InContext[hydra.errors.Error],
         Map[hydra.core.Name, hydra.core.Type], Map[hydra.core.Name, hydra.core.Type]]((_x: hydra.context.InContext[hydra.errors.Error]) => hydra.lib.maps.empty[hydra.core.Name,
         hydra.core.Type])((`x_`: Map[hydra.core.Name, hydra.core.Type]) => `x_`)(hydra.resolution.fieldTypes(cx)(g)(hydra.core.Type.variable(sn)))
      hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data]](hydra.lib.maybes.maybe[Boolean,
         hydra.core.Type](hydra.strip.deannotateAndDetypeTerm(ft) match
        case hydra.core.Term.unit => true
        case hydra.core.Term.record(v_Term_record_rec) => hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.Field](v_Term_record_rec.fields))(0)
        case _ => false)((dom: hydra.core.Type) =>
        hydra.strip.deannotateType(dom) match
        case hydra.core.Type.unit => true
        case hydra.core.Type.record(v_Type_record_rt) => hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.FieldType](v_Type_record_rt))(0)
        case _ => false)(hydra.lib.maps.lookup[hydra.core.Name, hydra.core.Type](fn)(unionFtypes)))(Right(lhs))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error],
           hydra.ext.scala.syntax.Data, hydra.ext.scala.syntax.Data](hydra.ext.scala.coder.encodeTerm(cx)(g)(ft))((sarg: hydra.ext.scala.syntax.Data) => Right(hydra.ext.scala.utils.sapply(lhs)(Seq(sarg)))))
    }
    case hydra.core.Term.variable(v_Term_variable_v) => {
      lazy val fullName: scala.Predef.String = v_Term_variable_v
      lazy val localName: scala.Predef.String = hydra.names.localNameOf(v_Term_variable_v)
      lazy val parts: Seq[scala.Predef.String] = hydra.lib.strings.splitOn(".")(fullName)
      lazy val numParts: Int = hydra.lib.lists.length[scala.Predef.String](parts)
      lazy val escaped: scala.Predef.String = hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.lte[Int](numParts)(1))(hydra.ext.scala.utils.scalaEscapeName(fullName))(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[Int](numParts)(2))(hydra.lib.strings.cat2(hydra.lib.lists.head[scala.Predef.String](parts))(hydra.lib.strings.cat2(".")(hydra.ext.scala.utils.scalaEscapeName(localName))))(hydra.lib.strings.intercalate(".")(hydra.lib.lists.concat2[scala.Predef.String](hydra.lib.lists.take[scala.Predef.String](hydra.lib.math.sub(numParts)(1))(parts))(Seq(hydra.ext.scala.utils.scalaEscapeName(localName))))))
      Right(hydra.ext.scala.utils.sname(escaped))
    }
    case hydra.core.Term.annotated(v_Term_annotated_at) => hydra.ext.scala.coder.encodeTerm(cx)(g)(v_Term_annotated_at.body)
    case hydra.core.Term.either(v_Term_either_e) => hydra.lib.eithers.either[hydra.core.Term, hydra.core.Term,
       Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data]]((l: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data,
         hydra.ext.scala.syntax.Data](hydra.ext.scala.coder.encodeTerm(cx)(g)(l))((sl: hydra.ext.scala.syntax.Data) =>
      Right(hydra.ext.scala.utils.sapply(hydra.ext.scala.utils.sname("Left"))(Seq(sl)))))((r: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data,
         hydra.ext.scala.syntax.Data](hydra.ext.scala.coder.encodeTerm(cx)(g)(r))((sr: hydra.ext.scala.syntax.Data) =>
      Right(hydra.ext.scala.utils.sapply(hydra.ext.scala.utils.sname("Right"))(Seq(sr)))))(v_Term_either_e)
    case hydra.core.Term.pair(v_Term_pair_p) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error],
       hydra.ext.scala.syntax.Data, hydra.ext.scala.syntax.Data](hydra.ext.scala.coder.encodeTerm(cx)(g)(hydra.lib.pairs.first[hydra.core.Term,
       hydra.core.Term](v_Term_pair_p)))((sf: hydra.ext.scala.syntax.Data) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data,
         hydra.ext.scala.syntax.Data](hydra.ext.scala.coder.encodeTerm(cx)(g)(hydra.lib.pairs.second[hydra.core.Term,
         hydra.core.Term](v_Term_pair_p)))((ss: hydra.ext.scala.syntax.Data) =>
      Right(hydra.ext.scala.utils.sapply(hydra.ext.scala.utils.sname("Tuple2"))(Seq(sf, ss)))))
    case hydra.core.Term.unit => Right(hydra.ext.scala.syntax.Data.lit(hydra.ext.scala.syntax.Lit.unit))
    case hydra.core.Term.let(v_Term_let_lt) => {
      lazy val bindings: Seq[hydra.core.Binding] = (v_Term_let_lt.bindings)
      lazy val body: hydra.core.Term = (v_Term_let_lt.body)
      lazy val gLet: hydra.graph.Graph = hydra.scoping.extendGraphForLet((g2: hydra.graph.Graph) =>
        (b: hydra.core.Binding) =>
        hydra.lib.logic.ifElse[Option[hydra.core.Term]](hydra.predicates.isComplexBinding(g2)(b))(Some(hydra.core.Term.literal(hydra.core.Literal.boolean(true))))(None))(g)(v_Term_let_lt)
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.scala.syntax.Stat],
         hydra.ext.scala.syntax.Data](hydra.lib.eithers.mapList[hydra.core.Binding, hydra.ext.scala.syntax.Stat,
         hydra.context.InContext[hydra.errors.Error]]((v1: hydra.core.Binding) =>
        hydra.ext.scala.coder.encodeLetBinding(cx)(gLet)(gLet.typeVariables)(v1))(bindings))((sbindings: Seq[hydra.ext.scala.syntax.Stat]) =>
        hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data,
           hydra.ext.scala.syntax.Data](hydra.ext.scala.coder.encodeTerm(cx)(gLet)(body))((sbody: hydra.ext.scala.syntax.Data) =>
        Right(hydra.ext.scala.syntax.Data.block(hydra.ext.scala.syntax.Data_Block(hydra.lib.lists.concat2[hydra.ext.scala.syntax.Stat](sbindings)(Seq(hydra.ext.scala.syntax.Stat.term(sbody))))))))
    }
    case _ => Left(hydra.context.InContext(hydra.errors.Error.other("unexpected term"), cx))
}

def encodeTermDefinition(cx: hydra.context.Context)(g: hydra.graph.Graph)(td: hydra.packaging.TermDefinition): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.ext.scala.syntax.Stat] =
  {
  lazy val name: hydra.core.Name = (td.name)
  lazy val term: hydra.core.Term = (td.term)
  lazy val lname: scala.Predef.String = hydra.ext.scala.utils.scalaEscapeName(hydra.names.localNameOf(name))
  lazy val `typ_`: hydra.core.Type = hydra.lib.maybes.maybe[hydra.core.Type, hydra.core.TypeScheme](hydra.core.Type.variable("hydra.core.Unit"))((x: hydra.core.TypeScheme) => (x.`type`))(td.`type`)
  lazy val isFunctionType: Boolean = hydra.strip.deannotateType(`typ_`) match
    case hydra.core.Type.function(v_Type_function__) => true
    case hydra.core.Type.forall(v_Type_forall_fa) => hydra.strip.deannotateType(v_Type_forall_fa.body) match
      case hydra.core.Type.function(v_Type_function__) => true
      case _ => false
    case _ => false
  hydra.lib.logic.ifElse[Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Stat]](isFunctionType)(hydra.ext.scala.coder.encodeComplexTermDef(cx)(g)(lname)(term)(`typ_`))(hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error],
     hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.Stat](hydra.ext.scala.coder.encodeType(cx)(g)(`typ_`))((stype: hydra.ext.scala.syntax.Type) =>
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Data, hydra.ext.scala.syntax.Stat](hydra.ext.scala.coder.encodeTerm(cx)(g)(term))((rhs: hydra.ext.scala.syntax.Data) =>
    Right(hydra.ext.scala.syntax.Stat.defn(hydra.ext.scala.syntax.Defn.`val`(hydra.ext.scala.syntax.Defn_Val(Seq(hydra.ext.scala.syntax.Mod.`lazy`),
       Seq(hydra.ext.scala.syntax.Pat.`var`(hydra.ext.scala.syntax.Pat_Var(hydra.ext.scala.syntax.Data_Name(lname)))),
       Some(stype), rhs)))))))
}

def encodeType[T0](cx: hydra.context.Context)(g: T0)(t: hydra.core.Type): Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Type] =
  hydra.strip.deannotateType(t) match
  case hydra.core.Type.application(v_Type_application_at) => {
    def collectTypeArgs(t2: hydra.core.Type)(acc: Seq[hydra.core.Type]): Tuple2[hydra.core.Type, Seq[hydra.core.Type]] =
      hydra.strip.deannotateType(t2) match
      case hydra.core.Type.application(v_Type_application_at2) => {
        lazy val f2: hydra.core.Type = (v_Type_application_at2.function)
        lazy val a2: hydra.core.Type = (v_Type_application_at2.argument)
        collectTypeArgs(f2)(hydra.lib.lists.cons[hydra.core.Type](a2)(acc))
      }
      case _ => Tuple2(t2, acc)
    lazy val collected: Tuple2[hydra.core.Type, Seq[hydra.core.Type]] = collectTypeArgs(hydra.core.Type.application(v_Type_application_at))(Seq())
    lazy val baseFun: hydra.core.Type = hydra.lib.pairs.first[hydra.core.Type, Seq[hydra.core.Type]](collected)
    lazy val allArgs: Seq[hydra.core.Type] = hydra.lib.pairs.second[hydra.core.Type, Seq[hydra.core.Type]](collected)
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.Type](hydra.ext.scala.coder.encodeType(cx)(g)(baseFun))((sfun: hydra.ext.scala.syntax.Type) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Seq[hydra.ext.scala.syntax.Type],
         hydra.ext.scala.syntax.Type](hydra.lib.eithers.mapList[hydra.core.Type, hydra.ext.scala.syntax.Type,
         hydra.context.InContext[hydra.errors.Error]]((a: hydra.core.Type) => hydra.ext.scala.coder.encodeType(cx)(g)(a))(allArgs))((sargs: Seq[hydra.ext.scala.syntax.Type]) => Right(hydra.ext.scala.utils.stapply(sfun)(sargs))))
  }
  case hydra.core.Type.unit => Right(hydra.ext.scala.syntax.Type.ref(hydra.ext.scala.syntax.Type_Ref.name(hydra.ext.scala.syntax.Type_Name("Unit"))))
  case hydra.core.Type.either(v_Type_either_et) => {
    lazy val lt: hydra.core.Type = (v_Type_either_et.left)
    lazy val rt: hydra.core.Type = (v_Type_either_et.right)
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.Type](hydra.ext.scala.coder.encodeType(cx)(g)(lt))((slt: hydra.ext.scala.syntax.Type) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Type,
         hydra.ext.scala.syntax.Type](hydra.ext.scala.coder.encodeType(cx)(g)(rt))((srt: hydra.ext.scala.syntax.Type) =>
      Right(hydra.ext.scala.utils.stapply2(hydra.ext.scala.syntax.Type.ref(hydra.ext.scala.syntax.Type_Ref.name(hydra.ext.scala.syntax.Type_Name("Either"))))(slt)(srt))))
  }
  case hydra.core.Type.function(v_Type_function_ft) => {
    lazy val dom: hydra.core.Type = (v_Type_function_ft.domain)
    lazy val cod: hydra.core.Type = (v_Type_function_ft.codomain)
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.Type](hydra.ext.scala.coder.encodeType(cx)(g)(dom))((sdom: hydra.ext.scala.syntax.Type) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Type,
         hydra.ext.scala.syntax.Type](hydra.ext.scala.coder.encodeType(cx)(g)(cod))((scod: hydra.ext.scala.syntax.Type) =>
      Right(hydra.ext.scala.syntax.Type.functionType(hydra.ext.scala.syntax.Type_FunctionType.function(hydra.ext.scala.syntax.Type_Function(Seq(sdom),
         scod))))))
  }
  case hydra.core.Type.list(v_Type_list_lt) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error],
     hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.Type](hydra.ext.scala.coder.encodeType(cx)(g)(v_Type_list_lt))((slt: hydra.ext.scala.syntax.Type) =>
    Right(hydra.ext.scala.utils.stapply1(hydra.ext.scala.syntax.Type.ref(hydra.ext.scala.syntax.Type_Ref.name(hydra.ext.scala.syntax.Type_Name("Seq"))))(slt)))
  case hydra.core.Type.literal(v_Type_literal_lt) => v_Type_literal_lt match
    case hydra.core.LiteralType.binary => Right(hydra.ext.scala.utils.stapply(hydra.ext.scala.syntax.Type.ref(hydra.ext.scala.syntax.Type_Ref.name(hydra.ext.scala.syntax.Type_Name("Array"))))(Seq(hydra.ext.scala.syntax.Type.ref(hydra.ext.scala.syntax.Type_Ref.name(hydra.ext.scala.syntax.Type_Name("Byte"))))))
    case hydra.core.LiteralType.boolean => Right(hydra.ext.scala.syntax.Type.ref(hydra.ext.scala.syntax.Type_Ref.name(hydra.ext.scala.syntax.Type_Name("Boolean"))))
    case hydra.core.LiteralType.float(v_LiteralType_float_ft) => v_LiteralType_float_ft match
      case hydra.core.FloatType.bigfloat => Right(hydra.ext.scala.syntax.Type.ref(hydra.ext.scala.syntax.Type_Ref.name(hydra.ext.scala.syntax.Type_Name("BigDecimal"))))
      case hydra.core.FloatType.float32 => Right(hydra.ext.scala.syntax.Type.ref(hydra.ext.scala.syntax.Type_Ref.name(hydra.ext.scala.syntax.Type_Name("Float"))))
      case hydra.core.FloatType.float64 => Right(hydra.ext.scala.syntax.Type.ref(hydra.ext.scala.syntax.Type_Ref.name(hydra.ext.scala.syntax.Type_Name("Double"))))
      case _ => Left(hydra.context.InContext(hydra.errors.Error.other("unsupported float type"), cx))
    case hydra.core.LiteralType.integer(v_LiteralType_integer_it) => v_LiteralType_integer_it match
      case hydra.core.IntegerType.bigint => Right(hydra.ext.scala.syntax.Type.ref(hydra.ext.scala.syntax.Type_Ref.name(hydra.ext.scala.syntax.Type_Name("BigInt"))))
      case hydra.core.IntegerType.int8 => Right(hydra.ext.scala.syntax.Type.ref(hydra.ext.scala.syntax.Type_Ref.name(hydra.ext.scala.syntax.Type_Name("Byte"))))
      case hydra.core.IntegerType.int16 => Right(hydra.ext.scala.syntax.Type.ref(hydra.ext.scala.syntax.Type_Ref.name(hydra.ext.scala.syntax.Type_Name("Short"))))
      case hydra.core.IntegerType.int32 => Right(hydra.ext.scala.syntax.Type.ref(hydra.ext.scala.syntax.Type_Ref.name(hydra.ext.scala.syntax.Type_Name("Int"))))
      case hydra.core.IntegerType.int64 => Right(hydra.ext.scala.syntax.Type.ref(hydra.ext.scala.syntax.Type_Ref.name(hydra.ext.scala.syntax.Type_Name("Long"))))
      case hydra.core.IntegerType.uint8 => Right(hydra.ext.scala.syntax.Type.ref(hydra.ext.scala.syntax.Type_Ref.name(hydra.ext.scala.syntax.Type_Name("Byte"))))
      case hydra.core.IntegerType.uint16 => Right(hydra.ext.scala.syntax.Type.ref(hydra.ext.scala.syntax.Type_Ref.name(hydra.ext.scala.syntax.Type_Name("Int"))))
      case hydra.core.IntegerType.uint32 => Right(hydra.ext.scala.syntax.Type.ref(hydra.ext.scala.syntax.Type_Ref.name(hydra.ext.scala.syntax.Type_Name("Long"))))
      case hydra.core.IntegerType.uint64 => Right(hydra.ext.scala.syntax.Type.ref(hydra.ext.scala.syntax.Type_Ref.name(hydra.ext.scala.syntax.Type_Name("BigInt"))))
      case _ => Left(hydra.context.InContext(hydra.errors.Error.other("unsupported integer type"), cx))
    case hydra.core.LiteralType.string => Right(hydra.ext.scala.syntax.Type.ref(hydra.ext.scala.syntax.Type_Ref.name(hydra.ext.scala.syntax.Type_Name("scala.Predef.String"))))
    case _ => Left(hydra.context.InContext(hydra.errors.Error.other("unsupported literal type"), cx))
  case hydra.core.Type.map(v_Type_map_mt) => {
    lazy val kt: hydra.core.Type = (v_Type_map_mt.keys)
    lazy val vt: hydra.core.Type = (v_Type_map_mt.values)
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.Type](hydra.ext.scala.coder.encodeType(cx)(g)(kt))((skt: hydra.ext.scala.syntax.Type) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Type,
         hydra.ext.scala.syntax.Type](hydra.ext.scala.coder.encodeType(cx)(g)(vt))((svt: hydra.ext.scala.syntax.Type) =>
      Right(hydra.ext.scala.utils.stapply2(hydra.ext.scala.syntax.Type.ref(hydra.ext.scala.syntax.Type_Ref.name(hydra.ext.scala.syntax.Type_Name("Map"))))(skt)(svt))))
  }
  case hydra.core.Type.maybe(v_Type_maybe_ot) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error],
     hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.Type](hydra.ext.scala.coder.encodeType(cx)(g)(v_Type_maybe_ot))((sot: hydra.ext.scala.syntax.Type) =>
    Right(hydra.ext.scala.utils.stapply1(hydra.ext.scala.syntax.Type.ref(hydra.ext.scala.syntax.Type_Ref.name(hydra.ext.scala.syntax.Type_Name("Option"))))(sot)))
  case hydra.core.Type.pair(v_Type_pair_pt) => {
    lazy val ft: hydra.core.Type = (v_Type_pair_pt.first)
    lazy val st: hydra.core.Type = (v_Type_pair_pt.second)
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.Type](hydra.ext.scala.coder.encodeType(cx)(g)(ft))((sft: hydra.ext.scala.syntax.Type) =>
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Type,
         hydra.ext.scala.syntax.Type](hydra.ext.scala.coder.encodeType(cx)(g)(st))((sst: hydra.ext.scala.syntax.Type) =>
      Right(hydra.ext.scala.utils.stapply2(hydra.ext.scala.syntax.Type.ref(hydra.ext.scala.syntax.Type_Ref.name(hydra.ext.scala.syntax.Type_Name("Tuple2"))))(sft)(sst))))
  }
  case hydra.core.Type.record(v_Type_record__) => Left(hydra.context.InContext(hydra.errors.Error.other("unexpected anonymous record type"), cx))
  case hydra.core.Type.set(v_Type_set_st) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error],
     hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.Type](hydra.ext.scala.coder.encodeType(cx)(g)(v_Type_set_st))((sst: hydra.ext.scala.syntax.Type) =>
    Right(hydra.ext.scala.utils.stapply1(hydra.ext.scala.syntax.Type.ref(hydra.ext.scala.syntax.Type_Ref.name(hydra.ext.scala.syntax.Type_Name("scala.collection.immutable.Set"))))(sst)))
  case hydra.core.Type.union(v_Type_union__) => Left(hydra.context.InContext(hydra.errors.Error.other("unexpected anonymous union type"), cx))
  case hydra.core.Type.wrap(v_Type_wrap__) => Left(hydra.context.InContext(hydra.errors.Error.other("unexpected anonymous wrap type"), cx))
  case hydra.core.Type.forall(v_Type_forall_ft) => {
    lazy val v: hydra.core.Name = (v_Type_forall_ft.parameter)
    lazy val body: hydra.core.Type = (v_Type_forall_ft.body)
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.Type](hydra.ext.scala.coder.encodeType(cx)(g)(body))((sbody: hydra.ext.scala.syntax.Type) =>
      Right(hydra.ext.scala.syntax.Type.lambda(hydra.ext.scala.syntax.Type_Lambda(Seq(hydra.ext.scala.utils.stparam(v)), sbody))))
  }
  case hydra.core.Type.variable(v_Type_variable_v) => {
    lazy val rawName: scala.Predef.String = v_Type_variable_v
    lazy val typeName: scala.Predef.String = hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.lists.elem[Int](46)(hydra.lib.strings.toList(rawName)))(rawName)(hydra.formatting.capitalize(rawName))
    Right(hydra.ext.scala.syntax.Type.`var`(hydra.ext.scala.syntax.Type_Var(hydra.ext.scala.syntax.Type_Name(typeName))))
  }
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other("unsupported type"), cx))

def encodeTypeDefinition[T0](cx: hydra.context.Context)(g: T0)(td: hydra.packaging.TypeDefinition): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.ext.scala.syntax.Stat] =
  {
  lazy val name: hydra.core.Name = (td.name)
  lazy val typ: hydra.core.Type = (td.`type`.`type`)
  lazy val lname: scala.Predef.String = hydra.names.localNameOf(name)
  lazy val tname: hydra.ext.scala.syntax.Type_Name = hydra.ext.scala.syntax.Type_Name(lname)
  lazy val dname: hydra.ext.scala.syntax.Data_Name = hydra.ext.scala.syntax.Data_Name(lname)
  lazy val freeVars: Seq[hydra.core.Name] = hydra.lib.lists.filter[hydra.core.Name]((v: hydra.core.Name) =>
    hydra.lib.logic.not(hydra.lib.lists.elem[Int](46)(hydra.lib.strings.toList(v))))(hydra.lib.sets.toList[hydra.core.Name](hydra.variables.freeVariablesInType(typ)))
  lazy val tparams: Seq[hydra.ext.scala.syntax.Type_Param] = hydra.lib.lists.map[hydra.core.Name, hydra.ext.scala.syntax.Type_Param]((__v: hydra.core.Name) =>
    {
    lazy val vn: scala.Predef.String = hydra.formatting.capitalize(__v)
    hydra.ext.scala.syntax.Type_Param(Seq(), hydra.ext.scala.syntax.Name.value(vn), Seq(), Seq(), Seq(), Seq())
  })(freeVars)
  hydra.strip.deannotateType(typ) match
    case hydra.core.Type.forall(v_Type_forall_ft) => {
      lazy val forallBody: hydra.core.Type = (v_Type_forall_ft.body)
      lazy val forallParam: hydra.core.Name = (v_Type_forall_ft.parameter)
      def collectForallParams(t: hydra.core.Type)(acc: Seq[hydra.core.Name]): Tuple2[Seq[hydra.core.Name], hydra.core.Type] =
        hydra.strip.deannotateType(t) match
        case hydra.core.Type.forall(v_Type_forall_ft2) => collectForallParams(v_Type_forall_ft2.body)(hydra.lib.lists.cons[hydra.core.Name](v_Type_forall_ft2.parameter)(acc))
        case _ => Tuple2(acc, t)
      lazy val collected: Tuple2[Seq[hydra.core.Name], hydra.core.Type] = collectForallParams(forallBody)(Seq(forallParam))
      lazy val allForallParams: Seq[hydra.core.Name] = hydra.lib.lists.reverse[hydra.core.Name](hydra.lib.pairs.first[Seq[hydra.core.Name],
         hydra.core.Type](collected))
      lazy val innerBody: hydra.core.Type = hydra.lib.pairs.second[Seq[hydra.core.Name], hydra.core.Type](collected)
      lazy val allTparams: Seq[hydra.ext.scala.syntax.Type_Param] = hydra.lib.lists.map[hydra.core.Name,
         hydra.ext.scala.syntax.Type_Param]((__v: hydra.core.Name) =>
        {
        lazy val vn: scala.Predef.String = hydra.formatting.capitalize(__v)
        hydra.ext.scala.syntax.Type_Param(Seq(), hydra.ext.scala.syntax.Name.value(vn), Seq(), Seq(), Seq(), Seq())
      })(allForallParams)
      hydra.strip.deannotateType(innerBody) match
        case hydra.core.Type.record(v_Type_record_rt2) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error],
           Seq[hydra.ext.scala.syntax.Data_Param], hydra.ext.scala.syntax.Stat](hydra.lib.eithers.mapList[hydra.core.FieldType,
           hydra.ext.scala.syntax.Data_Param, hydra.context.InContext[hydra.errors.Error]]((f: hydra.core.FieldType) => hydra.ext.scala.coder.fieldToParam(cx)(g)(f))(v_Type_record_rt2))((params: Seq[hydra.ext.scala.syntax.Data_Param]) =>
          Right(hydra.ext.scala.syntax.Stat.defn(hydra.ext.scala.syntax.Defn.`class`(hydra.ext.scala.syntax.Defn_Class(Seq(hydra.ext.scala.syntax.Mod.`case`),
             tname, allTparams, hydra.ext.scala.syntax.Ctor_Primary(Seq(), hydra.ext.scala.syntax.Name.value(""),
             Seq(params)), hydra.ext.scala.syntax.Template(Seq(), Seq(), (), Seq()))))))
        case hydra.core.Type.union(v_Type_union_rt2) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error],
           Seq[hydra.ext.scala.syntax.Stat], hydra.ext.scala.syntax.Stat](hydra.lib.eithers.mapList[hydra.core.FieldType,
           hydra.ext.scala.syntax.Stat, hydra.context.InContext[hydra.errors.Error]]((f: hydra.core.FieldType) =>
          hydra.ext.scala.coder.fieldToEnumCase(cx)(g)(lname)(allTparams)(f))(v_Type_union_rt2))((cases: Seq[hydra.ext.scala.syntax.Stat]) =>
          Right(hydra.ext.scala.syntax.Stat.defn(hydra.ext.scala.syntax.Defn.`enum`(hydra.ext.scala.syntax.Defn_Enum(Seq(),
             tname, allTparams, hydra.ext.scala.syntax.Ctor_Primary(Seq(), hydra.ext.scala.syntax.Name.value(""),
             Seq()), hydra.ext.scala.syntax.Template(Seq(), Seq(), (), cases))))))
        case hydra.core.Type.wrap(v_Type_wrap_wt2) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error],
           hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.Stat](hydra.ext.scala.coder.encodeType(cx)(g)(v_Type_wrap_wt2))((styp: hydra.ext.scala.syntax.Type) =>
          Right(hydra.ext.scala.syntax.Stat.defn(hydra.ext.scala.syntax.Defn.`type`(hydra.ext.scala.syntax.Defn_Type(Seq(), tname, allTparams, styp)))))
        case _ => {
          def mkAlias[T1](styp: hydra.ext.scala.syntax.Type): Either[T1, hydra.ext.scala.syntax.Stat] =
            Right(hydra.ext.scala.syntax.Stat.defn(hydra.ext.scala.syntax.Defn.`type`(hydra.ext.scala.syntax.Defn_Type(Seq(),
               hydra.ext.scala.syntax.Type_Name(lname), allTparams, styp))))
          hydra.lib.eithers.either[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Type,
             Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Stat]]((_x: hydra.context.InContext[hydra.errors.Error]) => mkAlias(hydra.ext.scala.utils.stref("Any")))(mkAlias)(hydra.ext.scala.coder.encodeType(cx)(g)(innerBody))
        }
    }
    case hydra.core.Type.record(v_Type_record_rt) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error],
       Seq[hydra.ext.scala.syntax.Data_Param], hydra.ext.scala.syntax.Stat](hydra.lib.eithers.mapList[hydra.core.FieldType,
       hydra.ext.scala.syntax.Data_Param, hydra.context.InContext[hydra.errors.Error]]((f: hydra.core.FieldType) => hydra.ext.scala.coder.fieldToParam(cx)(g)(f))(v_Type_record_rt))((params: Seq[hydra.ext.scala.syntax.Data_Param]) =>
      Right(hydra.ext.scala.syntax.Stat.defn(hydra.ext.scala.syntax.Defn.`class`(hydra.ext.scala.syntax.Defn_Class(Seq(hydra.ext.scala.syntax.Mod.`case`),
         tname, tparams, hydra.ext.scala.syntax.Ctor_Primary(Seq(), hydra.ext.scala.syntax.Name.value(""),
         Seq(params)), hydra.ext.scala.syntax.Template(Seq(), Seq(), (), Seq()))))))
    case hydra.core.Type.union(v_Type_union_rt) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error],
       Seq[hydra.ext.scala.syntax.Stat], hydra.ext.scala.syntax.Stat](hydra.lib.eithers.mapList[hydra.core.FieldType,
       hydra.ext.scala.syntax.Stat, hydra.context.InContext[hydra.errors.Error]]((f: hydra.core.FieldType) =>
      hydra.ext.scala.coder.fieldToEnumCase(cx)(g)(lname)(tparams)(f))(v_Type_union_rt))((cases: Seq[hydra.ext.scala.syntax.Stat]) =>
      Right(hydra.ext.scala.syntax.Stat.defn(hydra.ext.scala.syntax.Defn.`enum`(hydra.ext.scala.syntax.Defn_Enum(Seq(),
         tname, tparams, hydra.ext.scala.syntax.Ctor_Primary(Seq(), hydra.ext.scala.syntax.Name.value(""),
         Seq()), hydra.ext.scala.syntax.Template(Seq(), Seq(), (), cases))))))
    case hydra.core.Type.wrap(v_Type_wrap_wt) => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error],
       hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.Stat](hydra.ext.scala.coder.encodeType(cx)(g)(v_Type_wrap_wt))((styp: hydra.ext.scala.syntax.Type) =>
      Right(hydra.ext.scala.syntax.Stat.defn(hydra.ext.scala.syntax.Defn.`type`(hydra.ext.scala.syntax.Defn_Type(Seq(), tname, tparams, styp)))))
    case _ => {
      def mkAlias[T1](styp: hydra.ext.scala.syntax.Type): Either[T1, hydra.ext.scala.syntax.Stat] =
        Right(hydra.ext.scala.syntax.Stat.defn(hydra.ext.scala.syntax.Defn.`type`(hydra.ext.scala.syntax.Defn_Type(Seq(),
           hydra.ext.scala.syntax.Type_Name(lname), tparams, styp))))
      hydra.lib.eithers.either[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Type,
         Either[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Stat]]((_x: hydra.context.InContext[hydra.errors.Error]) => mkAlias(hydra.ext.scala.utils.stref("Any")))(mkAlias)(hydra.ext.scala.coder.encodeType(cx)(g)(typ))
    }
}

def encodeTypedParam[T0](cx: hydra.context.Context)(g: T0)(pair: Tuple2[hydra.core.Name, hydra.core.Type]): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.ext.scala.syntax.Data_Param] =
  {
  lazy val pname: scala.Predef.String = hydra.ext.scala.utils.scalaEscapeName(hydra.names.localNameOf(hydra.lib.pairs.first[hydra.core.Name,
     hydra.core.Type](pair)))
  lazy val pdom: hydra.core.Type = hydra.lib.pairs.second[hydra.core.Name, hydra.core.Type](pair)
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.Data_Param](hydra.ext.scala.coder.encodeType(cx)(g)(pdom))((sdom: hydra.ext.scala.syntax.Type) =>
    Right(hydra.ext.scala.syntax.Data_Param(Seq(), hydra.ext.scala.syntax.Name.value(pname), Some(sdom), None)))
}

def encodeUntypeApplicationTerm(cx: hydra.context.Context)(g: hydra.graph.Graph)(term: hydra.core.Term): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.ext.scala.syntax.Data] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.typing.InferenceResult, hydra.ext.scala.syntax.Data](hydra.inference.inferInGraphContext(cx)(g)(term))((result: hydra.typing.InferenceResult) => hydra.ext.scala.coder.encodeTerm(cx)(g)(result.term))

def extractBody(t: hydra.core.Term): hydra.core.Term =
  hydra.strip.deannotateAndDetypeTerm(t) match
  case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
    case hydra.core.Function.lambda(v_Function_lambda_lam) => hydra.ext.scala.coder.extractBody(v_Function_lambda_lam.body)
    case _ => t
  case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.ext.scala.coder.extractBody(v_Term_typeLambda_tl.body)
  case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => hydra.ext.scala.coder.extractBody(v_Term_typeApplication_ta.body)
  case hydra.core.Term.let(v_Term_let_lt) => hydra.ext.scala.coder.extractBody(v_Term_let_lt.body)
  case _ => t

def extractCodomain(t: hydra.core.Type): hydra.core.Type =
  hydra.strip.deannotateType(t) match
  case hydra.core.Type.function(v_Type_function_ft) => hydra.ext.scala.coder.extractCodomain(v_Type_function_ft.codomain)
  case hydra.core.Type.forall(v_Type_forall_fa) => hydra.ext.scala.coder.extractCodomain(v_Type_forall_fa.body)
  case _ => t

def extractDomains(t: hydra.core.Type): Seq[hydra.core.Type] =
  hydra.strip.deannotateType(t) match
  case hydra.core.Type.function(v_Type_function_ft) => hydra.lib.lists.cons[hydra.core.Type](v_Type_function_ft.domain)(hydra.ext.scala.coder.extractDomains(v_Type_function_ft.codomain))
  case hydra.core.Type.forall(v_Type_forall_fa) => hydra.ext.scala.coder.extractDomains(v_Type_forall_fa.body)
  case _ => Seq()

def extractLetBindings(t: hydra.core.Term): Seq[hydra.core.Binding] =
  hydra.strip.deannotateAndDetypeTerm(t) match
  case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
    case hydra.core.Function.lambda(v_Function_lambda_lam) => hydra.ext.scala.coder.extractLetBindings(v_Function_lambda_lam.body)
    case _ => Seq()
  case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.ext.scala.coder.extractLetBindings(v_Term_typeLambda_tl.body)
  case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => hydra.ext.scala.coder.extractLetBindings(v_Term_typeApplication_ta.body)
  case hydra.core.Term.let(v_Term_let_lt) => hydra.lib.lists.concat2[hydra.core.Binding](v_Term_let_lt.bindings)(hydra.ext.scala.coder.extractLetBindings(v_Term_let_lt.body))
  case _ => Seq()

def extractParams(t: hydra.core.Term): Seq[hydra.core.Name] =
  hydra.strip.deannotateAndDetypeTerm(t) match
  case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
    case hydra.core.Function.lambda(v_Function_lambda_lam) => hydra.lib.lists.cons[hydra.core.Name](v_Function_lambda_lam.parameter)(hydra.ext.scala.coder.extractParams(v_Function_lambda_lam.body))
    case _ => Seq()
  case hydra.core.Term.typeLambda(v_Term_typeLambda_tl) => hydra.ext.scala.coder.extractParams(v_Term_typeLambda_tl.body)
  case hydra.core.Term.typeApplication(v_Term_typeApplication_ta) => hydra.ext.scala.coder.extractParams(v_Term_typeApplication_ta.body)
  case hydra.core.Term.let(v_Term_let_lt) => hydra.ext.scala.coder.extractParams(v_Term_let_lt.body)
  case _ => Seq()

def fieldToEnumCase[T0](cx: hydra.context.Context)(g: T0)(parentName: scala.Predef.String)(tparams: Seq[hydra.ext.scala.syntax.Type_Param])(ft: hydra.core.FieldType): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.ext.scala.syntax.Stat] =
  {
  lazy val fname: scala.Predef.String = hydra.ext.scala.utils.scalaEscapeName(ft.name)
  lazy val ftyp: hydra.core.Type = (ft.`type`)
  lazy val caseName: hydra.ext.scala.syntax.Data_Name = hydra.ext.scala.syntax.Data_Name(fname)
  lazy val isUnit: Boolean = hydra.strip.deannotateType(ftyp) match
    case hydra.core.Type.unit => true
    case hydra.core.Type.record(v_Type_record_rt) => hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.core.FieldType](v_Type_record_rt))(0)
    case _ => false
  lazy val parentType: hydra.ext.scala.syntax.Type = hydra.lib.logic.ifElse[hydra.ext.scala.syntax.Type](hydra.lib.lists.`null`[hydra.ext.scala.syntax.Type_Param](tparams))(hydra.ext.scala.syntax.Type.ref(hydra.ext.scala.syntax.Type_Ref.name(hydra.ext.scala.syntax.Type_Name(parentName))))(hydra.ext.scala.syntax.Type.apply(hydra.ext.scala.syntax.Type_Apply(hydra.ext.scala.syntax.Type.ref(hydra.ext.scala.syntax.Type_Ref.name(hydra.ext.scala.syntax.Type_Name(parentName))),
     hydra.lib.lists.map[hydra.ext.scala.syntax.Type_Param, hydra.ext.scala.syntax.Type](hydra.ext.scala.coder.typeParamToTypeVar)(tparams))))
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.Stat](hydra.ext.scala.coder.encodeType(cx)(g)(ftyp))((sftyp: hydra.ext.scala.syntax.Type) =>
    Right(hydra.ext.scala.syntax.Stat.defn(hydra.ext.scala.syntax.Defn.enumCase(hydra.ext.scala.syntax.Defn_EnumCase(Seq(),
       caseName, Seq(), hydra.ext.scala.syntax.Ctor_Primary(Seq(), hydra.ext.scala.syntax.Name.value(""),
       Seq(hydra.lib.logic.ifElse[Seq[hydra.ext.scala.syntax.Data_Param]](isUnit)(Seq())(Seq(hydra.ext.scala.syntax.Data_Param(Seq(),
       hydra.ext.scala.syntax.Name.value("value"), Some(sftyp), None))))), Seq(hydra.ext.scala.syntax.Init(parentType,
       hydra.ext.scala.syntax.Name.value(""), Seq())))))))
}

def fieldToParam[T0](cx: hydra.context.Context)(g: T0)(ft: hydra.core.FieldType): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.ext.scala.syntax.Data_Param] =
  {
  lazy val fname: scala.Predef.String = hydra.ext.scala.utils.scalaEscapeName(ft.name)
  lazy val ftyp: hydra.core.Type = (ft.`type`)
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Type, hydra.ext.scala.syntax.Data_Param](hydra.ext.scala.coder.encodeType(cx)(g)(ftyp))((sftyp: hydra.ext.scala.syntax.Type) =>
    Right(hydra.ext.scala.syntax.Data_Param(Seq(), hydra.ext.scala.syntax.Name.value(fname), Some(sftyp), None)))
}

def findDomain(cx: hydra.context.Context)(g: hydra.graph.Graph)(meta: Map[hydra.core.Name, hydra.core.Term]): Either[hydra.context.InContext[hydra.errors.Error],
   hydra.core.Type] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Option[hydra.core.Type], hydra.core.Type](hydra.lib.eithers.bimap[hydra.errors.DecodingError,
     Option[hydra.core.Type], hydra.context.InContext[hydra.errors.Error], Option[hydra.core.Type]]((__de: hydra.errors.DecodingError) => hydra.context.InContext(hydra.errors.Error.other(__de),
     cx))((__a: Option[hydra.core.Type]) => __a)(hydra.annotations.getType(g)(meta)))((r: Option[hydra.core.Type]) =>
  hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], hydra.core.Type], hydra.core.Type](Left(hydra.context.InContext(hydra.errors.Error.other("expected a typed term"),
     cx)))((t: hydra.core.Type) =>
  hydra.strip.deannotateType(t) match
  case hydra.core.Type.function(v_Type_function_ft) => Right(v_Type_function_ft.domain)
  case _ => Left(hydra.context.InContext(hydra.errors.Error.other("expected a function type"), cx)))(r))

def findImports(cx: hydra.context.Context)(g: hydra.graph.Graph)(mod: hydra.packaging.Module): Either[hydra.context.InContext[hydra.errors.Error],
   Seq[hydra.ext.scala.syntax.Stat]] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], scala.collection.immutable.Set[hydra.packaging.Namespace],
     Seq[hydra.ext.scala.syntax.Stat]](hydra.analysis.moduleDependencyNamespaces(cx)(g)(false)(false)(true)(false)(mod))((elImps: scala.collection.immutable.Set[hydra.packaging.Namespace]) =>
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], scala.collection.immutable.Set[hydra.packaging.Namespace],
     Seq[hydra.ext.scala.syntax.Stat]](hydra.analysis.moduleDependencyNamespaces(cx)(g)(false)(true)(false)(false)(mod))((primImps: scala.collection.immutable.Set[hydra.packaging.Namespace]) =>
  Right(hydra.lib.lists.concat[hydra.ext.scala.syntax.Stat](Seq(hydra.lib.lists.map[hydra.packaging.Namespace,
     hydra.ext.scala.syntax.Stat](hydra.ext.scala.coder.toElImport)(hydra.lib.sets.toList[hydra.packaging.Namespace](elImps)),
     hydra.lib.lists.map[hydra.packaging.Namespace, hydra.ext.scala.syntax.Stat](hydra.ext.scala.coder.toPrimImport)(hydra.lib.sets.toList[hydra.packaging.Namespace](primImps)))))))

def findSdom(cx: hydra.context.Context)(g: hydra.graph.Graph)(meta: Map[hydra.core.Name, hydra.core.Term]): Either[hydra.context.InContext[hydra.errors.Error],
   Option[hydra.ext.scala.syntax.Type]] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], Option[hydra.core.Type], Option[hydra.ext.scala.syntax.Type]](hydra.lib.eithers.bimap[hydra.errors.DecodingError,
     Option[hydra.core.Type], hydra.context.InContext[hydra.errors.Error], Option[hydra.core.Type]]((__de: hydra.errors.DecodingError) => hydra.context.InContext(hydra.errors.Error.other(__de),
     cx))((__a: Option[hydra.core.Type]) => __a)(hydra.annotations.getType(g)(meta)))((mtyp: Option[hydra.core.Type]) =>
  hydra.lib.maybes.maybe[Either[hydra.context.InContext[hydra.errors.Error], Option[hydra.ext.scala.syntax.Type]],
     hydra.core.Type](Right(None))((t: hydra.core.Type) =>
  hydra.strip.deannotateType(t) match
  case hydra.core.Type.function(v_Type_function_ft) => {
    lazy val dom: hydra.core.Type = (v_Type_function_ft.domain)
    hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Type, Option[hydra.ext.scala.syntax.Type]](hydra.ext.scala.coder.encodeType(cx)(g)(dom))((sdom: hydra.ext.scala.syntax.Type) => Right(Some(sdom)))
  }
  case hydra.core.Type.forall(v_Type_forall_fa) => hydra.strip.deannotateType(v_Type_forall_fa.body) match
    case hydra.core.Type.function(v_Type_function_ft2) => {
      lazy val dom2: hydra.core.Type = (v_Type_function_ft2.domain)
      hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Type,
         Option[hydra.ext.scala.syntax.Type]](hydra.ext.scala.coder.encodeType(cx)(g)(dom2))((sdom2: hydra.ext.scala.syntax.Type) => Right(Some(sdom2)))
    }
    case _ => Right(None)
  case _ => hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Type,
     Option[hydra.ext.scala.syntax.Type]](hydra.ext.scala.coder.encodeType(cx)(g)(t))((st: hydra.ext.scala.syntax.Type) => Right(Some(st))))(mtyp))

def moduleToScala(mod: hydra.packaging.Module)(defs: Seq[hydra.packaging.Definition])(cx: hydra.context.Context)(g: hydra.graph.Graph): Either[hydra.context.InContext[hydra.errors.Error],
   Map[scala.Predef.String, scala.Predef.String]] =
  hydra.lib.eithers.bind[hydra.context.InContext[hydra.errors.Error], hydra.ext.scala.syntax.Pkg, Map[scala.Predef.String,
     scala.Predef.String]](hydra.ext.scala.coder.constructModule(cx)(g)(mod)(defs))((pkg: hydra.ext.scala.syntax.Pkg) =>
  {
  lazy val s: scala.Predef.String = hydra.serialization.printExpr(hydra.serialization.parenthesize(hydra.ext.scala.serde.writePkg(pkg)))
  Right(hydra.lib.maps.singleton[scala.Predef.String, scala.Predef.String](hydra.names.namespaceToFilePath(hydra.util.CaseConvention.camel)("scala")(mod.namespace))(s))
})

def stripWrapEliminations(t: hydra.core.Term): hydra.core.Term =
  hydra.strip.deannotateAndDetypeTerm(t) match
  case hydra.core.Term.application(v_Term_application_app) => {
    lazy val appFun: hydra.core.Term = (v_Term_application_app.function)
    lazy val appArg: hydra.core.Term = (v_Term_application_app.argument)
    hydra.strip.deannotateAndDetypeTerm(appFun) match
      case hydra.core.Term.function(v_Term_function_f) => v_Term_function_f match
        case hydra.core.Function.elimination(v_Function_elimination_e) => v_Function_elimination_e match
          case hydra.core.Elimination.wrap(v_Elimination_wrap__) => hydra.ext.scala.coder.stripWrapEliminations(appArg)
          case _ => t
        case _ => t
      case hydra.core.Term.application(v_Term_application_innerApp) => {
        lazy val innerFun: hydra.core.Term = (v_Term_application_innerApp.function)
        lazy val innerArg: hydra.core.Term = (v_Term_application_innerApp.argument)
        hydra.strip.deannotateAndDetypeTerm(innerFun) match
          case hydra.core.Term.function(v_Term_function_innerF) => v_Term_function_innerF match
            case hydra.core.Function.elimination(v_Function_elimination_innerE) => v_Function_elimination_innerE match
              case hydra.core.Elimination.wrap(v_Elimination_wrap__) => hydra.ext.scala.coder.stripWrapEliminations(hydra.core.Term.application(hydra.core.Application(innerArg,
                 appArg)))
              case _ => t
            case _ => t
          case _ => t
      }
      case _ => t
  }
  case _ => t

def toElImport(ns: hydra.packaging.Namespace): hydra.ext.scala.syntax.Stat =
  hydra.ext.scala.syntax.Stat.importExport(hydra.ext.scala.syntax.ImportExportStat.`import`(hydra.ext.scala.syntax.Import(Seq(hydra.ext.scala.syntax.Importer(hydra.ext.scala.syntax.Data_Ref.name(hydra.ext.scala.syntax.Data_Name(hydra.lib.strings.intercalate(".")(hydra.lib.strings.splitOn(".")(ns)))),
     Seq(hydra.ext.scala.syntax.Importee.wildcard))))))

def toPrimImport(ns: hydra.packaging.Namespace): hydra.ext.scala.syntax.Stat =
  hydra.ext.scala.syntax.Stat.importExport(hydra.ext.scala.syntax.ImportExportStat.`import`(hydra.ext.scala.syntax.Import(Seq(hydra.ext.scala.syntax.Importer(hydra.ext.scala.syntax.Data_Ref.name(hydra.ext.scala.syntax.Data_Name(hydra.lib.strings.intercalate(".")(hydra.lib.strings.splitOn(".")(ns)))),
     Seq())))))

def typeParamToTypeVar(tp: hydra.ext.scala.syntax.Type_Param): hydra.ext.scala.syntax.Type =
  {
  lazy val n: hydra.ext.scala.syntax.Name = (tp.name)
  lazy val s: scala.Predef.String = n match
    case hydra.ext.scala.syntax.Name.value(v_Name_value_v) => v_Name_value_v
    case _ => ""
  hydra.ext.scala.syntax.Type.`var`(hydra.ext.scala.syntax.Type_Var(hydra.ext.scala.syntax.Type_Name(s)))
}
