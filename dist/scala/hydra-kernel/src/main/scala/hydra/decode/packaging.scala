package hydra.decode.packaging

import hydra.core.*

import hydra.errors.*

import hydra.packaging.*

def definition(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.packaging.Definition] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.packaging.Definition]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.inject(v_Term_inject_inj) => {
    lazy val field: hydra.core.Field = (v_Term_inject_inj.field)
    lazy val fname: hydra.core.Name = (field.name)
    lazy val fterm: hydra.core.Term = (field.term)
    lazy val variantMap: Map[hydra.core.Name, (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.packaging.Definition])] = hydra.lib.maps.fromList[hydra.core.Name, (hydra.core.Term) => Either[hydra.errors.DecodingError,
         
         
         
         
         
         
         
         
         
         
         
         
         
         
       hydra.packaging.Definition]](Seq(Tuple2("term", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.packaging.TermDefinition, hydra.packaging.Definition,
         hydra.errors.DecodingError]((t: hydra.packaging.TermDefinition) => hydra.packaging.Definition.term(t))(hydra.decode.packaging.termDefinition(cx)(input))),
           
           
           
           
           
           
           
           
           
           
           
           
           
           
         Tuple2("type", (input: hydra.core.Term) =>
      hydra.lib.eithers.map[hydra.packaging.TypeDefinition, hydra.packaging.Definition,
         hydra.errors.DecodingError]((t: hydra.packaging.TypeDefinition) => hydra.packaging.Definition.`type`(t))(hydra.decode.packaging.typeDefinition(cx)(input)))))
    hydra.lib.maybes.maybe[Either[hydra.errors.DecodingError, hydra.packaging.Definition],
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.packaging.Definition]](Left(hydra.lib.strings.cat(Seq("no such field ",
         
         
         
         
         
         
         
         
         
         
         
         
         
         
       fname, " in union"))))((f: (hydra.core.Term => Either[hydra.errors.DecodingError,
       hydra.packaging.Definition])) => f(fterm))(hydra.lib.maps.lookup[hydra.core.Name,
       (hydra.core.Term) => Either[hydra.errors.DecodingError, hydra.packaging.Definition]](fname)(variantMap))
  }
  case _ => Left("expected union"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def fileExtension(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.packaging.FileExtension] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.packaging.FileExtension]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.eithers.map[scala.Predef.String,
     hydra.packaging.FileExtension, hydra.errors.DecodingError]((b: scala.Predef.String) => b)(hydra.lib.eithers.either[hydra.errors.DecodingError,
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term, Either[hydra.errors.DecodingError, scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
    stripped2 match
    case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
      case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
      case _ => Left("expected string literal")
    case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx)(v_Term_wrap_wrappedTerm.body)))
  case _ => Left("expected wrapped type"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def module(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError, hydra.packaging.Module] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.packaging.Module]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.packaging.Namespace,
       hydra.packaging.Module](hydra.extract.core.requireField("namespace")(hydra.decode.packaging.namespace)(fieldMap)(cx))((field_namespace: hydra.packaging.Namespace) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.packaging.Definition],
         hydra.packaging.Module](hydra.extract.core.requireField("definitions")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.core.decodeList(hydra.decode.packaging.definition)(v1)(v2))(fieldMap)(cx))((field_definitions: Seq[hydra.packaging.Definition]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.packaging.Namespace],
         hydra.packaging.Module](hydra.extract.core.requireField("termDependencies")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.core.decodeList(hydra.decode.packaging.namespace)(v1)(v2))(fieldMap)(cx))((field_termDependencies: Seq[hydra.packaging.Namespace]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.packaging.Namespace],
         hydra.packaging.Module](hydra.extract.core.requireField("typeDependencies")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.core.decodeList(hydra.decode.packaging.namespace)(v1)(v2))(fieldMap)(cx))((field_typeDependencies: Seq[hydra.packaging.Namespace]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Option[scala.Predef.String],
         hydra.packaging.Module](hydra.extract.core.requireField("description")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.core.decodeMaybe((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
         scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx2)(raw2)))(v1)(v2))(fieldMap)(cx))((field_description: Option[scala.Predef.String]) =>
      Right(hydra.packaging.Module(field_namespace, field_definitions, field_termDependencies,
         field_typeDependencies, field_description)))))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def namespace(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.packaging.Namespace] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.packaging.Namespace]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.eithers.map[scala.Predef.String,
     hydra.packaging.Namespace, hydra.errors.DecodingError]((b: scala.Predef.String) => b)(hydra.lib.eithers.either[hydra.errors.DecodingError,
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term, Either[hydra.errors.DecodingError, scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
    stripped2 match
    case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
      case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
      case _ => Left("expected string literal")
    case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx)(v_Term_wrap_wrappedTerm.body)))
  case _ => Left("expected wrapped type"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def namespaces[T0](n: (hydra.graph.Graph => hydra.core.Term => Either[hydra.errors.DecodingError,
   T0]))(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.packaging.Namespaces[T0]] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.packaging.Namespaces[T0]]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, Tuple2[hydra.packaging.Namespace,
       T0], hydra.packaging.Namespaces[T0]](hydra.extract.core.requireField("focus")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.core.decodePair(hydra.decode.packaging.namespace)(n)(v1)(v2))(fieldMap)(cx))((field_focus: Tuple2[hydra.packaging.Namespace,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         T0]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Map[hydra.packaging.Namespace,
         T0], hydra.packaging.Namespaces[T0]](hydra.extract.core.requireField("mapping")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.core.decodeMap(hydra.decode.packaging.namespace)(n)(v1)(v2))(fieldMap)(cx))((field_mapping: Map[hydra.packaging.Namespace,
        
        
        
        
        
        
        
        
        
        
        
        
        
        
         T0]) =>
      Right(hydra.packaging.Namespaces(field_focus, field_mapping))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def `package`(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.packaging.Package] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.packaging.Package]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.packaging.PackageName,
       hydra.packaging.Package](hydra.extract.core.requireField("name")(hydra.decode.packaging.packageName)(fieldMap)(cx))((field_name: hydra.packaging.PackageName) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.packaging.Module],
         hydra.packaging.Package](hydra.extract.core.requireField("modules")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.core.decodeList(hydra.decode.packaging.module)(v1)(v2))(fieldMap)(cx))((field_modules: Seq[hydra.packaging.Module]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Seq[hydra.packaging.PackageName],
         hydra.packaging.Package](hydra.extract.core.requireField("dependencies")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.core.decodeList(hydra.decode.packaging.packageName)(v1)(v2))(fieldMap)(cx))((field_dependencies: Seq[hydra.packaging.PackageName]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Option[scala.Predef.String],
         hydra.packaging.Package](hydra.extract.core.requireField("description")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.core.decodeMaybe((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
         scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx2)(raw2)))(v1)(v2))(fieldMap)(cx))((field_description: Option[scala.Predef.String]) =>
      Right(hydra.packaging.Package(field_name, field_modules, field_dependencies, field_description))))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def packageName(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.packaging.PackageName] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.packaging.PackageName]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.wrap(v_Term_wrap_wrappedTerm) => hydra.lib.eithers.map[scala.Predef.String,
     hydra.packaging.PackageName, hydra.errors.DecodingError]((b: scala.Predef.String) => b)(hydra.lib.eithers.either[hydra.errors.DecodingError,
       
       
       
       
       
       
       
       
       
       
       
       
       
       
     hydra.core.Term, Either[hydra.errors.DecodingError, scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
    stripped2 match
    case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
      case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
      case _ => Left("expected string literal")
    case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx)(v_Term_wrap_wrappedTerm.body)))
  case _ => Left("expected wrapped type"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def qualifiedName(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.packaging.QualifiedName] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.packaging.QualifiedName]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, Option[hydra.packaging.Namespace],
       hydra.packaging.QualifiedName](hydra.extract.core.requireField("namespace")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.core.decodeMaybe(hydra.decode.packaging.namespace)(v1)(v2))(fieldMap)(cx))((field_namespace: Option[hydra.packaging.Namespace]) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, scala.Predef.String, hydra.packaging.QualifiedName](hydra.extract.core.requireField("local")((cx2: hydra.graph.Graph) =>
      (raw2: hydra.core.Term) =>
      hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
         scala.Predef.String]]((err: hydra.errors.DecodingError) => Left(err))((stripped2: hydra.core.Term) =>
      stripped2 match
      case hydra.core.Term.literal(v_Term_literal_v) => v_Term_literal_v match
        case hydra.core.Literal.string(v_Literal_string_s) => Right(v_Literal_string_s)
        case _ => Left("expected string literal")
      case _ => Left("expected literal"))(hydra.extract.core.stripWithDecodingError(cx2)(raw2)))(fieldMap)(cx))((field_local: scala.Predef.String) =>
      Right(hydra.packaging.QualifiedName(field_namespace, field_local))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def termDefinition(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.packaging.TermDefinition] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.packaging.TermDefinition]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.packaging.TermDefinition](hydra.extract.core.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Term, hydra.packaging.TermDefinition](hydra.extract.core.requireField("term")(hydra.decode.core.term)(fieldMap)(cx))((field_term: hydra.core.Term) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, Option[hydra.core.TypeScheme],
         hydra.packaging.TermDefinition](hydra.extract.core.requireField("type")((v1: hydra.graph.Graph) =>
      (v2: hydra.core.Term) =>
      hydra.extract.core.decodeMaybe(hydra.decode.core.typeScheme)(v1)(v2))(fieldMap)(cx))((field_type: Option[hydra.core.TypeScheme]) =>
      Right(hydra.packaging.TermDefinition(field_name, field_term, field_type)))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))

def typeDefinition(cx: hydra.graph.Graph)(raw: hydra.core.Term): Either[hydra.errors.DecodingError,
   hydra.packaging.TypeDefinition] =
  hydra.lib.eithers.either[hydra.errors.DecodingError, hydra.core.Term, Either[hydra.errors.DecodingError,
     hydra.packaging.TypeDefinition]]((err: hydra.errors.DecodingError) => Left(err))((stripped: hydra.core.Term) =>
  stripped match
  case hydra.core.Term.record(v_Term_record_record) => {
    lazy val fieldMap: Map[hydra.core.Name, hydra.core.Term] = hydra.extract.core.toFieldMap(v_Term_record_record)
    hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.Name, hydra.packaging.TypeDefinition](hydra.extract.core.requireField("name")(hydra.decode.core.name)(fieldMap)(cx))((field_name: hydra.core.Name) =>
      hydra.lib.eithers.bind[hydra.errors.DecodingError, hydra.core.TypeScheme, hydra.packaging.TypeDefinition](hydra.extract.core.requireField("type")(hydra.decode.core.typeScheme)(fieldMap)(cx))((field_type: hydra.core.TypeScheme) =>
      Right(hydra.packaging.TypeDefinition(field_name, field_type))))
  }
  case _ => Left("expected record"))(hydra.extract.core.stripWithDecodingError(cx)(raw))
