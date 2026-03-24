package hydra.ext.python.serde

import hydra.ext.python.syntax.*

import hydra.lib.equality

import hydra.lib.lists

import hydra.lib.literals

import hydra.lib.logic

import hydra.lib.maybes

import hydra.lib.strings

def encodeAnnotatedRhs(arhs: hydra.ext.python.syntax.AnnotatedRhs): hydra.ast.Expr =
  hydra.serialization.spaceSep(Seq(hydra.serialization.cst("="), arhs match
  case hydra.ext.python.syntax.AnnotatedRhs.star(v_AnnotatedRhs_star_ses) => hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.python.syntax.StarExpression, hydra.ast.Expr](hydra.ext.python.serde.encodeStarExpression)(v_AnnotatedRhs_star_ses))
  case hydra.ext.python.syntax.AnnotatedRhs.`yield`() => hydra.serialization.cst("yield ...")))

def encodeAnnotatedStatement(`as_`: hydra.ext.python.syntax.AnnotatedStatement): hydra.ast.Expr =
  {
  lazy val `doc_`: scala.Predef.String = (`as_`.comment)
  lazy val stmt: hydra.ext.python.syntax.Statement = (`as_`.statement)
  hydra.serialization.newlineSep(Seq(hydra.serialization.cst(hydra.ext.python.serde.toPythonComments(`doc_`)), hydra.ext.python.serde.encodeStatement(stmt)))
}

def encodeAnnotation(ann: hydra.ext.python.syntax.Annotation): hydra.ast.Expr =
  hydra.serialization.spaceSep(Seq(hydra.serialization.cst(":"), hydra.ext.python.serde.encodeExpression(ann)))

def encodeArgs(args: hydra.ext.python.syntax.Args): hydra.ast.Expr =
  {
  lazy val pos: Seq[hydra.ext.python.syntax.PosArg] = (args.positional)
  lazy val ks: Seq[hydra.ext.python.syntax.KwargOrStarred] = (args.kwargOrStarred)
  lazy val kss: Seq[hydra.ext.python.syntax.KwargOrDoubleStarred] = (args.kwargOrDoubleStarred)
  hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.concat[hydra.ast.Expr](Seq(hydra.lib.lists.map[hydra.ext.python.syntax.PosArg, hydra.ast.Expr](hydra.ext.python.serde.encodePosArg)(pos), hydra.lib.lists.map[hydra.ext.python.syntax.KwargOrStarred, hydra.ast.Expr](hydra.ext.python.serde.encodeKwargOrStarred)(ks), hydra.lib.lists.map[hydra.ext.python.syntax.KwargOrDoubleStarred, hydra.ast.Expr](hydra.ext.python.serde.encodeKwargOrDoubleStarred)(kss))))
}

def encodeAssignment(a: hydra.ext.python.syntax.Assignment): hydra.ast.Expr =
  a match
  case hydra.ext.python.syntax.Assignment.typed(v_Assignment_typed_t) => hydra.ext.python.serde.encodeTypedAssignment(v_Assignment_typed_t)
  case hydra.ext.python.syntax.Assignment.untyped(v_Assignment_untyped_u) => hydra.ext.python.serde.encodeUntypedAssignment(v_Assignment_untyped_u)
  case hydra.ext.python.syntax.Assignment.aug() => hydra.serialization.cst("... += ...")

def encodeAssignmentExpression(ae: hydra.ext.python.syntax.AssignmentExpression): hydra.ast.Expr =
  {
  lazy val name: hydra.ext.python.syntax.Name = (ae.name)
  lazy val expr: hydra.ext.python.syntax.Expression = (ae.expression)
  hydra.serialization.spaceSep(Seq(hydra.ext.python.serde.encodeName(name), hydra.serialization.cst(":="), hydra.ext.python.serde.encodeExpression(expr)))
}

def encodeAtom(atom: hydra.ext.python.syntax.Atom): hydra.ast.Expr =
  atom match
  case hydra.ext.python.syntax.Atom.dict(v_Atom_dict_d) => hydra.ext.python.serde.encodeDict(v_Atom_dict_d)
  case hydra.ext.python.syntax.Atom.dictcomp() => hydra.serialization.cst("{...}")
  case hydra.ext.python.syntax.Atom.ellipsis() => hydra.serialization.cst("...")
  case hydra.ext.python.syntax.Atom.`false`() => hydra.serialization.cst("False")
  case hydra.ext.python.syntax.Atom.genexp() => hydra.serialization.cst("(...)")
  case hydra.ext.python.syntax.Atom.group(v_Atom_group_g) => hydra.ext.python.serde.encodeGroup(v_Atom_group_g)
  case hydra.ext.python.syntax.Atom.list(v_Atom_list_l) => hydra.ext.python.serde.encodeList(v_Atom_list_l)
  case hydra.ext.python.syntax.Atom.listcomp() => hydra.serialization.cst("[...]")
  case hydra.ext.python.syntax.Atom.name(v_Atom_name_n) => hydra.ext.python.serde.encodeName(v_Atom_name_n)
  case hydra.ext.python.syntax.Atom.none() => hydra.serialization.cst("None")
  case hydra.ext.python.syntax.Atom.number(v_Atom_number_n) => hydra.ext.python.serde.encodeNumber(v_Atom_number_n)
  case hydra.ext.python.syntax.Atom.set(v_Atom_set_s) => hydra.ext.python.serde.encodeSet(v_Atom_set_s)
  case hydra.ext.python.syntax.Atom.setcomp() => hydra.serialization.cst("{...}")
  case hydra.ext.python.syntax.Atom.string(v_Atom_string_s) => hydra.ext.python.serde.encodeString(v_Atom_string_s)
  case hydra.ext.python.syntax.Atom.`true`() => hydra.serialization.cst("True")
  case hydra.ext.python.syntax.Atom.tuple(v_Atom_tuple_t) => hydra.ext.python.serde.encodeTuple(v_Atom_tuple_t)

def encodeAttribute(attr: hydra.ext.python.syntax.Attribute): hydra.ast.Expr =
  hydra.serialization.dotSep(hydra.lib.lists.map[hydra.ext.python.syntax.Name, hydra.ast.Expr](hydra.ext.python.serde.encodeName)(attr))

def encodeAwaitPrimary(ap: hydra.ext.python.syntax.AwaitPrimary): hydra.ast.Expr =
  {
  lazy val `await_`: Boolean = (ap.await)
  lazy val primary: hydra.ext.python.syntax.Primary = (ap.primary)
  hydra.lib.logic.ifElse[hydra.ast.Expr](`await_`)(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("await"), hydra.ext.python.serde.encodePrimary(primary))))(hydra.ext.python.serde.encodePrimary(primary))
}

def encodeBitwiseAnd(band: hydra.ext.python.syntax.BitwiseAnd): hydra.ast.Expr =
  {
  lazy val lhs: Option[hydra.ext.python.syntax.BitwiseAnd] = (band.lhs)
  lazy val rhs: hydra.ext.python.syntax.ShiftExpression = (band.rhs)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.maybes.map[hydra.ext.python.syntax.BitwiseAnd, hydra.ast.Expr]((l: hydra.ext.python.syntax.BitwiseAnd) =>
    hydra.serialization.spaceSep(Seq(hydra.ext.python.serde.encodeBitwiseAnd(l), hydra.serialization.cst("&"))))(lhs), Some(hydra.ext.python.serde.encodeShiftExpression(rhs)))))
}

def encodeBitwiseOr(bor: hydra.ext.python.syntax.BitwiseOr): hydra.ast.Expr =
  {
  lazy val lhs: Option[hydra.ext.python.syntax.BitwiseOr] = (bor.lhs)
  lazy val rhs: hydra.ext.python.syntax.BitwiseXor = (bor.rhs)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.maybes.map[hydra.ext.python.syntax.BitwiseOr, hydra.ast.Expr]((l: hydra.ext.python.syntax.BitwiseOr) =>
    hydra.serialization.spaceSep(Seq(hydra.ext.python.serde.encodeBitwiseOr(l), hydra.serialization.cst("|"))))(lhs), Some(hydra.ext.python.serde.encodeBitwiseXor(rhs)))))
}

def encodeBitwiseXor(bxor: hydra.ext.python.syntax.BitwiseXor): hydra.ast.Expr =
  {
  lazy val lhs: Option[hydra.ext.python.syntax.BitwiseXor] = (bxor.lhs)
  lazy val rhs: hydra.ext.python.syntax.BitwiseAnd = (bxor.rhs)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.maybes.map[hydra.ext.python.syntax.BitwiseXor, hydra.ast.Expr]((l: hydra.ext.python.syntax.BitwiseXor) =>
    hydra.serialization.spaceSep(Seq(hydra.ext.python.serde.encodeBitwiseXor(l), hydra.serialization.cst("^"))))(lhs), Some(hydra.ext.python.serde.encodeBitwiseAnd(rhs)))))
}

def encodeBlock(b: hydra.ext.python.syntax.Block): hydra.ast.Expr =
  b match
  case hydra.ext.python.syntax.Block.indented(v_Block_indented_groups) => hydra.serialization.tabIndentDoubleSpace(hydra.lib.lists.map[Seq[hydra.ext.python.syntax.Statement], hydra.ast.Expr]((stmts: Seq[hydra.ext.python.syntax.Statement]) =>
    hydra.serialization.newlineSep(hydra.lib.lists.map[hydra.ext.python.syntax.Statement, hydra.ast.Expr](hydra.ext.python.serde.encodeStatement)(stmts)))(v_Block_indented_groups))
  case hydra.ext.python.syntax.Block.simple(v_Block_simple_ss) => hydra.serialization.semicolonSep(hydra.lib.lists.map[hydra.ext.python.syntax.SimpleStatement, hydra.ast.Expr](hydra.ext.python.serde.encodeSimpleStatement)(v_Block_simple_ss))

def encodeCapturePattern(cp: hydra.ext.python.syntax.CapturePattern): hydra.ast.Expr = hydra.ext.python.serde.encodePatternCaptureTarget(cp)

def encodeCaseBlock(cb: hydra.ext.python.syntax.CaseBlock): hydra.ast.Expr =
  {
  lazy val patterns: hydra.ext.python.syntax.Patterns = (cb.patterns)
  lazy val guard: Option[hydra.ext.python.syntax.Guard] = (cb.guard)
  lazy val body: hydra.ext.python.syntax.Block = (cb.body)
  hydra.serialization.newlineSep(Seq(hydra.serialization.noSep(Seq(hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.serialization.cst("case")), Some(hydra.ext.python.serde.encodePatterns(patterns)), hydra.lib.maybes.map[hydra.ext.python.syntax.Guard, hydra.ast.Expr](hydra.ext.python.serde.encodeGuard)(guard)))), hydra.serialization.cst(":"))), hydra.ext.python.serde.encodeBlock(body)))
}

def encodeClassDefinition(cd: hydra.ext.python.syntax.ClassDefinition): hydra.ast.Expr =
  {
  lazy val decs: Option[hydra.ext.python.syntax.Decorators] = (cd.decorators)
  lazy val name: hydra.ext.python.syntax.Name = (cd.name)
  lazy val args: Option[hydra.ext.python.syntax.Args] = (cd.arguments)
  lazy val body: hydra.ext.python.syntax.Block = (cd.body)
  lazy val argPart: Option[hydra.ast.Expr] = hydra.lib.maybes.map[hydra.ext.python.syntax.Args, hydra.ast.Expr]((a: hydra.ext.python.syntax.Args) =>
    hydra.serialization.noSep(Seq(hydra.serialization.cst("("), hydra.ext.python.serde.encodeArgs(a), hydra.serialization.cst(")"))))(args)
  hydra.serialization.newlineSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.maybes.map[hydra.ext.python.syntax.Decorators, hydra.ast.Expr](hydra.ext.python.serde.encodeDecorators)(decs), Some(hydra.serialization.noSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("class"), hydra.ext.python.serde.encodeName(name)))), argPart, Some(hydra.serialization.cst(":")))))), Some(hydra.ext.python.serde.encodeBlock(body)))))
}

def encodeClassPattern(cp: hydra.ext.python.syntax.ClassPattern): hydra.ast.Expr =
  {
  lazy val noa: hydra.ext.python.syntax.NameOrAttribute = (cp.nameOrAttribute)
  lazy val pos: Option[hydra.ext.python.syntax.PositionalPatterns] = (cp.positionalPatterns)
  lazy val kw: Option[hydra.ext.python.syntax.KeywordPatterns] = (cp.keywordPatterns)
  hydra.serialization.noSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.ext.python.serde.encodeNameOrAttribute(noa)), Some(hydra.serialization.cst("(")), hydra.lib.maybes.map[hydra.ext.python.syntax.PositionalPatterns, hydra.ast.Expr](hydra.ext.python.serde.encodePositionalPatterns)(pos), hydra.lib.maybes.map[hydra.ext.python.syntax.KeywordPatterns, hydra.ast.Expr](hydra.ext.python.serde.encodeKeywordPatterns)(kw), Some(hydra.serialization.cst(")")))))
}

def encodeClosedPattern(cp: hydra.ext.python.syntax.ClosedPattern): hydra.ast.Expr =
  cp match
  case hydra.ext.python.syntax.ClosedPattern.literal() => hydra.serialization.cst("...")
  case hydra.ext.python.syntax.ClosedPattern.capture(v_ClosedPattern_capture_c) => hydra.ext.python.serde.encodeCapturePattern(v_ClosedPattern_capture_c)
  case hydra.ext.python.syntax.ClosedPattern.wildcard() => hydra.serialization.cst("_")
  case hydra.ext.python.syntax.ClosedPattern.value(v_ClosedPattern_value_v) => hydra.ext.python.serde.encodeValuePattern(v_ClosedPattern_value_v)
  case hydra.ext.python.syntax.ClosedPattern.group() => hydra.serialization.cst("(...)")
  case hydra.ext.python.syntax.ClosedPattern.sequence() => hydra.serialization.cst("[...]")
  case hydra.ext.python.syntax.ClosedPattern.mapping() => hydra.serialization.cst("{...}")
  case hydra.ext.python.syntax.ClosedPattern.`class`(v_ClosedPattern_class_c) => hydra.ext.python.serde.encodeClassPattern(v_ClosedPattern_class_c)

def encodeComparison(cmp: hydra.ext.python.syntax.Comparison): hydra.ast.Expr = hydra.ext.python.serde.encodeBitwiseOr(cmp.lhs)

def encodeConditional(c: hydra.ext.python.syntax.Conditional): hydra.ast.Expr =
  {
  lazy val body: hydra.ext.python.syntax.Disjunction = (c.body)
  lazy val cond: hydra.ext.python.syntax.Disjunction = (c.`if`)
  lazy val elseExpr: hydra.ext.python.syntax.Expression = (c.`else`)
  hydra.serialization.spaceSep(Seq(hydra.ext.python.serde.encodeDisjunction(body), hydra.serialization.cst("if"), hydra.ext.python.serde.encodeDisjunction(cond), hydra.serialization.cst("else"), hydra.ext.python.serde.encodeExpression(elseExpr)))
}

def encodeCompoundStatement(cs: hydra.ext.python.syntax.CompoundStatement): hydra.ast.Expr =
  cs match
  case hydra.ext.python.syntax.CompoundStatement.function(v_CompoundStatement_function_f) => hydra.ext.python.serde.encodeFunctionDefinition(v_CompoundStatement_function_f)
  case hydra.ext.python.syntax.CompoundStatement.`if`() => hydra.serialization.cst("if ...")
  case hydra.ext.python.syntax.CompoundStatement.classDef(v_CompoundStatement_classDef_c) => hydra.ext.python.serde.encodeClassDefinition(v_CompoundStatement_classDef_c)
  case hydra.ext.python.syntax.CompoundStatement.`with`() => hydra.serialization.cst("with ...")
  case hydra.ext.python.syntax.CompoundStatement.`for`() => hydra.serialization.cst("for ...")
  case hydra.ext.python.syntax.CompoundStatement.`try`() => hydra.serialization.cst("try ...")
  case hydra.ext.python.syntax.CompoundStatement.`while`(v_CompoundStatement_while_w) => hydra.ext.python.serde.encodeWhileStatement(v_CompoundStatement_while_w)
  case hydra.ext.python.syntax.CompoundStatement.`match`(v_CompoundStatement_match_m) => hydra.ext.python.serde.encodeMatchStatement(v_CompoundStatement_match_m)

def encodeConjunction(c: hydra.ext.python.syntax.Conjunction): hydra.ast.Expr =
  hydra.serialization.symbolSep("and")(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.python.syntax.Inversion, hydra.ast.Expr](hydra.ext.python.serde.encodeInversion)(c))

def encodeDecorators(decs: hydra.ext.python.syntax.Decorators): hydra.ast.Expr =
  hydra.serialization.newlineSep(hydra.lib.lists.map[hydra.ext.python.syntax.NamedExpression, hydra.ast.Expr]((ne: hydra.ext.python.syntax.NamedExpression) =>
  hydra.serialization.noSep(Seq(hydra.serialization.cst("@"), hydra.ext.python.serde.encodeNamedExpression(ne))))(decs))

def encodeDict(d: hydra.ext.python.syntax.Dict): hydra.ast.Expr =
  hydra.serialization.curlyBracesList(None)(hydra.serialization.halfBlockStyle)(hydra.lib.lists.map[hydra.ext.python.syntax.DoubleStarredKvpair, hydra.ast.Expr](hydra.ext.python.serde.encodeDoubleStarredKvpair)(d))

def encodeDisjunction(d: hydra.ext.python.syntax.Disjunction): hydra.ast.Expr =
  hydra.serialization.symbolSep("or")(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.python.syntax.Conjunction, hydra.ast.Expr](hydra.ext.python.serde.encodeConjunction)(d))

def encodeDottedAsName(dan: hydra.ext.python.syntax.DottedAsName): hydra.ast.Expr =
  {
  lazy val name: hydra.ext.python.syntax.DottedName = (dan.name)
  lazy val alias: Option[hydra.ext.python.syntax.Name] = (dan.as)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.ext.python.serde.encodeDottedName(name)), hydra.lib.maybes.map[hydra.ext.python.syntax.Name, hydra.ast.Expr]((a: hydra.ext.python.syntax.Name) =>
    hydra.serialization.spaceSep(Seq(hydra.serialization.cst("as"), hydra.ext.python.serde.encodeName(a))))(alias))))
}

def encodeDottedName(dn: hydra.ext.python.syntax.DottedName): hydra.ast.Expr =
  hydra.serialization.cst(hydra.lib.strings.intercalate(".")(hydra.lib.lists.map[hydra.ext.python.syntax.Name, scala.Predef.String]((n: hydra.ext.python.syntax.Name) => n)(dn)))

def encodeDoubleStarredKvpair(dskv: hydra.ext.python.syntax.DoubleStarredKvpair): hydra.ast.Expr =
  dskv match
  case hydra.ext.python.syntax.DoubleStarredKvpair.pair(v_DoubleStarredKvpair_pair_p) => hydra.ext.python.serde.encodeKvpair(v_DoubleStarredKvpair_pair_p)
  case hydra.ext.python.syntax.DoubleStarredKvpair.starred(v_DoubleStarredKvpair_starred_e) => hydra.serialization.noSep(Seq(hydra.serialization.cst("**"), hydra.ext.python.serde.encodeBitwiseOr(v_DoubleStarredKvpair_starred_e)))

def encodeExpression(expr: hydra.ext.python.syntax.Expression): hydra.ast.Expr =
  expr match
  case hydra.ext.python.syntax.Expression.simple(v_Expression_simple_d) => hydra.ext.python.serde.encodeDisjunction(v_Expression_simple_d)
  case hydra.ext.python.syntax.Expression.conditional(v_Expression_conditional_c) => hydra.ext.python.serde.encodeConditional(v_Expression_conditional_c)
  case hydra.ext.python.syntax.Expression.lambda(v_Expression_lambda_l) => hydra.ext.python.serde.encodeLambda(v_Expression_lambda_l)

def encodeFactor(f: hydra.ext.python.syntax.Factor): hydra.ast.Expr =
  f match
  case hydra.ext.python.syntax.Factor.positive(v_Factor_positive_inner) => hydra.serialization.noSep(Seq(hydra.serialization.cst("+"), hydra.ext.python.serde.encodeFactor(v_Factor_positive_inner)))
  case hydra.ext.python.syntax.Factor.negative(v_Factor_negative_inner) => hydra.serialization.noSep(Seq(hydra.serialization.cst("-"), hydra.ext.python.serde.encodeFactor(v_Factor_negative_inner)))
  case hydra.ext.python.syntax.Factor.complement(v_Factor_complement_inner) => hydra.serialization.noSep(Seq(hydra.serialization.cst("~"), hydra.ext.python.serde.encodeFactor(v_Factor_complement_inner)))
  case hydra.ext.python.syntax.Factor.simple(v_Factor_simple_p) => hydra.ext.python.serde.encodePower(v_Factor_simple_p)

def encodeFunctionDefRaw(fdr: hydra.ext.python.syntax.FunctionDefRaw): hydra.ast.Expr =
  {
  lazy val `async_`: Boolean = (fdr.async)
  lazy val name: hydra.ext.python.syntax.Name = (fdr.name)
  lazy val tparams: Seq[hydra.ext.python.syntax.TypeParameter] = (fdr.typeParams)
  lazy val params: Option[hydra.ext.python.syntax.Parameters] = (fdr.params)
  lazy val retType: Option[hydra.ext.python.syntax.Expression] = (fdr.returnType)
  lazy val block: hydra.ext.python.syntax.Block = (fdr.block)
  lazy val asyncKw: Option[hydra.ast.Expr] = hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](`async_`)(Some(hydra.serialization.cst("async")))(None)
  lazy val tparamPart: Option[hydra.ast.Expr] = hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.python.syntax.TypeParameter](tparams))(None)(Some(hydra.serialization.bracketList(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.python.syntax.TypeParameter, hydra.ast.Expr](hydra.ext.python.serde.encodeTypeParameter)(tparams))))
  lazy val paramPart: Option[hydra.ast.Expr] = hydra.lib.maybes.map[hydra.ext.python.syntax.Parameters, hydra.ast.Expr](hydra.ext.python.serde.encodeParameters)(params)
  lazy val retPart: Option[hydra.ast.Expr] = hydra.lib.maybes.map[hydra.ext.python.syntax.Expression, hydra.ast.Expr]((t: hydra.ext.python.syntax.Expression) =>
    hydra.serialization.spaceSep(Seq(hydra.serialization.cst("->"), hydra.ext.python.serde.encodeExpression(t))))(retType)
  hydra.serialization.newlineSep(Seq(hydra.serialization.noSep(Seq(hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(asyncKw, Some(hydra.serialization.cst("def")), Some(hydra.serialization.noSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.ext.python.serde.encodeName(name)), tparamPart, Some(hydra.serialization.cst("(")), paramPart, Some(hydra.serialization.cst(")")))))), retPart))), hydra.serialization.cst(":"))), hydra.ext.python.serde.encodeBlock(block)))
}

def encodeFunctionDefinition(fd: hydra.ext.python.syntax.FunctionDefinition): hydra.ast.Expr =
  {
  lazy val decs: Option[hydra.ext.python.syntax.Decorators] = (fd.decorators)
  lazy val raw: hydra.ext.python.syntax.FunctionDefRaw = (fd.raw)
  hydra.serialization.newlineSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.maybes.map[hydra.ext.python.syntax.Decorators, hydra.ast.Expr](hydra.ext.python.serde.encodeDecorators)(decs), Some(hydra.ext.python.serde.encodeFunctionDefRaw(raw)))))
}

def encodeGroup(g: hydra.ext.python.syntax.Group): hydra.ast.Expr =
  g match
  case hydra.ext.python.syntax.Group.expression(v_Group_expression_ne) => hydra.ext.python.serde.encodeNamedExpression(v_Group_expression_ne)
  case hydra.ext.python.syntax.Group.`yield`() => hydra.serialization.cst("(yield ...)")

def encodeGuard(g: hydra.ext.python.syntax.Guard): hydra.ast.Expr =
  hydra.serialization.spaceSep(Seq(hydra.serialization.cst("if"), hydra.ext.python.serde.encodeNamedExpression(g)))

def encodeImportFrom(`if_`: hydra.ext.python.syntax.ImportFrom): hydra.ast.Expr =
  {
  lazy val prefixes: Seq[hydra.ext.python.syntax.RelativeImportPrefix] = (`if_`.prefixes)
  lazy val name: Option[hydra.ext.python.syntax.DottedName] = (`if_`.dottedName)
  lazy val targets: hydra.ext.python.syntax.ImportFromTargets = (`if_`.targets)
  lazy val lhs: hydra.ast.Expr = hydra.serialization.noSep(hydra.lib.maybes.cat[hydra.ast.Expr](hydra.lib.lists.concat[Option[hydra.ast.Expr]](Seq(hydra.lib.lists.map[hydra.ext.python.syntax.RelativeImportPrefix, Option[hydra.ast.Expr]]((p: hydra.ext.python.syntax.RelativeImportPrefix) => Some(hydra.ext.python.serde.encodeRelativeImportPrefix(p)))(prefixes), Seq(hydra.lib.maybes.map[hydra.ext.python.syntax.DottedName, hydra.ast.Expr](hydra.ext.python.serde.encodeDottedName)(name))))))
  hydra.serialization.spaceSep(Seq(hydra.serialization.cst("from"), lhs, hydra.serialization.cst("import"), hydra.ext.python.serde.encodeImportFromTargets(targets)))
}

def encodeImportFromAsName(ifan: hydra.ext.python.syntax.ImportFromAsName): hydra.ast.Expr =
  {
  lazy val name: hydra.ext.python.syntax.Name = (ifan.name)
  lazy val alias: Option[hydra.ext.python.syntax.Name] = (ifan.as)
  hydra.lib.maybes.maybe[hydra.ast.Expr, hydra.ext.python.syntax.Name](hydra.ext.python.serde.encodeName(name))((a: hydra.ext.python.syntax.Name) =>
    hydra.serialization.spaceSep(Seq(hydra.ext.python.serde.encodeName(name), hydra.serialization.cst("as"), hydra.ext.python.serde.encodeName(a))))(alias)
}

def encodeImportFromTargets(t: hydra.ext.python.syntax.ImportFromTargets): hydra.ast.Expr =
  t match
  case hydra.ext.python.syntax.ImportFromTargets.simple(v_ImportFromTargets_simple_names) => hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.python.syntax.ImportFromAsName, hydra.ast.Expr](hydra.ext.python.serde.encodeImportFromAsName)(v_ImportFromTargets_simple_names))
  case hydra.ext.python.syntax.ImportFromTargets.parens(v_ImportFromTargets_parens_names) => hydra.serialization.noSep(Seq(hydra.serialization.cst("("), hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.python.syntax.ImportFromAsName, hydra.ast.Expr](hydra.ext.python.serde.encodeImportFromAsName)(v_ImportFromTargets_parens_names)), hydra.serialization.cst(")")))
  case hydra.ext.python.syntax.ImportFromTargets.star() => hydra.serialization.cst("*")

def encodeImportName(`in_`: hydra.ext.python.syntax.ImportName): hydra.ast.Expr =
  hydra.serialization.spaceSep(Seq(hydra.serialization.cst("import"), hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.python.syntax.DottedAsName, hydra.ast.Expr](hydra.ext.python.serde.encodeDottedAsName)(`in_`))))

def encodeImportStatement(`is_`: hydra.ext.python.syntax.ImportStatement): hydra.ast.Expr =
  `is_` match
  case hydra.ext.python.syntax.ImportStatement.name(v_ImportStatement_name_n) => hydra.ext.python.serde.encodeImportName(v_ImportStatement_name_n)
  case hydra.ext.python.syntax.ImportStatement.from(v_ImportStatement_from_f) => hydra.ext.python.serde.encodeImportFrom(v_ImportStatement_from_f)

def encodeInversion(i: hydra.ext.python.syntax.Inversion): hydra.ast.Expr =
  i match
  case hydra.ext.python.syntax.Inversion.not(v_Inversion_not_other) => hydra.serialization.spaceSep(Seq(hydra.serialization.cst("not"), hydra.ext.python.serde.encodeInversion(v_Inversion_not_other)))
  case hydra.ext.python.syntax.Inversion.simple(v_Inversion_simple_c) => hydra.ext.python.serde.encodeComparison(v_Inversion_simple_c)

def encodeKeywordPattern(kp: hydra.ext.python.syntax.KeywordPattern): hydra.ast.Expr =
  {
  lazy val name: hydra.ext.python.syntax.Name = (kp.name)
  lazy val pat: hydra.ext.python.syntax.Pattern = (kp.pattern)
  hydra.serialization.noSep(Seq(hydra.ext.python.serde.encodeName(name), hydra.serialization.cst("="), hydra.ext.python.serde.encodePattern(pat)))
}

def encodeKeywordPatterns(kp: hydra.ext.python.syntax.KeywordPatterns): hydra.ast.Expr =
  hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.python.syntax.KeywordPattern, hydra.ast.Expr](hydra.ext.python.serde.encodeKeywordPattern)(kp))

def encodeKvpair(kv: hydra.ext.python.syntax.Kvpair): hydra.ast.Expr =
  {
  lazy val k: hydra.ext.python.syntax.Expression = (kv.key)
  lazy val v: hydra.ext.python.syntax.Expression = (kv.value)
  hydra.serialization.spaceSep(Seq(hydra.serialization.noSep(Seq(hydra.ext.python.serde.encodeExpression(k), hydra.serialization.cst(":"))), hydra.ext.python.serde.encodeExpression(v)))
}

def encodeKwarg(k: hydra.ext.python.syntax.Kwarg): hydra.ast.Expr =
  {
  lazy val name: hydra.ext.python.syntax.Name = (k.name)
  lazy val expr: hydra.ext.python.syntax.Expression = (k.value)
  hydra.serialization.noSep(Seq(hydra.ext.python.serde.encodeName(name), hydra.serialization.cst("="), hydra.ext.python.serde.encodeExpression(expr)))
}

def encodeKwargOrDoubleStarred(kds: hydra.ext.python.syntax.KwargOrDoubleStarred): hydra.ast.Expr =
  kds match
  case hydra.ext.python.syntax.KwargOrDoubleStarred.kwarg(v_KwargOrDoubleStarred_kwarg_k) => hydra.ext.python.serde.encodeKwarg(v_KwargOrDoubleStarred_kwarg_k)
  case hydra.ext.python.syntax.KwargOrDoubleStarred.doubleStarred(v_KwargOrDoubleStarred_doubleStarred_e) => hydra.serialization.noSep(Seq(hydra.serialization.cst("**"), hydra.ext.python.serde.encodeExpression(v_KwargOrDoubleStarred_doubleStarred_e)))

def encodeKwargOrStarred(ks: hydra.ext.python.syntax.KwargOrStarred): hydra.ast.Expr =
  ks match
  case hydra.ext.python.syntax.KwargOrStarred.kwarg(v_KwargOrStarred_kwarg_k) => hydra.ext.python.serde.encodeKwarg(v_KwargOrStarred_kwarg_k)
  case hydra.ext.python.syntax.KwargOrStarred.starred(v_KwargOrStarred_starred_se) => hydra.ext.python.serde.encodeStarredExpression(v_KwargOrStarred_starred_se)

def encodeLambda(l: hydra.ext.python.syntax.Lambda): hydra.ast.Expr =
  {
  lazy val params: hydra.ext.python.syntax.LambdaParameters = (l.params)
  lazy val body: hydra.ext.python.syntax.Expression = (l.body)
  hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("lambda"), hydra.serialization.noSep(Seq(hydra.ext.python.serde.encodeLambdaParameters(params), hydra.serialization.cst(":"))), hydra.ext.python.serde.encodeExpression(body))))
}

def encodeLambdaParamNoDefault(p: hydra.ext.python.syntax.LambdaParamNoDefault): hydra.ast.Expr = hydra.ext.python.serde.encodeName(p)

def encodeLambdaParameters(lp: hydra.ext.python.syntax.LambdaParameters): hydra.ast.Expr =
  {
  lazy val nodef: Seq[hydra.ext.python.syntax.LambdaParamNoDefault] = (lp.paramNoDefault)
  hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.python.syntax.LambdaParamNoDefault, hydra.ast.Expr](hydra.ext.python.serde.encodeLambdaParamNoDefault)(nodef))
}

def encodeLambdaStarEtc(lse: hydra.ext.python.syntax.LambdaStarEtc): hydra.ast.Expr =
  lse match
  case hydra.ext.python.syntax.LambdaStarEtc.paramNoDefault(v_LambdaStarEtc_paramNoDefault_p) => hydra.ext.python.serde.encodeLambdaParamNoDefault(v_LambdaStarEtc_paramNoDefault_p)
  case hydra.ext.python.syntax.LambdaStarEtc.star() => hydra.serialization.cst("*...")
  case hydra.ext.python.syntax.LambdaStarEtc.paramMaybeDefault() => hydra.serialization.cst("...")
  case hydra.ext.python.syntax.LambdaStarEtc.kwds() => hydra.serialization.cst("**...")

def encodeList(l: hydra.ext.python.syntax.List): hydra.ast.Expr =
  hydra.serialization.bracketListAdaptive(hydra.lib.lists.map[hydra.ext.python.syntax.StarNamedExpression, hydra.ast.Expr](hydra.ext.python.serde.encodeStarNamedExpression)(l))

def encodeMatchStatement(ms: hydra.ext.python.syntax.MatchStatement): hydra.ast.Expr =
  {
  lazy val subj: hydra.ext.python.syntax.SubjectExpression = (ms.subject)
  lazy val cases: Seq[hydra.ext.python.syntax.CaseBlock] = (ms.cases)
  hydra.serialization.newlineSep(Seq(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("match"), hydra.serialization.noSep(Seq(hydra.ext.python.serde.encodeSubjectExpression(subj), hydra.serialization.cst(":"))))), hydra.serialization.tabIndentDoubleSpace(hydra.lib.lists.map[hydra.ext.python.syntax.CaseBlock, hydra.ast.Expr](hydra.ext.python.serde.encodeCaseBlock)(cases))))
}

def encodeModule(mod: hydra.ext.python.syntax.Module): hydra.ast.Expr =
  {
  lazy val warning: hydra.ast.Expr = hydra.serialization.cst(hydra.ext.python.serde.toPythonComments(hydra.constants.warningAutoGeneratedFile))
  lazy val groups: Seq[hydra.ast.Expr] = hydra.lib.lists.map[Seq[hydra.ext.python.syntax.Statement], hydra.ast.Expr]((group: Seq[hydra.ext.python.syntax.Statement]) =>
    hydra.serialization.newlineSep(hydra.lib.lists.map[hydra.ext.python.syntax.Statement, hydra.ast.Expr](hydra.ext.python.serde.encodeStatement)(group)))(mod)
  hydra.serialization.doubleNewlineSep(hydra.lib.lists.cons[hydra.ast.Expr](warning)(groups))
}

def encodeName(n: hydra.ext.python.syntax.Name): hydra.ast.Expr = hydra.serialization.cst(n)

def encodeNamedExpression(ne: hydra.ext.python.syntax.NamedExpression): hydra.ast.Expr =
  ne match
  case hydra.ext.python.syntax.NamedExpression.simple(v_NamedExpression_simple_e) => hydra.ext.python.serde.encodeExpression(v_NamedExpression_simple_e)
  case hydra.ext.python.syntax.NamedExpression.assignment(v_NamedExpression_assignment_ae) => hydra.ext.python.serde.encodeAssignmentExpression(v_NamedExpression_assignment_ae)

def encodeNameOrAttribute(noa: hydra.ext.python.syntax.NameOrAttribute): hydra.ast.Expr =
  hydra.serialization.dotSep(hydra.lib.lists.map[hydra.ext.python.syntax.Name, hydra.ast.Expr](hydra.ext.python.serde.encodeName)(noa))

def encodeNumber(num: hydra.ext.python.syntax.Number): hydra.ast.Expr =
  num match
  case hydra.ext.python.syntax.Number.float(v_Number_float_f) => hydra.serialization.cst(hydra.lib.literals.showBigfloat(v_Number_float_f))
  case hydra.ext.python.syntax.Number.integer(v_Number_integer_i) => hydra.serialization.cst(hydra.lib.literals.showBigint(v_Number_integer_i))

def encodeOrPattern(op: hydra.ext.python.syntax.OrPattern): hydra.ast.Expr =
  hydra.serialization.symbolSep("|")(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.python.syntax.ClosedPattern, hydra.ast.Expr](hydra.ext.python.serde.encodeClosedPattern)(op))

def encodeParam(p: hydra.ext.python.syntax.Param): hydra.ast.Expr =
  {
  lazy val name: hydra.ext.python.syntax.Name = (p.name)
  lazy val ann: Option[hydra.ext.python.syntax.Annotation] = (p.annotation)
  hydra.serialization.noSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.ext.python.serde.encodeName(name)), hydra.lib.maybes.map[hydra.ext.python.syntax.Annotation, hydra.ast.Expr](hydra.ext.python.serde.encodeAnnotation)(ann))))
}

def encodeParamNoDefault(pnd: hydra.ext.python.syntax.ParamNoDefault): hydra.ast.Expr = hydra.ext.python.serde.encodeParam(pnd.param)

def encodeParamNoDefaultParameters(pndp: hydra.ext.python.syntax.ParamNoDefaultParameters): hydra.ast.Expr =
  {
  lazy val nodef: Seq[hydra.ext.python.syntax.ParamNoDefault] = (pndp.paramNoDefault)
  hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.python.syntax.ParamNoDefault, hydra.ast.Expr](hydra.ext.python.serde.encodeParamNoDefault)(nodef))
}

def encodeParameters(p: hydra.ext.python.syntax.Parameters): hydra.ast.Expr =
  p match
  case hydra.ext.python.syntax.Parameters.paramNoDefault(v_Parameters_paramNoDefault_pnd) => hydra.ext.python.serde.encodeParamNoDefaultParameters(v_Parameters_paramNoDefault_pnd)
  case hydra.ext.python.syntax.Parameters.slashNoDefault() => hydra.serialization.cst("...")
  case hydra.ext.python.syntax.Parameters.slashWithDefault() => hydra.serialization.cst("...")

def encodePattern(p: hydra.ext.python.syntax.Pattern): hydra.ast.Expr =
  p match
  case hydra.ext.python.syntax.Pattern.or(v_Pattern_or_op) => hydra.ext.python.serde.encodeOrPattern(v_Pattern_or_op)
  case hydra.ext.python.syntax.Pattern.as() => hydra.serialization.cst("... as ...")

def encodePatternCaptureTarget(pct: hydra.ext.python.syntax.PatternCaptureTarget): hydra.ast.Expr = hydra.ext.python.serde.encodeName(pct)

def encodePatterns(ps: hydra.ext.python.syntax.Patterns): hydra.ast.Expr =
  ps match
  case hydra.ext.python.syntax.Patterns.pattern(v_Patterns_pattern_p) => hydra.ext.python.serde.encodePattern(v_Patterns_pattern_p)
  case hydra.ext.python.syntax.Patterns.sequence() => hydra.serialization.cst("...")

def encodePosArg(pa: hydra.ext.python.syntax.PosArg): hydra.ast.Expr =
  pa match
  case hydra.ext.python.syntax.PosArg.starred(v_PosArg_starred_se) => hydra.ext.python.serde.encodeStarredExpression(v_PosArg_starred_se)
  case hydra.ext.python.syntax.PosArg.assignment(v_PosArg_assignment_ae) => hydra.ext.python.serde.encodeAssignmentExpression(v_PosArg_assignment_ae)
  case hydra.ext.python.syntax.PosArg.expression(v_PosArg_expression_e) => hydra.ext.python.serde.encodeExpression(v_PosArg_expression_e)

def encodePositionalPatterns(pp: hydra.ext.python.syntax.PositionalPatterns): hydra.ast.Expr =
  hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.python.syntax.Pattern, hydra.ast.Expr](hydra.ext.python.serde.encodePattern)(pp))

def encodePower(p: hydra.ext.python.syntax.Power): hydra.ast.Expr =
  {
  lazy val lhs: hydra.ext.python.syntax.AwaitPrimary = (p.lhs)
  lazy val rhs: Option[hydra.ext.python.syntax.Factor] = (p.rhs)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.ext.python.serde.encodeAwaitPrimary(lhs)), hydra.lib.maybes.map[hydra.ext.python.syntax.Factor, hydra.ast.Expr]((r: hydra.ext.python.syntax.Factor) =>
    hydra.serialization.spaceSep(Seq(hydra.serialization.cst("**"), hydra.ext.python.serde.encodeFactor(r))))(rhs))))
}

def encodePrimary(p: hydra.ext.python.syntax.Primary): hydra.ast.Expr =
  p match
  case hydra.ext.python.syntax.Primary.simple(v_Primary_simple_a) => hydra.ext.python.serde.encodeAtom(v_Primary_simple_a)
  case hydra.ext.python.syntax.Primary.compound(v_Primary_compound_pwr) => hydra.ext.python.serde.encodePrimaryWithRhs(v_Primary_compound_pwr)

def encodePrimaryRhs(rhs: hydra.ext.python.syntax.PrimaryRhs): hydra.ast.Expr =
  rhs match
  case hydra.ext.python.syntax.PrimaryRhs.call(v_PrimaryRhs_call_args) => hydra.serialization.noSep(Seq(hydra.serialization.cst("("), hydra.ext.python.serde.encodeArgs(v_PrimaryRhs_call_args), hydra.serialization.cst(")")))
  case hydra.ext.python.syntax.PrimaryRhs.project(v_PrimaryRhs_project_name) => hydra.serialization.noSep(Seq(hydra.serialization.cst("."), hydra.ext.python.serde.encodeName(v_PrimaryRhs_project_name)))
  case hydra.ext.python.syntax.PrimaryRhs.slices(v_PrimaryRhs_slices_slices) => hydra.serialization.noSep(Seq(hydra.serialization.cst("["), hydra.ext.python.serde.encodeSlices(v_PrimaryRhs_slices_slices), hydra.serialization.cst("]")))
  case hydra.ext.python.syntax.PrimaryRhs.genexp() => hydra.serialization.cst("[...]")

def encodePrimaryWithRhs(pwr: hydra.ext.python.syntax.PrimaryWithRhs): hydra.ast.Expr =
  {
  lazy val prim: hydra.ext.python.syntax.Primary = (pwr.primary)
  lazy val rhs: hydra.ext.python.syntax.PrimaryRhs = (pwr.rhs)
  hydra.serialization.noSep(Seq(hydra.ext.python.serde.encodePrimary(prim), hydra.ext.python.serde.encodePrimaryRhs(rhs)))
}

def encodeRaiseExpression(re: hydra.ext.python.syntax.RaiseExpression): hydra.ast.Expr =
  {
  lazy val expr: hydra.ext.python.syntax.Expression = (re.expression)
  lazy val `from_`: Option[hydra.ext.python.syntax.Expression] = (re.from)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.ext.python.serde.encodeExpression(expr)), hydra.lib.maybes.map[hydra.ext.python.syntax.Expression, hydra.ast.Expr]((f: hydra.ext.python.syntax.Expression) =>
    hydra.serialization.spaceSep(Seq(hydra.serialization.cst("from"), hydra.ext.python.serde.encodeExpression(f))))(`from_`))))
}

def encodeRaiseStatement(rs: hydra.ext.python.syntax.RaiseStatement): hydra.ast.Expr =
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.serialization.cst("raise")), hydra.lib.maybes.map[hydra.ext.python.syntax.RaiseExpression, hydra.ast.Expr](hydra.ext.python.serde.encodeRaiseExpression)(rs))))

def encodeRelativeImportPrefix(p: hydra.ext.python.syntax.RelativeImportPrefix): hydra.ast.Expr =
  p match
  case hydra.ext.python.syntax.RelativeImportPrefix.dot() => hydra.serialization.cst(".")
  case hydra.ext.python.syntax.RelativeImportPrefix.ellipsis() => hydra.serialization.cst("...")

def encodeReturnStatement(rs: hydra.ext.python.syntax.ReturnStatement): hydra.ast.Expr =
  hydra.serialization.spaceSep(Seq(hydra.serialization.cst("return"), hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.python.syntax.StarExpression, hydra.ast.Expr](hydra.ext.python.serde.encodeStarExpression)(rs))))

def encodeSet(s: hydra.ext.python.syntax.Set): hydra.ast.Expr =
  hydra.serialization.bracesListAdaptive(hydra.lib.lists.map[hydra.ext.python.syntax.StarNamedExpression, hydra.ast.Expr](hydra.ext.python.serde.encodeStarNamedExpression)(s))

def encodeShiftExpression(se: hydra.ext.python.syntax.ShiftExpression): hydra.ast.Expr = hydra.ext.python.serde.encodeSum(se.rhs)

def encodeSimpleStatement(ss: hydra.ext.python.syntax.SimpleStatement): hydra.ast.Expr =
  ss match
  case hydra.ext.python.syntax.SimpleStatement.assignment(v_SimpleStatement_assignment_a) => hydra.ext.python.serde.encodeAssignment(v_SimpleStatement_assignment_a)
  case hydra.ext.python.syntax.SimpleStatement.starExpressions(v_SimpleStatement_starExpressions_es) => hydra.serialization.newlineSep(hydra.lib.lists.map[hydra.ext.python.syntax.StarExpression, hydra.ast.Expr](hydra.ext.python.serde.encodeStarExpression)(v_SimpleStatement_starExpressions_es))
  case hydra.ext.python.syntax.SimpleStatement.`return`(v_SimpleStatement_return_r) => hydra.ext.python.serde.encodeReturnStatement(v_SimpleStatement_return_r)
  case hydra.ext.python.syntax.SimpleStatement.raise(v_SimpleStatement_raise_r) => hydra.ext.python.serde.encodeRaiseStatement(v_SimpleStatement_raise_r)
  case hydra.ext.python.syntax.SimpleStatement.pass() => hydra.serialization.cst("pass")
  case hydra.ext.python.syntax.SimpleStatement.break() => hydra.serialization.cst("break")
  case hydra.ext.python.syntax.SimpleStatement.continue() => hydra.serialization.cst("continue")
  case hydra.ext.python.syntax.SimpleStatement.`import`(v_SimpleStatement_import_i) => hydra.ext.python.serde.encodeImportStatement(v_SimpleStatement_import_i)
  case hydra.ext.python.syntax.SimpleStatement.typeAlias(v_SimpleStatement_typeAlias_t) => hydra.ext.python.serde.encodeTypeAlias(v_SimpleStatement_typeAlias_t)
  case hydra.ext.python.syntax.SimpleStatement.assert() => hydra.serialization.cst("assert ...")
  case hydra.ext.python.syntax.SimpleStatement.global() => hydra.serialization.cst("global ...")
  case hydra.ext.python.syntax.SimpleStatement.nonlocal() => hydra.serialization.cst("nonlocal ...")
  case hydra.ext.python.syntax.SimpleStatement.del() => hydra.serialization.cst("del ...")

def encodeSimpleTypeParameter(stp: hydra.ext.python.syntax.SimpleTypeParameter): hydra.ast.Expr = hydra.ext.python.serde.encodeName(stp.name)

def encodeSingleTarget(st: hydra.ext.python.syntax.SingleTarget): hydra.ast.Expr =
  st match
  case hydra.ext.python.syntax.SingleTarget.name(v_SingleTarget_name_n) => hydra.ext.python.serde.encodeName(v_SingleTarget_name_n)
  case hydra.ext.python.syntax.SingleTarget.parens() => hydra.serialization.cst("(...)")
  case hydra.ext.python.syntax.SingleTarget.subscriptAttributeTarget() => hydra.serialization.cst("...")

def encodeSlice(s: hydra.ext.python.syntax.Slice): hydra.ast.Expr =
  s match
  case hydra.ext.python.syntax.Slice.named(v_Slice_named_ne) => hydra.ext.python.serde.encodeNamedExpression(v_Slice_named_ne)
  case hydra.ext.python.syntax.Slice.`slice_`() => hydra.serialization.cst(":")

def encodeSliceOrStarredExpression(s: hydra.ext.python.syntax.SliceOrStarredExpression): hydra.ast.Expr =
  s match
  case hydra.ext.python.syntax.SliceOrStarredExpression.slice(v_SliceOrStarredExpression_slice_sl) => hydra.ext.python.serde.encodeSlice(v_SliceOrStarredExpression_slice_sl)
  case hydra.ext.python.syntax.SliceOrStarredExpression.starred(v_SliceOrStarredExpression_starred_se) => hydra.ext.python.serde.encodeStarredExpression(v_SliceOrStarredExpression_starred_se)

def encodeSlices(s: hydra.ext.python.syntax.Slices): hydra.ast.Expr =
  {
  lazy val hd: hydra.ext.python.syntax.Slice = (s.head)
  lazy val tl: Seq[hydra.ext.python.syntax.SliceOrStarredExpression] = (s.tail)
  hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.cons[hydra.ast.Expr](hydra.ext.python.serde.encodeSlice(hd))(hydra.lib.lists.map[hydra.ext.python.syntax.SliceOrStarredExpression, hydra.ast.Expr](hydra.ext.python.serde.encodeSliceOrStarredExpression)(tl)))
}

def encodeStarAtom(sa: hydra.ext.python.syntax.StarAtom): hydra.ast.Expr =
  sa match
  case hydra.ext.python.syntax.StarAtom.name(v_StarAtom_name_n) => hydra.ext.python.serde.encodeName(v_StarAtom_name_n)
  case hydra.ext.python.syntax.StarAtom.targetWithStarAtom() => hydra.serialization.cst("(...)")
  case hydra.ext.python.syntax.StarAtom.starTargetsTupleSeq() => hydra.serialization.cst("(...)")
  case hydra.ext.python.syntax.StarAtom.starTargetsListSeq() => hydra.serialization.cst("[...]")

def encodeStarExpression(se: hydra.ext.python.syntax.StarExpression): hydra.ast.Expr =
  se match
  case hydra.ext.python.syntax.StarExpression.star(v_StarExpression_star_bor) => hydra.serialization.noSep(Seq(hydra.serialization.cst("*"), hydra.ext.python.serde.encodeBitwiseOr(v_StarExpression_star_bor)))
  case hydra.ext.python.syntax.StarExpression.simple(v_StarExpression_simple_e) => hydra.ext.python.serde.encodeExpression(v_StarExpression_simple_e)

def encodeStarNamedExpression(sne: hydra.ext.python.syntax.StarNamedExpression): hydra.ast.Expr =
  sne match
  case hydra.ext.python.syntax.StarNamedExpression.star(v_StarNamedExpression_star_bor) => hydra.serialization.noSep(Seq(hydra.serialization.cst("*"), hydra.ext.python.serde.encodeBitwiseOr(v_StarNamedExpression_star_bor)))
  case hydra.ext.python.syntax.StarNamedExpression.simple(v_StarNamedExpression_simple_ne) => hydra.ext.python.serde.encodeNamedExpression(v_StarNamedExpression_simple_ne)

def encodeStarTarget(st: hydra.ext.python.syntax.StarTarget): hydra.ast.Expr =
  st match
  case hydra.ext.python.syntax.StarTarget.unstarred(v_StarTarget_unstarred_t) => hydra.ext.python.serde.encodeTargetWithStarAtom(v_StarTarget_unstarred_t)
  case hydra.ext.python.syntax.StarTarget.starred(v_StarTarget_starred_inner) => hydra.serialization.noSep(Seq(hydra.serialization.cst("*"), hydra.ext.python.serde.encodeStarTarget(v_StarTarget_starred_inner)))

def encodeStarredExpression(se: hydra.ext.python.syntax.StarredExpression): hydra.ast.Expr =
  hydra.serialization.noSep(Seq(hydra.serialization.cst("*"), hydra.ext.python.serde.encodeExpression(se)))

def encodeStatement(stmt: hydra.ext.python.syntax.Statement): hydra.ast.Expr =
  stmt match
  case hydra.ext.python.syntax.Statement.annotated(v_Statement_annotated_a) => hydra.ext.python.serde.encodeAnnotatedStatement(v_Statement_annotated_a)
  case hydra.ext.python.syntax.Statement.simple(v_Statement_simple_ss) => hydra.serialization.newlineSep(hydra.lib.lists.map[hydra.ext.python.syntax.SimpleStatement, hydra.ast.Expr](hydra.ext.python.serde.encodeSimpleStatement)(v_Statement_simple_ss))
  case hydra.ext.python.syntax.Statement.compound(v_Statement_compound_c) => hydra.ext.python.serde.encodeCompoundStatement(v_Statement_compound_c)

def encodeString(s: hydra.ext.python.syntax.String): hydra.ast.Expr =
  {
  lazy val content: scala.Predef.String = (s.value)
  lazy val style: hydra.ext.python.syntax.QuoteStyle = (s.quoteStyle)
  style match
    case hydra.ext.python.syntax.QuoteStyle.single() => hydra.serialization.cst(hydra.ext.python.serde.escapePythonString(false)(content))
    case hydra.ext.python.syntax.QuoteStyle.double() => hydra.serialization.cst(hydra.ext.python.serde.escapePythonString(true)(content))
    case hydra.ext.python.syntax.QuoteStyle.triple() => hydra.serialization.noSep(Seq(hydra.serialization.cst("r\"\"\""), hydra.serialization.cst(content), hydra.serialization.cst("\"\"\"")))
}

def encodeSubjectExpression(se: hydra.ext.python.syntax.SubjectExpression): hydra.ast.Expr =
  se match
  case hydra.ext.python.syntax.SubjectExpression.simple(v_SubjectExpression_simple_ne) => hydra.ext.python.serde.encodeNamedExpression(v_SubjectExpression_simple_ne)
  case hydra.ext.python.syntax.SubjectExpression.tuple() => hydra.serialization.cst("*...")

def encodeSum(s: hydra.ext.python.syntax.Sum): hydra.ast.Expr = hydra.ext.python.serde.encodeTerm(s.rhs)

def encodeTerm(t: hydra.ext.python.syntax.Term): hydra.ast.Expr = hydra.ext.python.serde.encodeFactor(t.rhs)

def encodeTargetWithStarAtom(t: hydra.ext.python.syntax.TargetWithStarAtom): hydra.ast.Expr =
  t match
  case hydra.ext.python.syntax.TargetWithStarAtom.atom(v_TargetWithStarAtom_atom_a) => hydra.ext.python.serde.encodeStarAtom(v_TargetWithStarAtom_atom_a)
  case hydra.ext.python.syntax.TargetWithStarAtom.project(v_TargetWithStarAtom_project_pn) => hydra.ext.python.serde.encodeTPrimaryAndName(v_TargetWithStarAtom_project_pn)
  case hydra.ext.python.syntax.TargetWithStarAtom.slices() => hydra.serialization.cst("...")

def encodeTPrimaryAndName(pn: hydra.ext.python.syntax.TPrimaryAndName): hydra.ast.Expr =
  {
  lazy val prim: hydra.ext.python.syntax.TPrimary = (pn.primary)
  lazy val `name_`: hydra.ext.python.syntax.Name = (pn.name)
  hydra.serialization.noSep(Seq(hydra.ext.python.serde.encodeTPrimary(prim), hydra.serialization.cst("."), hydra.ext.python.serde.encodeName(`name_`)))
}

def encodeTPrimary(tp: hydra.ext.python.syntax.TPrimary): hydra.ast.Expr =
  tp match
  case hydra.ext.python.syntax.TPrimary.atom(v_TPrimary_atom_a) => hydra.ext.python.serde.encodeAtom(v_TPrimary_atom_a)
  case hydra.ext.python.syntax.TPrimary.primaryAndName(v_TPrimary_primaryAndName_pn) => hydra.ext.python.serde.encodeTPrimaryAndName(v_TPrimary_primaryAndName_pn)
  case hydra.ext.python.syntax.TPrimary.primaryAndSlices() => hydra.serialization.cst("...")
  case hydra.ext.python.syntax.TPrimary.primaryAndGenexp() => hydra.serialization.cst("...")
  case hydra.ext.python.syntax.TPrimary.primaryAndArguments() => hydra.serialization.cst("...")

def encodeTuple(t: hydra.ext.python.syntax.Tuple): hydra.ast.Expr =
  {
  lazy val es: Seq[hydra.ext.python.syntax.StarNamedExpression] = t
  hydra.lib.logic.ifElse[hydra.ast.Expr](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.ext.python.syntax.StarNamedExpression](es))(1))(hydra.serialization.parens(hydra.serialization.noSep(Seq(hydra.ext.python.serde.encodeStarNamedExpression(hydra.lib.lists.head[hydra.ext.python.syntax.StarNamedExpression](es)), hydra.serialization.cst(",")))))(hydra.serialization.parenList(false)(hydra.lib.lists.map[hydra.ext.python.syntax.StarNamedExpression, hydra.ast.Expr](hydra.ext.python.serde.encodeStarNamedExpression)(es)))
}

def encodeTypeAlias(ta: hydra.ext.python.syntax.TypeAlias): hydra.ast.Expr =
  {
  lazy val name: hydra.ext.python.syntax.Name = (ta.name)
  lazy val tparams: Seq[hydra.ext.python.syntax.TypeParameter] = (ta.typeParams)
  lazy val expr: hydra.ext.python.syntax.Expression = (ta.expression)
  lazy val alias: hydra.ast.Expr = hydra.serialization.noSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.ext.python.serde.encodeName(name)), hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.ext.python.syntax.TypeParameter](tparams))(None)(Some(hydra.serialization.bracketList(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.ext.python.syntax.TypeParameter, hydra.ast.Expr](hydra.ext.python.serde.encodeTypeParameter)(tparams)))))))
  hydra.serialization.spaceSep(Seq(hydra.serialization.cst("type"), alias, hydra.serialization.cst("="), hydra.ext.python.serde.encodeExpression(expr)))
}

def encodeTypeParameter(tp: hydra.ext.python.syntax.TypeParameter): hydra.ast.Expr =
  tp match
  case hydra.ext.python.syntax.TypeParameter.simple(v_TypeParameter_simple_s) => hydra.ext.python.serde.encodeSimpleTypeParameter(v_TypeParameter_simple_s)
  case hydra.ext.python.syntax.TypeParameter.star() => hydra.serialization.cst("*...")
  case hydra.ext.python.syntax.TypeParameter.doubleStar() => hydra.serialization.cst("**...")

def encodeTypedAssignment(ta: hydra.ext.python.syntax.TypedAssignment): hydra.ast.Expr =
  {
  lazy val lhs: hydra.ext.python.syntax.SingleTarget = (ta.lhs)
  lazy val typ: hydra.ext.python.syntax.Expression = (ta.`type`)
  lazy val rhs: Option[hydra.ext.python.syntax.AnnotatedRhs] = (ta.rhs)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.serialization.noSep(Seq(hydra.ext.python.serde.encodeSingleTarget(lhs), hydra.serialization.cst(":")))), Some(hydra.ext.python.serde.encodeExpression(typ)), hydra.lib.maybes.map[hydra.ext.python.syntax.AnnotatedRhs, hydra.ast.Expr](hydra.ext.python.serde.encodeAnnotatedRhs)(rhs))))
}

def encodeUntypedAssignment(ua: hydra.ext.python.syntax.UntypedAssignment): hydra.ast.Expr =
  {
  lazy val targets: Seq[hydra.ext.python.syntax.StarTarget] = (ua.targets)
  lazy val rhs: hydra.ext.python.syntax.AnnotatedRhs = (ua.rhs)
  hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(hydra.lib.lists.map[hydra.ext.python.syntax.StarTarget, hydra.ast.Expr](hydra.ext.python.serde.encodeStarTarget)(targets), Seq(hydra.ext.python.serde.encodeAnnotatedRhs(rhs)))))
}

def encodeValuePattern(vp: hydra.ext.python.syntax.ValuePattern): hydra.ast.Expr = hydra.ext.python.serde.encodeAttribute(vp)

def encodeWhileStatement(ws: hydra.ext.python.syntax.WhileStatement): hydra.ast.Expr =
  {
  lazy val cond: hydra.ext.python.syntax.NamedExpression = (ws.condition)
  lazy val body: hydra.ext.python.syntax.Block = (ws.body)
  lazy val `else_`: Option[hydra.ext.python.syntax.Block] = (ws.`else`)
  hydra.serialization.newlineSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.serialization.newlineSep(Seq(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("while"), hydra.serialization.noSep(Seq(hydra.ext.python.serde.encodeNamedExpression(cond), hydra.serialization.cst(":"))))), hydra.ext.python.serde.encodeBlock(body)))), hydra.lib.maybes.map[hydra.ext.python.syntax.Block, hydra.ast.Expr]((eb: hydra.ext.python.syntax.Block) =>
    hydra.serialization.newlineSep(Seq(hydra.serialization.cst("else:"), hydra.ext.python.serde.encodeBlock(eb))))(`else_`))))
}

def escapePythonString(doubleQuoted: Boolean)(s: scala.Predef.String): scala.Predef.String =
  {
  def replace(old: scala.Predef.String)(`new`: scala.Predef.String)(str: scala.Predef.String): scala.Predef.String = hydra.lib.strings.intercalate(`new`)(hydra.lib.strings.splitOn(old)(str))
  lazy val s1: scala.Predef.String = replace("\\")("\\\\")(s)
  lazy val s2: scala.Predef.String = replace("\u0000")("\\x00")(s1)
  lazy val s3: scala.Predef.String = replace("\n")("\\n")(s2)
  lazy val s4: scala.Predef.String = replace("\t")("\\t")(s3)
  lazy val s5: scala.Predef.String = replace("\r")("\\r")(s4)
  lazy val escaped: scala.Predef.String = hydra.lib.logic.ifElse[scala.Predef.String](doubleQuoted)(replace("\"")("\\\"")(s5))(replace("'")("\\'")(s5))
  lazy val quote: scala.Predef.String = hydra.lib.logic.ifElse[scala.Predef.String](doubleQuoted)("\"")("'")
  hydra.lib.strings.cat2(quote)(hydra.lib.strings.cat2(escaped)(quote))
}

def toPythonComments(`doc_`: scala.Predef.String): scala.Predef.String =
  hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[scala.Predef.String](`doc_`)(""))("")(hydra.lib.strings.intercalate("\n")(hydra.lib.lists.map[scala.Predef.String, scala.Predef.String]((line: scala.Predef.String) => hydra.lib.strings.cat2("# ")(line))(hydra.lib.strings.lines(`doc_`))))
