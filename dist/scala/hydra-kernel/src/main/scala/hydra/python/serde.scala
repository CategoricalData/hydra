package hydra.python.serde

import hydra.python.syntax.*

def encodeAnnotatedRhs(arhs: hydra.python.syntax.AnnotatedRhs): hydra.ast.Expr =
  hydra.serialization.spaceSep(Seq(hydra.serialization.cst("="), arhs match
  case hydra.python.syntax.AnnotatedRhs.star(v_AnnotatedRhs_star_ses) => hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.python.syntax.StarExpression,
     hydra.ast.Expr](hydra.python.serde.encodeStarExpression)(v_AnnotatedRhs_star_ses))
  case hydra.python.syntax.AnnotatedRhs.`yield`(v_AnnotatedRhs_yield__) => hydra.serialization.cst("yield ...")))

def encodeAnnotatedStatement(`as_`: hydra.python.syntax.AnnotatedStatement): hydra.ast.Expr =
  {
  lazy val `doc_`: scala.Predef.String = (`as_`.comment)
  lazy val stmt: hydra.python.syntax.Statement = (`as_`.statement)
  hydra.serialization.newlineSep(Seq(hydra.serialization.cst(hydra.python.serde.toPythonComments(`doc_`)), hydra.python.serde.encodeStatement(stmt)))
}

def encodeAnnotation(ann: hydra.python.syntax.Annotation): hydra.ast.Expr =
  hydra.serialization.spaceSep(Seq(hydra.serialization.cst(":"), hydra.python.serde.encodeExpression(ann)))

def encodeArgs(args: hydra.python.syntax.Args): hydra.ast.Expr =
  {
  lazy val pos: Seq[hydra.python.syntax.PosArg] = (args.positional)
  lazy val ks: Seq[hydra.python.syntax.KwargOrStarred] = (args.kwargOrStarred)
  lazy val kss: Seq[hydra.python.syntax.KwargOrDoubleStarred] = (args.kwargOrDoubleStarred)
  hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.concat[hydra.ast.Expr](Seq(hydra.lib.lists.map[hydra.python.syntax.PosArg,
     hydra.ast.Expr](hydra.python.serde.encodePosArg)(pos), hydra.lib.lists.map[hydra.python.syntax.KwargOrStarred,
     hydra.ast.Expr](hydra.python.serde.encodeKwargOrStarred)(ks), hydra.lib.lists.map[hydra.python.syntax.KwargOrDoubleStarred,
     hydra.ast.Expr](hydra.python.serde.encodeKwargOrDoubleStarred)(kss))))
}

def encodeAssignment(a: hydra.python.syntax.Assignment): hydra.ast.Expr =
  a match
  case hydra.python.syntax.Assignment.typed(v_Assignment_typed_t) => hydra.python.serde.encodeTypedAssignment(v_Assignment_typed_t)
  case hydra.python.syntax.Assignment.untyped(v_Assignment_untyped_u) => hydra.python.serde.encodeUntypedAssignment(v_Assignment_untyped_u)
  case hydra.python.syntax.Assignment.aug(v_Assignment_aug__) => hydra.serialization.cst("... += ...")

def encodeAssignmentExpression(ae: hydra.python.syntax.AssignmentExpression): hydra.ast.Expr =
  {
  lazy val name: hydra.python.syntax.Name = (ae.name)
  lazy val expr: hydra.python.syntax.Expression = (ae.expression)
  hydra.serialization.spaceSep(Seq(hydra.python.serde.encodeName(name), hydra.serialization.cst(":="), hydra.python.serde.encodeExpression(expr)))
}

def encodeAtom(atom: hydra.python.syntax.Atom): hydra.ast.Expr =
  atom match
  case hydra.python.syntax.Atom.dict(v_Atom_dict_d) => hydra.python.serde.encodeDict(v_Atom_dict_d)
  case hydra.python.syntax.Atom.dictcomp(v_Atom_dictcomp__) => hydra.serialization.cst("{...}")
  case hydra.python.syntax.Atom.ellipsis => hydra.serialization.cst("...")
  case hydra.python.syntax.Atom.`false` => hydra.serialization.cst("False")
  case hydra.python.syntax.Atom.genexp(v_Atom_genexp__) => hydra.serialization.cst("(...)")
  case hydra.python.syntax.Atom.group(v_Atom_group_g) => hydra.python.serde.encodeGroup(v_Atom_group_g)
  case hydra.python.syntax.Atom.list(v_Atom_list_l) => hydra.python.serde.encodeList(v_Atom_list_l)
  case hydra.python.syntax.Atom.listcomp(v_Atom_listcomp__) => hydra.serialization.cst("[...]")
  case hydra.python.syntax.Atom.name(v_Atom_name_n) => hydra.python.serde.encodeName(v_Atom_name_n)
  case hydra.python.syntax.Atom.none => hydra.serialization.cst("None")
  case hydra.python.syntax.Atom.number(v_Atom_number_n) => hydra.python.serde.encodeNumber(v_Atom_number_n)
  case hydra.python.syntax.Atom.set(v_Atom_set_s) => hydra.python.serde.encodeSet(v_Atom_set_s)
  case hydra.python.syntax.Atom.setcomp(v_Atom_setcomp__) => hydra.serialization.cst("{...}")
  case hydra.python.syntax.Atom.string(v_Atom_string_s) => hydra.python.serde.encodeString(v_Atom_string_s)
  case hydra.python.syntax.Atom.`true` => hydra.serialization.cst("True")
  case hydra.python.syntax.Atom.tuple(v_Atom_tuple_t) => hydra.python.serde.encodeTuple(v_Atom_tuple_t)

def encodeAttribute(attr: hydra.python.syntax.Attribute): hydra.ast.Expr =
  hydra.serialization.dotSep(hydra.lib.lists.map[hydra.python.syntax.Name, hydra.ast.Expr](hydra.python.serde.encodeName)(attr))

def encodeAwaitPrimary(ap: hydra.python.syntax.AwaitPrimary): hydra.ast.Expr =
  {
  lazy val `await_`: Boolean = (ap.await)
  lazy val primary: hydra.python.syntax.Primary = (ap.primary)
  hydra.lib.logic.ifElse[hydra.ast.Expr](`await_`)(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("await"),
     hydra.python.serde.encodePrimary(primary))))(hydra.python.serde.encodePrimary(primary))
}

def encodeBitwiseAnd(band: hydra.python.syntax.BitwiseAnd): hydra.ast.Expr =
  {
  lazy val lhs: Option[hydra.python.syntax.BitwiseAnd] = (band.lhs)
  lazy val rhs: hydra.python.syntax.ShiftExpression = (band.rhs)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.maybes.map[hydra.python.syntax.BitwiseAnd,
     hydra.ast.Expr]((l: hydra.python.syntax.BitwiseAnd) =>
    hydra.serialization.spaceSep(Seq(hydra.python.serde.encodeBitwiseAnd(l), hydra.serialization.cst("&"))))(lhs),
       Some(hydra.python.serde.encodeShiftExpression(rhs)))))
}

def encodeBitwiseOr(bor: hydra.python.syntax.BitwiseOr): hydra.ast.Expr =
  {
  lazy val lhs: Option[hydra.python.syntax.BitwiseOr] = (bor.lhs)
  lazy val rhs: hydra.python.syntax.BitwiseXor = (bor.rhs)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.maybes.map[hydra.python.syntax.BitwiseOr,
     hydra.ast.Expr]((l: hydra.python.syntax.BitwiseOr) =>
    hydra.serialization.spaceSep(Seq(hydra.python.serde.encodeBitwiseOr(l), hydra.serialization.cst("|"))))(lhs),
       Some(hydra.python.serde.encodeBitwiseXor(rhs)))))
}

def encodeBitwiseXor(bxor: hydra.python.syntax.BitwiseXor): hydra.ast.Expr =
  {
  lazy val lhs: Option[hydra.python.syntax.BitwiseXor] = (bxor.lhs)
  lazy val rhs: hydra.python.syntax.BitwiseAnd = (bxor.rhs)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.maybes.map[hydra.python.syntax.BitwiseXor,
     hydra.ast.Expr]((l: hydra.python.syntax.BitwiseXor) =>
    hydra.serialization.spaceSep(Seq(hydra.python.serde.encodeBitwiseXor(l), hydra.serialization.cst("^"))))(lhs),
       Some(hydra.python.serde.encodeBitwiseAnd(rhs)))))
}

def encodeBlock(b: hydra.python.syntax.Block): hydra.ast.Expr =
  b match
  case hydra.python.syntax.Block.indented(v_Block_indented_groups) => hydra.serialization.tabIndentDoubleSpace(hydra.lib.lists.map[Seq[hydra.python.syntax.Statement],
     hydra.ast.Expr]((stmts: Seq[hydra.python.syntax.Statement]) =>
    hydra.serialization.newlineSep(hydra.lib.lists.map[hydra.python.syntax.Statement, hydra.ast.Expr](hydra.python.serde.encodeStatement)(stmts)))(v_Block_indented_groups))
  case hydra.python.syntax.Block.simple(v_Block_simple_ss) => hydra.serialization.semicolonSep(hydra.lib.lists.map[hydra.python.syntax.SimpleStatement,
     hydra.ast.Expr](hydra.python.serde.encodeSimpleStatement)(v_Block_simple_ss))

def encodeCapturePattern(cp: hydra.python.syntax.CapturePattern): hydra.ast.Expr = hydra.python.serde.encodePatternCaptureTarget(cp)

def encodeCaseBlock(cb: hydra.python.syntax.CaseBlock): hydra.ast.Expr =
  {
  lazy val patterns: hydra.python.syntax.Patterns = (cb.patterns)
  lazy val guard: Option[hydra.python.syntax.Guard] = (cb.guard)
  lazy val body: hydra.python.syntax.Block = (cb.body)
  hydra.serialization.newlineSep(Seq(hydra.serialization.noSep(Seq(hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.serialization.cst("case")),
     Some(hydra.python.serde.encodePatterns(patterns)), hydra.lib.maybes.map[hydra.python.syntax.Guard,
     hydra.ast.Expr](hydra.python.serde.encodeGuard)(guard)))), hydra.serialization.cst(":"))), hydra.python.serde.encodeBlock(body)))
}

def encodeClassDefinition(cd: hydra.python.syntax.ClassDefinition): hydra.ast.Expr =
  {
  lazy val decs: Option[hydra.python.syntax.Decorators] = (cd.decorators)
  lazy val name: hydra.python.syntax.Name = (cd.name)
  lazy val args: Option[hydra.python.syntax.Args] = (cd.arguments)
  lazy val body: hydra.python.syntax.Block = (cd.body)
  lazy val argPart: Option[hydra.ast.Expr] = hydra.lib.maybes.map[hydra.python.syntax.Args, hydra.ast.Expr]((a: hydra.python.syntax.Args) =>
    hydra.serialization.noSep(Seq(hydra.serialization.cst("("), hydra.python.serde.encodeArgs(a), hydra.serialization.cst(")"))))(args)
  hydra.serialization.newlineSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.maybes.map[hydra.python.syntax.Decorators,
     hydra.ast.Expr](hydra.python.serde.encodeDecorators)(decs), Some(hydra.serialization.noSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("class"),
     hydra.python.serde.encodeName(name)))), argPart, Some(hydra.serialization.cst(":")))))), Some(hydra.python.serde.encodeBlock(body)))))
}

def encodeClassPattern(cp: hydra.python.syntax.ClassPattern): hydra.ast.Expr =
  {
  lazy val noa: hydra.python.syntax.NameOrAttribute = (cp.nameOrAttribute)
  lazy val pos: Option[hydra.python.syntax.PositionalPatterns] = (cp.positionalPatterns)
  lazy val kw: Option[hydra.python.syntax.KeywordPatterns] = (cp.keywordPatterns)
  hydra.serialization.noSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.python.serde.encodeNameOrAttribute(noa)),
     Some(hydra.serialization.cst("(")), hydra.lib.maybes.map[hydra.python.syntax.PositionalPatterns,
     hydra.ast.Expr](hydra.python.serde.encodePositionalPatterns)(pos), hydra.lib.maybes.map[hydra.python.syntax.KeywordPatterns,
     hydra.ast.Expr](hydra.python.serde.encodeKeywordPatterns)(kw), Some(hydra.serialization.cst(")")))))
}

def encodeClosedPattern(cp: hydra.python.syntax.ClosedPattern): hydra.ast.Expr =
  cp match
  case hydra.python.syntax.ClosedPattern.literal(v_ClosedPattern_literal__) => hydra.serialization.cst("...")
  case hydra.python.syntax.ClosedPattern.capture(v_ClosedPattern_capture_c) => hydra.python.serde.encodeCapturePattern(v_ClosedPattern_capture_c)
  case hydra.python.syntax.ClosedPattern.wildcard => hydra.serialization.cst("_")
  case hydra.python.syntax.ClosedPattern.value(v_ClosedPattern_value_v) => hydra.python.serde.encodeValuePattern(v_ClosedPattern_value_v)
  case hydra.python.syntax.ClosedPattern.group(v_ClosedPattern_group__) => hydra.serialization.cst("(...)")
  case hydra.python.syntax.ClosedPattern.sequence(v_ClosedPattern_sequence__) => hydra.serialization.cst("[...]")
  case hydra.python.syntax.ClosedPattern.mapping(v_ClosedPattern_mapping__) => hydra.serialization.cst("{...}")
  case hydra.python.syntax.ClosedPattern.`class`(v_ClosedPattern_class_c) => hydra.python.serde.encodeClassPattern(v_ClosedPattern_class_c)

def encodeComparison(cmp: hydra.python.syntax.Comparison): hydra.ast.Expr = hydra.python.serde.encodeBitwiseOr(cmp.lhs)

def encodeCompoundStatement(cs: hydra.python.syntax.CompoundStatement): hydra.ast.Expr =
  cs match
  case hydra.python.syntax.CompoundStatement.function(v_CompoundStatement_function_f) => hydra.python.serde.encodeFunctionDefinition(v_CompoundStatement_function_f)
  case hydra.python.syntax.CompoundStatement.`if`(v_CompoundStatement_if__) => hydra.serialization.cst("if ...")
  case hydra.python.syntax.CompoundStatement.classDef(v_CompoundStatement_classDef_c) => hydra.python.serde.encodeClassDefinition(v_CompoundStatement_classDef_c)
  case hydra.python.syntax.CompoundStatement.`with`(v_CompoundStatement_with__) => hydra.serialization.cst("with ...")
  case hydra.python.syntax.CompoundStatement.`for`(v_CompoundStatement_for__) => hydra.serialization.cst("for ...")
  case hydra.python.syntax.CompoundStatement.`try`(v_CompoundStatement_try__) => hydra.serialization.cst("try ...")
  case hydra.python.syntax.CompoundStatement.`while`(v_CompoundStatement_while_w) => hydra.python.serde.encodeWhileStatement(v_CompoundStatement_while_w)
  case hydra.python.syntax.CompoundStatement.`match`(v_CompoundStatement_match_m) => hydra.python.serde.encodeMatchStatement(v_CompoundStatement_match_m)

def encodeConditional(c: hydra.python.syntax.Conditional): hydra.ast.Expr =
  {
  lazy val body: hydra.python.syntax.Disjunction = (c.body)
  lazy val cond: hydra.python.syntax.Disjunction = (c.`if`)
  lazy val elseExpr: hydra.python.syntax.Expression = (c.`else`)
  hydra.serialization.spaceSep(Seq(hydra.python.serde.encodeDisjunction(body), hydra.serialization.cst("if"),
     hydra.python.serde.encodeDisjunction(cond), hydra.serialization.cst("else"), hydra.python.serde.encodeExpression(elseExpr)))
}

def encodeConjunction(c: hydra.python.syntax.Conjunction): hydra.ast.Expr =
  hydra.serialization.symbolSep("and")(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.python.syntax.Inversion,
     hydra.ast.Expr](hydra.python.serde.encodeInversion)(c))

def encodeDecorators(decs: hydra.python.syntax.Decorators): hydra.ast.Expr =
  hydra.serialization.newlineSep(hydra.lib.lists.map[hydra.python.syntax.NamedExpression, hydra.ast.Expr]((ne: hydra.python.syntax.NamedExpression) =>
  hydra.serialization.noSep(Seq(hydra.serialization.cst("@"), hydra.python.serde.encodeNamedExpression(ne))))(decs))

def encodeDict(d: hydra.python.syntax.Dict): hydra.ast.Expr =
  hydra.serialization.curlyBracesList(None)(hydra.serialization.halfBlockStyle)(hydra.lib.lists.map[hydra.python.syntax.DoubleStarredKvpair,
     hydra.ast.Expr](hydra.python.serde.encodeDoubleStarredKvpair)(d))

def encodeDisjunction(d: hydra.python.syntax.Disjunction): hydra.ast.Expr =
  hydra.serialization.symbolSep("or")(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.python.syntax.Conjunction,
     hydra.ast.Expr](hydra.python.serde.encodeConjunction)(d))

def encodeDottedAsName(dan: hydra.python.syntax.DottedAsName): hydra.ast.Expr =
  {
  lazy val name: hydra.python.syntax.DottedName = (dan.name)
  lazy val alias: Option[hydra.python.syntax.Name] = (dan.as)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.python.serde.encodeDottedName(name)),
     hydra.lib.maybes.map[hydra.python.syntax.Name, hydra.ast.Expr]((a: hydra.python.syntax.Name) =>
    hydra.serialization.spaceSep(Seq(hydra.serialization.cst("as"), hydra.python.serde.encodeName(a))))(alias))))
}

def encodeDottedName(dn: hydra.python.syntax.DottedName): hydra.ast.Expr =
  hydra.serialization.cst(hydra.lib.strings.intercalate(".")(hydra.lib.lists.map[hydra.python.syntax.Name,
     scala.Predef.String]((n: hydra.python.syntax.Name) => n)(dn)))

def encodeDoubleStarredKvpair(dskv: hydra.python.syntax.DoubleStarredKvpair): hydra.ast.Expr =
  dskv match
  case hydra.python.syntax.DoubleStarredKvpair.pair(v_DoubleStarredKvpair_pair_p) => hydra.python.serde.encodeKvpair(v_DoubleStarredKvpair_pair_p)
  case hydra.python.syntax.DoubleStarredKvpair.starred(v_DoubleStarredKvpair_starred_e) => hydra.serialization.noSep(Seq(hydra.serialization.cst("**"),
     hydra.python.serde.encodeBitwiseOr(v_DoubleStarredKvpair_starred_e)))

def encodeExpression(expr: hydra.python.syntax.Expression): hydra.ast.Expr =
  expr match
  case hydra.python.syntax.Expression.simple(v_Expression_simple_d) => hydra.python.serde.encodeDisjunction(v_Expression_simple_d)
  case hydra.python.syntax.Expression.conditional(v_Expression_conditional_c) => hydra.python.serde.encodeConditional(v_Expression_conditional_c)
  case hydra.python.syntax.Expression.lambda(v_Expression_lambda_l) => hydra.python.serde.encodeLambda(v_Expression_lambda_l)

def encodeFactor(f: hydra.python.syntax.Factor): hydra.ast.Expr =
  f match
  case hydra.python.syntax.Factor.positive(v_Factor_positive_inner) => hydra.serialization.noSep(Seq(hydra.serialization.cst("+"),
     hydra.python.serde.encodeFactor(v_Factor_positive_inner)))
  case hydra.python.syntax.Factor.negative(v_Factor_negative_inner) => hydra.serialization.noSep(Seq(hydra.serialization.cst("-"),
     hydra.python.serde.encodeFactor(v_Factor_negative_inner)))
  case hydra.python.syntax.Factor.complement(v_Factor_complement_inner) => hydra.serialization.noSep(Seq(hydra.serialization.cst("~"),
     hydra.python.serde.encodeFactor(v_Factor_complement_inner)))
  case hydra.python.syntax.Factor.simple(v_Factor_simple_p) => hydra.python.serde.encodePower(v_Factor_simple_p)

def encodeFunctionDefRaw(fdr: hydra.python.syntax.FunctionDefRaw): hydra.ast.Expr =
  {
  lazy val `async_`: Boolean = (fdr.async)
  lazy val name: hydra.python.syntax.Name = (fdr.name)
  lazy val tparams: Seq[hydra.python.syntax.TypeParameter] = (fdr.typeParams)
  lazy val params: Option[hydra.python.syntax.Parameters] = (fdr.params)
  lazy val retType: Option[hydra.python.syntax.Expression] = (fdr.returnType)
  lazy val block: hydra.python.syntax.Block = (fdr.block)
  lazy val asyncKw: Option[hydra.ast.Expr] = hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](`async_`)(Some(hydra.serialization.cst("async")))(None)
  lazy val tparamPart: Option[hydra.ast.Expr] = hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.python.syntax.TypeParameter](tparams))(None)(Some(hydra.serialization.bracketList(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.python.syntax.TypeParameter,
     hydra.ast.Expr](hydra.python.serde.encodeTypeParameter)(tparams))))
  lazy val paramPart: Option[hydra.ast.Expr] = hydra.lib.maybes.map[hydra.python.syntax.Parameters, hydra.ast.Expr](hydra.python.serde.encodeParameters)(params)
  lazy val retPart: Option[hydra.ast.Expr] = hydra.lib.maybes.map[hydra.python.syntax.Expression, hydra.ast.Expr]((t: hydra.python.syntax.Expression) =>
    hydra.serialization.spaceSep(Seq(hydra.serialization.cst("->"), hydra.python.serde.encodeExpression(t))))(retType)
  hydra.serialization.newlineSep(Seq(hydra.serialization.noSep(Seq(hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(asyncKw,
     Some(hydra.serialization.cst("def")), Some(hydra.serialization.noSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.python.serde.encodeName(name)),
     tparamPart, Some(hydra.serialization.cst("(")), paramPart, Some(hydra.serialization.cst(")")))))),
     retPart))), hydra.serialization.cst(":"))), hydra.python.serde.encodeBlock(block)))
}

def encodeFunctionDefinition(fd: hydra.python.syntax.FunctionDefinition): hydra.ast.Expr =
  {
  lazy val decs: Option[hydra.python.syntax.Decorators] = (fd.decorators)
  lazy val raw: hydra.python.syntax.FunctionDefRaw = (fd.raw)
  hydra.serialization.newlineSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(hydra.lib.maybes.map[hydra.python.syntax.Decorators,
     hydra.ast.Expr](hydra.python.serde.encodeDecorators)(decs), Some(hydra.python.serde.encodeFunctionDefRaw(raw)))))
}

def encodeGroup(g: hydra.python.syntax.Group): hydra.ast.Expr =
  g match
  case hydra.python.syntax.Group.expression(v_Group_expression_ne) => hydra.python.serde.encodeNamedExpression(v_Group_expression_ne)
  case hydra.python.syntax.Group.`yield`(v_Group_yield__) => hydra.serialization.cst("(yield ...)")

def encodeGuard(g: hydra.python.syntax.Guard): hydra.ast.Expr =
  hydra.serialization.spaceSep(Seq(hydra.serialization.cst("if"), hydra.python.serde.encodeNamedExpression(g)))

def encodeImportFrom(`if_`: hydra.python.syntax.ImportFrom): hydra.ast.Expr =
  {
  lazy val prefixes: Seq[hydra.python.syntax.RelativeImportPrefix] = (`if_`.prefixes)
  lazy val name: Option[hydra.python.syntax.DottedName] = (`if_`.dottedName)
  lazy val targets: hydra.python.syntax.ImportFromTargets = (`if_`.targets)
  lazy val lhs: hydra.ast.Expr = hydra.serialization.noSep(hydra.lib.maybes.cat[hydra.ast.Expr](hydra.lib.lists.concat[Option[hydra.ast.Expr]](Seq(hydra.lib.lists.map[hydra.python.syntax.RelativeImportPrefix,
     Option[hydra.ast.Expr]]((p: hydra.python.syntax.RelativeImportPrefix) => Some(hydra.python.serde.encodeRelativeImportPrefix(p)))(prefixes),
     Seq(hydra.lib.maybes.map[hydra.python.syntax.DottedName, hydra.ast.Expr](hydra.python.serde.encodeDottedName)(name))))))
  hydra.serialization.spaceSep(Seq(hydra.serialization.cst("from"), lhs, hydra.serialization.cst("import"),
     hydra.python.serde.encodeImportFromTargets(targets)))
}

def encodeImportFromAsName(ifan: hydra.python.syntax.ImportFromAsName): hydra.ast.Expr =
  {
  lazy val name: hydra.python.syntax.Name = (ifan.name)
  lazy val alias: Option[hydra.python.syntax.Name] = (ifan.as)
  hydra.lib.maybes.maybe[hydra.ast.Expr, hydra.python.syntax.Name](hydra.python.serde.encodeName(name))((a: hydra.python.syntax.Name) =>
    hydra.serialization.spaceSep(Seq(hydra.python.serde.encodeName(name), hydra.serialization.cst("as"), hydra.python.serde.encodeName(a))))(alias)
}

def encodeImportFromTargets(t: hydra.python.syntax.ImportFromTargets): hydra.ast.Expr =
  t match
  case hydra.python.syntax.ImportFromTargets.simple(v_ImportFromTargets_simple_names) => hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.python.syntax.ImportFromAsName,
     hydra.ast.Expr](hydra.python.serde.encodeImportFromAsName)(v_ImportFromTargets_simple_names))
  case hydra.python.syntax.ImportFromTargets.parens(v_ImportFromTargets_parens_names) => hydra.serialization.noSep(Seq(hydra.serialization.cst("("),
     hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.python.syntax.ImportFromAsName,
     hydra.ast.Expr](hydra.python.serde.encodeImportFromAsName)(v_ImportFromTargets_parens_names)), hydra.serialization.cst(")")))
  case hydra.python.syntax.ImportFromTargets.star => hydra.serialization.cst("*")

def encodeImportName(`in_`: hydra.python.syntax.ImportName): hydra.ast.Expr =
  hydra.serialization.spaceSep(Seq(hydra.serialization.cst("import"), hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.python.syntax.DottedAsName,
     hydra.ast.Expr](hydra.python.serde.encodeDottedAsName)(`in_`))))

def encodeImportStatement(`is_`: hydra.python.syntax.ImportStatement): hydra.ast.Expr =
  `is_` match
  case hydra.python.syntax.ImportStatement.name(v_ImportStatement_name_n) => hydra.python.serde.encodeImportName(v_ImportStatement_name_n)
  case hydra.python.syntax.ImportStatement.from(v_ImportStatement_from_f) => hydra.python.serde.encodeImportFrom(v_ImportStatement_from_f)

def encodeInversion(i: hydra.python.syntax.Inversion): hydra.ast.Expr =
  i match
  case hydra.python.syntax.Inversion.not(v_Inversion_not_other) => hydra.serialization.spaceSep(Seq(hydra.serialization.cst("not"),
     hydra.python.serde.encodeInversion(v_Inversion_not_other)))
  case hydra.python.syntax.Inversion.simple(v_Inversion_simple_c) => hydra.python.serde.encodeComparison(v_Inversion_simple_c)

def encodeKeywordPattern(kp: hydra.python.syntax.KeywordPattern): hydra.ast.Expr =
  {
  lazy val name: hydra.python.syntax.Name = (kp.name)
  lazy val pat: hydra.python.syntax.Pattern = (kp.pattern)
  hydra.serialization.noSep(Seq(hydra.python.serde.encodeName(name), hydra.serialization.cst("="), hydra.python.serde.encodePattern(pat)))
}

def encodeKeywordPatterns(kp: hydra.python.syntax.KeywordPatterns): hydra.ast.Expr =
  hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.python.syntax.KeywordPattern,
     hydra.ast.Expr](hydra.python.serde.encodeKeywordPattern)(kp))

def encodeKvpair(kv: hydra.python.syntax.Kvpair): hydra.ast.Expr =
  {
  lazy val k: hydra.python.syntax.Expression = (kv.key)
  lazy val v: hydra.python.syntax.Expression = (kv.value)
  hydra.serialization.spaceSep(Seq(hydra.serialization.noSep(Seq(hydra.python.serde.encodeExpression(k),
     hydra.serialization.cst(":"))), hydra.python.serde.encodeExpression(v)))
}

def encodeKwarg(k: hydra.python.syntax.Kwarg): hydra.ast.Expr =
  {
  lazy val name: hydra.python.syntax.Name = (k.name)
  lazy val expr: hydra.python.syntax.Expression = (k.value)
  hydra.serialization.noSep(Seq(hydra.python.serde.encodeName(name), hydra.serialization.cst("="), hydra.python.serde.encodeExpression(expr)))
}

def encodeKwargOrDoubleStarred(kds: hydra.python.syntax.KwargOrDoubleStarred): hydra.ast.Expr =
  kds match
  case hydra.python.syntax.KwargOrDoubleStarred.kwarg(v_KwargOrDoubleStarred_kwarg_k) => hydra.python.serde.encodeKwarg(v_KwargOrDoubleStarred_kwarg_k)
  case hydra.python.syntax.KwargOrDoubleStarred.doubleStarred(v_KwargOrDoubleStarred_doubleStarred_e) => hydra.serialization.noSep(Seq(hydra.serialization.cst("**"),
     hydra.python.serde.encodeExpression(v_KwargOrDoubleStarred_doubleStarred_e)))

def encodeKwargOrStarred(ks: hydra.python.syntax.KwargOrStarred): hydra.ast.Expr =
  ks match
  case hydra.python.syntax.KwargOrStarred.kwarg(v_KwargOrStarred_kwarg_k) => hydra.python.serde.encodeKwarg(v_KwargOrStarred_kwarg_k)
  case hydra.python.syntax.KwargOrStarred.starred(v_KwargOrStarred_starred_se) => hydra.python.serde.encodeStarredExpression(v_KwargOrStarred_starred_se)

def encodeLambda(l: hydra.python.syntax.Lambda): hydra.ast.Expr =
  {
  lazy val params: hydra.python.syntax.LambdaParameters = (l.params)
  lazy val body: hydra.python.syntax.Expression = (l.body)
  hydra.serialization.parens(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("lambda"), hydra.serialization.noSep(Seq(hydra.python.serde.encodeLambdaParameters(params),
     hydra.serialization.cst(":"))), hydra.python.serde.encodeExpression(body))))
}

def encodeLambdaParamNoDefault(p: hydra.python.syntax.LambdaParamNoDefault): hydra.ast.Expr = hydra.python.serde.encodeName(p)

def encodeLambdaParameters(lp: hydra.python.syntax.LambdaParameters): hydra.ast.Expr =
  {
  lazy val nodef: Seq[hydra.python.syntax.LambdaParamNoDefault] = (lp.paramNoDefault)
  hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.python.syntax.LambdaParamNoDefault,
     hydra.ast.Expr](hydra.python.serde.encodeLambdaParamNoDefault)(nodef))
}

def encodeLambdaStarEtc(lse: hydra.python.syntax.LambdaStarEtc): hydra.ast.Expr =
  lse match
  case hydra.python.syntax.LambdaStarEtc.paramNoDefault(v_LambdaStarEtc_paramNoDefault_p) => hydra.python.serde.encodeLambdaParamNoDefault(v_LambdaStarEtc_paramNoDefault_p)
  case hydra.python.syntax.LambdaStarEtc.star(v_LambdaStarEtc_star__) => hydra.serialization.cst("*...")
  case hydra.python.syntax.LambdaStarEtc.paramMaybeDefault(v_LambdaStarEtc_paramMaybeDefault__) => hydra.serialization.cst("...")
  case hydra.python.syntax.LambdaStarEtc.kwds(v_LambdaStarEtc_kwds__) => hydra.serialization.cst("**...")

def encodeList(l: hydra.python.syntax.List): hydra.ast.Expr =
  hydra.serialization.bracketListAdaptive(hydra.lib.lists.map[hydra.python.syntax.StarNamedExpression,
     hydra.ast.Expr](hydra.python.serde.encodeStarNamedExpression)(l))

def encodeMatchStatement(ms: hydra.python.syntax.MatchStatement): hydra.ast.Expr =
  {
  lazy val subj: hydra.python.syntax.SubjectExpression = (ms.subject)
  lazy val cases: Seq[hydra.python.syntax.CaseBlock] = (ms.cases)
  hydra.serialization.newlineSep(Seq(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("match"),
     hydra.serialization.noSep(Seq(hydra.python.serde.encodeSubjectExpression(subj), hydra.serialization.cst(":"))))),
     hydra.serialization.tabIndentDoubleSpace(hydra.lib.lists.map[hydra.python.syntax.CaseBlock, hydra.ast.Expr](hydra.python.serde.encodeCaseBlock)(cases))))
}

def encodeModule(mod: hydra.python.syntax.Module): hydra.ast.Expr =
  {
  lazy val warning: hydra.ast.Expr = hydra.serialization.cst(hydra.python.serde.toPythonComments(hydra.constants.warningAutoGeneratedFile))
  lazy val groups: Seq[hydra.ast.Expr] = hydra.lib.lists.map[Seq[hydra.python.syntax.Statement], hydra.ast.Expr]((group: Seq[hydra.python.syntax.Statement]) =>
    hydra.serialization.newlineSep(hydra.lib.lists.map[hydra.python.syntax.Statement, hydra.ast.Expr](hydra.python.serde.encodeStatement)(group)))(mod)
  hydra.serialization.doubleNewlineSep(hydra.lib.lists.cons[hydra.ast.Expr](warning)(groups))
}

def encodeName(n: hydra.python.syntax.Name): hydra.ast.Expr = hydra.serialization.cst(n)

def encodeNameOrAttribute(noa: hydra.python.syntax.NameOrAttribute): hydra.ast.Expr =
  hydra.serialization.dotSep(hydra.lib.lists.map[hydra.python.syntax.Name, hydra.ast.Expr](hydra.python.serde.encodeName)(noa))

def encodeNamedExpression(ne: hydra.python.syntax.NamedExpression): hydra.ast.Expr =
  ne match
  case hydra.python.syntax.NamedExpression.simple(v_NamedExpression_simple_e) => hydra.python.serde.encodeExpression(v_NamedExpression_simple_e)
  case hydra.python.syntax.NamedExpression.assignment(v_NamedExpression_assignment_ae) => hydra.python.serde.encodeAssignmentExpression(v_NamedExpression_assignment_ae)

def encodeNumber(num: hydra.python.syntax.Number): hydra.ast.Expr =
  num match
  case hydra.python.syntax.Number.float(v_Number_float_f) => hydra.serialization.cst(hydra.python.serde.pythonFloatLiteralText(hydra.lib.literals.showBigfloat(v_Number_float_f)))
  case hydra.python.syntax.Number.integer(v_Number_integer_i) => hydra.serialization.cst(hydra.lib.literals.showBigint(v_Number_integer_i))

def encodeOrPattern(op: hydra.python.syntax.OrPattern): hydra.ast.Expr =
  hydra.serialization.symbolSep("|")(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.python.syntax.ClosedPattern,
     hydra.ast.Expr](hydra.python.serde.encodeClosedPattern)(op))

def encodeParam(p: hydra.python.syntax.Param): hydra.ast.Expr =
  {
  lazy val name: hydra.python.syntax.Name = (p.name)
  lazy val ann: Option[hydra.python.syntax.Annotation] = (p.annotation)
  hydra.serialization.noSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.python.serde.encodeName(name)),
     hydra.lib.maybes.map[hydra.python.syntax.Annotation, hydra.ast.Expr](hydra.python.serde.encodeAnnotation)(ann))))
}

def encodeParamNoDefault(pnd: hydra.python.syntax.ParamNoDefault): hydra.ast.Expr = hydra.python.serde.encodeParam(pnd.param)

def encodeParamNoDefaultParameters(pndp: hydra.python.syntax.ParamNoDefaultParameters): hydra.ast.Expr =
  {
  lazy val nodef: Seq[hydra.python.syntax.ParamNoDefault] = (pndp.paramNoDefault)
  hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.python.syntax.ParamNoDefault,
     hydra.ast.Expr](hydra.python.serde.encodeParamNoDefault)(nodef))
}

def encodeParameters(p: hydra.python.syntax.Parameters): hydra.ast.Expr =
  p match
  case hydra.python.syntax.Parameters.paramNoDefault(v_Parameters_paramNoDefault_pnd) => hydra.python.serde.encodeParamNoDefaultParameters(v_Parameters_paramNoDefault_pnd)
  case hydra.python.syntax.Parameters.slashNoDefault(v_Parameters_slashNoDefault__) => hydra.serialization.cst("...")
  case hydra.python.syntax.Parameters.slashWithDefault(v_Parameters_slashWithDefault__) => hydra.serialization.cst("...")

def encodePattern(p: hydra.python.syntax.Pattern): hydra.ast.Expr =
  p match
  case hydra.python.syntax.Pattern.or(v_Pattern_or_op) => hydra.python.serde.encodeOrPattern(v_Pattern_or_op)
  case hydra.python.syntax.Pattern.as(v_Pattern_as__) => hydra.serialization.cst("... as ...")

def encodePatternCaptureTarget(pct: hydra.python.syntax.PatternCaptureTarget): hydra.ast.Expr = hydra.python.serde.encodeName(pct)

def encodePatterns(ps: hydra.python.syntax.Patterns): hydra.ast.Expr =
  ps match
  case hydra.python.syntax.Patterns.pattern(v_Patterns_pattern_p) => hydra.python.serde.encodePattern(v_Patterns_pattern_p)
  case hydra.python.syntax.Patterns.sequence(v_Patterns_sequence__) => hydra.serialization.cst("...")

def encodePosArg(pa: hydra.python.syntax.PosArg): hydra.ast.Expr =
  pa match
  case hydra.python.syntax.PosArg.starred(v_PosArg_starred_se) => hydra.python.serde.encodeStarredExpression(v_PosArg_starred_se)
  case hydra.python.syntax.PosArg.assignment(v_PosArg_assignment_ae) => hydra.python.serde.encodeAssignmentExpression(v_PosArg_assignment_ae)
  case hydra.python.syntax.PosArg.expression(v_PosArg_expression_e) => hydra.python.serde.encodeExpression(v_PosArg_expression_e)

def encodePositionalPatterns(pp: hydra.python.syntax.PositionalPatterns): hydra.ast.Expr =
  hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.python.syntax.Pattern,
     hydra.ast.Expr](hydra.python.serde.encodePattern)(pp))

def encodePower(p: hydra.python.syntax.Power): hydra.ast.Expr =
  {
  lazy val lhs: hydra.python.syntax.AwaitPrimary = (p.lhs)
  lazy val rhs: Option[hydra.python.syntax.Factor] = (p.rhs)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.python.serde.encodeAwaitPrimary(lhs)),
     hydra.lib.maybes.map[hydra.python.syntax.Factor, hydra.ast.Expr]((r: hydra.python.syntax.Factor) =>
    hydra.serialization.spaceSep(Seq(hydra.serialization.cst("**"), hydra.python.serde.encodeFactor(r))))(rhs))))
}

def encodePrimary(p: hydra.python.syntax.Primary): hydra.ast.Expr =
  p match
  case hydra.python.syntax.Primary.simple(v_Primary_simple_a) => hydra.python.serde.encodeAtom(v_Primary_simple_a)
  case hydra.python.syntax.Primary.compound(v_Primary_compound_pwr) => hydra.python.serde.encodePrimaryWithRhs(v_Primary_compound_pwr)

def encodePrimaryRhs(rhs: hydra.python.syntax.PrimaryRhs): hydra.ast.Expr =
  rhs match
  case hydra.python.syntax.PrimaryRhs.call(v_PrimaryRhs_call_args) => hydra.serialization.noSep(Seq(hydra.serialization.cst("("),
     hydra.python.serde.encodeArgs(v_PrimaryRhs_call_args), hydra.serialization.cst(")")))
  case hydra.python.syntax.PrimaryRhs.project(v_PrimaryRhs_project_name) => hydra.serialization.noSep(Seq(hydra.serialization.cst("."),
     hydra.python.serde.encodeName(v_PrimaryRhs_project_name)))
  case hydra.python.syntax.PrimaryRhs.slices(v_PrimaryRhs_slices_slices) => hydra.serialization.noSep(Seq(hydra.serialization.cst("["),
     hydra.python.serde.encodeSlices(v_PrimaryRhs_slices_slices), hydra.serialization.cst("]")))
  case hydra.python.syntax.PrimaryRhs.genexp(v_PrimaryRhs_genexp__) => hydra.serialization.cst("[...]")

def encodePrimaryWithRhs(pwr: hydra.python.syntax.PrimaryWithRhs): hydra.ast.Expr =
  {
  lazy val prim: hydra.python.syntax.Primary = (pwr.primary)
  lazy val rhs: hydra.python.syntax.PrimaryRhs = (pwr.rhs)
  hydra.serialization.noSep(Seq(hydra.python.serde.encodePrimary(prim), hydra.python.serde.encodePrimaryRhs(rhs)))
}

def encodeRaiseExpression(re: hydra.python.syntax.RaiseExpression): hydra.ast.Expr =
  {
  lazy val expr: hydra.python.syntax.Expression = (re.expression)
  lazy val `from_`: Option[hydra.python.syntax.Expression] = (re.from)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.python.serde.encodeExpression(expr)),
     hydra.lib.maybes.map[hydra.python.syntax.Expression, hydra.ast.Expr]((f: hydra.python.syntax.Expression) =>
    hydra.serialization.spaceSep(Seq(hydra.serialization.cst("from"), hydra.python.serde.encodeExpression(f))))(`from_`))))
}

def encodeRaiseStatement(rs: hydra.python.syntax.RaiseStatement): hydra.ast.Expr =
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.serialization.cst("raise")),
     hydra.lib.maybes.map[hydra.python.syntax.RaiseExpression, hydra.ast.Expr](hydra.python.serde.encodeRaiseExpression)(rs))))

def encodeRelativeImportPrefix(p: hydra.python.syntax.RelativeImportPrefix): hydra.ast.Expr =
  p match
  case hydra.python.syntax.RelativeImportPrefix.dot => hydra.serialization.cst(".")
  case hydra.python.syntax.RelativeImportPrefix.ellipsis => hydra.serialization.cst("...")

def encodeReturnStatement(rs: hydra.python.syntax.ReturnStatement): hydra.ast.Expr =
  hydra.serialization.spaceSep(Seq(hydra.serialization.cst("return"), hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.python.syntax.StarExpression,
     hydra.ast.Expr](hydra.python.serde.encodeStarExpression)(rs))))

def encodeSet(s: hydra.python.syntax.Set): hydra.ast.Expr =
  hydra.serialization.bracesListAdaptive(hydra.lib.lists.map[hydra.python.syntax.StarNamedExpression,
     hydra.ast.Expr](hydra.python.serde.encodeStarNamedExpression)(s))

def encodeShiftExpression(se: hydra.python.syntax.ShiftExpression): hydra.ast.Expr = hydra.python.serde.encodeSum(se.rhs)

def encodeSimpleStatement(ss: hydra.python.syntax.SimpleStatement): hydra.ast.Expr =
  ss match
  case hydra.python.syntax.SimpleStatement.assignment(v_SimpleStatement_assignment_a) => hydra.python.serde.encodeAssignment(v_SimpleStatement_assignment_a)
  case hydra.python.syntax.SimpleStatement.starExpressions(v_SimpleStatement_starExpressions_es) => hydra.serialization.newlineSep(hydra.lib.lists.map[hydra.python.syntax.StarExpression,
     hydra.ast.Expr](hydra.python.serde.encodeStarExpression)(v_SimpleStatement_starExpressions_es))
  case hydra.python.syntax.SimpleStatement.`return`(v_SimpleStatement_return_r) => hydra.python.serde.encodeReturnStatement(v_SimpleStatement_return_r)
  case hydra.python.syntax.SimpleStatement.raise(v_SimpleStatement_raise_r) => hydra.python.serde.encodeRaiseStatement(v_SimpleStatement_raise_r)
  case hydra.python.syntax.SimpleStatement.pass => hydra.serialization.cst("pass")
  case hydra.python.syntax.SimpleStatement.break => hydra.serialization.cst("break")
  case hydra.python.syntax.SimpleStatement.continue => hydra.serialization.cst("continue")
  case hydra.python.syntax.SimpleStatement.`import`(v_SimpleStatement_import_i) => hydra.python.serde.encodeImportStatement(v_SimpleStatement_import_i)
  case hydra.python.syntax.SimpleStatement.typeAlias(v_SimpleStatement_typeAlias_t) => hydra.python.serde.encodeTypeAlias(v_SimpleStatement_typeAlias_t)
  case hydra.python.syntax.SimpleStatement.assert(v_SimpleStatement_assert__) => hydra.serialization.cst("assert ...")
  case hydra.python.syntax.SimpleStatement.global(v_SimpleStatement_global__) => hydra.serialization.cst("global ...")
  case hydra.python.syntax.SimpleStatement.nonlocal(v_SimpleStatement_nonlocal__) => hydra.serialization.cst("nonlocal ...")
  case hydra.python.syntax.SimpleStatement.del(v_SimpleStatement_del__) => hydra.serialization.cst("del ...")

def encodeSimpleTypeParameter(stp: hydra.python.syntax.SimpleTypeParameter): hydra.ast.Expr = hydra.python.serde.encodeName(stp.name)

def encodeSingleTarget(st: hydra.python.syntax.SingleTarget): hydra.ast.Expr =
  st match
  case hydra.python.syntax.SingleTarget.name(v_SingleTarget_name_n) => hydra.python.serde.encodeName(v_SingleTarget_name_n)
  case hydra.python.syntax.SingleTarget.parens(v_SingleTarget_parens__) => hydra.serialization.cst("(...)")
  case hydra.python.syntax.SingleTarget.subscriptAttributeTarget(v_SingleTarget_subscriptAttributeTarget__) => hydra.serialization.cst("...")

def encodeSlice(s: hydra.python.syntax.Slice): hydra.ast.Expr =
  s match
  case hydra.python.syntax.Slice.named(v_Slice_named_ne) => hydra.python.serde.encodeNamedExpression(v_Slice_named_ne)
  case hydra.python.syntax.Slice.`slice_`(v_Slice_slice___) => hydra.serialization.cst(":")

def encodeSliceOrStarredExpression(s: hydra.python.syntax.SliceOrStarredExpression): hydra.ast.Expr =
  s match
  case hydra.python.syntax.SliceOrStarredExpression.slice(v_SliceOrStarredExpression_slice_sl) => hydra.python.serde.encodeSlice(v_SliceOrStarredExpression_slice_sl)
  case hydra.python.syntax.SliceOrStarredExpression.starred(v_SliceOrStarredExpression_starred_se) => hydra.python.serde.encodeStarredExpression(v_SliceOrStarredExpression_starred_se)

def encodeSlices(s: hydra.python.syntax.Slices): hydra.ast.Expr =
  {
  lazy val hd: hydra.python.syntax.Slice = (s.head)
  lazy val tl: Seq[hydra.python.syntax.SliceOrStarredExpression] = (s.tail)
  hydra.serialization.commaSep(hydra.serialization.inlineStyle)(hydra.lib.lists.cons[hydra.ast.Expr](hydra.python.serde.encodeSlice(hd))(hydra.lib.lists.map[hydra.python.syntax.SliceOrStarredExpression,
     hydra.ast.Expr](hydra.python.serde.encodeSliceOrStarredExpression)(tl)))
}

def encodeStarAtom(sa: hydra.python.syntax.StarAtom): hydra.ast.Expr =
  sa match
  case hydra.python.syntax.StarAtom.name(v_StarAtom_name_n) => hydra.python.serde.encodeName(v_StarAtom_name_n)
  case hydra.python.syntax.StarAtom.targetWithStarAtom(v_StarAtom_targetWithStarAtom__) => hydra.serialization.cst("(...)")
  case hydra.python.syntax.StarAtom.starTargetsTupleSeq(v_StarAtom_starTargetsTupleSeq__) => hydra.serialization.cst("(...)")
  case hydra.python.syntax.StarAtom.starTargetsListSeq(v_StarAtom_starTargetsListSeq__) => hydra.serialization.cst("[...]")

def encodeStarExpression(se: hydra.python.syntax.StarExpression): hydra.ast.Expr =
  se match
  case hydra.python.syntax.StarExpression.star(v_StarExpression_star_bor) => hydra.serialization.noSep(Seq(hydra.serialization.cst("*"),
     hydra.python.serde.encodeBitwiseOr(v_StarExpression_star_bor)))
  case hydra.python.syntax.StarExpression.simple(v_StarExpression_simple_e) => hydra.python.serde.encodeExpression(v_StarExpression_simple_e)

def encodeStarNamedExpression(sne: hydra.python.syntax.StarNamedExpression): hydra.ast.Expr =
  sne match
  case hydra.python.syntax.StarNamedExpression.star(v_StarNamedExpression_star_bor) => hydra.serialization.noSep(Seq(hydra.serialization.cst("*"),
     hydra.python.serde.encodeBitwiseOr(v_StarNamedExpression_star_bor)))
  case hydra.python.syntax.StarNamedExpression.simple(v_StarNamedExpression_simple_ne) => hydra.python.serde.encodeNamedExpression(v_StarNamedExpression_simple_ne)

def encodeStarTarget(st: hydra.python.syntax.StarTarget): hydra.ast.Expr =
  st match
  case hydra.python.syntax.StarTarget.unstarred(v_StarTarget_unstarred_t) => hydra.python.serde.encodeTargetWithStarAtom(v_StarTarget_unstarred_t)
  case hydra.python.syntax.StarTarget.starred(v_StarTarget_starred_inner) => hydra.serialization.noSep(Seq(hydra.serialization.cst("*"),
     hydra.python.serde.encodeStarTarget(v_StarTarget_starred_inner)))

def encodeStarredExpression(se: hydra.python.syntax.StarredExpression): hydra.ast.Expr =
  hydra.serialization.noSep(Seq(hydra.serialization.cst("*"), hydra.python.serde.encodeExpression(se)))

def encodeStatement(stmt: hydra.python.syntax.Statement): hydra.ast.Expr =
  stmt match
  case hydra.python.syntax.Statement.annotated(v_Statement_annotated_a) => hydra.python.serde.encodeAnnotatedStatement(v_Statement_annotated_a)
  case hydra.python.syntax.Statement.simple(v_Statement_simple_ss) => hydra.serialization.newlineSep(hydra.lib.lists.map[hydra.python.syntax.SimpleStatement,
     hydra.ast.Expr](hydra.python.serde.encodeSimpleStatement)(v_Statement_simple_ss))
  case hydra.python.syntax.Statement.compound(v_Statement_compound_c) => hydra.python.serde.encodeCompoundStatement(v_Statement_compound_c)

def encodeString(s: hydra.python.syntax.String): hydra.ast.Expr =
  {
  lazy val content: scala.Predef.String = (s.value)
  lazy val style: hydra.python.syntax.QuoteStyle = (s.quoteStyle)
  style match
    case hydra.python.syntax.QuoteStyle.single => hydra.serialization.cst(hydra.python.serde.escapePythonString(false)(content))
    case hydra.python.syntax.QuoteStyle.double => hydra.serialization.cst(hydra.python.serde.escapePythonString(true)(content))
    case hydra.python.syntax.QuoteStyle.triple => hydra.serialization.noSep(Seq(hydra.serialization.cst("r\"\"\""),
       hydra.serialization.cst(content), hydra.serialization.cst("\"\"\"")))
}

def encodeSubjectExpression(se: hydra.python.syntax.SubjectExpression): hydra.ast.Expr =
  se match
  case hydra.python.syntax.SubjectExpression.simple(v_SubjectExpression_simple_ne) => hydra.python.serde.encodeNamedExpression(v_SubjectExpression_simple_ne)
  case hydra.python.syntax.SubjectExpression.tuple(v_SubjectExpression_tuple__) => hydra.serialization.cst("*...")

def encodeSum(s: hydra.python.syntax.Sum): hydra.ast.Expr = hydra.python.serde.encodeTerm(s.rhs)

def encodeTPrimary(tp: hydra.python.syntax.TPrimary): hydra.ast.Expr =
  tp match
  case hydra.python.syntax.TPrimary.atom(v_TPrimary_atom_a) => hydra.python.serde.encodeAtom(v_TPrimary_atom_a)
  case hydra.python.syntax.TPrimary.primaryAndName(v_TPrimary_primaryAndName_pn) => hydra.python.serde.encodeTPrimaryAndName(v_TPrimary_primaryAndName_pn)
  case hydra.python.syntax.TPrimary.primaryAndSlices(v_TPrimary_primaryAndSlices__) => hydra.serialization.cst("...")
  case hydra.python.syntax.TPrimary.primaryAndGenexp(v_TPrimary_primaryAndGenexp__) => hydra.serialization.cst("...")
  case hydra.python.syntax.TPrimary.primaryAndArguments(v_TPrimary_primaryAndArguments__) => hydra.serialization.cst("...")

def encodeTPrimaryAndName(pn: hydra.python.syntax.TPrimaryAndName): hydra.ast.Expr =
  {
  lazy val prim: hydra.python.syntax.TPrimary = (pn.primary)
  lazy val `name_`: hydra.python.syntax.Name = (pn.name)
  hydra.serialization.noSep(Seq(hydra.python.serde.encodeTPrimary(prim), hydra.serialization.cst("."), hydra.python.serde.encodeName(`name_`)))
}

def encodeTargetWithStarAtom(t: hydra.python.syntax.TargetWithStarAtom): hydra.ast.Expr =
  t match
  case hydra.python.syntax.TargetWithStarAtom.atom(v_TargetWithStarAtom_atom_a) => hydra.python.serde.encodeStarAtom(v_TargetWithStarAtom_atom_a)
  case hydra.python.syntax.TargetWithStarAtom.project(v_TargetWithStarAtom_project_pn) => hydra.python.serde.encodeTPrimaryAndName(v_TargetWithStarAtom_project_pn)
  case hydra.python.syntax.TargetWithStarAtom.slices(v_TargetWithStarAtom_slices__) => hydra.serialization.cst("...")

def encodeTerm(t: hydra.python.syntax.Term): hydra.ast.Expr = hydra.python.serde.encodeFactor(t.rhs)

def encodeTuple(t: hydra.python.syntax.Tuple): hydra.ast.Expr =
  {
  lazy val es: Seq[hydra.python.syntax.StarNamedExpression] = t
  hydra.lib.logic.ifElse[hydra.ast.Expr](hydra.lib.equality.equal[Int](hydra.lib.lists.length[hydra.python.syntax.StarNamedExpression](es))(1))(hydra.serialization.parens(hydra.serialization.noSep(Seq(hydra.python.serde.encodeStarNamedExpression(hydra.lib.lists.head[hydra.python.syntax.StarNamedExpression](es)),
     hydra.serialization.cst(",")))))(hydra.serialization.parenList(false)(hydra.lib.lists.map[hydra.python.syntax.StarNamedExpression,
     hydra.ast.Expr](hydra.python.serde.encodeStarNamedExpression)(es)))
}

def encodeTypeAlias(ta: hydra.python.syntax.TypeAlias): hydra.ast.Expr =
  {
  lazy val name: hydra.python.syntax.Name = (ta.name)
  lazy val tparams: Seq[hydra.python.syntax.TypeParameter] = (ta.typeParams)
  lazy val expr: hydra.python.syntax.Expression = (ta.expression)
  lazy val alias: hydra.ast.Expr = hydra.serialization.noSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.python.serde.encodeName(name)),
     hydra.lib.logic.ifElse[Option[hydra.ast.Expr]](hydra.lib.lists.`null`[hydra.python.syntax.TypeParameter](tparams))(None)(Some(hydra.serialization.bracketList(hydra.serialization.inlineStyle)(hydra.lib.lists.map[hydra.python.syntax.TypeParameter,
     hydra.ast.Expr](hydra.python.serde.encodeTypeParameter)(tparams)))))))
  hydra.serialization.spaceSep(Seq(hydra.serialization.cst("type"), alias, hydra.serialization.cst("="), hydra.python.serde.encodeExpression(expr)))
}

def encodeTypeParameter(tp: hydra.python.syntax.TypeParameter): hydra.ast.Expr =
  tp match
  case hydra.python.syntax.TypeParameter.simple(v_TypeParameter_simple_s) => hydra.python.serde.encodeSimpleTypeParameter(v_TypeParameter_simple_s)
  case hydra.python.syntax.TypeParameter.star(v_TypeParameter_star__) => hydra.serialization.cst("*...")
  case hydra.python.syntax.TypeParameter.doubleStar(v_TypeParameter_doubleStar__) => hydra.serialization.cst("**...")

def encodeTypedAssignment(ta: hydra.python.syntax.TypedAssignment): hydra.ast.Expr =
  {
  lazy val lhs: hydra.python.syntax.SingleTarget = (ta.lhs)
  lazy val typ: hydra.python.syntax.Expression = (ta.`type`)
  lazy val rhs: Option[hydra.python.syntax.AnnotatedRhs] = (ta.rhs)
  hydra.serialization.spaceSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.serialization.noSep(Seq(hydra.python.serde.encodeSingleTarget(lhs),
     hydra.serialization.cst(":")))), Some(hydra.python.serde.encodeExpression(typ)), hydra.lib.maybes.map[hydra.python.syntax.AnnotatedRhs,
     hydra.ast.Expr](hydra.python.serde.encodeAnnotatedRhs)(rhs))))
}

def encodeUntypedAssignment(ua: hydra.python.syntax.UntypedAssignment): hydra.ast.Expr =
  {
  lazy val targets: Seq[hydra.python.syntax.StarTarget] = (ua.targets)
  lazy val rhs: hydra.python.syntax.AnnotatedRhs = (ua.rhs)
  hydra.serialization.spaceSep(hydra.lib.lists.concat[hydra.ast.Expr](Seq(hydra.lib.lists.map[hydra.python.syntax.StarTarget,
     hydra.ast.Expr](hydra.python.serde.encodeStarTarget)(targets), Seq(hydra.python.serde.encodeAnnotatedRhs(rhs)))))
}

def encodeValuePattern(vp: hydra.python.syntax.ValuePattern): hydra.ast.Expr = hydra.python.serde.encodeAttribute(vp)

def encodeWhileStatement(ws: hydra.python.syntax.WhileStatement): hydra.ast.Expr =
  {
  lazy val cond: hydra.python.syntax.NamedExpression = (ws.condition)
  lazy val body: hydra.python.syntax.Block = (ws.body)
  lazy val `else_`: Option[hydra.python.syntax.Block] = (ws.`else`)
  hydra.serialization.newlineSep(hydra.lib.maybes.cat[hydra.ast.Expr](Seq(Some(hydra.serialization.newlineSep(Seq(hydra.serialization.spaceSep(Seq(hydra.serialization.cst("while"),
     hydra.serialization.noSep(Seq(hydra.python.serde.encodeNamedExpression(cond), hydra.serialization.cst(":"))))),
     hydra.python.serde.encodeBlock(body)))), hydra.lib.maybes.map[hydra.python.syntax.Block, hydra.ast.Expr]((eb: hydra.python.syntax.Block) =>
    hydra.serialization.newlineSep(Seq(hydra.serialization.cst("else:"), hydra.python.serde.encodeBlock(eb))))(`else_`))))
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

def pythonFloatLiteralText(s: scala.Predef.String): scala.Predef.String =
  hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[scala.Predef.String](s)("NaN"))("float('nan')")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[scala.Predef.String](s)("Infinity"))("float('inf')")(hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[scala.Predef.String](s)("-Infinity"))("float('-inf')")(s)))

def toPythonComments(`doc_`: scala.Predef.String): scala.Predef.String =
  hydra.lib.logic.ifElse[scala.Predef.String](hydra.lib.equality.equal[scala.Predef.String](`doc_`)(""))("")(hydra.lib.strings.intercalate("\n")(hydra.lib.lists.map[scala.Predef.String,
     scala.Predef.String]((line: scala.Predef.String) => hydra.lib.strings.cat2("# ")(line))(hydra.lib.strings.lines(`doc_`))))
