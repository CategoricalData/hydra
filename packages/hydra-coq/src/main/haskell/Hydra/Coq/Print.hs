-- | Direct Coq pretty-printer for the Coq syntax AST.
-- This is a simple, direct Haskell implementation (not DSL) that converts
-- Coq AST nodes to source text. Used for testing and verification.

module Hydra.Coq.Print where

import Hydra.Coq.Syntax
import Data.List (intercalate)

-- | Print a complete Document
printDocument :: Document -> String
printDocument (Document sentences) =
  intercalate "\n\n" $ map printSentence sentences

-- | Print a Sentence
printSentence :: Sentence -> String
printSentence (Sentence mComment content) =
  let cmtPart = maybe "" (\c -> printComment c ++ "\n") mComment
  in cmtPart ++ printSentenceContent content

-- | Print a Comment
printComment :: Comment -> String
printComment (Comment s) = "(* " ++ s ++ " *)"

-- | Print SentenceContent
printSentenceContent :: SentenceContent -> String
printSentenceContent sc = case sc of
  SentenceContentDefinition d -> printDefinition d
  SentenceContentFixpoint f -> printFixpoint f
  SentenceContentInductive i -> printInductive i
  SentenceContentModule m -> printModule m
  SentenceContentNotation _ -> "(* notation *)"
  SentenceContentRecord r -> printRecord r
  SentenceContentRequireImport ri -> printRequireImport ri
  SentenceContentSection s -> printSection s
  SentenceContentTheorem t -> printTheorem t

-- | Print Ident
printIdent :: Ident -> String
printIdent (Ident (String_ s)) = s

-- | Print Qualid
printQualid :: Qualid -> String
printQualid (Qualid ident fields) =
  intercalate "." $ printIdent ident : map (\(FieldIdent i) -> printIdent i) fields

-- | Print a Sort
printSort :: Sort -> String
printSort s = case s of
  SortSet -> "Set"
  SortProp -> "Prop"
  SortSProp -> "SProp"
  SortType -> "Type"
  SortTypeWithAnyUniverse -> "Type"
  SortTypeWithUniverse _ -> "Type"

-- | Print a Type (wrapper around Term)
printType :: Type -> String
printType (Type t) = printTerm t

-- | Print a Type, parenthesizing if it's an arrow type (for use in domain position)
printTypeParensIfArrow :: Type -> String
printTypeParensIfArrow ty@(Type t) = case t of
  TermForallOrFun (ForallOrFunForall (Forall (OpenBindersBinders [BinderType (TypeBinders [Name Nothing] _)]) _)) ->
    "(" ++ printType ty ++ ")"
  _ -> printType ty

-- | Print a Term
printTerm :: Term -> String
printTerm t = case t of
  TermForallOrFun fof -> case fof of
    ForallOrFunForall (Forall binders typ) ->
      -- Detect arrow type: forall (_ : A), B  renders as  A -> B
      -- Parenthesize the domain if it's also an arrow type
      case binders of
        OpenBindersBinders [BinderType (TypeBinders [Name Nothing] domType)] ->
          printTypeParensIfArrow domType ++ " -> " ++ printType typ
        _ ->
          "forall " ++ printOpenBinders binders ++ ", " ++ printType typ
    ForallOrFunFun (Fun binders body) ->
      "fun " ++ printOpenBinders binders ++ " => " ++ printTerm body
  TermLet (Let bindings body) ->
    "let " ++ printLetBindings bindings ++ " in " ++ printTerm body
  TermIf (If cond _ thn els) ->
    "if " ++ printTerm cond ++ " then " ++ printTerm thn ++ " else " ++ printTerm els
  TermFix f -> printFix f
  TermCofix _ -> "cofix ..."
  TermTerm100 t100 -> printTerm100 t100

-- | Print OpenBinders
printOpenBinders :: OpenBinders -> String
printOpenBinders ob = case ob of
  OpenBindersType tb -> printTypeBinders tb
  OpenBindersBinders bs -> unwords $ map printBinder bs

-- | Print TypeBinders
printTypeBinders :: TypeBinders -> String
printTypeBinders (TypeBinders names typ) =
  "(" ++ unwords (map printName names) ++ " : " ++ printType typ ++ ")"

-- | Print a Name
printName :: Name -> String
printName (Name Nothing) = "_"
printName (Name (Just i)) = printIdent i

-- | Print a Binder
printBinder :: Binder -> String
printBinder b = case b of
  BinderName n -> printName n
  BinderType tb -> printTypeBinders tb
  BinderTerm _ -> "..."
  BinderImplicit _ -> "{...}"
  BinderGeneralizing _ -> "`(...)"
  BinderPattern _ -> "_"

-- | Print LetBindings
printLetBindings :: LetBindings -> String
printLetBindings lb = case lb of
  LetBindingsNamed (LetNamed binder binders) ->
    let (LetBinder name mType body) = binder
        nameStr = printName name
        typeStr = maybe "" (\t -> " : " ++ printType t) mType
        bindersStr = if null binders then "" else " " ++ unwords (map printBinder binders)
    in nameStr ++ bindersStr ++ typeStr ++ " := " ++ printTerm body
  LetBindingsDestructuring _ -> "..."

-- | Print Term100
printTerm100 :: Term100 -> String
printTerm100 t = case t of
  Term100Cast (TypeCast term typ _) ->
    printTerm10 term ++ " : " ++ printType typ
  Term100Term10 t10 -> printTerm10 t10

-- | Print Term10
printTerm10 :: Term10 -> String
printTerm10 t = case t of
  Term10Application app -> printApplication app
  Term10OneTerm ot -> printOneTerm ot

-- | Print OneTerm
printOneTerm :: OneTerm -> String
printOneTerm ot = case ot of
  OneTermExplicit qa -> printQualidAnnotated qa
  OneTermTerm1 t1 -> printTerm1 t1

-- | Print QualidAnnotated
printQualidAnnotated :: QualidAnnotated -> String
printQualidAnnotated (QualidAnnotated q _) = printQualid q

-- | Print Term1
printTerm1 :: Term1 -> String
printTerm1 t = case t of
  Term1Projection -> "?projection"
  Term1Scope -> "?scope"
  Term1Term0 t0 -> printTerm0 t0

-- | Print Term0
printTerm0 :: Term0 -> String
printTerm0 t = case t of
  Term0QualidAnnotated qa -> printQualidAnnotated qa
  Term0Sort s -> printSort s
  Term0PrimitiveNotations pn -> case pn of
    PrimitiveNotationsNumber (Number n) -> show n
    PrimitiveNotationsString (String_ s) -> "\"" ++ s ++ "\""
  Term0Evar _ -> "?evar"
  Term0Match m -> printMatch m
  Term0Record -> "{| |}"
  Term0Generalizing -> "`()"
  Term0Ltac -> "ltac:()"
  Term0Parens inner -> "(" ++ printTerm inner ++ ")"

-- | Print Application
printApplication :: Application -> String
printApplication app = case app of
  ApplicationNormal (NormalApplication lhs args) ->
    printTerm1 lhs ++ " " ++ unwords (map printArg args)
  ApplicationAnnotated _ -> "@..."

-- | Print Arg
printArg :: Arg -> String
printArg a = case a of
  ArgIdent (IdentArg ident term) -> "(" ++ printIdent ident ++ " := " ++ printTerm term ++ ")"
  ArgNatural (NaturalArg (Natural n) _) -> show n
  ArgTerm t1 -> printTerm1Parens t1

-- | Print Term1 with parens if it's a complex expression
printTerm1Parens :: Term1 -> String
printTerm1Parens t = case t of
  Term1Term0 (Term0QualidAnnotated _) -> printTerm1 t
  Term1Term0 (Term0Sort _) -> printTerm1 t
  Term1Term0 (Term0Parens _) -> printTerm1 t
  _ -> "(" ++ printTerm1 t ++ ")"

-- | Print a Fix expression (anonymous fixpoint)
printFix :: Fix -> String
printFix f = case f of
  FixDecl (Fix_Decl ident binders _annot mType body) ->
    let nameStr = printIdent ident
        bindersStr = if null binders then "" else " " ++ unwords (map printBinder binders)
        typeStr = maybe "" (\t -> " : " ++ printType t) mType
    in "fix " ++ nameStr ++ bindersStr ++ typeStr ++ " := " ++ printTerm body
  FixQual _ -> "fix ..."

-- | Print Match
printMatch :: Match -> String
printMatch (Match items mRet _ eqs) =
  let itemsStr = intercalate ", " $ map printCaseItem items
      retStr = maybe "" (\r -> " return " ++ printTerm100 r) mRet
      eqsStr = intercalate "\n" $ map printEquation eqs
  in "match " ++ itemsStr ++ retStr ++ " with\n" ++ eqsStr ++ "\nend"

-- | Print an Equation
printEquation :: Equation -> String
printEquation (Equation pats body) =
  "| " ++ intercalate " | " (map (intercalate " " . map printPattern) pats) ++
  " => " ++ printTerm body

-- | Print a Pattern
printPattern :: Pattern -> String
printPattern p = case p of
  PatternPattern p10 -> printPattern10 p10
  PatternTerm _ -> "_"

-- | Print Pattern10
printPattern10 :: Pattern10 -> String
printPattern10 p = case p of
  Pattern10As (Pattern10_As p1 n) -> printPattern1 p1 ++ " as " ++ printName n
  Pattern10Patterns (Pattern10_Patterns p1 ps) ->
    unwords $ map printPattern1 (p1 : ps)
  Pattern10Qualiid (Pattern10_Qualid q ps) ->
    printQualid q ++ if null ps then "" else " " ++ unwords (map printPattern1 ps)

-- | Print Pattern1
printPattern1 :: Pattern1 -> String
printPattern1 (Pattern1 p0 _) = printPattern0 p0

-- | Print Pattern0
printPattern0 :: Pattern0 -> String
printPattern0 p = case p of
  Pattern0Qualid q -> printQualid q
  Pattern0QualIdAndPattern _ -> "..."
  Pattern0Placeholder -> "_"
  Pattern0Parens ps -> "(" ++ intercalate ", " (map printPattern ps) ++ ")"
  Pattern0Number (Number n) -> show n
  Pattern0String (String_ s) -> "\"" ++ s ++ "\""

-- | Print CaseItem
printCaseItem :: CaseItem -> String
printCaseItem (CaseItem term mAs _) =
  let asStr = maybe "" (\n -> " as " ++ printName n) mAs
  in printTerm100 term ++ asStr

-- ===========================================================================
-- Vernacular printing
-- ===========================================================================

-- | Print a RequireImport
printRequireImport :: RequireImport -> String
printRequireImport (RequireImport mFrom req mQual mods) =
  let fromStr = maybe "" (\q -> "From " ++ printQualid q ++ " ") mFrom
      reqStr = if req then "Require " else ""
      qualStr = case mQual of
        Nothing -> ""
        Just ImportQualificationImport -> "Import "
        Just ImportQualificationExport -> "Export "
      modsStr = unwords $ map printQualid mods
  in fromStr ++ reqStr ++ qualStr ++ modsStr ++ "."

-- | Print an InductiveDefinition
printInductive :: InductiveDefinition -> String
printInductive (InductiveDefinition mLoc coind bodies) =
  let locStr = maybe "" (\l -> printLocality l ++ " ") mLoc
      kwStr = if coind then "CoInductive" else "Inductive"
      bodiesStr = intercalate "\nwith " $ map printInductiveBody bodies
  in locStr ++ kwStr ++ " " ++ bodiesStr ++ "."

-- | Print an InductiveBody
printInductiveBody :: InductiveBody -> String
printInductiveBody (InductiveBody name binders mType constrs) =
  let nameStr = printIdent name
      bindersStr = if null binders then "" else " " ++ unwords (map printBinder binders)
      typeStr = maybe "" (\t -> " : " ++ printType t) mType
      constrsStr = intercalate "\n" $ map printConstructor constrs
  in nameStr ++ bindersStr ++ typeStr ++ " :=\n" ++ constrsStr

-- | Print a Constructor
printConstructor :: Constructor -> String
printConstructor (Constructor name binders mType) =
  let nameStr = printIdent name
      bindersStr = if null binders then "" else " " ++ unwords (map printBinder binders)
      typeStr = maybe "" (\t -> " : " ++ printType t) mType
  in "| " ++ nameStr ++ bindersStr ++ typeStr

-- | Print a RecordDefinition
printRecord :: RecordDefinition -> String
printRecord (RecordDefinition mLoc name binders mSort body) =
  let locStr = maybe "" (\l -> printLocality l ++ " ") mLoc
      nameStr = printIdent name
      bindersStr = if null binders then "" else " " ++ unwords (map printBinder binders)
      sortStr = maybe "" (\s -> " : " ++ printSort s) mSort
      (RecordBody mConstr fields) = body
      constrStr = maybe "" (\c -> printIdent c ++ " ") mConstr
      fieldsStr = intercalate " ;\n  " $ map printRecordField fields
  in locStr ++ "Record " ++ nameStr ++ bindersStr ++ sortStr ++ " := " ++ constrStr ++ "{\n  " ++ fieldsStr ++ "\n}."

-- | Print a RecordField
printRecordField :: RecordField -> String
printRecordField (RecordField name typ) =
  printIdent name ++ " : " ++ printType typ

-- | Print a Definition
printDefinition :: Definition -> String
printDefinition (Definition mLoc name binders mType body) =
  let locStr = maybe "" (\l -> printLocality l ++ " ") mLoc
      nameStr = printIdent name
      bindersStr = if null binders then "" else " " ++ unwords (map printBinder binders)
      typeStr = maybe "" (\t -> " : " ++ printType t) mType
  in locStr ++ "Definition " ++ nameStr ++ bindersStr ++ typeStr ++ " :=\n  " ++ printTerm body ++ "."

-- | Print a FixpointDefinition
printFixpoint :: FixpointDefinition -> String
printFixpoint (FixpointDefinition mLoc name binders _ mType body withDecls) =
  let locStr = maybe "" (\l -> printLocality l ++ " ") mLoc
      nameStr = printIdent name
      bindersStr = if null binders then "" else " " ++ unwords (map printBinder binders)
      typeStr = maybe "" (\t -> " : " ++ printType t) mType
      mainDef = locStr ++ "Fixpoint " ++ nameStr ++ bindersStr ++ typeStr ++ " :=\n  " ++ printTerm body
      withDefs = concatMap printWithDecl withDecls
  in mainDef ++ withDefs ++ "."
  where
    printWithDecl (Fix_Decl wName wBinders _ wType wBody) =
      let wNameStr = printIdent wName
          wBindersStr = if null wBinders then "" else " " ++ unwords (map printBinder wBinders)
          wTypeStr = maybe "" (\t -> " : " ++ printType t) wType
      in "\nwith " ++ wNameStr ++ wBindersStr ++ wTypeStr ++ " :=\n  " ++ printTerm wBody

-- | Print a TheoremBody
printTheorem :: TheoremBody -> String
printTheorem (TheoremBody kind name binders typ proof) =
  let kwStr = case kind of
        TheoremKindTheorem -> "Theorem"
        TheoremKindLemma -> "Lemma"
        TheoremKindProposition -> "Proposition"
        TheoremKindCorollary -> "Corollary"
        TheoremKindExample -> "Example"
      nameStr = printIdent name
      bindersStr = if null binders then "" else " " ++ unwords (map printBinder binders)
  in kwStr ++ " " ++ nameStr ++ bindersStr ++ " : " ++ printType typ ++ ".\n" ++
     "Proof.\n  exact (" ++ printTerm proof ++ ").\nQed."

-- | Print a ModuleDefinition
printModule :: ModuleDefinition -> String
printModule (ModuleDefinition name sentences) =
  let nameStr = printIdent name
      body = intercalate "\n\n" $ map printSentence sentences
  in "Module " ++ nameStr ++ ".\n\n" ++ body ++ "\n\nEnd " ++ nameStr ++ "."

-- | Print a SectionDefinition
printSection :: SectionDefinition -> String
printSection (SectionDefinition name sentences) =
  let nameStr = printIdent name
      body = intercalate "\n\n" $ map printSentence sentences
  in "Section " ++ nameStr ++ ".\n\n" ++ body ++ "\n\nEnd " ++ nameStr ++ "."

-- | Print Locality
printLocality :: Locality -> String
printLocality LocalityLocal = "Local"
printLocality LocalityGlobal = "Global"
