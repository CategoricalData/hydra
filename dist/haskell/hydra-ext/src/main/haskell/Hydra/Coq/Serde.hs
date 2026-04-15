-- Note: this is an automatically generated file. Do not edit.

-- | Coq serializer: converts Coq AST to concrete Coq source code

module Hydra.Coq.Serde where

import qualified Hydra.Ast as Ast
import qualified Hydra.Coq.Syntax as Syntax
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Serialization as Serialization
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci

applicationToExpr :: Syntax.Application -> Ast.Expr
applicationToExpr app =
    case app of
      Syntax.ApplicationNormal v0 -> Serialization.spaceSep [
        term1ToExpr (Syntax.normalApplicationLhs v0),
        (Serialization.spaceSep (Lists.map (\a -> case a of
          Syntax.ArgIdent v1 -> Serialization.spaceSep [
            Serialization.parens (Serialization.spaceSep [
              identToExpr (Syntax.identArgIdent v1),
              (Serialization.cst ":="),
              (termToExpr (Syntax.identArgTerm v1))])]
          Syntax.ArgNatural v1 ->
            let v = Syntax.unNatural (Syntax.naturalArgNatural v1)
            in (Serialization.cst (Literals.showBigint v))
          Syntax.ArgTerm v1 -> term1ToExpr v1) (Syntax.normalApplicationRhs v0)))]
      Syntax.ApplicationAnnotated v0 -> Serialization.spaceSep [
        Serialization.cst "@",
        (qualidToExpr (Syntax.qualidAnnotatedQualid (Syntax.annotatedApplicationAnnot v0)))]

binderToExpr :: Syntax.Binder -> Ast.Expr
binderToExpr b =
    case b of
      Syntax.BinderName v0 -> Maybes.maybe (Serialization.cst "_") (\i -> identToExpr i) (Syntax.unName v0)
      Syntax.BinderType v0 ->
        let names =
                Lists.map (\n -> Maybes.maybe (Serialization.cst "_") (\i -> identToExpr i) (Syntax.unName n)) (Syntax.typeBindersNames v0)
            ty = typeToExpr (Syntax.typeBindersType v0)
        in (Serialization.parens (Serialization.spaceSep [
          Serialization.spaceSep names,
          (Serialization.cst ":"),
          ty]))
      Syntax.BinderTerm v0 ->
        let name = Maybes.maybe (Serialization.cst "_") (\i -> identToExpr i) (Syntax.unName (Syntax.letBinderName v0))
            ty =
                    Maybes.maybe [] (\t -> [
                      Serialization.cst ":",
                      (typeToExpr t)]) (Syntax.letBinderType v0)
            body = termToExpr (Syntax.letBinderTerm v0)
        in (Serialization.parens (Serialization.spaceSep (Lists.concat [
          [
            name],
          ty,
          [
            Serialization.cst ":=",
            body]])))
      Syntax.BinderImplicit v0 -> case v0 of
        Syntax.ImplicitBindersMaximallyInserted v1 ->
          let names =
                  Lists.map (\n -> Maybes.maybe (Serialization.cst "_") (\i -> identToExpr i) (Syntax.unName n)) (Syntax.typeBindersNames v1)
              ty = typeToExpr (Syntax.typeBindersType v1)
          in (Serialization.brackets Serialization.curlyBraces Serialization.inlineStyle (Serialization.spaceSep [
            Serialization.spaceSep names,
            (Serialization.cst ":"),
            ty]))
        Syntax.ImplicitBindersNonMaximallyInserted v1 ->
          let names =
                  Lists.map (\n -> Maybes.maybe (Serialization.cst "_") (\i -> identToExpr i) (Syntax.unName n)) (Syntax.typeBindersNames v1)
              ty = typeToExpr (Syntax.typeBindersType v1)
          in (Serialization.brackets Serialization.squareBrackets Serialization.inlineStyle (Serialization.spaceSep [
            Serialization.spaceSep names,
            (Serialization.cst ":"),
            ty]))
      Syntax.BinderGeneralizing v0 -> case v0 of
        Syntax.GeneralizingBinderExplicit v1 -> Serialization.parens (termToExpr (Syntax.typeclassConstraintTerm v1))
        Syntax.GeneralizingBinderImplicitMaximallyInserted v1 -> Serialization.brackets Serialization.curlyBraces Serialization.inlineStyle (termToExpr (Syntax.typeclassConstraintTerm v1))
        Syntax.GeneralizingBinderImplicitNonMaximallyInserted v1 -> Serialization.brackets Serialization.squareBrackets Serialization.inlineStyle (termToExpr (Syntax.typeclassConstraintTerm v1))
      Syntax.BinderPattern _ -> Serialization.cst "_"

commentToExpr :: Syntax.Comment -> Ast.Expr
commentToExpr c =
    Serialization.cst (Strings.cat [
      "(* ",
      (Syntax.unComment c),
      " *)"])

constructorToExpr :: Syntax.Constructor -> Ast.Expr
constructorToExpr c =

      let name = identToExpr (Syntax.constructorName c)
          binders = Lists.map (\b -> binderToExpr b) (Syntax.constructorBinders c)
          ty =
                  Maybes.maybe [] (\t -> [
                    Serialization.cst ":",
                    (typeToExpr t)]) (Syntax.constructorType c)
      in (Serialization.spaceSep (Lists.concat [
        [
          Serialization.cst "|",
          name],
        binders,
        ty]))

definitionToExpr :: Syntax.Definition -> Ast.Expr
definitionToExpr d =

      let locPart = Maybes.maybe [] (\l -> [
            localityToExpr l]) (Syntax.definitionLocality d)
          name = identToExpr (Syntax.definitionName d)
          binders = Lists.map (\b -> binderToExpr b) (Syntax.definitionBinders d)
          ty =
                  Maybes.maybe [] (\t -> [
                    Serialization.cst ":",
                    (typeToExpr t)]) (Syntax.definitionType d)
          body = termToExpr (Syntax.definitionBody d)
      in (Serialization.suffix "." (Serialization.spaceSep (Lists.concat [
        locPart,
        [
          Serialization.cst "Definition",
          name],
        binders,
        ty,
        [
          Serialization.cst ":=",
          body]])))

documentToExpr :: Syntax.Document -> Ast.Expr
documentToExpr doc = Serialization.doubleNewlineSep (Lists.map (\s -> sentenceToExpr s) (Syntax.documentSentences doc))

fixpointDefinitionToExpr :: Syntax.FixpointDefinition -> Ast.Expr
fixpointDefinitionToExpr fd =

      let locPart = Maybes.maybe [] (\l -> [
            localityToExpr l]) (Syntax.fixpointDefinitionLocality fd)
          name = identToExpr (Syntax.fixpointDefinitionName fd)
          binders = Lists.map (\b -> binderToExpr b) (Syntax.fixpointDefinitionBinders fd)
          ty =
                  Maybes.maybe [] (\t -> [
                    Serialization.cst ":",
                    (typeToExpr t)]) (Syntax.fixpointDefinitionType fd)
          body = termToExpr (Syntax.fixpointDefinitionBody fd)
      in (Serialization.suffix "." (Serialization.spaceSep (Lists.concat [
        locPart,
        [
          Serialization.cst "Fixpoint",
          name],
        binders,
        ty,
        [
          Serialization.cst ":=",
          body]])))

identToExpr :: Syntax.Ident -> Ast.Expr
identToExpr ident = Serialization.cst (Syntax.unString (Syntax.unIdent ident))

inductiveBodyToExpr :: Syntax.InductiveBody -> Ast.Expr
inductiveBodyToExpr ib =

      let name = identToExpr (Syntax.inductiveBodyName ib)
          binders = Lists.map (\b -> binderToExpr b) (Syntax.inductiveBodyBinders ib)
          ty =
                  Maybes.maybe [] (\t -> [
                    Serialization.cst ":",
                    (typeToExpr t)]) (Syntax.inductiveBodyType ib)
          constrs = Lists.map (\c -> constructorToExpr c) (Syntax.inductiveBodyConstructors ib)
      in (Serialization.newlineSep (Lists.concat [
        [
          Serialization.spaceSep (Lists.concat [
            [
              name],
            binders,
            ty,
            [
              Serialization.cst ":="]])],
        constrs]))

inductiveDefinitionToExpr :: Syntax.InductiveDefinition -> Ast.Expr
inductiveDefinitionToExpr id =

      let locPart = Maybes.maybe [] (\l -> [
            localityToExpr l]) (Syntax.inductiveDefinitionLocality id)
          kwPart =
                  Logic.ifElse (Syntax.inductiveDefinitionCoinductive id) (Serialization.cst "CoInductive") (Serialization.cst "Inductive")
          bodyExprs = Lists.map (\b -> inductiveBodyToExpr b) (Syntax.inductiveDefinitionBodies id)
      in (Serialization.suffix "." (Serialization.newlineSep (Lists.concat [
        [
          Serialization.spaceSep (Lists.concat [
            locPart,
            [
              kwPart]])],
        bodyExprs])))

localityToExpr :: Syntax.Locality -> Ast.Expr
localityToExpr loc =
    case loc of
      Syntax.LocalityLocal -> Serialization.cst "Local"
      Syntax.LocalityGlobal -> Serialization.cst "Global"

matchToExpr :: Syntax.Match -> Ast.Expr
matchToExpr m =

      let items =
              Lists.map (\ci ->
                let t = term100ToExpr (Syntax.caseItemTerm ci)
                    asP =
                            Maybes.maybe [] (\n -> [
                              Serialization.cst "as",
                              (Maybes.maybe (Serialization.cst "_") (\i -> identToExpr i) (Syntax.unName n))]) (Syntax.caseItemAs ci)
                in (Serialization.spaceSep (Lists.concat [
                  [
                    t],
                  asP]))) (Syntax.matchCaseItems m)
          ret =
                  Maybes.maybe [] (\r -> [
                    Serialization.cst "return",
                    (term100ToExpr r)]) (Syntax.matchReturn m)
          eqs =
                  Lists.map (\eq ->
                    let pats = Serialization.cst "| ..."
                        body = termToExpr (Syntax.equationTerm eq)
                    in (Serialization.spaceSep [
                      Serialization.cst "|",
                      pats,
                      (Serialization.cst "=>"),
                      body])) (Syntax.matchEquations m)
      in (Serialization.newlineSep (Lists.concat [
        [
          Serialization.spaceSep (Lists.concat [
            [
              Serialization.cst "match"],
            items,
            ret,
            [
              Serialization.cst "with"]])],
        eqs,
        [
          Serialization.cst "end"]]))

moduleDefinitionToExpr :: Syntax.ModuleDefinition -> Ast.Expr
moduleDefinitionToExpr md =

      let name = identToExpr (Syntax.moduleDefinitionName md)
          sentences = Lists.map (\s -> sentenceToExpr s) (Syntax.moduleDefinitionSentences md)
      in (Serialization.doubleNewlineSep (Lists.concat [
        [
          Serialization.suffix "." (Serialization.spaceSep [
            Serialization.cst "Module",
            name])],
        sentences,
        [
          Serialization.suffix "." (Serialization.spaceSep [
            Serialization.cst "End",
            name])]]))

qualidToExpr :: Syntax.Qualid -> Ast.Expr
qualidToExpr q =

      let idExpr = identToExpr (Syntax.qualidId q)
          fieldIds = Syntax.qualidFieldIds q
          fieldExprs = Lists.map (\f -> identToExpr (Syntax.unFieldIdent f)) fieldIds
      in (Logic.ifElse (Lists.null fieldExprs) idExpr (Serialization.dotSep (Lists.concat2 [
        idExpr] fieldExprs)))

recordDefinitionToExpr :: Syntax.RecordDefinition -> Ast.Expr
recordDefinitionToExpr rd =

      let locPart = Maybes.maybe [] (\l -> [
            localityToExpr l]) (Syntax.recordDefinitionLocality rd)
          name = identToExpr (Syntax.recordDefinitionName rd)
          binders = Lists.map (\b -> binderToExpr b) (Syntax.recordDefinitionBinders rd)
          sortPart =
                  Maybes.maybe [] (\s -> [
                    Serialization.cst ":",
                    (sortToExpr s)]) (Syntax.recordDefinitionSort rd)
          body = Syntax.recordDefinitionBody rd
          constrPart = Maybes.maybe [] (\c -> [
                identToExpr c]) (Syntax.recordBodyConstructor body)
          fields = Lists.map (\f -> Serialization.suffix " ;" (recordFieldToExpr f)) (Syntax.recordBodyFields body)
      in (Serialization.suffix "." (Serialization.newlineSep (Lists.concat [
        [
          Serialization.spaceSep (Lists.concat [
            locPart,
            [
              Serialization.cst "Record"],
            [
              name],
            binders,
            sortPart,
            [
              Serialization.cst ":="],
            constrPart,
            [
              Serialization.cst "{"]])],
        fields,
        [
          Serialization.cst "}"]])))

recordFieldToExpr :: Syntax.RecordField -> Ast.Expr
recordFieldToExpr rf =
    Serialization.spaceSep [
      identToExpr (Syntax.recordFieldName rf),
      (Serialization.cst ":"),
      (typeToExpr (Syntax.recordFieldType rf))]

requireImportToExpr :: Syntax.RequireImport -> Ast.Expr
requireImportToExpr ri =

      let fromPart =
              Maybes.maybe [] (\q -> [
                Serialization.cst "From",
                (qualidToExpr q)]) (Syntax.requireImportFrom ri)
          requirePart = Logic.ifElse (Syntax.requireImportRequire ri) [
                Serialization.cst "Require"] []
          qualPart =
                  Maybes.maybe [] (\q -> case q of
                    Syntax.ImportQualificationImport -> [
                      Serialization.cst "Import"]
                    Syntax.ImportQualificationExport -> [
                      Serialization.cst "Export"]) (Syntax.requireImportQualification ri)
          mods = Lists.map (\m -> qualidToExpr m) (Syntax.requireImportModules ri)
      in (Serialization.suffix "." (Serialization.spaceSep (Lists.concat [
        fromPart,
        requirePart,
        qualPart,
        mods])))

sectionDefinitionToExpr :: Syntax.SectionDefinition -> Ast.Expr
sectionDefinitionToExpr sd =

      let name = identToExpr (Syntax.sectionDefinitionName sd)
          sentences = Lists.map (\s -> sentenceToExpr s) (Syntax.sectionDefinitionSentences sd)
      in (Serialization.doubleNewlineSep (Lists.concat [
        [
          Serialization.suffix "." (Serialization.spaceSep [
            Serialization.cst "Section",
            name])],
        sentences,
        [
          Serialization.suffix "." (Serialization.spaceSep [
            Serialization.cst "End",
            name])]]))

sentenceContentToExpr :: Syntax.SentenceContent -> Ast.Expr
sentenceContentToExpr sc =
    case sc of
      Syntax.SentenceContentDefinition v0 -> definitionToExpr v0
      Syntax.SentenceContentFixpoint v0 -> fixpointDefinitionToExpr v0
      Syntax.SentenceContentInductive v0 -> inductiveDefinitionToExpr v0
      Syntax.SentenceContentModule v0 -> moduleDefinitionToExpr v0
      Syntax.SentenceContentNotation _ -> Serialization.cst "(* notation *)"
      Syntax.SentenceContentRecord v0 -> recordDefinitionToExpr v0
      Syntax.SentenceContentRequireImport v0 -> requireImportToExpr v0
      Syntax.SentenceContentSection v0 -> sectionDefinitionToExpr v0
      Syntax.SentenceContentTheorem v0 -> theoremBodyToExpr v0

sentenceToExpr :: Syntax.Sentence -> Ast.Expr
sentenceToExpr s =

      let cmtPart = Maybes.maybe [] (\c -> [
            commentToExpr c]) (Syntax.sentenceComment s)
          content = sentenceContentToExpr (Syntax.sentenceContent s)
      in (Serialization.newlineSep (Lists.concat [
        cmtPart,
        [
          content]]))

sortToExpr :: Syntax.Sort -> Ast.Expr
sortToExpr s =
    case s of
      Syntax.SortSet -> Serialization.cst "Set"
      Syntax.SortProp -> Serialization.cst "Prop"
      Syntax.SortSProp -> Serialization.cst "SProp"
      Syntax.SortType -> Serialization.cst "Type"
      Syntax.SortTypeWithAnyUniverse -> Serialization.cst "Type"
      Syntax.SortTypeWithUniverse _ -> Serialization.noSep [
        Serialization.cst "Type",
        (Serialization.cst "@{"),
        (Serialization.cst "}")]

term0ToExpr :: Syntax.Term0 -> Ast.Expr
term0ToExpr t =
    case t of
      Syntax.Term0QualidAnnotated v0 -> qualidToExpr (Syntax.qualidAnnotatedQualid v0)
      Syntax.Term0Sort v0 -> sortToExpr v0
      Syntax.Term0PrimitiveNotations v0 -> case v0 of
        Syntax.PrimitiveNotationsNumber v1 ->
          let v = Syntax.unNumber v1
          in (Serialization.cst (Literals.showBigfloat v))
        Syntax.PrimitiveNotationsString v1 -> Serialization.spaceSep [
          Serialization.cst "\"",
          (Serialization.cst (Syntax.unString v1)),
          (Serialization.cst "\"")]
      Syntax.Term0Evar _ -> Serialization.cst "?evar"
      Syntax.Term0Match v0 -> matchToExpr v0
      Syntax.Term0Record -> Serialization.cst "{| |}"
      Syntax.Term0Generalizing -> Serialization.cst "`( )"
      Syntax.Term0Ltac -> Serialization.cst "ltac:( )"
      Syntax.Term0Parens v0 -> Serialization.parens (termToExpr v0)

term100ToExpr :: Syntax.Term100 -> Ast.Expr
term100ToExpr t =
    case t of
      Syntax.Term100Cast v0 -> Serialization.spaceSep [
        term10ToExpr (Syntax.typeCastTerm v0),
        (Serialization.cst ":"),
        (typeToExpr (Syntax.typeCastType v0))]
      Syntax.Term100Term10 v0 -> term10ToExpr v0

term10ToExpr :: Syntax.Term10 -> Ast.Expr
term10ToExpr t =
    case t of
      Syntax.Term10Application v0 -> applicationToExpr v0
      Syntax.Term10OneTerm v0 -> case v0 of
        Syntax.OneTermExplicit v1 -> qualidToExpr (Syntax.qualidAnnotatedQualid v1)
        Syntax.OneTermTerm1 v1 -> term1ToExpr v1

term1ToExpr :: Syntax.Term1 -> Ast.Expr
term1ToExpr t =
    case t of
      Syntax.Term1Projection -> Serialization.cst "?projection"
      Syntax.Term1Scope -> Serialization.cst "?scope"
      Syntax.Term1Term0 v0 -> term0ToExpr v0

termToExpr :: Syntax.Term -> Ast.Expr
termToExpr t =
    case t of
      Syntax.TermForallOrFun v0 -> case v0 of
        Syntax.ForallOrFunForall v1 -> Serialization.spaceSep [
          Serialization.cst "forall",
          case (Syntax.forallBinders v1) of
            Syntax.OpenBindersType v2 ->
              let names =
                      Lists.map (\n -> Maybes.maybe (Serialization.cst "_") (\i -> identToExpr i) (Syntax.unName n)) (Syntax.typeBindersNames v2)
                  ty = typeToExpr (Syntax.typeBindersType v2)
              in (Serialization.spaceSep [
                Serialization.parens (Serialization.spaceSep [
                  Serialization.spaceSep names,
                  (Serialization.cst ":"),
                  ty])])
            Syntax.OpenBindersBinders v2 -> Serialization.spaceSep (Lists.map (\b -> binderToExpr b) v2),
          (Serialization.cst ","),
          (typeToExpr (Syntax.forallType v1))]
        Syntax.ForallOrFunFun v1 -> Serialization.spaceSep [
          Serialization.cst "fun",
          case (Syntax.funBinders v1) of
            Syntax.OpenBindersType v2 ->
              let names =
                      Lists.map (\n -> Maybes.maybe (Serialization.cst "_") (\i -> identToExpr i) (Syntax.unName n)) (Syntax.typeBindersNames v2)
                  ty = typeToExpr (Syntax.typeBindersType v2)
              in (Serialization.spaceSep [
                Serialization.parens (Serialization.spaceSep [
                  Serialization.spaceSep names,
                  (Serialization.cst ":"),
                  ty])])
            Syntax.OpenBindersBinders v2 -> Serialization.spaceSep (Lists.map (\b -> binderToExpr b) v2),
          (Serialization.cst "=>"),
          (termToExpr (Syntax.funBody v1))]
      Syntax.TermLet v0 ->
        let bindings = Syntax.letBindings v0
            body = termToExpr (Syntax.letIn v0)
        in case bindings of
          Syntax.LetBindingsNamed v1 ->
            let binder = Syntax.letNamedBinder v1
                name = Maybes.maybe (Serialization.cst "_") (\i -> identToExpr i) (Syntax.unName (Syntax.letBinderName binder))
                binders = Lists.map (\b -> binderToExpr b) (Syntax.letNamedBinders v1)
                ty =
                        Maybes.maybe [] (\t2 -> [
                          Serialization.cst ":",
                          (typeToExpr t2)]) (Syntax.letBinderType binder)
                val = termToExpr (Syntax.letBinderTerm binder)
            in (Serialization.spaceSep [
              Serialization.cst "let",
              (Serialization.spaceSep (Lists.concat [
                [
                  name],
                binders,
                ty,
                [
                  Serialization.cst ":=",
                  val],
                [
                  Serialization.cst "in"]])),
              body])
          Syntax.LetBindingsDestructuring _ -> Serialization.spaceSep [
            Serialization.cst "let",
            (Serialization.cst "..."),
            (Serialization.cst "in"),
            body]
      Syntax.TermIf v0 ->
        let cond = termToExpr (Syntax.ifCondition v0)
            thn = termToExpr (Syntax.ifThen v0)
            els = termToExpr (Syntax.ifElse v0)
        in (Serialization.spaceSep [
          Serialization.cst "if",
          cond,
          (Serialization.cst "then"),
          thn,
          (Serialization.cst "else"),
          els])
      Syntax.TermFix v0 -> case v0 of
        Syntax.FixDecl v1 -> Serialization.spaceSep [
          Serialization.cst "fix",

            let name = identToExpr (Syntax.fix_DeclIdent v1)
                binders = Lists.map (\b -> binderToExpr b) (Syntax.fix_DeclBinders v1)
                ty =
                        Maybes.maybe [] (\t2 -> [
                          Serialization.cst ":",
                          (typeToExpr t2)]) (Syntax.fix_DeclType v1)
                body = termToExpr (Syntax.fix_DeclTerm v1)
            in (Serialization.spaceSep (Lists.concat [
              [
                name],
              binders,
              ty,
              [
                Serialization.cst ":=",
                body]]))]
        Syntax.FixQual _ -> Serialization.cst "fix"
      Syntax.TermCofix _ -> Serialization.spaceSep [
        Serialization.cst "cofix",
        (Serialization.cst "...")]
      Syntax.TermTerm100 v0 -> term100ToExpr v0

theoremBodyToExpr :: Syntax.TheoremBody -> Ast.Expr
theoremBodyToExpr tb =

      let kindKw =
              case (Syntax.theoremBodyKind tb) of
                Syntax.TheoremKindTheorem -> Serialization.cst "Theorem"
                Syntax.TheoremKindLemma -> Serialization.cst "Lemma"
                Syntax.TheoremKindProposition -> Serialization.cst "Proposition"
                Syntax.TheoremKindCorollary -> Serialization.cst "Corollary"
                Syntax.TheoremKindExample -> Serialization.cst "Example"
          name = identToExpr (Syntax.theoremBodyName tb)
          binders = Lists.map (\b -> binderToExpr b) (Syntax.theoremBodyBinders tb)
          ty = typeToExpr (Syntax.theoremBodyType tb)
          proof = termToExpr (Syntax.theoremBodyProof tb)
      in (Serialization.newlineSep [
        Serialization.suffix "." (Serialization.spaceSep (Lists.concat [
          [
            kindKw,
            name],
          binders,
          [
            Serialization.cst ":",
            ty]])),
        (Serialization.cst "Proof."),
        (Serialization.spaceSep [
          Serialization.cst "exact",
          (Serialization.parens proof)]),
        (Serialization.cst "Qed.")])

typeToExpr :: Syntax.Type -> Ast.Expr
typeToExpr t = termToExpr (Syntax.unType t)
