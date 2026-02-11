-- Note: this is an automatically generated file. Do not edit.

-- | Rust serializer: converts Rust AST to concrete syntax

module Hydra.Ext.Rust.Serde where

import qualified Hydra.Ast as Ast
import qualified Hydra.Ext.Rust.Syntax as Syntax
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Serialization as Serialization
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.ByteString as B
import qualified Data.Int as I
import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Set as S

-- | Serialize a Rust crate to an AST expression
crateToExpr :: (Syntax.Crate -> Ast.Expr)
crateToExpr crate = (Serialization.doubleNewlineSep (Lists.map itemWithCommentsToExpr (Syntax.crateItems crate)))

-- | Serialize a Rust item to an AST expression
itemToExpr :: (Syntax.Item -> Ast.Expr)
itemToExpr item = ((\x -> case x of
  Syntax.ItemUse v1 -> (useDeclarationToExpr v1)
  Syntax.ItemStruct v1 -> (structDefToExpr v1)
  Syntax.ItemEnum v1 -> (enumDefToExpr v1)
  Syntax.ItemFn v1 -> (fnDefToExpr v1)
  Syntax.ItemTypeAlias v1 -> (typeAliasToExpr v1)
  Syntax.ItemImpl v1 -> (implBlockToExpr v1)
  Syntax.ItemTrait v1 -> (traitDefToExpr v1)
  Syntax.ItemMod v1 -> (modDefToExpr v1)
  Syntax.ItemConst v1 -> (constDefToExpr v1)
  Syntax.ItemStatic v1 -> (staticDefToExpr v1)
  Syntax.ItemMacro v1 -> (macroInvocationToExpr v1)) item)

-- | Serialize an item with optional doc comments and visibility
itemWithCommentsToExpr :: (Syntax.ItemWithComments -> Ast.Expr)
itemWithCommentsToExpr iwc =  
  let doc = (Syntax.itemWithCommentsDoc iwc) 
      vis = (Syntax.itemWithCommentsVisibility iwc)
      item = (Syntax.itemWithCommentsItem iwc)
      docPart = (Maybes.maybe [] (\d -> [
              Serialization.cst (toRustDocComment d)]) doc)
      visPart = (visibilityToExpr vis)
      itemPart = (itemToExpr item)
  in (Serialization.newlineSep (Lists.concat [
    docPart,
    [
      Serialization.spaceSep (Maybes.cat [
        visPart,
        (Just itemPart)])]]))

-- | Serialize a use declaration
useDeclarationToExpr :: (Syntax.UseDeclaration -> Ast.Expr)
useDeclarationToExpr use =  
  let pub = (Syntax.useDeclarationPublic use) 
      tree = (Syntax.useDeclarationTree use)
      pubKw = (Logic.ifElse pub (Just (Serialization.cst "pub")) Nothing)
  in (Serialization.spaceSep (Maybes.cat [
    pubKw,
    (Just (Serialization.cst "use")),
    (Just (useTreeToExpr tree)),
    (Just (Serialization.cst ";"))]))

-- | Serialize a use tree
useTreeToExpr :: (Syntax.UseTree -> Ast.Expr)
useTreeToExpr tree = ((\x -> case x of
  Syntax.UseTreePath v1 -> (Serialization.cst (Strings.intercalate "::" (Syntax.usePathSegments v1)))
  Syntax.UseTreeRename v1 ->  
    let path = (Syntax.useRenamePath v1) 
        alias = (Syntax.useRenameAlias v1)
    in (Serialization.spaceSep [
      Serialization.cst (Strings.intercalate "::" path),
      (Serialization.cst "as"),
      (Serialization.cst alias)])
  Syntax.UseTreeGlob v1 -> (Serialization.cst (Strings.cat2 (Strings.intercalate "::" v1) "::*"))
  Syntax.UseTreeGroup v1 ->  
    let prefix = (Syntax.useGroupPrefix v1) 
        trees = (Syntax.useGroupTrees v1)
        prefixStr = (Logic.ifElse (Lists.null prefix) "" (Strings.cat2 (Strings.intercalate "::" prefix) "::"))
    in (Serialization.cst (Strings.cat [
      prefixStr,
      "{",
      (Strings.intercalate ", " (Lists.map (\t -> Serialization.printExpr (useTreeToExpr t)) trees)),
      "}"]))) tree)

-- | Serialize a struct definition
structDefToExpr :: (Syntax.StructDef -> Ast.Expr)
structDefToExpr s =  
  let name = (Syntax.structDefName s) 
      generics = (Syntax.structDefGenerics s)
      whereC = (Syntax.structDefWhereClause s)
      body = (Syntax.structDefBody s)
      derives = (Syntax.structDefDerives s)
      docC = (Syntax.structDefDoc s)
      derivesAttr = (derivesToExpr derives)
      docPart = (Maybes.maybe [] (\d -> [
              Serialization.cst (toRustDocComment d)]) docC)
      header = (Serialization.spaceSep (Maybes.cat [
              Just (Serialization.cst "struct"),
              (Just (Serialization.cst name)),
              (genericParamsToExpr generics)]))
      wherePart = (Maybes.maybe Nothing (\w -> Just (whereClauseToExpr w)) whereC)
  in (Serialization.newlineSep (Lists.concat [
    docPart,
    (Maybes.maybe [] (\d -> [
      d]) derivesAttr),
    [
      Serialization.spaceSep (Maybes.cat [
        Just header,
        wherePart,
        (Just (structBodyToExpr body))])]]))

-- | Serialize a struct body
structBodyToExpr :: (Syntax.StructBody -> Ast.Expr)
structBodyToExpr body = ((\x -> case x of
  Syntax.StructBodyNamed v1 -> (Serialization.curlyBracesList Nothing Serialization.halfBlockStyle (Lists.map structFieldToExpr v1))
  Syntax.StructBodyTuple v1 -> (Serialization.spaceSep [
    Serialization.parenList False (Lists.map (\f -> typeToExpr (Syntax.tupleFieldType f)) v1),
    (Serialization.cst ";")])
  Syntax.StructBodyUnit -> (Serialization.cst ";")) body)

-- | Serialize a struct field
structFieldToExpr :: (Syntax.StructField -> Ast.Expr)
structFieldToExpr field =  
  let name = (Syntax.structFieldName field) 
      typ = (Syntax.structFieldType field)
      pub = (Syntax.structFieldPublic field)
      docC = (Syntax.structFieldDoc field)
      pubKw = (Logic.ifElse pub (Just (Serialization.cst "pub")) Nothing)
      docPart = (Maybes.maybe [] (\d -> [
              Serialization.cst (toRustDocComment d)]) docC)
  in (Serialization.newlineSep (Lists.concat [
    docPart,
    [
      Serialization.spaceSep (Maybes.cat [
        pubKw,
        (Just (Serialization.cst (Strings.cat2 name ":"))),
        (Just (typeToExpr typ))])]]))

-- | Serialize an enum definition
enumDefToExpr :: (Syntax.EnumDef -> Ast.Expr)
enumDefToExpr e =  
  let name = (Syntax.enumDefName e) 
      generics = (Syntax.enumDefGenerics e)
      whereC = (Syntax.enumDefWhereClause e)
      variants = (Syntax.enumDefVariants e)
      derives = (Syntax.enumDefDerives e)
      docC = (Syntax.enumDefDoc e)
      derivesAttr = (derivesToExpr derives)
      docPart = (Maybes.maybe [] (\d -> [
              Serialization.cst (toRustDocComment d)]) docC)
      header = (Serialization.spaceSep (Maybes.cat [
              Just (Serialization.cst "enum"),
              (Just (Serialization.cst name)),
              (genericParamsToExpr generics)]))
      wherePart = (Maybes.maybe Nothing (\w -> Just (whereClauseToExpr w)) whereC)
      body = (Serialization.curlyBracesList Nothing Serialization.halfBlockStyle (Lists.map enumVariantToExpr variants))
  in (Serialization.newlineSep (Lists.concat [
    docPart,
    (Maybes.maybe [] (\d -> [
      d]) derivesAttr),
    [
      Serialization.spaceSep (Maybes.cat [
        Just header,
        wherePart,
        (Just body)])]]))

-- | Serialize an enum variant
enumVariantToExpr :: (Syntax.EnumVariant -> Ast.Expr)
enumVariantToExpr v =  
  let name = (Syntax.enumVariantName v) 
      body = (Syntax.enumVariantBody v)
      docC = (Syntax.enumVariantDoc v)
      docPart = (Maybes.maybe [] (\d -> [
              Serialization.cst (toRustDocComment d)]) docC)
  in (Serialization.newlineSep (Lists.concat [
    docPart,
    [
      Serialization.spaceSep [
        Serialization.cst name,
        (enumVariantBodyToExpr body)]]]))

-- | Serialize an enum variant body
enumVariantBodyToExpr :: (Syntax.EnumVariantBody -> Ast.Expr)
enumVariantBodyToExpr body = ((\x -> case x of
  Syntax.EnumVariantBodyUnit -> (Serialization.cst "")
  Syntax.EnumVariantBodyTuple v1 -> (Serialization.parenList False (Lists.map typeToExpr v1))
  Syntax.EnumVariantBodyStruct v1 -> (Serialization.curlyBracesList Nothing Serialization.halfBlockStyle (Lists.map structFieldToExpr v1))) body)

-- | Serialize a function definition
fnDefToExpr :: (Syntax.FnDef -> Ast.Expr)
fnDefToExpr f =  
  let name = (Syntax.fnDefName f) 
      generics = (Syntax.fnDefGenerics f)
      whereC = (Syntax.fnDefWhereClause f)
      params = (Syntax.fnDefParams f)
      retType = (Syntax.fnDefReturnType f)
      body = (Syntax.fnDefBody f)
      isAsync = (Syntax.fnDefAsync f)
      isConst = (Syntax.fnDefConst f)
      isUnsafe = (Syntax.fnDefUnsafe f)
      docC = (Syntax.fnDefDoc f)
      docPart = (Maybes.maybe [] (\d -> [
              Serialization.cst (toRustDocComment d)]) docC)
      asyncKw = (Logic.ifElse isAsync (Just (Serialization.cst "async")) Nothing)
      constKw = (Logic.ifElse isConst (Just (Serialization.cst "const")) Nothing)
      unsafeKw = (Logic.ifElse isUnsafe (Just (Serialization.cst "unsafe")) Nothing)
      fnKw = (Serialization.cst "fn")
      nameExpr = (Serialization.cst name)
      genericsExpr = (genericParamsToExpr generics)
      paramsExpr = (Serialization.parenList False (Lists.map fnParamToExpr params))
      retTypeExpr = (Maybes.maybe Nothing (\t -> Just (Serialization.spaceSep [
              Serialization.cst "->",
              (typeToExpr t)])) retType)
      whereExpr = (Maybes.maybe Nothing (\w -> Just (whereClauseToExpr w)) whereC)
      header = (Serialization.spaceSep (Maybes.cat [
              asyncKw,
              constKw,
              unsafeKw,
              (Just fnKw),
              (Just nameExpr),
              genericsExpr,
              (Just paramsExpr),
              retTypeExpr,
              whereExpr]))
  in (Serialization.newlineSep (Lists.concat [
    docPart,
    [
      Serialization.spaceSep [
        header,
        (blockToExpr body)]]]))

-- | Serialize a function parameter
fnParamToExpr :: (Syntax.FnParam -> Ast.Expr)
fnParamToExpr param =  
  let pat = (Syntax.fnParamPattern param) 
      typ = (Syntax.fnParamType param)
  in (Serialization.spaceSep [
    Serialization.cst (Strings.cat2 (Serialization.printExpr (patternToExpr pat)) ":"),
    (typeToExpr typ)])

-- | Serialize a method parameter
methodParamToExpr :: (Syntax.MethodParam -> Ast.Expr)
methodParamToExpr param = ((\x -> case x of
  Syntax.MethodParamSelf v1 -> ((\x -> case x of
    Syntax.SelfParamOwned -> (Serialization.cst "self")
    Syntax.SelfParamRef -> (Serialization.cst "&self")
    Syntax.SelfParamRefMut -> (Serialization.cst "&mut self")) v1)
  Syntax.MethodParamRegular v1 -> (fnParamToExpr v1)) param)

-- | Serialize a type alias
typeAliasToExpr :: (Syntax.TypeAlias -> Ast.Expr)
typeAliasToExpr ta =  
  let name = (Syntax.typeAliasName ta) 
      generics = (Syntax.typeAliasGenerics ta)
      typ = (Syntax.typeAliasType ta)
      docC = (Syntax.typeAliasDoc ta)
      docPart = (Maybes.maybe [] (\d -> [
              Serialization.cst (toRustDocComment d)]) docC)
  in (Serialization.newlineSep (Lists.concat [
    docPart,
    [
      Serialization.spaceSep (Maybes.cat [
        Just (Serialization.cst "type"),
        (Just (Serialization.cst name)),
        (genericParamsToExpr generics),
        (Just (Serialization.cst "=")),
        (Just (typeToExpr typ)),
        (Just (Serialization.cst ";"))])]]))

-- | Serialize a const definition
constDefToExpr :: (Syntax.ConstDef -> Ast.Expr)
constDefToExpr c =  
  let name = (Syntax.constDefName c) 
      typ = (Syntax.constDefType c)
      val = (Syntax.constDefValue c)
  in (Serialization.spaceSep [
    Serialization.cst "const",
    (Serialization.cst (Strings.cat2 name ":")),
    (typeToExpr typ),
    (Serialization.cst "="),
    (expressionToExpr val),
    (Serialization.cst ";")])

-- | Serialize a static definition
staticDefToExpr :: (Syntax.StaticDef -> Ast.Expr)
staticDefToExpr s =  
  let name = (Syntax.staticDefName s) 
      typ = (Syntax.staticDefType s)
      val = (Syntax.staticDefValue s)
      mut = (Syntax.staticDefMutable s)
      mutKw = (Logic.ifElse mut (Just (Serialization.cst "mut")) Nothing)
  in (Serialization.spaceSep (Maybes.cat [
    Just (Serialization.cst "static"),
    mutKw,
    (Just (Serialization.cst (Strings.cat2 name ":"))),
    (Just (typeToExpr typ)),
    (Just (Serialization.cst "=")),
    (Just (expressionToExpr val)),
    (Just (Serialization.cst ";"))]))

-- | Serialize a module definition
modDefToExpr :: (Syntax.ModDef -> Ast.Expr)
modDefToExpr m =  
  let name = (Syntax.modDefName m) 
      body = (Syntax.modDefBody m)
  in (Maybes.maybe (Serialization.spaceSep [
    Serialization.cst "mod",
    (Serialization.cst name),
    (Serialization.cst ";")]) (\items -> Serialization.spaceSep [
    Serialization.cst "mod",
    (Serialization.cst name),
    (Serialization.curlyBracesList Nothing Serialization.halfBlockStyle (Lists.map itemToExpr items))]) body)

-- | Serialize an impl block
implBlockToExpr :: (Syntax.ImplBlock -> Ast.Expr)
implBlockToExpr i =  
  let generics = (Syntax.implBlockGenerics i) 
      whereC = (Syntax.implBlockWhereClause i)
      trait = (Syntax.implBlockTrait i)
      selfType = (Syntax.implBlockSelfType i)
      items = (Syntax.implBlockItems i)
      genericsExpr = (genericParamsToExpr generics)
      traitPart = (Maybes.maybe Nothing (\t -> Just (Serialization.spaceSep [
              typePathToExpr t,
              (Serialization.cst "for")])) trait)
      wherePart = (Maybes.maybe Nothing (\w -> Just (whereClauseToExpr w)) whereC)
      header = (Serialization.spaceSep (Maybes.cat [
              Just (Serialization.cst "impl"),
              genericsExpr,
              traitPart,
              (Just (typeToExpr selfType)),
              wherePart]))
      body = (Serialization.curlyBracesList Nothing Serialization.halfBlockStyle (Lists.map implItemToExpr items))
  in (Serialization.spaceSep [
    header,
    body])

-- | Serialize an impl item
implItemToExpr :: (Syntax.ImplItem -> Ast.Expr)
implItemToExpr item = ((\x -> case x of
  Syntax.ImplItemMethod v1 -> (implMethodToExpr v1)
  Syntax.ImplItemType v1 -> (typeAliasToExpr v1)
  Syntax.ImplItemConst v1 -> (constDefToExpr v1)) item)

-- | Serialize an impl method
implMethodToExpr :: (Syntax.ImplMethod -> Ast.Expr)
implMethodToExpr m =  
  let name = (Syntax.implMethodName m) 
      generics = (Syntax.implMethodGenerics m)
      whereC = (Syntax.implMethodWhereClause m)
      params = (Syntax.implMethodParams m)
      retType = (Syntax.implMethodReturnType m)
      body = (Syntax.implMethodBody m)
      pub = (Syntax.implMethodPublic m)
      docC = (Syntax.implMethodDoc m)
      docPart = (Maybes.maybe [] (\d -> [
              Serialization.cst (toRustDocComment d)]) docC)
      pubKw = (Logic.ifElse pub (Just (Serialization.cst "pub")) Nothing)
      genericsExpr = (genericParamsToExpr generics)
      paramsExpr = (Serialization.parenList False (Lists.map methodParamToExpr params))
      retTypeExpr = (Maybes.maybe Nothing (\t -> Just (Serialization.spaceSep [
              Serialization.cst "->",
              (typeToExpr t)])) retType)
      whereExpr = (Maybes.maybe Nothing (\w -> Just (whereClauseToExpr w)) whereC)
      header = (Serialization.spaceSep (Maybes.cat [
              pubKw,
              (Just (Serialization.cst "fn")),
              (Just (Serialization.cst name)),
              genericsExpr,
              (Just paramsExpr),
              retTypeExpr,
              whereExpr]))
  in (Serialization.newlineSep (Lists.concat [
    docPart,
    [
      Serialization.spaceSep [
        header,
        (blockToExpr body)]]]))

-- | Serialize a trait definition
traitDefToExpr :: (Syntax.TraitDef -> Ast.Expr)
traitDefToExpr t =  
  let name = (Syntax.traitDefName t) 
      generics = (Syntax.traitDefGenerics t)
      whereC = (Syntax.traitDefWhereClause t)
      supers = (Syntax.traitDefSuperTraits t)
      items = (Syntax.traitDefItems t)
      isUnsafe = (Syntax.traitDefUnsafe t)
      docC = (Syntax.traitDefDoc t)
      docPart = (Maybes.maybe [] (\d -> [
              Serialization.cst (toRustDocComment d)]) docC)
      unsafeKw = (Logic.ifElse isUnsafe (Just (Serialization.cst "unsafe")) Nothing)
      genericsExpr = (genericParamsToExpr generics)
      superPart = (Logic.ifElse (Lists.null supers) Nothing (Just (Serialization.spaceSep [
              Serialization.cst ":",
              (Serialization.cst (Strings.intercalate " + " (Lists.map (\b -> Serialization.printExpr (typeParamBoundToExpr b)) supers)))])))
      wherePart = (Maybes.maybe Nothing (\w -> Just (whereClauseToExpr w)) whereC)
      header = (Serialization.spaceSep (Maybes.cat [
              unsafeKw,
              (Just (Serialization.cst "trait")),
              (Just (Serialization.cst name)),
              genericsExpr,
              superPart,
              wherePart]))
      body = (Serialization.curlyBracesList Nothing Serialization.halfBlockStyle (Lists.map traitItemToExpr items))
  in (Serialization.newlineSep (Lists.concat [
    docPart,
    [
      Serialization.spaceSep [
        header,
        body]]]))

-- | Serialize a trait item
traitItemToExpr :: (Syntax.TraitItem -> Ast.Expr)
traitItemToExpr item = ((\x -> case x of
  Syntax.TraitItemMethod v1 -> (traitMethodToExpr v1)
  Syntax.TraitItemType v1 -> (traitTypeToExpr v1)
  Syntax.TraitItemConst v1 -> (traitConstToExpr v1)) item)

-- | Serialize a trait method
traitMethodToExpr :: (Syntax.TraitMethod -> Ast.Expr)
traitMethodToExpr m =  
  let name = (Syntax.traitMethodName m) 
      generics = (Syntax.traitMethodGenerics m)
      params = (Syntax.traitMethodParams m)
      retType = (Syntax.traitMethodReturnType m)
      defBody = (Syntax.traitMethodDefaultBody m)
      genericsExpr = (genericParamsToExpr generics)
      paramsExpr = (Serialization.parenList False (Lists.map methodParamToExpr params))
      retTypeExpr = (Maybes.maybe Nothing (\t -> Just (Serialization.spaceSep [
              Serialization.cst "->",
              (typeToExpr t)])) retType)
      header = (Serialization.spaceSep (Maybes.cat [
              Just (Serialization.cst "fn"),
              (Just (Serialization.cst name)),
              genericsExpr,
              (Just paramsExpr),
              retTypeExpr]))
  in (Maybes.maybe (Serialization.spaceSep [
    header,
    (Serialization.cst ";")]) (\body -> Serialization.spaceSep [
    header,
    (blockToExpr body)]) defBody)

-- | Serialize a trait associated type
traitTypeToExpr :: (Syntax.TraitType -> Ast.Expr)
traitTypeToExpr t =  
  let name = (Syntax.traitTypeName t) 
      bounds = (Syntax.traitTypeBounds t)
      def = (Syntax.traitTypeDefault t)
      boundsPart = (Logic.ifElse (Lists.null bounds) Nothing (Just (Serialization.spaceSep [
              Serialization.cst ":",
              (Serialization.cst (Strings.intercalate " + " (Lists.map (\b -> Serialization.printExpr (typeParamBoundToExpr b)) bounds)))])))
      defPart = (Maybes.maybe Nothing (\d -> Just (Serialization.spaceSep [
              Serialization.cst "=",
              (typeToExpr d)])) def)
  in (Serialization.spaceSep (Maybes.cat [
    Just (Serialization.cst "type"),
    (Just (Serialization.cst name)),
    boundsPart,
    defPart,
    (Just (Serialization.cst ";"))]))

-- | Serialize a trait associated constant
traitConstToExpr :: (Syntax.TraitConst -> Ast.Expr)
traitConstToExpr c =  
  let name = (Syntax.traitConstName c) 
      typ = (Syntax.traitConstType c)
      def = (Syntax.traitConstDefault c)
      defPart = (Maybes.maybe Nothing (\d -> Just (Serialization.spaceSep [
              Serialization.cst "=",
              (expressionToExpr d)])) def)
  in (Serialization.spaceSep (Maybes.cat [
    Just (Serialization.cst "const"),
    (Just (Serialization.cst (Strings.cat2 name ":"))),
    (Just (typeToExpr typ)),
    defPart,
    (Just (Serialization.cst ";"))]))

-- | Serialize a generic parameter
genericParamToExpr :: (Syntax.GenericParam -> Ast.Expr)
genericParamToExpr gp =  
  let name = (Syntax.genericParamName gp) 
      bounds = (Syntax.genericParamBounds gp)
  in (Logic.ifElse (Lists.null bounds) (Serialization.cst name) (Serialization.spaceSep [
    Serialization.cst (Strings.cat2 name ":"),
    (Serialization.cst (Strings.intercalate " + " (Lists.map (\b -> Serialization.printExpr (typeParamBoundToExpr b)) bounds)))]))

-- | Serialize a list of generic parameters
genericParamsToExpr :: ([Syntax.GenericParam] -> Maybe Ast.Expr)
genericParamsToExpr gps = (Logic.ifElse (Lists.null gps) Nothing (Just (Serialization.angleBracesList Serialization.inlineStyle (Lists.map genericParamToExpr gps))))

-- | Serialize a type parameter bound
typeParamBoundToExpr :: (Syntax.TypeParamBound -> Ast.Expr)
typeParamBoundToExpr bound = ((\x -> case x of
  Syntax.TypeParamBoundTrait v1 -> (typePathToExpr v1)
  Syntax.TypeParamBoundLifetime v1 -> (Serialization.cst (Strings.cat2 "'" (Syntax.lifetimeName v1)))) bound)

-- | Serialize a where clause
whereClauseToExpr :: (Syntax.WhereClause -> Ast.Expr)
whereClauseToExpr wc =  
  let preds = (Syntax.whereClausePredicates wc) 
      predExprs = (Lists.map (\p ->  
              let typ = (Syntax.wherePredicateType p) 
                  bounds = (Syntax.wherePredicateBounds p)
              in (Serialization.spaceSep [
                typeToExpr typ,
                (Serialization.cst ":"),
                (Serialization.cst (Strings.intercalate " + " (Lists.map (\b -> Serialization.printExpr (typeParamBoundToExpr b)) bounds)))])) preds)
  in (Serialization.spaceSep [
    Serialization.cst "where",
    (Serialization.commaSep Serialization.inlineStyle predExprs)])

-- | Serialize a Rust type
typeToExpr :: (Syntax.Type -> Ast.Expr)
typeToExpr typ = ((\x -> case x of
  Syntax.TypePath_ v1 -> (typePathToExpr v1)
  Syntax.TypeReference v1 -> (referenceTypeToExpr v1)
  Syntax.TypeSlice v1 -> (Serialization.bracketList Serialization.inlineStyle [
    typeToExpr v1])
  Syntax.TypeArray v1 ->  
    let elem = (Syntax.arrayTypeElement v1) 
        len = (Syntax.arrayTypeLength v1)
    in (Serialization.cst (Strings.cat [
      "[",
      (Serialization.printExpr (typeToExpr elem)),
      "; ",
      (Serialization.printExpr (expressionToExpr len)),
      "]"]))
  Syntax.TypeTuple v1 -> (Serialization.parenList False (Lists.map typeToExpr v1))
  Syntax.TypeFnPointer v1 ->  
    let params = (Syntax.fnPointerTypeParams v1) 
        ret = (Syntax.fnPointerTypeReturnType v1)
    in (Serialization.spaceSep [
      Serialization.cst "fn",
      (Serialization.parenList False (Lists.map typeToExpr params)),
      (Serialization.cst "->"),
      (typeToExpr ret)])
  Syntax.TypeImplTrait v1 -> (Serialization.spaceSep [
    Serialization.cst "impl",
    (Serialization.cst (Strings.intercalate " + " (Lists.map (\b -> Serialization.printExpr (typeParamBoundToExpr b)) v1)))])
  Syntax.TypeDynTrait v1 -> (Serialization.spaceSep [
    Serialization.cst "dyn",
    (Serialization.cst (Strings.intercalate " + " (Lists.map (\b -> Serialization.printExpr (typeParamBoundToExpr b)) v1)))])
  Syntax.TypeInferred -> (Serialization.cst "_")
  Syntax.TypeUnit -> (Serialization.cst "()")
  Syntax.TypeNever -> (Serialization.cst "!")
  Syntax.TypeRawPointer v1 ->  
    let mut = (Syntax.rawPointerTypeMutable v1) 
        t = (Syntax.rawPointerTypeType v1)
        kw = (Logic.ifElse mut "*mut" "*const")
    in (Serialization.spaceSep [
      Serialization.cst kw,
      (typeToExpr t)])
  Syntax.TypeMacro v1 -> (macroInvocationToExpr v1)) typ)

-- | Serialize a type path
typePathToExpr :: (Syntax.TypePath -> Ast.Expr)
typePathToExpr tp =  
  let global = (Syntax.typePathGlobal tp) 
      segs = (Syntax.typePathSegments tp)
      prefix = (Logic.ifElse global "::" "")
      segStrs = (Lists.map (\s -> Serialization.printExpr (pathSegmentToExpr s)) segs)
  in (Serialization.cst (Strings.cat2 prefix (Strings.intercalate "::" segStrs)))

-- | Serialize a path segment
pathSegmentToExpr :: (Syntax.PathSegment -> Ast.Expr)
pathSegmentToExpr seg =  
  let name = (Syntax.pathSegmentName seg) 
      args = (Syntax.pathSegmentArguments seg)
  in (Serialization.spaceSep (Maybes.cat [
    Just (Serialization.cst name),
    (genericArgumentsToExpr args)]))

-- | Serialize generic arguments
genericArgumentsToExpr :: (Syntax.GenericArguments -> Maybe Ast.Expr)
genericArgumentsToExpr args = ((\x -> case x of
  Syntax.GenericArgumentsNone -> Nothing
  Syntax.GenericArgumentsAngleBracketed v1 ->  
    let args = (Syntax.angleBracketedArgsArgs v1)
    in (Just (Serialization.angleBracesList Serialization.inlineStyle (Lists.map genericArgToExpr args)))
  Syntax.GenericArgumentsParenthesized v1 ->  
    let inputs = (Syntax.parenthesizedArgsInputs v1) 
        output = (Syntax.parenthesizedArgsOutput v1)
        inputPart = (Serialization.parenList False (Lists.map typeToExpr inputs))
        outputPart = (Maybes.maybe Nothing (\t -> Just (Serialization.spaceSep [
                Serialization.cst "->",
                (typeToExpr t)])) output)
    in (Just (Serialization.spaceSep (Maybes.cat [
      Just inputPart,
      outputPart])))) args)

-- | Serialize a generic argument
genericArgToExpr :: (Syntax.GenericArg -> Ast.Expr)
genericArgToExpr arg = ((\x -> case x of
  Syntax.GenericArgType v1 -> (typeToExpr v1)
  Syntax.GenericArgLifetime v1 -> (Serialization.cst (Strings.cat2 "'" (Syntax.lifetimeName v1)))
  Syntax.GenericArgConst v1 -> (expressionToExpr v1)
  Syntax.GenericArgBinding v1 ->  
    let name = (Syntax.typeBindingName v1) 
        typ = (Syntax.typeBindingType v1)
    in (Serialization.spaceSep [
      Serialization.cst name,
      (Serialization.cst "="),
      (typeToExpr typ)])) arg)

-- | Serialize a reference type
referenceTypeToExpr :: (Syntax.ReferenceType -> Ast.Expr)
referenceTypeToExpr rt =  
  let lt = (Syntax.referenceTypeLifetime rt) 
      mut = (Syntax.referenceTypeMutable rt)
      t = (Syntax.referenceTypeType rt)
      ltPart = (Maybes.maybe "" (\l -> Strings.cat2 "'" (Strings.cat2 (Syntax.lifetimeName l) " ")) lt)
      mutPart = (Logic.ifElse mut "mut " "")
  in (Serialization.cst (Strings.cat [
    "&",
    ltPart,
    mutPart,
    (Serialization.printExpr (typeToExpr t))]))

-- | Serialize a Rust expression
expressionToExpr :: (Syntax.Expression -> Ast.Expr)
expressionToExpr expr = ((\x -> case x of
  Syntax.ExpressionLiteral v1 -> (literalToExpr v1)
  Syntax.ExpressionPath v1 -> (exprPathToExpr v1)
  Syntax.ExpressionBlock v1 -> (blockToExpr v1)
  Syntax.ExpressionCall v1 -> (callExprToExpr v1)
  Syntax.ExpressionMethodCall v1 -> (methodCallExprToExpr v1)
  Syntax.ExpressionFieldAccess v1 -> (fieldAccessExprToExpr v1)
  Syntax.ExpressionTupleIndex v1 -> (tupleIndexExprToExpr v1)
  Syntax.ExpressionClosure v1 -> (closureExprToExpr v1)
  Syntax.ExpressionIf v1 -> (ifExprToExpr v1)
  Syntax.ExpressionMatch v1 -> (matchExprToExpr v1)
  Syntax.ExpressionLoop v1 -> (loopExprToExpr v1)
  Syntax.ExpressionWhile v1 -> (whileExprToExpr v1)
  Syntax.ExpressionFor v1 -> (forExprToExpr v1)
  Syntax.ExpressionBinary v1 -> (binaryExprToExpr v1)
  Syntax.ExpressionUnary v1 -> (unaryExprToExpr v1)
  Syntax.ExpressionReference v1 -> (refExprToExpr v1)
  Syntax.ExpressionDereference v1 -> (Serialization.prefix "*" (expressionToExpr v1))
  Syntax.ExpressionStruct v1 -> (structExprToExpr v1)
  Syntax.ExpressionTuple v1 -> (Serialization.parenList False (Lists.map expressionToExpr v1))
  Syntax.ExpressionArray v1 -> (arrayExprToExpr v1)
  Syntax.ExpressionIndex v1 -> (indexExprToExpr v1)
  Syntax.ExpressionRange v1 -> (rangeExprToExpr v1)
  Syntax.ExpressionReturn v1 -> (Maybes.maybe (Serialization.cst "return") (\e -> Serialization.spaceSep [
    Serialization.cst "return",
    (expressionToExpr e)]) v1)
  Syntax.ExpressionBreak v1 -> (Maybes.maybe (Serialization.cst "break") (\e -> Serialization.spaceSep [
    Serialization.cst "break",
    (expressionToExpr e)]) v1)
  Syntax.ExpressionContinue -> (Serialization.cst "continue")
  Syntax.ExpressionTry v1 -> (Serialization.cst (Strings.cat2 (Serialization.printExpr (expressionToExpr v1)) "?"))
  Syntax.ExpressionCast v1 -> (castExprToExpr v1)
  Syntax.ExpressionTypeAscription v1 -> (typeAscriptionExprToExpr v1)
  Syntax.ExpressionAwait v1 -> (Serialization.cst (Strings.cat2 (Serialization.printExpr (expressionToExpr v1)) ".await"))
  Syntax.ExpressionAssign v1 -> (assignExprToExpr v1)
  Syntax.ExpressionCompoundAssign v1 -> (compoundAssignExprToExpr v1)
  Syntax.ExpressionMacro v1 -> (macroInvocationToExpr v1)
  Syntax.ExpressionParen v1 -> (Serialization.parenthesize (expressionToExpr v1))) expr)

-- | Serialize an expression path
exprPathToExpr :: (Syntax.ExprPath -> Ast.Expr)
exprPathToExpr ep =  
  let global = (Syntax.exprPathGlobal ep) 
      segs = (Syntax.exprPathSegments ep)
      prefix = (Logic.ifElse global "::" "")
      segStrs = (Lists.map (\s -> Serialization.printExpr (pathSegmentToExpr s)) segs)
  in (Serialization.cst (Strings.cat2 prefix (Strings.intercalate "::" segStrs)))

-- | Serialize a function call expression
callExprToExpr :: (Syntax.CallExpr -> Ast.Expr)
callExprToExpr c =  
  let func = (Syntax.callExprFunction c) 
      args = (Syntax.callExprArgs c)
  in (Serialization.spaceSep [
    expressionToExpr func,
    (Serialization.parenList False (Lists.map expressionToExpr args))])

-- | Serialize a method call expression
methodCallExprToExpr :: (Syntax.MethodCallExpr -> Ast.Expr)
methodCallExprToExpr m =  
  let recv = (Syntax.methodCallExprReceiver m) 
      method = (Syntax.methodCallExprMethod m)
      turbo = (Syntax.methodCallExprTurbofish m)
      args = (Syntax.methodCallExprArgs m)
      turboPart = (Logic.ifElse (Lists.null turbo) "" (Strings.cat [
              "::<",
              (Strings.intercalate ", " (Lists.map (\t -> Serialization.printExpr (typeToExpr t)) turbo)),
              ">"]))
  in (Serialization.cst (Strings.cat [
    Serialization.printExpr (expressionToExpr recv),
    ".",
    method,
    turboPart,
    "(",
    (Strings.intercalate ", " (Lists.map (\a -> Serialization.printExpr (expressionToExpr a)) args)),
    ")"]))

-- | Serialize a field access expression
fieldAccessExprToExpr :: (Syntax.FieldAccessExpr -> Ast.Expr)
fieldAccessExprToExpr f =  
  let obj = (Syntax.fieldAccessExprObject f) 
      field = (Syntax.fieldAccessExprField f)
  in (Serialization.cst (Strings.cat [
    Serialization.printExpr (expressionToExpr obj),
    ".",
    field]))

-- | Serialize a tuple index expression
tupleIndexExprToExpr :: (Syntax.TupleIndexExpr -> Ast.Expr)
tupleIndexExprToExpr t =  
  let tuple = (Syntax.tupleIndexExprTuple t) 
      idx = (Syntax.tupleIndexExprIndex t)
  in (Serialization.cst (Strings.cat [
    Serialization.printExpr (expressionToExpr tuple),
    ".",
    (Literals.showInt32 idx)]))

-- | Serialize a closure expression
closureExprToExpr :: (Syntax.ClosureExpr -> Ast.Expr)
closureExprToExpr c =  
  let move = (Syntax.closureExprMove c) 
      params = (Syntax.closureExprParams c)
      retType = (Syntax.closureExprReturnType c)
      body = (Syntax.closureExprBody c)
      moveKw = (Logic.ifElse move (Just (Serialization.cst "move")) Nothing)
      paramsStr = (Strings.cat [
              "|",
              (Strings.intercalate ", " (Lists.map closureParamToStr params)),
              "|"])
      retPart = (Maybes.maybe Nothing (\t -> Just (Serialization.spaceSep [
              Serialization.cst "->",
              (typeToExpr t)])) retType)
  in (Serialization.spaceSep (Maybes.cat [
    moveKw,
    (Just (Serialization.cst paramsStr)),
    retPart,
    (Just (expressionToExpr body))]))

-- | Serialize a closure parameter to string
closureParamToStr :: (Syntax.ClosureParam -> String)
closureParamToStr cp =  
  let pat = (Syntax.closureParamPattern cp) 
      typ = (Syntax.closureParamType cp)
      patStr = (Serialization.printExpr (patternToExpr pat))
  in (Maybes.maybe patStr (\t -> Strings.cat [
    patStr,
    ": ",
    (Serialization.printExpr (typeToExpr t))]) typ)

-- | Serialize an if expression
ifExprToExpr :: (Syntax.IfExpr -> Ast.Expr)
ifExprToExpr i =  
  let cond = (Syntax.ifExprCondition i) 
      thenB = (Syntax.ifExprThenBlock i)
      elseB = (Syntax.ifExprElseBranch i)
      condExpr = ((\x -> case x of
              Syntax.IfConditionBool v1 -> (expressionToExpr v1)
              Syntax.IfConditionLet v1 ->  
                let pat = (Syntax.letConditionPattern v1) 
                    expr = (Syntax.letConditionExpr v1)
                in (Serialization.spaceSep [
                  Serialization.cst "let",
                  (patternToExpr pat),
                  (Serialization.cst "="),
                  (expressionToExpr expr)])) cond)
      elsePart = (Maybes.maybe Nothing (\e -> Just (Serialization.spaceSep [
              Serialization.cst "else",
              (expressionToExpr e)])) elseB)
  in (Serialization.spaceSep (Maybes.cat [
    Just (Serialization.cst "if"),
    (Just condExpr),
    (Just (blockToExpr thenB)),
    elsePart]))

-- | Serialize a match expression
matchExprToExpr :: (Syntax.MatchExpr -> Ast.Expr)
matchExprToExpr m =  
  let scrut = (Syntax.matchExprScrutinee m) 
      arms = (Syntax.matchExprArms m)
  in (Serialization.spaceSep [
    Serialization.cst "match",
    (expressionToExpr scrut),
    (Serialization.curlyBracesList Nothing Serialization.halfBlockStyle (Lists.map matchArmToExpr arms))])

-- | Serialize a match arm
matchArmToExpr :: (Syntax.MatchArm -> Ast.Expr)
matchArmToExpr arm =  
  let pat = (Syntax.matchArmPattern arm) 
      guard = (Syntax.matchArmGuard arm)
      body = (Syntax.matchArmBody arm)
      guardPart = (Maybes.maybe Nothing (\g -> Just (Serialization.spaceSep [
              Serialization.cst "if",
              (expressionToExpr g)])) guard)
  in (Serialization.spaceSep (Maybes.cat [
    Just (patternToExpr pat),
    guardPart,
    (Just (Serialization.cst "=>")),
    (Just (expressionToExpr body)),
    (Just (Serialization.cst ","))]))

-- | Serialize a loop expression
loopExprToExpr :: (Syntax.LoopExpr -> Ast.Expr)
loopExprToExpr l =  
  let label = (Syntax.loopExprLabel l) 
      body = (Syntax.loopExprBody l)
      labelPart = (Maybes.maybe Nothing (\lbl -> Just (Serialization.cst (Strings.cat2 "'" (Strings.cat2 lbl ":")))) label)
  in (Serialization.spaceSep (Maybes.cat [
    labelPart,
    (Just (Serialization.cst "loop")),
    (Just (blockToExpr body))]))

-- | Serialize a while expression
whileExprToExpr :: (Syntax.WhileExpr -> Ast.Expr)
whileExprToExpr w =  
  let label = (Syntax.whileExprLabel w) 
      cond = (Syntax.whileExprCondition w)
      body = (Syntax.whileExprBody w)
      labelPart = (Maybes.maybe Nothing (\lbl -> Just (Serialization.cst (Strings.cat2 "'" (Strings.cat2 lbl ":")))) label)
      condExpr = ((\x -> case x of
              Syntax.IfConditionBool v1 -> (expressionToExpr v1)
              Syntax.IfConditionLet v1 ->  
                let pat = (Syntax.letConditionPattern v1) 
                    expr = (Syntax.letConditionExpr v1)
                in (Serialization.spaceSep [
                  Serialization.cst "let",
                  (patternToExpr pat),
                  (Serialization.cst "="),
                  (expressionToExpr expr)])) cond)
  in (Serialization.spaceSep (Maybes.cat [
    labelPart,
    (Just (Serialization.cst "while")),
    (Just condExpr),
    (Just (blockToExpr body))]))

-- | Serialize a for expression
forExprToExpr :: (Syntax.ForExpr -> Ast.Expr)
forExprToExpr f =  
  let label = (Syntax.forExprLabel f) 
      pat = (Syntax.forExprPattern f)
      iter = (Syntax.forExprIter f)
      body = (Syntax.forExprBody f)
      labelPart = (Maybes.maybe Nothing (\lbl -> Just (Serialization.cst (Strings.cat2 "'" (Strings.cat2 lbl ":")))) label)
  in (Serialization.spaceSep (Maybes.cat [
    labelPart,
    (Just (Serialization.cst "for")),
    (Just (patternToExpr pat)),
    (Just (Serialization.cst "in")),
    (Just (expressionToExpr iter)),
    (Just (blockToExpr body))]))

-- | Serialize a binary expression
binaryExprToExpr :: (Syntax.BinaryExpr -> Ast.Expr)
binaryExprToExpr b =  
  let left = (Syntax.binaryExprLeft b) 
      op = (Syntax.binaryExprOp b)
      right = (Syntax.binaryExprRight b)
  in (Serialization.spaceSep [
    expressionToExpr left,
    (binaryOpToExpr op),
    (expressionToExpr right)])

-- | Serialize a binary operator
binaryOpToExpr :: (Syntax.BinaryOp -> Ast.Expr)
binaryOpToExpr op = (Serialization.cst ((\x -> case x of
  Syntax.BinaryOpAdd -> "+"
  Syntax.BinaryOpSub -> "-"
  Syntax.BinaryOpMul -> "*"
  Syntax.BinaryOpDiv -> "/"
  Syntax.BinaryOpRem -> "%"
  Syntax.BinaryOpAnd -> "&&"
  Syntax.BinaryOpOr -> "||"
  Syntax.BinaryOpBitAnd -> "&"
  Syntax.BinaryOpBitOr -> "|"
  Syntax.BinaryOpBitXor -> "^"
  Syntax.BinaryOpShl -> "<<"
  Syntax.BinaryOpShr -> ">>"
  Syntax.BinaryOpEq -> "=="
  Syntax.BinaryOpNe -> "!="
  Syntax.BinaryOpLt -> "<"
  Syntax.BinaryOpLe -> "<="
  Syntax.BinaryOpGt -> ">"
  Syntax.BinaryOpGe -> ">=") op))

-- | Serialize a unary expression
unaryExprToExpr :: (Syntax.UnaryExpr -> Ast.Expr)
unaryExprToExpr u =  
  let op = (Syntax.unaryExprOp u) 
      operand = (Syntax.unaryExprOperand u)
      opStr = ((\x -> case x of
              Syntax.UnaryOpNeg -> "-"
              Syntax.UnaryOpNot -> "!") op)
  in (Serialization.cst (Strings.cat2 opStr (Serialization.printExpr (expressionToExpr operand))))

-- | Serialize a reference expression
refExprToExpr :: (Syntax.RefExpr -> Ast.Expr)
refExprToExpr r =  
  let mut = (Syntax.refExprMutable r) 
      expr = (Syntax.refExprExpr r)
      prefix = (Logic.ifElse mut "&mut " "&")
  in (Serialization.cst (Strings.cat2 prefix (Serialization.printExpr (expressionToExpr expr))))

-- | Serialize a struct literal expression
structExprToExpr :: (Syntax.StructExpr -> Ast.Expr)
structExprToExpr s =  
  let path = (Syntax.structExprPath s) 
      fields = (Syntax.structExprFields s)
      rest = (Syntax.structExprRest s)
      fieldExprs = (Lists.map fieldValueToExpr fields)
      restExpr = (Maybes.maybe [] (\r -> [
              Serialization.spaceSep [
                Serialization.cst "..",
                (expressionToExpr r)]]) rest)
      allFields = (Lists.concat2 fieldExprs restExpr)
  in (Serialization.spaceSep [
    exprPathToExpr path,
    (Serialization.curlyBracesList Nothing Serialization.halfBlockStyle allFields)])

-- | Serialize a field-value pair
fieldValueToExpr :: (Syntax.FieldValue -> Ast.Expr)
fieldValueToExpr fv =  
  let name = (Syntax.fieldValueName fv) 
      val = (Syntax.fieldValueValue fv)
  in (Maybes.maybe (Serialization.cst name) (\v -> Serialization.spaceSep [
    Serialization.cst (Strings.cat2 name ":"),
    (expressionToExpr v)]) val)

-- | Serialize an array expression
arrayExprToExpr :: (Syntax.ArrayExpr -> Ast.Expr)
arrayExprToExpr a = ((\x -> case x of
  Syntax.ArrayExprElements v1 -> (Serialization.bracketList Serialization.halfBlockStyle (Lists.map expressionToExpr v1))
  Syntax.ArrayExprRepeat v1 ->  
    let elem = (Syntax.arrayRepeatElement v1) 
        len = (Syntax.arrayRepeatLength v1)
    in (Serialization.cst (Strings.cat [
      "[",
      (Serialization.printExpr (expressionToExpr elem)),
      "; ",
      (Serialization.printExpr (expressionToExpr len)),
      "]"]))) a)

-- | Serialize an index expression
indexExprToExpr :: (Syntax.IndexExpr -> Ast.Expr)
indexExprToExpr i =  
  let obj = (Syntax.indexExprObject i) 
      idx = (Syntax.indexExprIndex i)
  in (Serialization.cst (Strings.cat [
    Serialization.printExpr (expressionToExpr obj),
    "[",
    (Serialization.printExpr (expressionToExpr idx)),
    "]"]))

-- | Serialize a range expression
rangeExprToExpr :: (Syntax.RangeExpr -> Ast.Expr)
rangeExprToExpr r =  
  let from = (Syntax.rangeExprFrom r) 
      to = (Syntax.rangeExprTo r)
      incl = (Syntax.rangeExprInclusive r)
      fromStr = (Maybes.maybe "" (\f -> Serialization.printExpr (expressionToExpr f)) from)
      toStr = (Maybes.maybe "" (\t -> Serialization.printExpr (expressionToExpr t)) to)
      op = (Logic.ifElse incl "..=" "..")
  in (Serialization.cst (Strings.cat [
    fromStr,
    op,
    toStr]))

-- | Serialize a cast expression
castExprToExpr :: (Syntax.CastExpr -> Ast.Expr)
castExprToExpr c =  
  let expr = (Syntax.castExprExpr c) 
      typ = (Syntax.castExprType c)
  in (Serialization.spaceSep [
    expressionToExpr expr,
    (Serialization.cst "as"),
    (typeToExpr typ)])

-- | Serialize a type ascription expression
typeAscriptionExprToExpr :: (Syntax.TypeAscriptionExpr -> Ast.Expr)
typeAscriptionExprToExpr t =  
  let expr = (Syntax.typeAscriptionExprExpr t) 
      typ = (Syntax.typeAscriptionExprType t)
  in (Serialization.spaceSep [
    expressionToExpr expr,
    (Serialization.cst ":"),
    (typeToExpr typ)])

-- | Serialize an assignment expression
assignExprToExpr :: (Syntax.AssignExpr -> Ast.Expr)
assignExprToExpr a =  
  let target = (Syntax.assignExprTarget a) 
      val = (Syntax.assignExprValue a)
  in (Serialization.spaceSep [
    expressionToExpr target,
    (Serialization.cst "="),
    (expressionToExpr val)])

-- | Serialize a compound assignment expression
compoundAssignExprToExpr :: (Syntax.CompoundAssignExpr -> Ast.Expr)
compoundAssignExprToExpr c =  
  let target = (Syntax.compoundAssignExprTarget c) 
      op = (Syntax.compoundAssignExprOp c)
      val = (Syntax.compoundAssignExprValue c)
      opStr = ((\x -> case x of
              Syntax.CompoundAssignOpAddAssign -> "+="
              Syntax.CompoundAssignOpSubAssign -> "-="
              Syntax.CompoundAssignOpMulAssign -> "*="
              Syntax.CompoundAssignOpDivAssign -> "/="
              Syntax.CompoundAssignOpRemAssign -> "%="
              Syntax.CompoundAssignOpBitAndAssign -> "&="
              Syntax.CompoundAssignOpBitOrAssign -> "|="
              Syntax.CompoundAssignOpBitXorAssign -> "^="
              Syntax.CompoundAssignOpShlAssign -> "<<="
              Syntax.CompoundAssignOpShrAssign -> ">>=") op)
  in (Serialization.spaceSep [
    expressionToExpr target,
    (Serialization.cst opStr),
    (expressionToExpr val)])

-- | Serialize a macro invocation
macroInvocationToExpr :: (Syntax.MacroInvocation -> Ast.Expr)
macroInvocationToExpr m =  
  let path = (Syntax.macroInvocationPath m) 
      delim = (Syntax.macroInvocationDelimiter m)
      tokens = (Syntax.macroInvocationTokens m)
      pathStr = (Strings.intercalate "::" path)
      open = ((\x -> case x of
              Syntax.MacroDelimiterParen -> "("
              Syntax.MacroDelimiterBracket -> "["
              Syntax.MacroDelimiterBrace -> "{") delim)
      close = ((\x -> case x of
              Syntax.MacroDelimiterParen -> ")"
              Syntax.MacroDelimiterBracket -> "]"
              Syntax.MacroDelimiterBrace -> "}") delim)
  in (Serialization.cst (Strings.cat [
    pathStr,
    "!",
    open,
    tokens,
    close]))

-- | Serialize a statement
statementToExpr :: (Syntax.Statement -> Ast.Expr)
statementToExpr stmt = ((\x -> case x of
  Syntax.StatementLet v1 -> (letStatementToExpr v1)
  Syntax.StatementExpression v1 -> (Serialization.spaceSep [
    expressionToExpr v1,
    (Serialization.cst ";")])
  Syntax.StatementItem v1 -> (itemToExpr v1)
  Syntax.StatementEmpty -> (Serialization.cst ";")) stmt)

-- | Serialize a let statement
letStatementToExpr :: (Syntax.LetStatement -> Ast.Expr)
letStatementToExpr l =  
  let pat = (Syntax.letStatementPattern l) 
      mut = (Syntax.letStatementMutable l)
      typ = (Syntax.letStatementType l)
      init = (Syntax.letStatementInit l)
      mutKw = (Logic.ifElse mut (Just (Serialization.cst "mut")) Nothing)
      typPart = (Maybes.maybe Nothing (\t -> Just (Serialization.spaceSep [
              Serialization.cst ":",
              (typeToExpr t)])) typ)
      initPart = (Maybes.maybe Nothing (\e -> Just (Serialization.spaceSep [
              Serialization.cst "=",
              (expressionToExpr e)])) init)
  in (Serialization.spaceSep (Maybes.cat [
    Just (Serialization.cst "let"),
    mutKw,
    (Just (patternToExpr pat)),
    typPart,
    initPart,
    (Just (Serialization.cst ";"))]))

-- | Serialize a block
blockToExpr :: (Syntax.Block -> Ast.Expr)
blockToExpr b =  
  let stmts = (Syntax.blockStatements b) 
      expr = (Syntax.blockExpression b)
      stmtExprs = (Lists.map statementToExpr stmts)
      exprPart = (Maybes.maybe [] (\e -> [
              expressionToExpr e]) expr)
      allParts = (Lists.concat2 stmtExprs exprPart)
  in (Serialization.curlyBracesList Nothing Serialization.halfBlockStyle allParts)

-- | Serialize a pattern
patternToExpr :: (Syntax.Pattern -> Ast.Expr)
patternToExpr pat = ((\x -> case x of
  Syntax.PatternWildcard -> (Serialization.cst "_")
  Syntax.PatternIdentifier v1 -> (identifierPatternToExpr v1)
  Syntax.PatternLiteral v1 -> (literalToExpr v1)
  Syntax.PatternReference v1 -> (refPatternToExpr v1)
  Syntax.PatternStruct v1 -> (structPatternToExpr v1)
  Syntax.PatternTupleStruct v1 -> (tupleStructPatternToExpr v1)
  Syntax.PatternTuple v1 -> (Serialization.parenList False (Lists.map patternToExpr v1))
  Syntax.PatternSlice v1 -> (Serialization.bracketList Serialization.halfBlockStyle (Lists.map patternToExpr v1))
  Syntax.PatternOr v1 -> (Serialization.cst (Strings.intercalate " | " (Lists.map (\p -> Serialization.printExpr (patternToExpr p)) v1)))
  Syntax.PatternPath v1 -> (exprPathToExpr v1)
  Syntax.PatternRange v1 -> (rangePatternToExpr v1)
  Syntax.PatternRest -> (Serialization.cst "..")
  Syntax.PatternParen v1 -> (Serialization.parenthesize (patternToExpr v1))) pat)

-- | Serialize an identifier pattern
identifierPatternToExpr :: (Syntax.IdentifierPattern -> Ast.Expr)
identifierPatternToExpr ip =  
  let name = (Syntax.identifierPatternName ip) 
      mut = (Syntax.identifierPatternMutable ip)
      atPat = (Syntax.identifierPatternAtPattern ip)
      mutKw = (Logic.ifElse mut (Just (Serialization.cst "mut")) Nothing)
      atPart = (Maybes.maybe Nothing (\p -> Just (Serialization.spaceSep [
              Serialization.cst "@",
              (patternToExpr p)])) atPat)
  in (Serialization.spaceSep (Maybes.cat [
    mutKw,
    (Just (Serialization.cst name)),
    atPart]))

-- | Serialize a reference pattern
refPatternToExpr :: (Syntax.RefPattern -> Ast.Expr)
refPatternToExpr rp =  
  let mut = (Syntax.refPatternMutable rp) 
      pat = (Syntax.refPatternPattern rp)
      prefix = (Logic.ifElse mut "&mut " "&")
  in (Serialization.cst (Strings.cat2 prefix (Serialization.printExpr (patternToExpr pat))))

-- | Serialize a struct pattern
structPatternToExpr :: (Syntax.StructPattern -> Ast.Expr)
structPatternToExpr sp =  
  let path = (Syntax.structPatternPath sp) 
      fields = (Syntax.structPatternFields sp)
      rest = (Syntax.structPatternRest sp)
      fieldExprs = (Lists.map fieldPatternToExpr fields)
      restExpr = (Logic.ifElse rest [
              Serialization.cst ".."] [])
      allFields = (Lists.concat2 fieldExprs restExpr)
  in (Serialization.spaceSep [
    exprPathToExpr path,
    (Serialization.curlyBracesList Nothing Serialization.halfBlockStyle allFields)])

-- | Serialize a field pattern
fieldPatternToExpr :: (Syntax.FieldPattern -> Ast.Expr)
fieldPatternToExpr fp =  
  let name = (Syntax.fieldPatternName fp) 
      pat = (Syntax.fieldPatternPattern fp)
  in (Maybes.maybe (Serialization.cst name) (\p -> Serialization.spaceSep [
    Serialization.cst (Strings.cat2 name ":"),
    (patternToExpr p)]) pat)

-- | Serialize a tuple struct pattern
tupleStructPatternToExpr :: (Syntax.TupleStructPattern -> Ast.Expr)
tupleStructPatternToExpr tsp =  
  let path = (Syntax.tupleStructPatternPath tsp) 
      elems = (Syntax.tupleStructPatternElements tsp)
  in (Serialization.spaceSep [
    exprPathToExpr path,
    (Serialization.parenList False (Lists.map patternToExpr elems))])

-- | Serialize a range pattern
rangePatternToExpr :: (Syntax.RangePattern -> Ast.Expr)
rangePatternToExpr rp =  
  let from = (Syntax.rangePatternFrom rp) 
      to = (Syntax.rangePatternTo rp)
      incl = (Syntax.rangePatternInclusive rp)
      fromStr = (Maybes.maybe "" (\p -> Serialization.printExpr (patternToExpr p)) from)
      toStr = (Maybes.maybe "" (\p -> Serialization.printExpr (patternToExpr p)) to)
      op = (Logic.ifElse incl "..=" "..")
  in (Serialization.cst (Strings.cat [
    fromStr,
    op,
    toStr]))

-- | Serialize a literal
literalToExpr :: (Syntax.Literal -> Ast.Expr)
literalToExpr lit = ((\x -> case x of
  Syntax.LiteralInteger v1 -> (integerLiteralToExpr v1)
  Syntax.LiteralFloat v1 -> (floatLiteralToExpr v1)
  Syntax.LiteralString v1 -> (Serialization.cst (Literals.showString v1))
  Syntax.LiteralRawString v1 -> (Serialization.cst (Strings.cat [
    "r\"",
    v1,
    "\""]))
  Syntax.LiteralByteString _ -> (Serialization.cst "b\"...\"")
  Syntax.LiteralChar v1 -> (Serialization.cst (Strings.cat [
    "'",
    (Literals.showUint32 v1),
    "'"]))
  Syntax.LiteralByte v1 -> (Serialization.cst (Strings.cat [
    "b'",
    (Literals.showUint8 v1),
    "'"]))
  Syntax.LiteralBool v1 -> (Serialization.cst (Logic.ifElse v1 "true" "false"))) lit)

-- | Serialize an integer literal
integerLiteralToExpr :: (Syntax.IntegerLiteral -> Ast.Expr)
integerLiteralToExpr il =  
  let val = (Syntax.integerLiteralValue il) 
      suf = (Syntax.integerLiteralSuffix il)
      valStr = (Literals.showBigint val)
      sufStr = (Maybes.maybe "" (\s -> s) suf)
  in (Serialization.cst (Strings.cat2 valStr sufStr))

-- | Serialize a float literal
floatLiteralToExpr :: (Syntax.FloatLiteral -> Ast.Expr)
floatLiteralToExpr fl =  
  let val = (Syntax.floatLiteralValue fl) 
      suf = (Syntax.floatLiteralSuffix fl)
      valStr = (Literals.showFloat64 val)
      sufStr = (Maybes.maybe "" (\s -> s) suf)
  in (Serialization.cst (Strings.cat2 valStr sufStr))

-- | Serialize visibility to an optional expression
visibilityToExpr :: (Syntax.Visibility -> Maybe Ast.Expr)
visibilityToExpr vis = ((\x -> case x of
  Syntax.VisibilityPublic -> (Just (Serialization.cst "pub"))
  Syntax.VisibilityCrate -> (Just (Serialization.cst "pub(crate)"))
  Syntax.VisibilityRestricted v1 -> (Just (Serialization.cst (Strings.cat [
    "pub(in ",
    (Strings.intercalate "::" v1),
    ")"])))
  Syntax.VisibilityPrivate -> Nothing) vis)

-- | Serialize an attribute
attributeToExpr :: (Syntax.Attribute -> Ast.Expr)
attributeToExpr attr =  
  let inner = (Syntax.attributeInner attr) 
      path = (Syntax.attributePath attr)
      tokens = (Syntax.attributeTokens attr)
      prefix = (Logic.ifElse inner "#![" "#[")
      pathStr = (Strings.intercalate "::" path)
      tokensPart = (Maybes.maybe "" (\t -> Strings.cat [
              "(",
              t,
              ")"]) tokens)
  in (Serialization.cst (Strings.cat [
    prefix,
    pathStr,
    tokensPart,
    "]"]))

-- | Serialize derive macros to an attribute expression
derivesToExpr :: ([String] -> Maybe Ast.Expr)
derivesToExpr derives = (Logic.ifElse (Lists.null derives) Nothing (Just (Serialization.cst (Strings.cat [
  "#[derive(",
  (Strings.intercalate ", " derives),
  ")]"]))))

-- | Convert a string to Rust doc comments
toRustDocComment :: (String -> String)
toRustDocComment c = (Strings.intercalate "\n" (Lists.map (\s -> Strings.cat2 "/// " s) (Strings.lines c)))

-- | Convert a string to Rust line comments
toRustComment :: (String -> String)
toRustComment c = (Strings.intercalate "\n" (Lists.map (\s -> Strings.cat2 "// " s) (Strings.lines c)))
