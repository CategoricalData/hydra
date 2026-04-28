-- Note: this is an automatically generated file. Do not edit.
-- | Free variable analysis, term-level substitution, and unshadowing

module Hydra.Variables where
import qualified Hydra.Coders as Coders
import qualified Hydra.Core as Core
import qualified Hydra.Lib.Equality as Equality
import qualified Hydra.Lib.Lists as Lists
import qualified Hydra.Lib.Literals as Literals
import qualified Hydra.Lib.Logic as Logic
import qualified Hydra.Lib.Maps as Maps
import qualified Hydra.Lib.Math as Math
import qualified Hydra.Lib.Maybes as Maybes
import qualified Hydra.Lib.Pairs as Pairs
import qualified Hydra.Lib.Sets as Sets
import qualified Hydra.Lib.Strings as Strings
import qualified Hydra.Rewriting as Rewriting
import Prelude hiding  (Enum, Ordering, decodeFloat, encodeFloat, fail, map, pure, sum)
import qualified Data.Scientific as Sci
import qualified Data.Map as M
import qualified Data.Set as S
-- | Get the set of free type variables in a term (including schema names, where they appear in type annotations). In this context, only the type schemes of let bindings can bind type variables; type lambdas do not.
freeTypeVariablesInTerm :: Core.Term -> S.Set Core.Name
freeTypeVariablesInTerm term0 =

      let allOf = \sets -> Lists.foldl Sets.union Sets.empty sets
          tryType = \tvars -> \typ -> Sets.difference (freeVariablesInType typ) tvars
          getAll =
                  \vars -> \term ->
                    let recurse = getAll vars
                        dflt = allOf (Lists.map recurse (Rewriting.subterms term))
                    in case term of
                      Core.TermLambda v0 ->
                        let domt = Maybes.maybe Sets.empty (tryType vars) (Core.lambdaDomain v0)
                        in (Sets.union domt (recurse (Core.lambdaBody v0)))
                      Core.TermLet v0 ->
                        let forBinding =
                                \b ->
                                  let newVars =
                                          Maybes.maybe vars (\ts -> Sets.union vars (Sets.fromList (Core.typeSchemeVariables ts))) (Core.bindingTypeScheme b)
                                  in (Sets.union (getAll newVars (Core.bindingTerm b)) (Maybes.maybe Sets.empty (\ts -> tryType newVars (Core.typeSchemeBody ts)) (Core.bindingTypeScheme b)))
                        in (Sets.union (allOf (Lists.map forBinding (Core.letBindings v0))) (recurse (Core.letBody v0)))
                      Core.TermTypeApplication v0 -> Sets.union (tryType vars (Core.typeApplicationTermType v0)) (recurse (Core.typeApplicationTermBody v0))
                      Core.TermTypeLambda v0 -> Sets.union (tryType vars (Core.TypeVariable (Core.typeLambdaParameter v0))) (recurse (Core.typeLambdaBody v0))
                      _ -> dflt
      in (getAll Sets.empty term0)
-- | Find the free variables (i.e. variables not bound by a lambda or let) in a term
freeVariablesInTerm :: Core.Term -> S.Set Core.Name
freeVariablesInTerm term =

      let dfltVars = \_ -> Lists.foldl (\s -> \t -> Sets.union s (freeVariablesInTerm t)) Sets.empty (Rewriting.subterms term)
      in case term of
        Core.TermLambda v0 -> Sets.delete (Core.lambdaParameter v0) (freeVariablesInTerm (Core.lambdaBody v0))
        Core.TermLet v0 -> Sets.difference (dfltVars ()) (Sets.fromList (Lists.map Core.bindingName (Core.letBindings v0)))
        Core.TermVariable v0 -> Sets.singleton v0
        _ -> dfltVars ()
-- | Find the free variables (i.e. variables not bound by a lambda or let) in a type
freeVariablesInType :: Core.Type -> S.Set Core.Name
freeVariablesInType typ =

      let dfltVars = Lists.foldl (\s -> \t -> Sets.union s (freeVariablesInType t)) Sets.empty (Rewriting.subtypes typ)
      in case typ of
        Core.TypeForall v0 -> Sets.delete (Core.forallTypeParameter v0) (freeVariablesInType (Core.forallTypeBody v0))
        Core.TypeVariable v0 -> Sets.singleton v0
        _ -> dfltVars
-- | Find the free variables in a type in deterministic left-to-right order
freeVariablesInTypeOrdered :: Core.Type -> [Core.Name]
freeVariablesInTypeOrdered typ =

      let collectVars =
              \boundVars -> \t -> case t of
                Core.TypeVariable v0 -> Logic.ifElse (Sets.member v0 boundVars) [] [
                  v0]
                Core.TypeForall v0 -> collectVars (Sets.insert (Core.forallTypeParameter v0) boundVars) (Core.forallTypeBody v0)
                _ -> Lists.concat (Lists.map (collectVars boundVars) (Rewriting.subtypes t))
      in (Lists.nub (collectVars Sets.empty typ))
-- | Find free variables in a type scheme
freeVariablesInTypeScheme :: Core.TypeScheme -> S.Set Core.Name
freeVariablesInTypeScheme ts =

      let vars = Core.typeSchemeVariables ts
          t = Core.typeSchemeBody ts
      in (Sets.difference (freeVariablesInType t) (Sets.fromList vars))
-- | Find free variables in a type scheme (simple version)
freeVariablesInTypeSchemeSimple :: Core.TypeScheme -> S.Set Core.Name
freeVariablesInTypeSchemeSimple ts =

      let vars = Core.typeSchemeVariables ts
          t = Core.typeSchemeBody ts
      in (Sets.difference (freeVariablesInTypeSimple t) (Sets.fromList vars))
-- | Same as freeVariablesInType, but ignores the binding action of lambda types
freeVariablesInTypeSimple :: Core.Type -> S.Set Core.Name
freeVariablesInTypeSimple typ =

      let helper =
              \types -> \typ2 -> case typ2 of
                Core.TypeVariable v0 -> Sets.insert v0 types
                _ -> types
      in (Rewriting.foldOverType Coders.TraversalOrderPre helper Sets.empty typ)
-- | Check whether a variable is free (not bound) in a term
isFreeVariableInTerm :: Core.Name -> Core.Term -> Bool
isFreeVariableInTerm v term = Logic.not (Sets.member v (freeVariablesInTerm term))
-- | Recursively replace the type variables of let bindings with the systematic type variables t0, t1, t2, ...
normalizeTypeVariablesInTerm :: Core.Term -> Core.Term
normalizeTypeVariablesInTerm term =

      let replaceName = \subst -> \v -> Maybes.fromMaybe v (Maps.lookup v subst)
          substType =
                  \subst -> \typ ->
                    let rewrite =
                            \recurse -> \typ2 -> case typ2 of
                              Core.TypeVariable v0 -> Core.TypeVariable (replaceName subst v0)
                              _ -> recurse typ2
                    in (Rewriting.rewriteType rewrite typ)
          rewriteWithSubst =
                  \state -> \term0 ->
                    let sb = Pairs.first state
                        next = Pairs.second state
                        subst = Pairs.first sb
                        boundVars = Pairs.second sb
                        rewrite =
                                \recurse -> \term2 -> case term2 of
                                  Core.TermLambda v0 ->
                                    let domain = Core.lambdaDomain v0
                                    in (Core.TermLambda (Core.Lambda {
                                      Core.lambdaParameter = (Core.lambdaParameter v0),
                                      Core.lambdaDomain = (Maybes.map (substType subst) domain),
                                      Core.lambdaBody = (rewriteWithSubst ((subst, boundVars), next) (Core.lambdaBody v0))}))
                                  Core.TermLet v0 ->
                                    let bindings0 = Core.letBindings v0
                                        body0 = Core.letBody v0
                                        step =
                                                \acc -> \bs -> Maybes.maybe (Lists.reverse acc) (\uc ->
                                                  let b = Pairs.first uc
                                                      tl = Pairs.second uc
                                                      noType =

                                                                let newVal = rewriteWithSubst ((subst, boundVars), next) (Core.bindingTerm b)
                                                                    b1 =
                                                                            Core.Binding {
                                                                              Core.bindingName = (Core.bindingName b),
                                                                              Core.bindingTerm = newVal,
                                                                              Core.bindingTypeScheme = Nothing}
                                                                in (step (Lists.cons b1 acc) tl)
                                                      withType =
                                                              \ts ->
                                                                let vars = Core.typeSchemeVariables ts
                                                                    typ = Core.typeSchemeBody ts
                                                                    k = Lists.length vars
                                                                    gen =
                                                                            \i -> \rem -> \acc2 ->
                                                                              let ti = Core.Name (Strings.cat2 "t" (Literals.showInt32 (Math.add next i)))
                                                                              in (Logic.ifElse (Equality.equal rem 0) (Lists.reverse acc2) (gen (Math.add i 1) (Math.sub rem 1) (Lists.cons ti acc2)))
                                                                    newVars = gen 0 k []
                                                                    newSubst = Maps.union (Maps.fromList (Lists.zip vars newVars)) subst
                                                                    newBound = Sets.union boundVars (Sets.fromList newVars)
                                                                    newVal = rewriteWithSubst ((newSubst, newBound), (Math.add next k)) (Core.bindingTerm b)
                                                                    renameConstraintKeys =
                                                                            \constraintMap -> Maps.fromList (Lists.map (\p ->
                                                                              let oldName = Pairs.first p
                                                                                  meta = Pairs.second p
                                                                                  newName = Maybes.fromMaybe oldName (Maps.lookup oldName newSubst)
                                                                              in (newName, meta)) (Maps.toList constraintMap))
                                                                    oldConstraints = Core.typeSchemeConstraints ts
                                                                    newConstraints = Maybes.map renameConstraintKeys oldConstraints
                                                                    b1 =
                                                                            Core.Binding {
                                                                              Core.bindingName = (Core.bindingName b),
                                                                              Core.bindingTerm = newVal,
                                                                              Core.bindingTypeScheme = (Just (Core.TypeScheme {
                                                                                Core.typeSchemeVariables = newVars,
                                                                                Core.typeSchemeBody = (substType newSubst typ),
                                                                                Core.typeSchemeConstraints = newConstraints}))}
                                                                in (step (Lists.cons b1 acc) tl)
                                                  in (Maybes.maybe noType (\ts -> withType ts) (Core.bindingTypeScheme b))) (Lists.uncons bs)
                                        bindings1 = step [] bindings0
                                    in (Core.TermLet (Core.Let {
                                      Core.letBindings = bindings1,
                                      Core.letBody = (rewriteWithSubst ((subst, boundVars), next) body0)}))
                                  Core.TermTypeApplication v0 -> Core.TermTypeApplication (Core.TypeApplicationTerm {
                                    Core.typeApplicationTermBody = (rewriteWithSubst ((subst, boundVars), next) (Core.typeApplicationTermBody v0)),
                                    Core.typeApplicationTermType = (substType subst (Core.typeApplicationTermType v0))})
                                  Core.TermTypeLambda v0 -> Core.TermTypeLambda (Core.TypeLambda {
                                    Core.typeLambdaParameter = (replaceName subst (Core.typeLambdaParameter v0)),
                                    Core.typeLambdaBody = (rewriteWithSubst ((subst, boundVars), next) (Core.typeLambdaBody v0))})
                                  _ -> recurse term2
                    in (Rewriting.rewriteTerm rewrite term0)
      in (rewriteWithSubst ((Maps.empty, Sets.empty), 0) term)
-- | Replace a free variable in a term
replaceFreeTermVariable :: Core.Name -> Core.Term -> Core.Term -> Core.Term
replaceFreeTermVariable vold tnew term =

      let rewrite =
              \recurse -> \t -> case t of
                Core.TermLambda v0 ->
                  let v = Core.lambdaParameter v0
                  in (Logic.ifElse (Equality.equal v vold) t (recurse t))
                Core.TermVariable v0 -> Logic.ifElse (Equality.equal v0 vold) tnew (Core.TermVariable v0)
                _ -> recurse t
      in (Rewriting.rewriteTerm rewrite term)
-- | Replace free occurrences of a name in a type
replaceFreeTypeVariable :: Core.Name -> Core.Type -> Core.Type -> Core.Type
replaceFreeTypeVariable v rep typ =

      let mapExpr =
              \recurse -> \t -> case t of
                Core.TypeForall v0 -> Logic.ifElse (Equality.equal v (Core.forallTypeParameter v0)) t (Core.TypeForall (Core.ForallType {
                  Core.forallTypeParameter = (Core.forallTypeParameter v0),
                  Core.forallTypeBody = (recurse (Core.forallTypeBody v0))}))
                Core.TypeVariable v0 -> Logic.ifElse (Equality.equal v v0) rep t
                _ -> recurse t
      in (Rewriting.rewriteType mapExpr typ)
-- | Substitute type variables in a type
substituteTypeVariables :: M.Map Core.Name Core.Name -> Core.Type -> Core.Type
substituteTypeVariables subst typ =

      let replace =
              \recurse -> \typ2 -> case typ2 of
                Core.TypeVariable v0 -> Core.TypeVariable (Maybes.fromMaybe v0 (Maps.lookup v0 subst))
                _ -> recurse typ2
      in (Rewriting.rewriteType replace typ)
-- | Substitute one variable for another in a term
substituteVariable :: Core.Name -> Core.Name -> Core.Term -> Core.Term
substituteVariable from to term =

      let replace =
              \recurse -> \term2 -> case term2 of
                Core.TermVariable v0 -> Core.TermVariable (Logic.ifElse (Equality.equal v0 from) to v0)
                Core.TermLambda v0 -> Logic.ifElse (Equality.equal (Core.lambdaParameter v0) from) term2 (recurse term2)
                _ -> recurse term2
      in (Rewriting.rewriteTerm replace term)
-- | Substitute multiple variables in a term
substituteVariables :: M.Map Core.Name Core.Name -> Core.Term -> Core.Term
substituteVariables subst term =

      let replace =
              \recurse -> \term2 -> case term2 of
                Core.TermVariable v0 -> Core.TermVariable (Maybes.fromMaybe v0 (Maps.lookup v0 subst))
                Core.TermLambda v0 -> Maybes.maybe (recurse term2) (\_ -> term2) (Maps.lookup (Core.lambdaParameter v0) subst)
                _ -> recurse term2
      in (Rewriting.rewriteTerm replace term)
-- | Rename all shadowed variables (both lambda parameters and let-bound variables that shadow lambda parameters) in a term.
unshadowVariables :: Core.Term -> Core.Term
unshadowVariables term0 =

      let freshName =
              \base -> \i -> \m ->
                let candidate = Core.Name (Strings.cat2 (Core.unName base) (Literals.showInt32 i))
                in (Logic.ifElse (Maps.member candidate m) (freshName base (Math.add i 1) m) candidate)
          f =
                  \recurse -> \m -> \term -> case term of
                    Core.TermLambda v0 ->
                      let v = Core.lambdaParameter v0
                          domain = Core.lambdaDomain v0
                          body = Core.lambdaBody v0
                      in (Logic.ifElse (Maps.member v m) (
                        let v2 = freshName v 2 m
                            m2 = Maps.insert v v2 (Maps.insert v2 v2 m)
                        in (Core.TermLambda (Core.Lambda {
                          Core.lambdaParameter = v2,
                          Core.lambdaDomain = domain,
                          Core.lambdaBody = (f recurse m2 body)}))) (Core.TermLambda (Core.Lambda {
                        Core.lambdaParameter = v,
                        Core.lambdaDomain = domain,
                        Core.lambdaBody = (f recurse (Maps.insert v v m) body)})))
                    Core.TermLet v0 ->
                      let m2 =
                              Lists.foldl (\acc -> \b ->
                                let bname = Core.bindingName b
                                in (Logic.ifElse (Maps.member bname acc) acc (Maps.insert bname bname acc))) m (Core.letBindings v0)
                      in (recurse m2 term)
                    Core.TermVariable v0 -> Core.TermVariable (Maybes.maybe v0 (\renamed -> renamed) (Maps.lookup v0 m))
                    _ -> recurse m term
      in (Rewriting.rewriteTermWithContext f Maps.empty term0)
