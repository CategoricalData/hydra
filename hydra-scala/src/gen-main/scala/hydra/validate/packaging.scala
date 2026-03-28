package hydra.validate.packaging

import hydra.core.*

import hydra.error.packaging.*

import hydra.module.*

import hydra.packaging.*

import hydra.lib.equality

import hydra.lib.lists

import hydra.lib.logic

import hydra.lib.maps

import hydra.lib.maybes

import hydra.lib.pairs

import hydra.lib.sets

import hydra.lib.strings

def checkConflictingModuleNamespaces(pkg: hydra.packaging.Package): Option[hydra.error.packaging.InvalidPackageError] =
  {
  lazy val result: Tuple2[Map[scala.Predef.String, hydra.module.Namespace], Option[hydra.error.packaging.InvalidPackageError]] = hydra.lib.lists.foldl[Tuple2[Map[scala.Predef.String,
     hydra.module.Namespace], Option[hydra.error.packaging.InvalidPackageError]], hydra.module.Module]((acc: Tuple2[Map[scala.Predef.String,
     hydra.module.Namespace], Option[hydra.error.packaging.InvalidPackageError]]) =>
    (mod: hydra.module.Module) =>
    {
    lazy val seen: Map[scala.Predef.String, hydra.module.Namespace] = hydra.lib.pairs.first[Map[scala.Predef.String,
       hydra.module.Namespace], Option[hydra.error.packaging.InvalidPackageError]](acc)
    {
      lazy val err: Option[hydra.error.packaging.InvalidPackageError] = hydra.lib.pairs.second[Map[scala.Predef.String,
         hydra.module.Namespace], Option[hydra.error.packaging.InvalidPackageError]](acc)
      hydra.lib.maybes.cases[hydra.error.packaging.InvalidPackageError, Tuple2[Map[scala.Predef.String,
         hydra.module.Namespace], Option[hydra.error.packaging.InvalidPackageError]]](err)({
        lazy val ns: hydra.module.Namespace = (mod.namespace)
        {
          lazy val key: scala.Predef.String = hydra.lib.strings.toLower(ns)
          {
            lazy val existing: Option[hydra.module.Namespace] = hydra.lib.maps.lookup[scala.Predef.String, hydra.module.Namespace](key)(seen)
            hydra.lib.maybes.cases[hydra.module.Namespace, Tuple2[Map[scala.Predef.String, hydra.module.Namespace],
               Option[hydra.error.packaging.InvalidPackageError]]](existing)(Tuple2(hydra.lib.maps.insert[scala.Predef.String,
               hydra.module.Namespace](key)(ns)(seen), None))((first: hydra.module.Namespace) =>
              Tuple2(seen, Some(hydra.error.packaging.InvalidPackageError.conflictingModuleNamespace(hydra.error.packaging.ConflictingModuleNamespaceError(first,
                 ns)))))
          }
        }
      })((_x: hydra.error.packaging.InvalidPackageError) => acc)
    }
  })(Tuple2(hydra.lib.maps.empty[scala.Predef.String, hydra.module.Namespace], None))(pkg.modules)
  hydra.lib.pairs.second[Map[scala.Predef.String, hydra.module.Namespace], Option[hydra.error.packaging.InvalidPackageError]](result)
}

def checkConflictingVariantNames(mod: hydra.module.Module): Option[hydra.error.packaging.InvalidModuleError] =
  {
  lazy val ns: hydra.module.Namespace = (mod.namespace)
  lazy val defs: Seq[hydra.module.Definition] = (mod.definitions)
  lazy val defNames: scala.collection.immutable.Set[scala.Predef.String] = hydra.lib.lists.foldl[scala.collection.immutable.Set[scala.Predef.String],
     hydra.module.Definition]((acc: scala.collection.immutable.Set[scala.Predef.String]) =>
    (`def`: hydra.module.Definition) =>
    hydra.lib.sets.insert[scala.Predef.String](hydra.names.localNameOf(hydra.validate.packaging.definitionName(`def`)))(acc))(hydra.lib.sets.empty[scala.Predef.String])(defs)
  hydra.lib.lists.foldl[Option[hydra.error.packaging.InvalidModuleError], hydra.module.Definition]((acc: Option[hydra.error.packaging.InvalidModuleError]) =>
    (`def`: hydra.module.Definition) =>
    hydra.lib.maybes.cases[hydra.error.packaging.InvalidModuleError, Option[hydra.error.packaging.InvalidModuleError]](acc)(`def` match
    case hydra.module.Definition.`type`(v_Definition_type_td) => {
      lazy val typeName: hydra.core.Name = (v_Definition_type_td.name)
      {
        lazy val localTypeName: scala.Predef.String = hydra.names.localNameOf(typeName)
        {
          lazy val typ: hydra.core.Type = (v_Definition_type_td.`type`)
          typ match
            case hydra.core.Type.union(v_Type_union_fields) => hydra.lib.lists.foldl[Option[hydra.error.packaging.InvalidModuleError],
               hydra.core.FieldType]((innerAcc: Option[hydra.error.packaging.InvalidModuleError]) =>
              (field: hydra.core.FieldType) =>
              hydra.lib.maybes.cases[hydra.error.packaging.InvalidModuleError, Option[hydra.error.packaging.InvalidModuleError]](innerAcc)({
              lazy val fieldName: hydra.core.Name = (field.name)
              {
                lazy val localFieldName: scala.Predef.String = hydra.names.localNameOf(fieldName)
                {
                  lazy val constructorName: scala.Predef.String = hydra.lib.strings.cat2(hydra.formatting.capitalize(localTypeName))(hydra.formatting.capitalize(localFieldName))
                  hydra.lib.logic.ifElse[Option[hydra.error.packaging.InvalidModuleError]](hydra.lib.sets.member[scala.Predef.String](constructorName)(defNames))(Some(hydra.error.packaging.InvalidModuleError.conflictingVariantName(hydra.error.packaging.ConflictingVariantNameError(ns,
                     typeName, fieldName, constructorName))))(None)
                }
              }
            })((_x: hydra.error.packaging.InvalidModuleError) => innerAcc))(None)(v_Type_union_fields)
            case _ => None
        }
      }
    }
    case _ => None)((_x: hydra.error.packaging.InvalidModuleError) => acc))(None)(defs)
}

def checkDefinitionNamespaces(mod: hydra.module.Module): Option[hydra.error.packaging.InvalidModuleError] =
  {
  lazy val ns: hydra.module.Namespace = (mod.namespace)
  lazy val prefix: scala.Predef.String = hydra.lib.strings.cat2(ns)(".")
  lazy val prefixLen: Int = hydra.lib.strings.length(prefix)
  hydra.lib.lists.foldl[Option[hydra.error.packaging.InvalidModuleError], hydra.module.Definition]((acc: Option[hydra.error.packaging.InvalidModuleError]) =>
    (`def`: hydra.module.Definition) =>
    hydra.lib.maybes.cases[hydra.error.packaging.InvalidModuleError, Option[hydra.error.packaging.InvalidModuleError]](acc)({
    lazy val name: hydra.core.Name = hydra.validate.packaging.definitionName(`def`)
    {
      lazy val nameStr: scala.Predef.String = name
      {
        lazy val namePrefix: Seq[Int] = hydra.lib.lists.take[Int](prefixLen)(hydra.lib.strings.toList(nameStr))
        hydra.lib.logic.ifElse[Option[hydra.error.packaging.InvalidModuleError]](hydra.lib.equality.equal[scala.Predef.String](hydra.lib.strings.fromList(namePrefix))(prefix))(None)(Some(hydra.error.packaging.InvalidModuleError.definitionNotInModuleNamespace(hydra.error.packaging.DefinitionNotInModuleNamespaceError(ns,
           name))))
      }
    }
  })((_x: hydra.error.packaging.InvalidModuleError) => acc))(None)(mod.definitions)
}

def checkDuplicateDefinitionNames(mod: hydra.module.Module): Option[hydra.error.packaging.InvalidModuleError] =
  {
  lazy val ns: hydra.module.Namespace = (mod.namespace)
  lazy val result: Tuple2[scala.collection.immutable.Set[hydra.core.Name], Option[hydra.error.packaging.InvalidModuleError]] = hydra.lib.lists.foldl[Tuple2[scala.collection.immutable.Set[hydra.core.Name],
     Option[hydra.error.packaging.InvalidModuleError]], hydra.module.Definition]((acc: Tuple2[scala.collection.immutable.Set[hydra.core.Name],
     Option[hydra.error.packaging.InvalidModuleError]]) =>
    (`def`: hydra.module.Definition) =>
    {
    lazy val seen: scala.collection.immutable.Set[hydra.core.Name] = hydra.lib.pairs.first[scala.collection.immutable.Set[hydra.core.Name],
       Option[hydra.error.packaging.InvalidModuleError]](acc)
    {
      lazy val err: Option[hydra.error.packaging.InvalidModuleError] = hydra.lib.pairs.second[scala.collection.immutable.Set[hydra.core.Name],
         Option[hydra.error.packaging.InvalidModuleError]](acc)
      hydra.lib.maybes.cases[hydra.error.packaging.InvalidModuleError, Tuple2[scala.collection.immutable.Set[hydra.core.Name],
         Option[hydra.error.packaging.InvalidModuleError]]](err)({
        lazy val name: hydra.core.Name = hydra.validate.packaging.definitionName(`def`)
        hydra.lib.logic.ifElse[Tuple2[scala.collection.immutable.Set[hydra.core.Name], Option[hydra.error.packaging.InvalidModuleError]]](hydra.lib.sets.member[hydra.core.Name](name)(seen))(Tuple2(seen,
           Some(hydra.error.packaging.InvalidModuleError.duplicateDefinitionName(hydra.error.packaging.DuplicateDefinitionNameError(ns,
           name)))))(Tuple2(hydra.lib.sets.insert[hydra.core.Name](name)(seen), None))
      })((_x: hydra.error.packaging.InvalidModuleError) => acc)
    }
  })(Tuple2(hydra.lib.sets.empty[hydra.core.Name], None))(mod.definitions)
  hydra.lib.pairs.second[scala.collection.immutable.Set[hydra.core.Name], Option[hydra.error.packaging.InvalidModuleError]](result)
}

def checkDuplicateModuleNamespaces(pkg: hydra.packaging.Package): Option[hydra.error.packaging.InvalidPackageError] =
  {
  lazy val result: Tuple2[scala.collection.immutable.Set[hydra.module.Namespace], Option[hydra.error.packaging.InvalidPackageError]] = hydra.lib.lists.foldl[Tuple2[scala.collection.immutable.Set[hydra.module.Namespace],
     Option[hydra.error.packaging.InvalidPackageError]], hydra.module.Module]((acc: Tuple2[scala.collection.immutable.Set[hydra.module.Namespace],
     Option[hydra.error.packaging.InvalidPackageError]]) =>
    (mod: hydra.module.Module) =>
    {
    lazy val seen: scala.collection.immutable.Set[hydra.module.Namespace] = hydra.lib.pairs.first[scala.collection.immutable.Set[hydra.module.Namespace],
       Option[hydra.error.packaging.InvalidPackageError]](acc)
    {
      lazy val err: Option[hydra.error.packaging.InvalidPackageError] = hydra.lib.pairs.second[scala.collection.immutable.Set[hydra.module.Namespace],
         Option[hydra.error.packaging.InvalidPackageError]](acc)
      hydra.lib.maybes.cases[hydra.error.packaging.InvalidPackageError, Tuple2[scala.collection.immutable.Set[hydra.module.Namespace],
         Option[hydra.error.packaging.InvalidPackageError]]](err)({
        lazy val ns: hydra.module.Namespace = (mod.namespace)
        hydra.lib.logic.ifElse[Tuple2[scala.collection.immutable.Set[hydra.module.Namespace], Option[hydra.error.packaging.InvalidPackageError]]](hydra.lib.sets.member[hydra.module.Namespace](ns)(seen))(Tuple2(seen,
           Some(hydra.error.packaging.InvalidPackageError.duplicateModuleNamespace(hydra.error.packaging.DuplicateModuleNamespaceError(ns)))))(Tuple2(hydra.lib.sets.insert[hydra.module.Namespace](ns)(seen),
           None))
      })((_x: hydra.error.packaging.InvalidPackageError) => acc)
    }
  })(Tuple2(hydra.lib.sets.empty[hydra.module.Namespace], None))(pkg.modules)
  hydra.lib.pairs.second[scala.collection.immutable.Set[hydra.module.Namespace], Option[hydra.error.packaging.InvalidPackageError]](result)
}

def definitionName(`def`: hydra.module.Definition): hydra.core.Name =
  `def` match
  case hydra.module.Definition.term(v_Definition_term_td) => (v_Definition_term_td.name)
  case hydra.module.Definition.`type`(v_Definition_type_td) => (v_Definition_type_td.name)

def module(mod: hydra.module.Module): Option[hydra.error.packaging.InvalidModuleError] =
  {
  lazy val r1: Option[hydra.error.packaging.InvalidModuleError] = hydra.validate.packaging.checkDefinitionNamespaces(mod)
  hydra.lib.maybes.cases[hydra.error.packaging.InvalidModuleError, Option[hydra.error.packaging.InvalidModuleError]](r1)({
    lazy val r2: Option[hydra.error.packaging.InvalidModuleError] = hydra.validate.packaging.checkDuplicateDefinitionNames(mod)
    hydra.lib.maybes.cases[hydra.error.packaging.InvalidModuleError, Option[hydra.error.packaging.InvalidModuleError]](r2)(hydra.validate.packaging.checkConflictingVariantNames(mod))((_x: hydra.error.packaging.InvalidModuleError) => r2)
  })((_x: hydra.error.packaging.InvalidModuleError) => r1)
}

def `package`(pkg: hydra.packaging.Package): Option[hydra.error.packaging.InvalidPackageError] =
  {
  lazy val r1: Option[hydra.error.packaging.InvalidPackageError] = hydra.validate.packaging.checkDuplicateModuleNamespaces(pkg)
  hydra.lib.maybes.cases[hydra.error.packaging.InvalidPackageError, Option[hydra.error.packaging.InvalidPackageError]](r1)({
    lazy val r2: Option[hydra.error.packaging.InvalidPackageError] = hydra.validate.packaging.checkConflictingModuleNamespaces(pkg)
    hydra.lib.maybes.cases[hydra.error.packaging.InvalidPackageError, Option[hydra.error.packaging.InvalidPackageError]](r2)(hydra.lib.lists.foldl[Option[hydra.error.packaging.InvalidPackageError],
       hydra.module.Module]((acc: Option[hydra.error.packaging.InvalidPackageError]) =>
      (mod: hydra.module.Module) =>
      hydra.lib.maybes.cases[hydra.error.packaging.InvalidPackageError, Option[hydra.error.packaging.InvalidPackageError]](acc)(hydra.lib.maybes.map[hydra.error.packaging.InvalidModuleError,
         hydra.error.packaging.InvalidPackageError]((err: hydra.error.packaging.InvalidModuleError) => hydra.error.packaging.InvalidPackageError.invalidModule(err))(hydra.validate.packaging.module(mod)))((_x: hydra.error.packaging.InvalidPackageError) => acc))(None)(pkg.modules))((_x: hydra.error.packaging.InvalidPackageError) => r2)
  })((_x: hydra.error.packaging.InvalidPackageError) => r1)
}
