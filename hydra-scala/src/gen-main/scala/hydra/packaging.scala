package hydra.packaging

import hydra.core.*

import hydra.module.*

import hydra.module

case class Package(name: hydra.packaging.PackageName, modules: Seq[hydra.module.Module], dependencies: Seq[hydra.packaging.PackageName],
   description: Option[scala.Predef.String])

type PackageName = scala.Predef.String
