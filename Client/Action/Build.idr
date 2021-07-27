module Client.Action.Build

import Data.List
import Data.String
import Inigo.Async.Base
import Inigo.Async.FS
import Inigo.Async.Promise
import Inigo.Package.CodeGen
import Inigo.Package.Package
import Inigo.Async.Package
import Inigo.Paths

export
writeIPkgFile : Promise Package
writeIPkgFile =
  do
    pkg <- currPackage
    mods <- getModulesFor rootDir pkg
    -- TODO: Only build if not exists ?
    fs_writeFile inigoIPkgPath $ generateIPkg Nothing pkg mods
    pure pkg

export
runBuild : CodeGen -> Package -> Promise ()
runBuild codeGen pkg =
  do
    ignore $ system "idris2" ["--build", inigoIPkgPath, "--cg", toString codeGen] Nothing False True

export
build : CodeGen -> Promise ()
build codeGen =
  do
    pkg <- writeIPkgFile
    runBuild codeGen pkg
