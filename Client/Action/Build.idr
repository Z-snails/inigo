module Client.Action.Build

import Inigo.Async.Base
import Inigo.Async.FS
import Inigo.Async.Promise
import Inigo.Package.CodeGen
import Inigo.Package.Package
import Inigo.Async.Package
import Inigo.Paths
import System.File

export
writeIPkgFile : Promise String Package
writeIPkgFile = do
    pkg <- currPackage
    -- TODO: Only build if not exists ?
    mapErr show $ writeFile inigoIPkgPath (generateIPkg True Nothing pkg)
    pure pkg

export
runBuild : CodeGen -> Promise a ()
runBuild codeGen =
    ignore $ system "idris2" ["--build", inigoIPkgPath, "--cg", toString codeGen] Nothing False True

export
build : CodeGen -> Promise String ()
build codeGen = do
    pkg <- writeIPkgFile
    runBuild codeGen
