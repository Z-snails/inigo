module Client.Action.Repl

import Inigo.Async.Base
import Inigo.Async.FS
import Inigo.Async.Promise
import Inigo.Package.CodeGen
import Inigo.Package.Package
import Inigo.Async.Package
import Inigo.Paths
import System.File

writeIPkgNoExe : Promise String ()
writeIPkgNoExe = do
    pkg <- currPackage
    mapErr show $ writeFile inigoIPkgPath (generateIPkg False Nothing pkg)

export
repl : Promise String ()
repl = do
    writeIPkgNoExe
    ignore $ systemWithStdIO "idris2" ["--repl", inigoIPkgPath] True True
