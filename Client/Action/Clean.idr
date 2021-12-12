module Client.Action.Clean

import Client.Action.Build
import Data.List
import Data.String
import Inigo.Async.Base
import Inigo.Async.Promise
import Inigo.Async.FS
import Inigo.Package.Package
import Inigo.Paths
import System.File

cleanIPkg : String -> Promise a ()
cleanIPkg ipkg =
  ignore $ system "idris2" ["--clean", ipkg] Nothing False True

export
clean : Bool -> Promise String ()
clean deps = do
    ignore writeIPkgFile
    ignore $ cleanIPkg inigoIPkgPath
    when (deps && !(exists inigoDepDir)) $ do
        files <- mapErr show $ getFilesRec inigoDepDir
        let ipkgs = filter (isSuffixOf ".ipkg") files
        ignore $ traverse cleanIPkg ipkgs
