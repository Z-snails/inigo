module Client.Action.Init

import Client.Skeleton.Skeleton
import Data.List
import Fmt
import Inigo.Async.Base
import Inigo.Async.FS
import Inigo.Async.Promise
import Inigo.Template
import System.Path
import System.File

export
init :
    String -> -- template
    -- String -> -- package namespace
    String -> -- package name
    Promise String ()
init tmplFile {- packageNS -} packageName = do
    tmplInp <- mapErr show $ readFile tmplFile
    tmpl <- liftEither $ runTemplate {- packageNS -} packageName tmplFile tmplInp
    ignore $ all $ map writeTmplFile tmpl
    log (fmt "Successfully built %s" tmplFile)
  where
    ensureParent : String -> Promise String ()
    ensureParent path = case parent path of
        Just parentPath => unless (parentPath == "") $ mapErr show $ mkdir True parentPath
        Nothing => pure ()

    writeTmplFile : (String, String) -> Promise String ()
    writeTmplFile (path, contents) = do
        ensureParent path
        mapErr show $ writeFile path contents
