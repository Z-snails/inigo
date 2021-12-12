module Inigo.Async.Git

import Inigo.Async.Base
import Inigo.Async.Promise
import Inigo.Async.FS
import System.Path
import System.File

||| Download a git repository optionally specifying the commit, then remove the .git folder
||| Returns `True` iff the file already exists.
export
downloadTo : (url : String) -> (commit : Maybe String) -> (dest : String) -> Promise FileError Bool
downloadTo url commit dest = do
    case !(exists dest) of
        False => do
            ignore $ system "git" ["clone", "-q", "--progress", "--recurse-submodules", url, dest] Nothing False False
            maybe
                (pure ())
                (\com => ignore $ system "git" ["checkout", "-q", com] (Just dest) False False)
                commit
            rmdir True (dest </> ".git")
            pure False
        True => pure True
