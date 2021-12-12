module Client.Action.Test

import Client.Action.Build as Build
import Fmt
import Inigo.Async.Base
import Inigo.Async.Promise
import Inigo.Package.CodeGen as CodeGen
import Inigo.Package.Package

-- TODO: Consider moving this out from Inigo into toml config
export
test : CodeGen -> Promise String ()
test codeGen = do
    pkg <- writeIPkgFile
    log (fmt "Running tests...")
    ignore $ system "idris2" ["--find-ipkg", "Test/Suite.idr", "--cg", CodeGen.toString codeGen, "-x", "suite"] Nothing True True
