module Inigo.Async.Package

import Inigo.Paths
import Inigo.Async.Base
-- import Inigo.Async.CloudFlare.KV
import Inigo.Async.FS
import Inigo.Async.Promise
import Inigo.Package.Package
import Inigo.Package.PackageDhall
import Inigo.Package.PackageIndex
import System.File

{-
||| Gets a package from the "packages" KV
export
getPackage : String -> Promise (Either String Package)
getPackage package =
  map parsePackage (read "packages" package)

||| Returns an index with all packages
export
index : Promise (Either String PackageIndex)
index =
  map parsePackageIndex (read "packages" "index")
-}

public export
data InigoPackagePath
  = TomlPath String
  | DhallPath String

export
Show InigoPackagePath where
  show (TomlPath x) = x
  show (DhallPath x) = x

export
packageFilePath : String -> Promise String InigoPackagePath
packageFilePath src =
  let tomlPath = src </> inigoTomlPath
      dhallPath = src </> inigoDhallPath
  in do
    tp <- exists tomlPath
    dp <- exists dhallPath
    case (tp, dp) of
         (True, False) => pure $ TomlPath tomlPath
         (False, True) => pure $ DhallPath dhallPath
         (True, True) => fail "Conflict: both Inigo.toml and Inigo.dhall found"
         (False, False) => fail "Inigo.toml or Inigo.dhall file not found"

parseFile : InigoPackagePath -> Promise String Package
parseFile (TomlPath path) = do
    contents <- mapErr show $ readFile path
    let Right package = parsePackage contents
        | Left err => fail $ "Error reading toml package: " ++ err
    pure package
parseFile (DhallPath path) =
    -- can pass the file path to dhall so it can handle relative imports
    mapErr ("Error reading dhall package: " ++) $ parsePackageDhall path

export
readPackage : String -> Promise String Package
readPackage dir = parseFile !(packageFilePath dir)

export
currPackage : Promise String Package
currPackage = readPackage "." -- current dir
