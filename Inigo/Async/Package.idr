module Inigo.Async.Package

import Data.List
import Data.List1
import Data.String
import Extra.String
import Inigo.Async.Base
import Inigo.Async.CloudFlare.KV
import Inigo.Async.FS
import Inigo.Async.Promise
import Inigo.Package.Package
import Inigo.Package.PackageIndex
import Inigo.Paths

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

export
readPackage : String -> Promise Package
readPackage packageFile =
  do
    contents <- fs_readFile packageFile
    let Right package = parsePackage contents
      | Left err => reject ("Error reading package: " ++ err)
    pure package

export
currPackage : Promise Package
currPackage =
  do
    contents <- fs_readFile inigoTomlPath
    let Right package = parsePackage contents
      | Left err => reject ("Error reading package: " ++ err)
    pure package

satisfyModules :
    List String ->
    ModFilter ->
    List String
satisfyModules raw None = raw
satisfyModules raw mods = filter (`satisfyModule` mods) raw

dropLast : List a -> List a
dropLast [] = []
dropLast [_] = []
dropLast (x :: xs) = x :: dropLast xs

export
getModulesFor : String -> (pkg : Package) -> Promise (ModulesFor pkg)
getModulesFor dir pkg = do
    files <- fs_getFilesR $ dir </> pkg.sourcedir
    let rawMods = filter (".idr" `isSuffixOf`) files
    let mods = parseMod <$> files
    pure $ MkModules $ satisfyModules mods pkg.modFilter
  where
    removeIdr : String -> String
    removeIdr inp = if ".idr" `isSuffixOf` inp
        then
            let len = cast $ length inp
             in assert_total $ strSubstr 0 (len - 4) inp
        else inp
    parseParts : List String -> List String
    parseParts path =
        let srcLen = length $ forget $ split (== '/') dir ++ split (== '/') pkg.sourcedir
         in removeIdr <$> drop srcLen path
    parseMod : String -> String
    parseMod = join "." . parseParts . forget . split (\c => c == '/')
