module Client.Action.CacheDeps

import Data.List
import Inigo.Async.Base
import Inigo.Async.FS
import Inigo.Async.Promise
import Inigo.Package.Package
import Inigo.Package.JSON
import Inigo.Paths
import Language.JSON
import SemVar
import System.File

export
writeDepCache : List (String, Package) -> Promise FileError ()
writeDepCache pkgWithSrcs = do
    mkdir True inigoDepDir
    writeFile inigoDepPkgCache json
  where
    encodeWithSrc : (String, Package) -> JSON
    encodeWithSrc (src, pkg) = JObject
        [ ("src", JString src)
        , ("package", encodePackage pkg)
        ]
    json : String
    json = show $ JArray $ encodeWithSrc <$> pkgWithSrcs

export
readDepCache : Promise String (List (String, Package))
readDepCache = do
    inp <- mapErr show $ readFile inigoDepPkgCache
    let Just (JArray json) = parse inp
        | _ => fail "Corrupt dependency cache (\{inigoDepPkgCache}) (not an array)"
    let Just pkgWithSrc = the (Maybe (List (String, Package))) $ traverse decodeWithSrc json
        | _ => fail "Corrupt dependency cache (\{inigoDepPkgCache}) (couldn't parse packages)"
    pure pkgWithSrc
  where
    decodeWithSrc : JSON -> Maybe (String, Package)
    decodeWithSrc (JObject kvs) = do
        JString src <- lookup "src" kvs
            | _ => Nothing
        pkg <- lookup "package" kvs >>= parsePackage
        pure (src, pkg)
    decodeWithSrc _ = Nothing
