module Inigo.Async.FS

import Inigo.Async.Promise
import Inigo.Async.Util
import Data.Buffer
import Extra.Buffer
import System.Path
import System.File
import System.Directory

{-
%foreign (promisifyPrim "(path)=>require('fs').promises.readFile(path,'utf8')")
fs_readFile__prim : String -> promise String

%foreign (promisifyPrim "(path)=>require('fs').promises.readFile(path)")
fs_readFileBuf__prim : String -> promise Buffer

%foreign (promisifyPrim "(path,contents)=>require('fs').promises.writeFile(path,contents)")
fs_writeFile__prim : String -> String -> promise ()

%foreign (promisifyPrim "(path,contents)=>require('fs').promises.writeFile(path,contents)")
fs_writeFileBuf__prim : String -> Buffer -> promise ()

%foreign (promisifyPrim "(path,r)=>require('fs').promises.mkdir(path,{recursive: r === 1})")
fs_mkdir__prim : String -> Int -> promise ()

%foreign (promisifyPrim "(path,r)=>require('fs').promises.rmdir(path,{recursive: r === 1})")
fs_rmdir__prim : String -> Int -> promise ()

%foreign (promisifyPrim "(path)=>require('fs').promises.readdir(path).then(__prim_js2idris_array)")
fs_getFiles__prim : String -> promise (List String)

%foreign (promisifyPrim "(path)=>require('fs').promises.stat(path).then((s)=>s.isDirectory() ? 1 : 0)")
fs_isDir__prim : String -> promise Int

%foreign (promisifyPrim "(path)=>require('fs').promises.access(path).then(()=>1).catch(()=>0)")
fs_exists__prim : String -> promise Int
-}

export
readFile : String -> Promise FileError String
readFile path = liftIOEither $ readFile path

export
writeFile : (oath : String) -> (content : String) -> Promise FileError ()
writeFile path contents = liftIOEither $ writeFile path contents

export
readFileBuf : String -> Promise FileError Buffer
readFileBuf path = liftIOEither $ createBufferFromFile path

export
writeFileBuf : String -> Buffer -> Promise FileError ()
writeFileBuf path contents = do
    len <- rawSize contents
    liftIOEither $ writeBufferToFile path contents len

export
mkdir : Bool -> String -> Promise FileError ()
mkdir recursive path = liftIOEither $ createDir path

export
rmdir : Bool -> String -> Promise err ()
rmdir recursive path = liftIO $ removeDir path

export
getFiles : String -> Promise FileError (List String)
getFiles path = liftIOEither $ listDir path

||| Check if a path is a directory, fails if the directory doesn't exist
||| Use @ exists to check if it exists first.
export
isDir : String -> Promise FileError Bool
isDir path = do
    True <- exists path
        | False => fail FileNotFound
    Right dir <- openDir path
        | Left (GenericFileError 20) => pure False
        | Left err => fail err
    closeDir dir
    pure True

export
getFilesRec : String -> Promise FileError (List String)
getFilesRec path = do
    dir <- isDir path
    if dir
        then do
            entries <- getFiles path
            let fullEntries = map (path </>) entries
            allFiles <- traverse getFilesRec fullEntries
            pure $ concat allFiles
        else pure [path]
