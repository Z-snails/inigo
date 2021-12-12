module Inigo.Async.Base

import Data.Maybe
import Inigo.Async.Promise
import Inigo.Async.Util
import Inigo.Paths
import System

{-
%foreign (promisifyPrim "()=>new Promise((resolve,reject)=>{})")
never__prim : promise ()


%foreign (promisifyPrim "(_,err)=>new Promise((resolve,reject)=>reject(err))")
reject__prim : String -> promise a


%foreign (promisifyResolve "null" "(text)=>console.log(text)")
log__prim : String -> promise ()

%foreign (promisifyPrim (toArray "(cmd,args,workDir,detached,verbose)=>new Promise((resolve,reject)=>{let opts={detached:detached===1, stdio: ['ignore', process.stdout, process.stderr],cwd:workDir};require('child_process').spawn(cmd, toArray(args), opts).on('close', (code) => resolve(code))})"))
system__prim : String -> List String -> String -> Int -> Int -> promise Int

%foreign (promisifyPrim (toArray "(cmd,args,detached,verbose)=>new Promise((resolve,reject)=>{let opts={detached:detached===1, stdio: 'inherit'};require('child_process').spawn(cmd, toArray(args), opts).on('close', (code) => resolve(code))})"))
systemWithStdIO__prim : String -> List String -> Int -> Int -> promise Int
-}

export
reject : err -> Promise err a
reject = fail

export
log : String -> Promise a ()
log text = liftIO $ putStrLn text

export
debugLog : String -> Promise a ()
debugLog text = when DEBUG $ log text

export
system : String -> List String -> Maybe String -> Bool -> Bool -> Promise a Int
system cmd args cwd detached verbose = liftIO $ system (cmd :: args)

export
systemWithStdIO : String -> List String -> Bool -> Bool -> Promise a Int
systemWithStdIO cmd args detached verbose = system cmd args Nothing detached verbose -- TODO: fix this

-- This is here and not in `Promise.idr` since it relies on `reject`
export
liftEither : Either err a -> Promise err a
liftEither x = do
    let Right res = x
        | Left err => reject err
    pure res
