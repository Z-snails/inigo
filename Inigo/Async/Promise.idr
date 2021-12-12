module Inigo.Async.Promise

export
data Promise : Type -> Type -> Type where
  MkPromise : ((a -> IO ()) -> (err -> IO ()) -> IO ()) -> Promise err a

export
Functor (Promise err) where
  map f (MkPromise cmd) = MkPromise (\succ => \err => cmd (\x => succ (f x)) err)

mutual
  export
  Applicative (Promise err) where
    pure x = MkPromise (\succ => \err => succ x)
    x <*> y = x >>= (\f => f <$> y)

  export
  Monad (Promise err) where
    (MkPromise cmd) >>= f =
        MkPromise (\succ, err =>
            cmd
                (\x =>
                    let (MkPromise cmd_) = (f x)
                     in cmd_ succ err)
                err)

export
HasIO (Promise err) where
    liftIO act = MkPromise (\ok, err => act >>= ok)

export
resolve : Promise err a -> (a -> IO ()) -> (err -> IO ()) -> IO ()
resolve (MkPromise cmd) ok err = cmd ok err

export
run : Show err => Promise err a -> IO ()
run p =
  resolve p (\_ => pure ()) (\err => putStrLn ("Error: " ++ show err))

-- I can fold these, but that's a bit of an issue since
-- they will end up running sequentially, which is really
-- not the intent here, but for now...
export
all : List (Promise err a) -> Promise err (List a)
all = sequence

export
fail : err -> Promise err a
fail x = MkPromise (\ok, err => err x)

export
parallel : Promise err a -> Promise err a -> Promise err a
parallel (MkPromise s1) (MkPromise s2) = MkPromise $ \err => \cb => do
  s1 err cb
  s2 err cb

public export
promise : Type -> Type -> Type
promise err a = (a -> IO ()) -> (err -> IO ()) -> PrimIO ()

export
promisify : promise err a -> Promise err a
promisify prim =
  MkPromise (\ok, err => primIO $ prim ok err)

export
boolToInt : Bool -> Int
boolToInt False = 0
boolToInt True = 1

export
liftIOEither : IO (Either err x) -> Promise err x
liftIOEither act = do
    Right x <- liftIO act
        | Left err => fail err
    pure x

export
mapErr : (e1 -> e2) -> Promise e1 a -> Promise e2 a
mapErr f (MkPromise act) = MkPromise $ \ok, err => act ok (err . f)
