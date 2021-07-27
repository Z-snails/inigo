module Inigo.Package.Modules

import Inigo.Package.ParseHelpers
import Toml

public export
data ModFilter : Type where
    None : ModFilter
    Include : List String -> ModFilter
    Exclude : List String -> ModFilter

export
Eq ModFilter where
    None == None = True
    Include ms1 == Include ms2 = ms1 == ms2
    Exclude ms1 == Exclude ms2 = ms1 == ms2
    _ == _ = False

export
Show ModFilter where
    show None = "none"
    show (Include ms) = "(include " ++ show ms ++ ")"
    show (Exclude ms) = "(exclude " ++ show ms ++ ")"

export
parseModFilter : Toml -> Either String ModFilter
parseModFilter toml =
    case get ["include"] toml of
        Just inc => Include <$> getStrings ["include"] inc
        Nothing => case get ["exclude"] toml of
            Just exc => Exclude <$> getStrings ["exclude"] exc
            Nothing => Right None

export
toToml : ModFilter -> Maybe (List String, Value)
toToml None = Nothing
toToml (Include ms) = Just (["include"], Lst $ Str <$> ms)
toToml (Exclude ms) = Just (["exclude"], Lst $ Str <$> ms)

-- TODO: include = ["Compiler.*"] (wildcards)
export
satisfyModule : String -> ModFilter -> Bool
satisfyModule fp None = True
satisfyModule fp (Include inc) = elem fp inc
satisfyModule fp (Exclude exc) = not $ elem fp exc
