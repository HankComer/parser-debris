module Importing where
import ParserType
import Control.Applicative


getBuiltin :: Parser String
getBuiltin = do
    space
    char '<'
    name <- some (sat (/= '>'))
    char '>'
    space
    return name

getUserMade :: Parser String
getUserMade = do
    space
    char '"'
    name <- some (sat (/= '"'))
    char '"'
    space
    return name

simpleImport = terminal $ getUserMade <|> getBuiltin

