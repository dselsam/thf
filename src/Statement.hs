module Statement where

import qualified Formula as THF

data Statement = Include String [String]
               | AnnotatedFormula String Role THF.Formula
               | Comment
               deriving (Show)

data Role = Role String
          deriving (Show)


