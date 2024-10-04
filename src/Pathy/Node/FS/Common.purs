module Pathy.Node.FS.Common where

import Prelude

import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Maybe (Maybe)
import Data.Tuple (Tuple)
import Effect (Effect)
import Effect.Aff (Aff, Error)
import Effect.Unsafe (unsafePerformEffect)
import Node.Buffer (Buffer)
import Node.Encoding (Encoding)
import Node.OS (type_)
import Node.Path (FilePath)
import Pathy (Parser(..), Printer, posixParser, posixPrinter, windowsPrinter)

data OS = Windows | Posix

myOS :: Effect OS
myOS = do
  os <- type_
  pure case os of
    "Windows_NT" -> Windows
    _ -> Posix

getPrinter :: Effect { printer :: Printer, parser :: Parser }
getPrinter = do
  myOS >>=
    case _ of
      Windows -> pure { printer: windowsPrinter, parser: posixParser } -- windowsParser }
      Posix -> pure { printer: posixPrinter, parser: posixParser }

-- or memoize?
unsafeMyPrinter :: { printer :: Printer, parser :: Parser }
unsafeMyPrinter = unsafePerformEffect getPrinter
