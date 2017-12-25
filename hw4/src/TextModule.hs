{-# LANGUAGE TemplateHaskell    #-}


module TextModule where

import TH_L1(makeShowText, showText)
import qualified Data.Text as T


data Testing = Testing deriving Show

makeShowText ''Testing
    
--main = print $ showText $ Testing
 