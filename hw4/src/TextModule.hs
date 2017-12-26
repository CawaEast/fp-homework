{-# LANGUAGE TemplateHaskell    #-}


module TextModule(Testing(..)) where

import TH_L1(makeShowText)


data Testing = Testing deriving Show

makeShowText ''Testing
    
--main = print $ showText $ Testing
 