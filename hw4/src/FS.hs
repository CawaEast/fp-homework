{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE LambdaCase #-}

module FS where

import Data.Either(either)
import Control.Lens
import System.Directory(listDirectory, doesFileExist)
import Control.Monad.Trans.Class
import Control.Monad.IO.Class(liftIO)
import System.FilePath.Posix( (</>), takeFileName, takeDirectory )
import Control.Lens.Operators((^..))



data FS 
    = Dir 
          {_name     :: FilePath  -- название папки, не полный путь
          ,_contents :: [FS]
          }
    | File
          {_name     :: FilePath  -- название файла, не полный путь
          } deriving (Show)
          
makeLenses ''FS
--makePrisms ''FS

getDirectory :: FilePath -> IO FS
getDirectory path = do
    b <- doesFileExist path
    if b
    then return File {_name = takeFileName path} 
    else do
        all <- listDirectory path
        fs <- liftIO $ sequence $ map (getDirectory . (\t -> path </> t)) all
        return Dir {_name = takeFileName path, _contents = fs}

_File :: Traversal' FS FilePath
_File f = \case (File x) -> File <$> f x; other -> pure other

_Dir :: Traversal' FS (FilePath, [FS])
_Dir f = \case (Dir x y) -> (\(x', y') -> Dir x' y') <$> f (x, y); other -> pure other

--ls :: FS -> [FilePath]    
--ls :: Lens [FS] FilePath [FilePath] FilePath
--ls f fs = f $ fmap (^.name) (fs^.contents) 

--dir_name fs = fst <$> (fs^?_Dir)

--cd :: Lens' FS FilePath
--cd =


