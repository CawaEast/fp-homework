{-# LANGUAGE TemplateHaskell    #-}
{-# LANGUAGE RankNTypes    #-}
{-# LANGUAGE LambdaCase #-}

module FS(FS(..), getDirectory, contents, name, _Dir, _File, ls, cd, file, (^..), Tree(..)) where

import Control.Lens(makeLenses, makePrisms, Traversal')
import System.Directory(listDirectory, doesFileExist)
--import Control.Monad.Trans.Class
import Control.Monad.IO.Class(liftIO)
import System.FilePath.Posix((</>), takeFileName)
import Control.Lens.Operators((^..), (^.))
import Control.Lens.Fold(filtered, has)
import Control.Lens.Traversal(traversed)
import Control.Comonad(Comonad(..))



data FS 
    = Dir 
          {_name     :: FilePath  -- название папки, не полный путь
          ,_contents :: [FS]
          }
    | File
          {_name     :: FilePath  -- название файла, не полный путь
          } deriving (Show)
          
makeLenses ''FS
makePrisms ''FS

getDirectory :: FilePath -> IO FS
getDirectory path = do
    b <- doesFileExist path
    if b
    then return File {_name = takeFileName path} 
    else do
        all1 <- listDirectory path
        fs <- liftIO $ mapM (getDirectory . (\t -> path </> t)) all1
        return Dir {_name = takeFileName path, _contents = fs}

--_File :: Traversal' FS FilePath
--_File f = \case (File x) -> File <$> f x; other -> pure other

--_Dir :: Traversal' FS (FilePath, [FS])
--_Dir f = \case (Dir x y) -> (\(x', y') -> Dir x' y') <$> f (x, y); other -> pure other

ls :: Traversal' FS FilePath
ls = contents . traverse . name

--dir_name fs = fst <$> (fs^?_Dir)

cd :: FilePath -> Traversal' FS FS
cd path = contents . traversed . filtered (\x -> has _Dir x && x ^. name == path)

file :: FilePath -> Traversal' FS FilePath
file path = contents . traversed . filtered (\x -> has _File x && x ^. name == path) . name

data Tree a = Node a [Tree a]

instance Functor Tree where
    --fmap :: (a -> b) -> Tree a -> Tree b
    fmap f (Node a childrens) = Node (f a) (childrens >>= (\child -> [fmap f child]))

instance Comonad Tree where

    --extract :: Tree a -> a 
    extract (Node a _) = a
    
    --duplicate :: Tree a -> Tree (Tree a) 
    duplicate (Node a childrens) = Node (Node a childrens) (childrens >>= (\child -> [duplicate child]))
    
    --extend :: (Tree a -> b) -> Tree a -> Tree b
    extend func node = fmap func (duplicate node)