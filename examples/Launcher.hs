import Control.Monad
import Control.Lens
import System.Directory
import System.Environment
import System.Process
import Control.Exception
import Data.Maybe

import qualified DMenu

cmdOpts :: DMenu.MonadDMenu m => m ()
cmdOpts = do
  DMenu.numLines        .= 10
  DMenu.font            .= "FiraMono:size=11"
  DMenu.prompt          .= "run"
  DMenu.caseInsensitive .= True

sepBy :: String → Char → [String]
sepBy s c = go "" s where
  go s' = \case
    []                   → [ s' | not $ null s' ]
    (c':cs') | c == c'   → s' : go "" cs'
             | otherwise → go (s' ++ [c']) cs'

getPaths :: IO [FilePath]
getPaths = (`sepBy` ':') . fromMaybe "/bin:/usr/bin" <$> lookupEnv "PATH"

listDirectoryDef :: FilePath → IO [FilePath]
listDirectoryDef p = listDirectory p `catch` (\(_ :: SomeException) → pure [])

main :: IO ()
main = do
  paths ← getPaths
  progs ← concat <$> forM paths listDirectoryDef
  DMenu.runAsk cmdOpts progs >>= \case
    Right (prog:_) → callProcess prog []
    _              → pure ()