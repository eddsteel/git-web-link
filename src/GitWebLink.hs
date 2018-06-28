{-# LANGUAGE OverloadedStrings #-}
-- git-web-link provide links to web UIs for git projects
-- Copyright (C) 2017-2018 Edd Steel
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.
--

module GitWebLink(runArguments) where
import GitWebLink.GitOps
import GitWebLink.GitRemote
import GitWebLink.GitWebProvider
import GitWebLink.Types
import GitWebLink.FileOps(relDirOrFile)
import Control.Monad.Trans.Maybe
import Data.Text(Text)
import qualified Data.Text as T
import Network.URI
import Data.Map(Map)
import qualified Data.Map as M

runArguments :: [Text] -> IO (Maybe URI)
runArguments args = do
  branch <- activeGitBranch
  remotes <- gitRemotesByKey
  root <- gitRootDir
  runMaybeT $ dispatchArgs remotes branch root args

dispatchArgs :: Map Text GitRemote -> GitBranch -> FilePath -> [Text] -> MaybeT IO URI
dispatchArgs rs _ root ("-b":branch:rest) = dispatchArgs rs (Branch branch) root rest
dispatchArgs rs (ActiveBranch _) _ (r:[]) = mkLinkHome <$> resolveProvider r rs
dispatchArgs rs b _ (r:[])                = mkLinkBranch b <$> resolveProvider r rs
dispatchArgs rs b root (r:p:[])           = mkLinkFile b <$> resolvePath root p <*> resolveProvider r rs
dispatchArgs rs b root (r:p:l:[])         = mkLinkLine (i l) b <$> resolvePath root p <*> resolveProvider r rs
dispatchArgs rs b root (r:p:s:e:[])       = mkLinkRange (i s) (i e) b <$> resolvePath root p <*> resolveProvider r rs >>= MaybeT . return
dispatchArgs _ _ _ _                      = MaybeT $ return Nothing

i :: Text -> Int
i = read . T.unpack

resolveProvider :: Text -> Map Text GitRemote -> MaybeT IO GitWebProvider
resolveProvider r rs = MaybeT . return $ M.lookup r rs >>= recogniseProvider

resolvePath :: FilePath -> Text -> MaybeT IO DirOrFile
resolvePath root path = relDirOrFile root (T.unpack path)
