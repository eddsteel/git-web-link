{-# LANGUAGE OverloadedStrings #-}
-- git-web-link provide links to web UIs for git projects
-- Copyright (C) 2017-2019 Edd Steel
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
module GitWebLink.GitOps( gitRemotes
                        , gitRemotesByKey
                        , gitRemoteNames
                        , gitBranches
                        , gitBranchNames
                        , gitDereference
                        , gitActiveBranch
                        , gitRemoteForBranch
                        , gitRootDir
                        , gitTagNames) where
import Control.Exception
import GitWebLink.Parsing
import GitWebLink.Paths(git)
import GitWebLink.Types
import Data.List(nub)
import Data.Map(Map, fromList, lookup)
import Data.Maybe(catMaybes)
import Turtle hiding (FilePath,nub)
import qualified Data.Text as T

gitRemotes :: IO [GitRemote]
gitRemotes = runAndExtract git ["remote", "-v"] remoteFromLine

gitRemotesByKey :: IO (Map Text GitRemote)
gitRemotesByKey = do
  remotes <- gitRemotes
  let names = fmap name remotes
  return . fromList $ zip names remotes

gitRemoteNames :: IO [Text]
gitRemoteNames = fmap lineToText <$> runAndExtract git ["remote"] Just

gitBranchNames :: IO [Text]
gitBranchNames = fmap snd <$> gitBranches

gitTagNames :: IO [Text]
gitTagNames = fmap lineToText <$> runAndExtract git ["tag"] Just

gitBranches :: IO [(IsActive, Text)]
gitBranches = runAndExtract git ["branch"] branchFromLine

gitRootDir :: IO FilePath
gitRootDir = fmap (T.unpack . T.strip . linesToText) $ runAndExtract git ["rev-parse", "--show-toplevel"] Just

gitRemoteForBranch :: GitReference -> IO (Maybe Text)
gitRemoteForBranch (Branch b) = run $ inproc git ["config", "--get", configKey] empty
  where configKey = T.concat ["branch.", b, ".pushRemote"]
        run io = handle handler $ Just . T.strip <$> strict io
        handler :: SomeException -> IO (Maybe Text)
        handler _ = pure Nothing
gitRemoteForBranch _ = pure Nothing

-- Dereference a named branch or tag e.g. "master" to a ref e.g. "8ce13c8bed94afba0615b515f465447073b366c8".
-- Uses `git show-ref` because
-- > Use of this utility is encouraged in favor of directly accessing files under the .git
-- > directory.
-- If the dereference fails, the original input is just returned
gitDereference :: GitReference -> IO GitReference
gitDereference b = head <$> runAndExtract git args toRef
  where
    args = ["show-ref", "-s", "--verify", path b]
    path (Branch t) = T.concat ["refs/heads/", t]
    path (Tag t) = T.concat ["refs/tags/", t]
    toRef t = if T.null (lineToText t)
              then Just b
              else Just $ Reference . lineToText $ t

gitActiveBranch :: IO GitReference
gitActiveBranch = do
  branches <- gitBranches
  return . Branch . head . map snd . filter fst $ branches

runAndExtract :: Eq a => Text -> [Text] -> (Line -> Maybe a) -> IO [a]
runAndExtract cmd args extract = fold (inproc cmd args empty) foldLine
  where foldLine = Fold step [] (nub . catMaybes)
        step acc l = extract l : acc
