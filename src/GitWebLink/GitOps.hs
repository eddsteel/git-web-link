{-# LANGUAGE OverloadedStrings #-}
-- git-web-link provide links to web UIs for git projects
-- Copyright (C) 2017 Edd Steel
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

module GitWebLink.GitOps(gitRemotes, gitRemotesByKey, gitBranches, activeGitBranch) where

import GitWebLink.Types
import GitWebLink.GitRemote
import GitWebLink.Parsing
import Data.List(nub)
import Data.Map(Map, fromList)
import Data.Maybe(catMaybes)
import Turtle

gitRemotes :: IO [GitRemote]
gitRemotes = runAndExtract "git" ["remote", "-v"] remoteFromLine

gitRemotesByKey :: IO (Map Text GitRemote)
gitRemotesByKey = do
  remotes <- gitRemotes
  let names = fmap name remotes
  return . fromList $ zip names remotes

gitBranches :: IO [(IsActive, GitBranch)]
gitBranches = runAndExtract "git" ["branch"] branchFromLine

activeGitBranch :: IO GitBranch
activeGitBranch = do
  branches <- gitBranches
  return . snd . head . filter fst $ branches

runAndExtract :: Eq a => Text -> [Text] -> (Line -> Maybe a) -> IO [a]
runAndExtract cmd args extract = fold (inproc cmd args empty) foldLine
  where foldLine = Fold step [] (nub . catMaybes)
        step acc l = extract l : acc
