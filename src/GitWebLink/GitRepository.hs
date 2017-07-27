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

module GitWebLink.GitRepository( recogniseRepo
                               , mkLinkHome
                               , mkLinkBranch
                               , mkLinkFile
                               , mkLinkLine
                               , mkLinkRange) where

import Data.Text(Text)
import qualified Data.Text as T
import Network.URL
import Network.URI
import GitWebLink.GitHub
import GitWebLink.GitRemote
import GitWebLink.Types

data GitRepository = GitHub { ghUser :: GHUser, ghProject :: Project }
                   | GitHubEnterprise { gheUser :: GHUser, gheProject :: Project, gheHost :: Host}
--                 | Bitbucket {...}
--                 | GitWeb  {...}
--                 | ...
                   deriving (Show, Eq)

-- | Derive a repository from a remote. Currently powered by a very dodgy heuristic.
--
-- >>> :set -XOverloadedStrings
-- >>> recogniseRepo (SshRemote "test1" "git@github.com:eddsteel/git-web-link" "git" "github.com" "eddsteel/git-web-link")
-- Just (GitHub {ghUser = "eddsteel", ghProject = "git-web-link"})
-- >>> recogniseRepo (SshRemote "test1" "git@github.evilcorp.com:eddsteel/git-web-link" "git" "github.evilcorp.com" "eddsteel/git-web-link")
-- Just (GitHubEnterprise {gheUser = "eddsteel", gheProject = "git-web-link", gheHost = Host {protocol = HTTP True, host = "github.evilcorp.com", port = Nothing}})
recogniseRepo :: GitRemote -> Maybe GitRepository
recogniseRepo HttpRemote {} = Nothing
recogniseRepo (SshRemote n r "git" "github.com" p) =
  let
    user = takeWhile (/= '/') p
    project = tail . dropWhile (/= '/') $ p
  in
   Just $ GitHub (T.pack user) (T.pack project)
recogniseRepo (SshRemote n r "git" h p) =
  let
    user = takeWhile (/= '/') p
    project = tail . dropWhile (/= '/') $ p
    host = Host (HTTP True) (T.unpack h) Nothing
  in
   Just $ GitHubEnterprise (T.pack user) (T.pack project) host
recogniseRepo SshRemote {} = Nothing

-- | Link to repository's home (/)
--
-- >>> mkLinkHome (GitHub "foo" "bar")
-- https://github.com/foo/bar
mkLinkHome :: GitRepository -> URI
mkLinkHome (GitHub u p) = ghURI (pathJoin [u, p]) ""
mkLinkHome (GitHubEnterprise u p h) = gheURI h (pathJoin [u, p]) ""

mkLinkBranch :: GitBranch -> GitRepository -> URI
mkLinkBranch b (GitHub u p) = ghURI (pathJoin [u, p, b]) ""
mkLinkBranch b (GitHubEnterprise u p h) = gheURI h (pathJoin [u, p, b]) ""

-- |
-- >>> mkLinkFile "topic/bit-risky" (File "src/main.hs") (GitHub "foo" "bar")
-- https://github.com/foo/bar/blob/topic/bit-risky/src/main.hs
mkLinkFile :: GitBranch -> DirOrFile -> GitRepository -> URI
mkLinkFile b fp (GitHub u p) = ghURI (ghFile u p b fp) ""
mkLinkFile b fp (GitHubEnterprise u p h) = gheURI h (ghFile u p b fp) ""

-- |
-- >>> mkLinkLine "feature/fontify-binaries" (File "alpaca-mode.el") 18 (GitHub "eddsteel" "alpaca-mode")
-- https://github.com/eddsteel/alpaca-mode/blob/feature/fontify-binaries/alpaca-mode.el#L18
mkLinkLine :: GitBranch -> DirOrFile -> Int -> GitRepository -> URI
mkLinkLine b fp line (GitHub u p) = ghURI (ghFile u p b fp) ("#L" ++ (show line))
mkLinkLine b fp line (GitHubEnterprise u p h) = gheURI h (ghFile u p b fp) ("#L" ++ (show line))

-- |
-- >>> mkLinkRange "master" (File "src/GitWebLink/GitRepository.hs") 81 83 (GitHub "eddsteel" "git-link-remote")
-- https://github.com/eddsteel/git-link-remote/blob/master/src/GitWebLink/GitRepository.hs#L81-L83
mkLinkRange :: GitBranch -> DirOrFile -> Int -> Int -> GitRepository -> URI
mkLinkRange b fp start end (GitHub u p) =
  ghURI (ghFile u p b fp) (Prelude.concat ["#L", show start, "-L", show end])
