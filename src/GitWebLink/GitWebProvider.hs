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

module GitWebLink.GitWebProvider( recogniseProvider
                                , mkLinkHome
                                , mkLinkBranch
                                , mkLinkFile
                                , mkLinkLine
                                , mkLinkRange
                                , GitWebProvider(..)
                                ) where
import GitWebLink.GitHub
import GitWebLink.GitRemote
import GitWebLink.Types
import qualified Data.Text as T
import Network.URL
import Network.URI

data GitWebProvider = GitHub { ghUser :: GHUser, ghProject :: Project }
                   | GitHubEnterprise { gheUser :: GHUser, gheProject :: Project, gheHost :: Host}
--                 | Bitbucket {...}
--                 | GitWeb  {...}
--                 | ...
                   deriving (Show, Eq)

-- | Derive a web provider from a remote. Currently powered by a very dodgy heuristic.
--
-- >>> :set -XOverloadedStrings
-- >>> recogniseProvider (SshRemote "test1" "git@github.com:eddsteel/git-web-link" "git" "github.com" "eddsteel/git-web-link")
-- Just (GitHub {ghUser = "eddsteel", ghProject = "git-web-link"})
-- >>> recogniseProvider (SshRemote "test1" "git@github.evilcorp.com:eddsteel/git-web-link" "git" "github.evilcorp.com" "eddsteel/git-web-link")
-- Just (GitHubEnterprise {gheUser = "eddsteel", gheProject = "git-web-link", gheHost = Host {protocol = HTTP True, host = "github.evilcorp.com", port = Nothing}})
recogniseProvider :: GitRemote -> Maybe GitWebProvider
recogniseProvider HttpRemote {} = Nothing
recogniseProvider (SshRemote n r "git" "github.com" p) =
  let
    user = takeWhile (/= '/') p
    project = tail . dropWhile (/= '/') $ p
  in
   Just $ GitHub (T.pack user) (T.pack project)
recogniseProvider (SshRemote n r "git" h p) =
  let
    user = takeWhile (/= '/') p
    project = tail . dropWhile (/= '/') $ p
    host = Host (HTTP True) (T.unpack h) Nothing
  in
   Just $ GitHubEnterprise (T.pack user) (T.pack project) host
recogniseProvider SshRemote {} = Nothing

-- | Link to webProvider's home (/)
--
-- >>> mkLinkHome (GitHub "foo" "bar")
-- https://github.com/foo/bar
mkLinkHome :: GitWebProvider -> URI
mkLinkHome (GitHub u p) = ghURI (pathJoin [u, p]) ""
mkLinkHome (GitHubEnterprise u p h) = gheURI h (pathJoin [u, p]) ""

mkLinkBranch :: GitBranch -> GitWebProvider -> URI
mkLinkBranch b (GitHub u p) = ghURI (pathJoin [u, p, b]) ""
mkLinkBranch b (GitHubEnterprise u p h) = gheURI h (pathJoin [u, p, b]) ""

-- |
-- >>> mkLinkFile "topic/bit-risky" (File "src/main.hs") (GitHub "foo" "bar")
-- https://github.com/foo/bar/blob/topic/bit-risky/src/main.hs
mkLinkFile :: GitBranch -> DirOrFile -> GitWebProvider -> URI
mkLinkFile b fp (GitHub u p) = ghURI (ghFile u p b fp) ""
mkLinkFile b fp (GitHubEnterprise u p h) = gheURI h (ghFile u p b fp) ""

-- |
-- >>> mkLinkLine "feature/fontify-binaries" (File "alpaca-mode.el") 18 (GitHub "eddsteel" "alpaca-mode")
-- https://github.com/eddsteel/alpaca-mode/blob/feature/fontify-binaries/alpaca-mode.el#L18
mkLinkLine :: Int -> GitBranch -> DirOrFile  -> GitWebProvider -> URI
mkLinkLine line b fp (GitHub u p) = ghURI (ghFile u p b fp) ("#L" ++ (show line))
mkLinkLine line b fp (GitHubEnterprise u p h) = gheURI h (ghFile u p b fp) ("#L" ++ (show line))

-- |
-- >>> mkLinkRange 81 83 "master" (File "src/GitWebLink/GitWebProvider.hs") (GitHub "eddsteel" "git-link-remote")
-- https://github.com/eddsteel/git-link-remote/blob/master/src/GitWebLink/GitWebProvider.hs#L81-L83
mkLinkRange :: Int -> Int -> GitBranch -> DirOrFile -> GitWebProvider -> URI
mkLinkRange start end b fp (GitHub u p) =
  ghURI (ghFile u p b fp) (Prelude.concat ["#L", show start, "-L", show end])
mkLinkRange start end b fp (GitHubEnterprise u p h) =
  gheURI h (ghFile u p b fp) (Prelude.concat ["#L", show start, "-L", show end])
