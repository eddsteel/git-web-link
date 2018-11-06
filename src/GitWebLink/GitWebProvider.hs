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

module GitWebLink.GitWebProvider( recogniseProvider
                                , mkLink
                                , GitWebProvider(..)
                                ) where
import GitWebLink.GitHub

import GitWebLink.Types
import Data.Text(Text)
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
-- >>> recogniseProvider (SshRemote "test1" "git@github.com:eddsteel/git-web-link.git" "git" "github.com" "eddsteel/git-web-link")
-- Just (GitHub {ghUser = "eddsteel", ghProject = "git-web-link"})
recogniseProvider :: GitRemote -> Maybe GitWebProvider
recogniseProvider HttpRemote {} = Nothing
recogniseProvider (SshRemote n r "git" "github.com" p) =
  let
    (user, rest) = T.breakOn "/" p
    project = cleanProject rest
  in
   Just $ GitHub user project
recogniseProvider (SshRemote n r "git" h p) =
  let
    (user, rest) = T.breakOn "/" p
    project = cleanProject rest
    host = Host (HTTP True) (T.unpack h) Nothing
  in
   Just $ GitHubEnterprise user project host
recogniseProvider SshRemote {} = Nothing


-- | goes from /project.git to project
--
-- >>> cleanProject "/project"
-- "project"
--
-- >>> cleanProject "/project.git"
-- "project"
--
cleanProject :: Text -> Text
cleanProject t
  | ".git" `T.isSuffixOf` t = T.tail . T.dropEnd 4 $  t
  | otherwise               = T.tail t


-- | Use git web provider to create a link from the given parameters
-- >>> mkLink HomeP (GitHub "foo" "bar")
-- https://github.com/foo/bar
-- >>> mkLink (BranchP (Branch "master")) (GitHub "foo" "bar")
-- https://github.com/foo/bar/tree/master
-- >>> mkLink (BranchP (Branch "foo")) (GitHub "foo" "bar")
-- https://github.com/foo/bar/tree/foo
-- >>> mkLink (PathP (Branch "topic/bit-risky") (File "src/main.hs"))  (GitHub "foo" "bar")
-- https://github.com/foo/bar/blob/topic/bit-risky/src/main.hs
-- >>> mkLink (PathP (Branch "master") (Dir "src/main/java")) (GitHub "enterprise" "ManagerManagerFactory")
-- https://github.com/enterprise/ManagerManagerFactory/tree/master/src/main/java
-- >>> mkLink (RegionP (Branch "feature/fontify-binaries") (Line 18) "alpaca-mode.el") (GitHub "eddsteel" "alpaca-mode")
-- https://github.com/eddsteel/alpaca-mode/blob/feature/fontify-binaries/alpaca-mode.el#L18
-- >>> mkLink (RegionP (Branch "master") (Range 81 83) "src/GitWebLink/GitWebProvider.hs") (GitHub "eddsteel" "git-link-remote")
-- https://github.com/eddsteel/git-link-remote/blob/master/src/GitWebLink/GitWebProvider.hs#L81-L83
mkLink :: GWLParameters -> GitWebProvider -> URI
mkLink HomeP (GitHub u p) = ghURI (pathJoin [u, p]) ""
mkLink HomeP (GitHubEnterprise u p h) = gheURI h (pathJoin [u, p]) ""
mkLink (BranchP b) (GitHub u p) = ghURI (ghFile u p b Root) ""
mkLink (BranchP b) (GitHubEnterprise u p h) = gheURI h (ghFile u p b Root) ""
mkLink (PathP b f) (GitHub u p) = ghURI (ghFile u p b f) ""
mkLink (PathP b f) (GitHubEnterprise u p h) = gheURI h (ghFile u p b f) ""
mkLink (RegionP b (Line l) f) (GitHub u p) = ghURI (ghFile u p b (File f)) ("#L" ++ (show l))
mkLink (RegionP b (Line l) f) (GitHubEnterprise u p h) = gheURI h (ghFile u p b (File f)) ("#L" ++ (show l))
mkLink (RegionP b (Range s e) f) (GitHub u p) =
  ghURI (ghFile u p b (File f)) (Prelude.concat ["#L", show s, "-L", show e])
mkLink (RegionP b (Range s e) f) (GitHubEnterprise u p h) =
  gheURI h (ghFile u p b (File f)) (Prelude.concat ["#L", show s, "-L", show e])
