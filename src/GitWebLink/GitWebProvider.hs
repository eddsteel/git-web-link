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

data GitWebProvider = GitHub { ghUser :: User, ghProject :: Project }
                   | GitHubEnterprise {gheHost :: Host, gheUser :: User, gheProject :: Project}
                   | GitLab { glUser :: User, glProject :: Project}
                   | BitBucket { bbUser :: User, bbProject :: Project}
--                 | GitWeb  {...}
--                 | ...
                   deriving (Show, Eq)

-- | Derive a web provider from a remote. Currently powered by a very dodgy heuristic.
--
-- >>> :set -XOverloadedStrings
-- >>> recogniseProvider (SshRemote "test1" "git@github.com:eddsteel/git-web-link" "git" "github.com" "eddsteel/git-web-link")
-- Just (GitHub {ghUser = "eddsteel", ghProject = "git-web-link"})
-- >>> recogniseProvider (SshRemote "test1" "git@github.evilcorp.com:eddsteel/git-web-link" "git" "github.evilcorp.com" "eddsteel/git-web-link")
-- Just (GitHubEnterprise {gheHost = Host {protocol = HTTP True, host = "github.evilcorp.com", port = Nothing}, gheUser = "eddsteel", gheProject = "git-web-link"})
-- >>> recogniseProvider (SshRemote "test1" "git@github.com:eddsteel/git-web-link.git" "git" "github.com" "eddsteel/git-web-link")
-- Just (GitHub {ghUser = "eddsteel", ghProject = "git-web-link"})
-- >>> recogniseProvider (SshRemote "test1" "git@gitlab.com:eddsteel/git-web-link.git" "git" "gitlab.com" "eddsteel/git-web-link")
-- Just (GitLab {glUser = "eddsteel", glProject = "git-web-link"})
recogniseProvider :: GitRemote -> Maybe GitWebProvider
recogniseProvider HttpRemote {} = Nothing
recogniseProvider (SshRemote _ _ "git" "github.com" path) =
  sshProvider GitHub path
recogniseProvider (SshRemote _ _ "git" "gitlab.com" path) =
  sshProvider GitLab path
recogniseProvider (SshRemote _ _ "git" "bitbucket.org" path) =
  sshProvider BitBucket path
recogniseProvider (SshRemote _ _ "git" h path) =
  let provider = GitHubEnterprise $ Host (HTTP True) (T.unpack h) Nothing
  in sshProvider provider path
recogniseProvider SshRemote {} = Nothing

sshProvider :: (Text -> Text -> GitWebProvider) -> Text -> Maybe GitWebProvider
sshProvider prv p = Just $
  let
    (user, rest) = T.breakOn "/" p
    project = cleanProject rest
  in
   prv user project

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
   | ".git" `T.isSuffixOf` t = T.tail . T.dropEnd 4 $ t
   | otherwise               = T.tail t


-- | Use git web provider to create a link from the given parameters
-- >>> mkLink HomeP (GitHub "foo" "bar")
-- https://github.com/foo/bar
-- >>> mkLink (RefP (Branch "master")) (GitHub "foo" "bar")
-- https://github.com/foo/bar/tree/master
-- >>> mkLink (RefP (Branch "foo")) (GitHub "foo" "bar")
-- https://github.com/foo/bar/tree/foo
-- >>> mkLink (RefP (Tag "v7")) (GitHub "foo" "bar")
-- https://github.com/foo/bar/tree/v7
-- >>> mkLink (RefP (Branch "foo")) (GitHub "foo" "bar")
-- https://github.com/foo/bar/tree/foo
-- >>> mkLink (PathP (Branch "topic/bit-risky") (File "src/main.hs"))  (GitHub "foo" "bar")
-- https://github.com/foo/bar/blob/topic/bit-risky/src/main.hs
-- >>> mkLink (PathP (Reference "abc123") (File "src/main.hs"))  (GitHub "foo" "bar")
-- https://github.com/foo/bar/blob/abc123/src/main.hs
-- >>> mkLink (PathP (Branch "master") (Dir "src/main/java")) (GitHub "enterprise" "ManagerManagerFactory")
-- https://github.com/enterprise/ManagerManagerFactory/tree/master/src/main/java
-- >>> mkLink (RegionP (Branch "feature/fontify-binaries") (Line 18) "alpaca-mode.el") (GitHub "eddsteel" "alpaca-mode")
-- https://github.com/eddsteel/alpaca-mode/blob/feature/fontify-binaries/alpaca-mode.el#L18
-- >>> mkLink (RegionP (Branch "master") (Range 81 83) "src/GitWebLink/GitWebProvider.hs") (GitHub "eddsteel" "git-link-remote")
-- https://github.com/eddsteel/git-link-remote/blob/master/src/GitWebLink/GitWebProvider.hs#L81-L83
-- >>> mkLink (CommitP (Reference "8ce13c8bed94afba0615b515f465447073b366c8")) (GitHub "eddsteel" "git-web-link")
-- https://github.com/eddsteel/git-web-link/commit/8ce13c8bed94afba0615b515f465447073b366c8
-- >>> mkLink HomeP (GitLab "foo" "bar")
-- https://gitlab.com/foo/bar
-- >>> mkLink (RefP (Branch "master")) (GitLab "foo" "bar")
-- https://gitlab.com/foo/bar/tree/master
-- >>> mkLink (RefP (Branch "foo")) (GitLab "foo" "bar")
-- https://gitlab.com/foo/bar/tree/foo
-- >>> mkLink (PathP (Branch "topic/bit-risky") (File "src/main.hs"))  (GitLab "foo" "bar")
-- https://gitlab.com/foo/bar/blob/topic/bit-risky/src/main.hs
-- >>> mkLink (PathP (Branch "master") (Dir "src/main/java")) (GitLab "enterprise" "ManagerManagerFactory")
-- https://gitlab.com/enterprise/ManagerManagerFactory/tree/master/src/main/java
-- >>> mkLink (RegionP (Branch "feature/fontify-binaries") (Line 18) "alpaca-mode.el") (GitLab "eddsteel" "alpaca-mode")
-- https://gitlab.com/eddsteel/alpaca-mode/blob/feature/fontify-binaries/alpaca-mode.el#L18
-- >>> mkLink (RegionP (Branch "master") (Range 81 83) "src/GitWebLink/GitWebProvider.hs") (GitLab "eddsteel" "git-web-link")
-- https://gitlab.com/eddsteel/git-web-link/blob/master/src/GitWebLink/GitWebProvider.hs#L81-83
-- >>> mkLink (CommitP (Reference "8ce13c8bed94afba0615b515f465447073b366c8")) (GitLab "eddsteel" "git-web-link")
-- https://gitlab.com/eddsteel/git-web-link/commit/8ce13c8bed94afba0615b515f465447073b366c8
-- >>> mkLink HomeP (BitBucket "eddsteel" "git-web-link")
-- https://bitbucket.org/eddsteel/git-web-link
-- >>> mkLink (RefP (Branch "master")) (BitBucket "eddsteel" "git-web-link")
-- https://bitbucket.org/eddsteel/git-web-link/src/master/
-- >>> mkLink (PathP (Branch "master") (Dir "src")) (BitBucket "eddsteel" "git-web-link")
-- https://bitbucket.org/eddsteel/git-web-link/src/master/src
-- >>> mkLink (PathP (Branch "master") (File "src/GitWebLink.hs")) (BitBucket "eddsteel" "git-web-link")
-- https://bitbucket.org/eddsteel/git-web-link/src/master/src/GitWebLink.hs
-- >>> mkLink (RegionP (Branch "example") (Line 23) "src/GitWebLink.hs") (BitBucket "eddsteel" "git-web-link")
-- https://bitbucket.org/eddsteel/git-web-link/src/example/src/GitWebLink.hs#lines-23
-- >>> mkLink (RegionP (Branch "example") (Range 23 25) "src/GitWebLink.hs") (BitBucket "eddsteel" "git-web-link")
-- https://bitbucket.org/eddsteel/git-web-link/src/example/src/GitWebLink.hs#lines-23:25
 -- >>> mkLink (RegionP (Reference "d492bb0fe1ebf3c5a116ec0787e04748666de290") (Range 23 25) "src/GitWebLink.hs") (BitBucket "eddsteel" "git-web-link")
-- https://bitbucket.org/eddsteel/git-web-link/src/d492bb0fe1ebf3c5a116ec0787e04748666de290/src/GitWebLink.hs#lines-23:25
-- >>> mkLink (CommitP (Reference "d492bb0fe1ebf3c5a116ec0787e04748666de290")) (BitBucket "eddsteel" "git-web-link")
-- https://bitbucket.org/eddsteel/git-web-link/commits/d492bb0fe1ebf3c5a116ec0787e04748666de290
--
mkLink :: GWLParameters -> GitWebProvider -> URI
mkLink (CommitP (Reference commit)) (GitHub u p) =
  ghURI (pathJoin [u, p, "commit", commit]) ""
mkLink (CommitP (Reference commit)) (GitLab u p) =
  glURI (pathJoin [u, p, "commit", commit]) ""
mkLink (CommitP (Reference commit)) (BitBucket u p) =
  bbURI (pathJoin [u, p, "commits", commit]) ""
mkLink (CommitP (Reference commit)) (GitHubEnterprise h u p) =
  gheURI h (pathJoin [u, p, "commit", commit]) ""
mkLink HomeP (GitHub u p) = ghURI (pathJoin [u, p]) ""
mkLink HomeP (GitLab u p) = glURI (pathJoin [u, p]) ""
mkLink HomeP (BitBucket u p) = bbURI (pathJoin [u, p]) ""
mkLink HomeP (GitHubEnterprise h u p) = gheURI h (pathJoin [u, p]) ""
mkLink (RefP b) (GitHub u p) = ghURI (ghFile u p b Root) ""
mkLink (RefP b) (GitLab u p) = glURI (ghFile u p b Root) ""
mkLink (RefP b) (BitBucket u p) = bbURI (bbFile u p b Root) ""
mkLink (RefP b) (GitHubEnterprise h u p) = gheURI h (ghFile u p b Root) ""
mkLink (PathP b f) (GitHub u p) = ghURI (ghFile u p b f) ""
mkLink (PathP b f) (GitLab u p) = glURI (ghFile u p b f) ""
mkLink (PathP b f) (BitBucket u p) = bbURI (bbFile u p b f) ""
mkLink (PathP b f) (GitHubEnterprise h u p) = gheURI h (ghFile u p b f) ""
mkLink (RegionP b (Line l) f) (GitHub u p) = ghURI (ghFile u p b (File f)) ("#L" ++ (show l))
mkLink (RegionP b (Line l) f) (GitLab u p) = glURI (ghFile u p b (File f)) ("#L" ++ (show l))
mkLink (RegionP b (Line l) f) (BitBucket u p) = bbURI (bbFile u p b (File f)) ("#lines-" ++ (show l))
mkLink (RegionP b (Line l) f) (GitHubEnterprise h u p) = gheURI h (ghFile u p b (File f)) ("#L" ++ (show l))
mkLink (RegionP b (Range s e) f) (GitHub u p) =
  ghURI (ghFile u p b (File f)) (Prelude.concat ["#L", show s, "-L", show e])
mkLink (RegionP b (Range s e) f) (GitLab u p) =
  glURI (ghFile u p b (File f)) (Prelude.concat ["#L", show s, "-", show e])
mkLink (RegionP b (Range s e) f) (BitBucket u p) =
  bbURI (bbFile u p b (File f)) (Prelude.concat ["#lines-", show s, ":", show e])
mkLink (RegionP b (Range s e) f) (GitHubEnterprise h u p) =
  gheURI h (ghFile u p b (File f)) (Prelude.concat ["#L", show s, "-L", show e])
