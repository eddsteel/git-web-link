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

module GitWebLink.Parsing(branchFromLine, remoteFromLine) where

import GitWebLink.GitRemote
import GitWebLink.Types
import Data.Text(Text)
import qualified Data.Text as T
import Network.URL hiding (host)
import Text.Regex.Base
import Text.Regex.TDFA
import Turtle.Line
import System.FilePath(isValid)

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f = \(a, b, c) -> f a b c

sshUserRE :: String
sshUserRE = "^([^@]+)@"

sshRemoteRE :: String
sshRemoteRE = "^([^a]+)@([^:]+):(.*)$"

matchSshRemote :: String -> Maybe (SshUser, SshHost, FilePath)
matchSshRemote t =
  let (_, _, _, matched) = t =~ sshRemoteRE :: (String, String, String, [String])
  in case matched of
    [user, host, fp] -> if isValid fp
                        then Just $ (T.pack user, T.pack host, fp)
                        else Nothing
    _ -> Nothing

remoteFromRaw :: GitName -> Text -> Maybe GitRemote
remoteFromRaw name raw
  | "https://" `T.isPrefixOf` raw = (HttpRemote name raw) <$> importURL (T.unpack raw)
  | T.unpack raw =~ sshUserRE = (uncurry3 (SshRemote name raw)) <$> matchSshRemote (T.unpack raw)

remoteFromLine :: Line -> Maybe GitRemote
remoteFromLine l = (toTuple . take 2 . T.words . lineToText) l >>= uncurry remoteFromRaw
  where toTuple [a, b] = Just (a, b)
        toTuple _ = Nothing

branchFromLine :: Line -> Maybe (IsActive, GitBranch)
branchFromLine = toBranch . T.words . lineToText
  where toBranch (["*", name]) = Just (True, name)
        toBranch ([name]) = Just (False, name)
        toBranch _ = Nothing
