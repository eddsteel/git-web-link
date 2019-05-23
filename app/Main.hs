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

module Main where

import Control.Applicative
import Data.Monoid (mconcat)
import Data.Semigroup ((<>))
import Data.Text(Text)
import GitWebLink(run)
import GitWebLink.GitOps
import GitWebLink.Types
import Options.Applicative
import Text.PrettyPrint.ANSI.Leijen (Doc, text, line, group)
import Turtle(arguments)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do
  -- for completion, get current branches and remotes
  branches <- fmap T.unpack <$> gitBranchNames
  remotes <- fmap T.unpack <$> gitRemoteNames
  let parser = inputParameters branches remotes
  params <- execParser parser
  uri <- run params
  case uri of
    Just u -> TIO.putStrLn (T.pack . show $ u)
    Nothing -> TIO.putStrLn "Unable to produce link.\n"

copyright :: String
copyright  = "git-web-link 0.6 Copyright (C) 2017-2019 Edd Steel"

inputParameters :: [String] -> [String] -> ParserInfo InputParameters
inputParameters branches remotes = info parser mods
  where
    parser = parseInputParameters branches remotes <**> version <**> helper
    mods = fullDesc <> progDescDoc desc
    desc = Just . group . mconcat $
      [ line
      , text "Provides a URL for the web-location of this file or dir in the given", line
      , text "remote repository's web UI. Repository providers currently supported: github", line
      , text "and github enterprise.", line
      , text "Example", line
      , line
      , text "$ git web-link -b example -r origin -p app/Main.hs -l 31 -m 32", line
      , text "https://github.com/eddsteel/git-web-link/blob/example/app/Main.hs#L31-L32", line]

parseInputParameters :: [String] -> [String] -> Parser InputParameters
parseInputParameters branches remotes  = Params
                       <$> optional (remoteName remotes)
                       <*> optional (branchName branches)
                       <*> optional filepath
                       <*> optional start
                       <*> optional end
                       <*> optional deref
                       <*> optional comm

remoteName :: [String] -> Parser Text
remoteName remotes = strOption remoteMods
  where remoteMods = mconcat [ short 'r'
                             , long "remote"
                             , help "Link to this remote (defaults to branch default push)"
                             , metavar "NAME"
                             , completeWith remotes]

branchName :: [String] -> Parser GitBranch
branchName branches = Branch <$> strOption branchMods
  where branchMods = mconcat [ short 'b'
                             , long "branch"
                             , help "Link to this branch (defaults to active branch)"
                             , metavar "NAME"
                             , completeWith branches]

filepath :: Parser Text
filepath = strOption fpMods
  where fpMods = mconcat [ short 'p'
                         , long "path"
                         , help "Link to this file or directory"
                         , metavar "FILE-OR-DIR"
                         , action "file"
                         , action "directory"]

start :: Parser Int
start = option auto startMods
  where startMods = mconcat [ short 'l'
                              , long "line"
                              , help "Link to this line number (or range starting here, requires path option, set to a filename)"
                              , metavar "NUM"]

end :: Parser Int
end = option auto endMods
  where endMods = mconcat [ short 'm'
                            , long "line-end"
                            , help "Link to range ending here (requires start and path option, set to a filename)"
                            , metavar "NUM"]

deref :: Parser Bool
deref = switch derefMods
  where derefMods = mconcat [short 'd'
                              , long "deref"
                              , help "Dereference to commit hash in link (off by default)"]

comm :: Parser Text
comm = strOption commitMods
  where commitMods = mconcat [short 'c'
                             , long "commit"
                             , metavar "HASH"
                             , help "Link to a commit, by hash (disregards path, branch, line, region, etc)"]

version :: Parser (a -> a)
version = infoOption vers $ mconcat [short 'v', long "version", help "Show version information"]
  where vers = unlines [ copyright
                       , ""
                       , "This program comes with ABSOLUTELY NO WARRANTY."
                       , "This is free software, and you are welcome to redistribute it"
                       , "under certain conditions."
                       , ""
                       , "See https://github.com/eddsteel/git-web-link/blob/master/LICENSE"]
