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

import GitWebLink(runArguments)

import Turtle(arguments)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

-- git web-link
-- git web-link [-b <branch> -f <file or dir> -l <line or lines>] <remote>
--
main :: IO ()
main = do
  args <- arguments
  case args of
    ["--help"] -> usage -- but git will intercept this.
    ["-h"] -> usage
    ["help"] -> usage
    ["--version"] -> version
    ["-v"] -> version
    ["version"] -> version
    args -> do
      uri <- runArguments args
      case uri of
        Just u -> TIO.putStrLn (T.pack . show $ u)
        Nothing -> TIO.putStrLn "Unable to produce link.\n" >> usage

usage :: IO ()
usage = putStrLn . unlines $ text
  where text =
          [ "Usage: git web-link [options] [parameters]"
          , ""
          , "Provide a URL for the web-location of this file or dir in the given"
          , "remote repository's web UI. Repository providers currently supported: github"
          , "and github enterprise."
          , ""
          , "Example"
          , ""
          , "$ git web-link origin app/Main.hs 31 32"
          , "https://github.com/eddsteel/git-web-link/blob/example/app/Main.hs#L31-L32"
          , ""
          , "Options"
          , ""
          , "(none)                         Uses the currently active branch."
          , "-b <branch>                    Specify the branch to link to."
          , "-d                             Deference branch name to canonical reference"
          , "Note: specifying -b after -d will squash it."
          , "-f                             Link to a file or directory"
          , "-l                             Line or lines, comma-separated, only valid in conjunction with a file"
          , "-h                             Prints usage (this)"
          , ""
          , ""
          , "Parameters"
          , ""
          , "(none)                         Prints usage (this)" -- TODO: make origin optional
          , "help                           Prints usage (this)"
          , "<remote>                       Specifies the remote to link to, optionally including a specific branch, path and region."]

version :: IO ()
version = putStrLn . unlines $ text
  where text =
          [ "git-web-link 0.1 Copyright (C) 2017-2018 Edd Steel"
          , ""
          , "This program comes with ABSOLUTELY NO WARRANTY."
          , "This is free software, and you are welcome to redistribute it"
          , "under certain conditions."
          , ""
          , "See https://github.com/eddsteel/git-web-link/blob/master/LICENSE"]
