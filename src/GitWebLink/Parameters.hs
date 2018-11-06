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

module GitWebLink.Parameters where
import GitWebLink.Types
import Data.Text(Text)
import GitWebLink.Types
import Control.Applicative
import Options.Applicative
import Data.Semigroup ((<>))

data InputParameters =
  Params {
    pBranch :: Maybe GitBranch,
    fp :: Maybe FilePath,
    s :: Maybe Int,
    e :: Maybe Int }
  deriving Show

data GWLParameters = HomeP
                   | BranchP {branch :: GitBranch}
                   | PathP   {branch :: GitBranch, path :: DirOrFile}
                   | RegionP {branch :: GitBranch, filePath :: FilePath, region :: Region}

inputOptions :: ParserInfo InputParameters
inputOptions = info (parseInputParameters <**> helper) (fullDesc)

parseInputParameters :: Parser InputParameters
parseInputParameters = Params <$> optional parseBranch <*> optional parsePath <*> optional parseStart <*> optional parseEnd

parseBranch :: Parser GitBranch
parseBranch = Branch <$> strOption (short 'b')

parsePath :: Parser FilePath
parsePath = strOption (short 'p')

parseStart :: Parser Int
parseStart = option auto (short 'l')

parseEnd :: Parser Int
parseEnd = option auto (short 'm')

