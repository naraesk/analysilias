# Copyright 2014 David Baum <david.baum@uni-leipzig.de>
# 
# This file is part of ilias-analysis.
# 
# ilias-analysis is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
# 
# ilias-analysis is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
# 
# You should have received a copy of the GNU General Public License
# along with ilias-analysis.  If not, see <http://www.gnu.org/licenses/>.

library(psych)
library(stringr)
library(R.utils)
library(XML)
library(rCharts)
library(digest)
library(plyr)
library(base64enc)

source("src/import.R")
source("src/statistics.R")
source("src/serialize.R")
source("src/graph.R")
source("src/grades.R")

options(stringsAsFactors = FALSE)

# Creae some data structures

users 		<- data.frame(id=numeric(), Matrikel=numeric(), score=numeric(), mark=numeric(), stringsAsFactors = FALSE)
answers		<- data.frame(id=numeric())
points		<- data.frame(id=numeric())
questions 	<- data.frame(id=numeric(), solution=numeric(), title=character())
exam 		<- list(mean=numeric(), maxScore=numeric(), minScore=numeric(), relScore=numeric(), rate=numeric(), title=character())
marks 	   <- data.frame(grades, percentage)
alternatives	<- vector()
solutions	<- vector()

cleanup <- function() {
  rm(grades, percentage, marks, alternatives, envir = .GlobalEnv)
  unlink("tmp", recursive=TRUE)
}