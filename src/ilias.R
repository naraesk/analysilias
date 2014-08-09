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
questions 	<- data.frame(id=numeric(), solution=numeric(), title=character())
exam 		<- list(mean=numeric(), maxScore=numeric(), minScore=numeric(), relScore=numeric(), rate=numeric(), title=character())

cleanup <- function()
{
  rm(grades, percentage, marks, score, envir = .GlobalEnv)
  unlink("tmp/", recursive=TRUE)
}