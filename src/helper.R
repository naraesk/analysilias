library(psych)
library(stringr)
library(R.utils)
library(XML)
library(rCharts)
library(digest)
library(plyr)
library(R2HTML)

# Creae some data structures

users 		<- data.frame(id=numeric(), Matrikel=numeric(), score=numeric(), mark=numeric(), stringsAsFactors = FALSE)
questions 	<- data.frame(id=numeric(), solution=numeric(), title=character())
exam 		<- list(mean=numeric(), maxScore=numeric(), minScore=numeric(), relScore=numeric(), rate=numeric(), title=character())

importFromIlias <- function ()
{	
	files <- list.files("input/", pattern="*.zip")
	lapply(files, extractArchives)
	dirs <- list.files("tmp/") 
	lapply(dirs, importData)
}
  
importData <- function (dir)
{
	qtiFileName 	<- gsub("tst", "qti", dir)
	resultFileName 	<- gsub("tst", "results", dir)
	qtiDoc 	 		<- xmlInternalTreeParse(paste("tmp/", dir, "/", qtiFileName, ".xml", sep=""))
	resultDoc 	 	<- xmlInternalTreeParse(paste("tmp/", dir, "/", resultFileName, ".xml", sep=""))

# Liest aus der result-Datei die Probanden ein

	id   			<- as.numeric(xpathSApply(resultDoc, "//tst_active/row[@active_id]", xmlGetAttr, "active_id"))
	score     		<- c(NA)
	mark	  		<- c(NA)
	Matrikel 		<- c(NA)
	users_new  		<- data.frame (id, Matrikel, score, mark, stringsAsFactors = FALSE)
	users_new  		<- users_new[order(users_new$id),]
	users 			<<- rbind(users, users_new)

# Liest aus der qti-Datei alle relevanten Informationen zu den gestellten Fragen in den Frame questions ein

	question_id_raw <- xpathSApply(qtiDoc, "//item", xmlGetAttr, "ident")
	id 		  		<- sapply(question_id_raw, function(x) as.numeric(str_sub(x, -4)))
	solution_raw	<- xpathSApply(qtiDoc,"//setvar[.='1']/../displayfeedback", xmlGetAttr, "linkrefid")
	solution 		<- sapply(solution_raw, function(x) as.numeric(str_sub(x, -1)))
	title 	  		<- xpathSApply(qtiDoc, "//item", xmlGetAttr, "title")
	comment_raw 	<- xpathSApply(qtiDoc, "//item/qticomment", xmlValue)
	comment  		<- sapply(comment_raw, function(x) str_sub(x, -2))
	
# 	get metadata from description
#	pattern: "foliensatz, seitenzahl, type diff"
#	example: "oogp_21_qt, 21 – 34, LF"

	type			<- sapply(comment, function(string) getType(string))
	diff_tobe 		<- sapply(comment, function(string) getDifficulty(string))
  
	count			<- xpathSApply(qtiDoc, "//response_label", xmlGetAttr, "ident")
	exam["numberOfAlternatives"] <<- max(count)
	
	opts 			<- sapply(c(0:exam[["numberOfAlternatives"]]), function(x) xpathSApply(qtiDoc, paste("//response_label[@ident=",x,"]/material/mattext", sep=''), xmlValue))
	optsWithTitle	<- data.frame(title, opts, stringsAsFactors = FALSE)
	hash 			<- apply(optsWithTitle, 1, function(x) digest(x))
  
	questions_new   <- data.frame (id, solution, title, hash, type, diff_tobe, stringsAsFactors = FALSE)
	apply(questions_new["id"], 1, function (x) getResults(x, resultDoc=resultDoc))
	questions 	    <<- rbind(questions, questions_new)
	exam[["title"]] <<- xpathSApply(qtiDoc, "//assessment", xmlGetAttr, "title")
  
	print("Daten wurden erfolgreich eingelesen")	
}

getType <- function(string)
{
  if(grepl("F", string)) return ("Fakt")
  if(grepl("A", string)) return ("Anwendung")
  if(grepl("T", string)) return ("Transfer")
  return("")
}

getDifficulty <- function (string)
{
  if(grepl("L", string)) return ("leicht")
  if(grepl("M", string)) return ("mittel")
  if(grepl("S", string)) return ("schwer")
  return("")
}

extractArchives <- function (file)
{
	dir.create("tmp/", showWarnings = FALSE)
	filepath <- paste("input/", file, sep="")
	unzip(filepath, exdir="tmp/")
	file.rename(filepath, paste("backup/", file, sep=""))
}

getResults <- function (id, resultDoc)
{
	string 		<- paste("//tst_solutions/row[@question_fi=", id, "]", sep="")
	value1 		<- as.numeric(xpathSApply(resultDoc, string, xmlGetAttr, "value1"))
	active_fi	<- as.numeric(xpathSApply(resultDoc, string, xmlGetAttr, "active_fi"))
	list 		<- data.frame(value1, active_fi)
	list 		<- list[order (list[["active_fi"]]),]
	users[as.character(id)] <<- list[["value1"]]
}

calcScores <- function (percentage, variant)
{
	if(variant == "22%")
	{
		return (round(percentage * exam[["relScore"]] + exam[["minScore"]]))

	}
	else
	{
		return (round(percentage * exam[["maxScore"]]))

	}
}

calcMarks <- function (score)
{
	if(score < exam[["minScore"]]) return (5)
	return (min(marks$grades[marks$score<=score]))
}

addQuestionToPool <- function (x)
{
	rowPool <<- as.integer(rownames(subset(pool, id==x)))
	rowQuestions <<- as.integer(rownames(subset(questions, hash==x)))
   
	if(!is.integer(rowPool)) { return(NULL) }

	if(questions[["Schwierigkeitsindex"]][rowQuestions] > 0.67)
	{
		pool[["diff_easy"]][rowPool] <<- pool[["diff_easy"]][rowPool] +1
	}
	else if(questions[["Schwierigkeitsindex"]][rowQuestions] <= 1/3)
	{
		pool[["diff_hard"]][rowPool] <<- pool[["diff_hard"]][rowPool] +1
	}
	else pool[["diff_med"]][rowPool] <<- pool[["diff_med"]][rowPool] +1
  
	if(length(questions[["Trennschärfe"]][rowQuestions]) == 0) {return(NULL)}
  
	if(is.na(questions[["Trennschärfe"]][rowQuestions])) { return(NULL)} 
  
	if(questions[["Trennschärfe"]][rowQuestions] > 0.3)
	{
		pool[["ITC_high"]][rowPool] <<- pool[["ITC_high"]][rowPool] +1
	}
	else if (questions[["Trennschärfe"]][rowQuestions] <= 0)
	{
		pool[["ITC_neg"]][rowPool] <<- pool[["ITC_neg"]][rowPool] +1
	}
	else pool[["ITC_low"]][rowPool] <<- pool[["ITC_low"]][rowPool] +1
}

removeQuestionFromPool <- function(x)
{
	rowPool <- as.integer(rownames(subset(pool, id==x)))
	rowQuestions <- as.integer(rownames(subset(questions, hash==x)))
   
	if(!is.integer(rowPool)) { return(NULL) }

	if(questions[["Schwierigkeitsindex"]][rowQuestions] > 0.67)
	{
		pool[["diff_easy"]][rowPool] <<- pool[["diff_easy"]][rowPool] -1
	}
	else if(questions[["Schwierigkeitsindex"]][rowQuestions] <= 1/3)
	{
		pool[["diff_hard"]][rowPool] <<- pool[["diff_hard"]][rowPool] -1
	}
	else pool[["diff_med"]][rowPool] <<- pool[["diff_med"]][rowPool] -1
  
	if(length(questions[["Trennschärfe"]][rowQuestions]) == 0) {return(NULL)}
  
	if(is.na(questions[["Trennschärfe"]][rowQuestions])) { return(NULL)} 
  
	if(questions[["Trennschärfe"]][rowQuestions] > 0.3)
	{
		pool[["ITC_high"]][rowPool] <<- pool[["ITC_high"]][rowPool] -1
	}
	else if (questions[["Trennschärfe"]][rowQuestions] <= 0)
	{
		pool[["ITC_neg"]][rowPool] <<- pool[["ITC_neg"]][rowPool] -1
	}
	else pool[["ITC_low"]][rowPool] <<- pool[["ITC_low"]][rowPool] -1
}

cleanup <- function()
{
  rm(grades, percentage, marks, score, envir = .GlobalEnv)
  unlink("tmp/", recursive=TRUE)
}