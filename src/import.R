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

import <- function(file)
{
	if(missing(file))
	{
		importFromIlias()
	}
	else
	{
		path = paste("data/", file, ".RData", sep="")
		load(file= path, envir=.GlobalEnv)
	}
}

importFromIlias <- function ()
{

	zipfile		<- list.files("input/", pattern="*.zip")[1]
	zippath 	<- paste("input/", zipfile, sep="")
	dir.create("tmp/", showWarnings = FALSE)
	
	unzip(zippath, exdir="tmp/")
	file.rename(zippath, paste("backup/", zipfile, sep=""))
	dir 		<- list.files("tmp/")[1] 

	qtiFileName 	<- gsub("tst", "qti", dir)
	resultsFileName <- gsub("tst", "results", dir)
	qtiDoc 	 	<- xmlInternalTreeParse(paste("tmp/", dir, "/", qtiFileName, ".xml", sep=""))
	resultsDoc 	<- xmlInternalTreeParse(paste("tmp/", dir, "/", resultsFileName, ".xml", sep=""))

# Reads user info from the results file

	id   		<- as.numeric(xpathSApply(resultsDoc, "//tst_active/row", xmlGetAttr, "active_id"))
	name 		<- xpathSApply(resultsDoc, "//tst_active/row", xmlGetAttr, "fullname")
	score     	<- as.numeric(xpathSApply(resultsDoc, "//tst_pass_result/row", xmlGetAttr, "points"))
	duration_raw	<- as.numeric(xpathSApply(resultsDoc, "//tst_pass_result/row", xmlGetAttr, "workingtime"))
	duration	<- sapply(duration_raw, secondsToHours)
	tmp		<- data.frame(id, name)
	tmp		<- tmp[order (tmp[["id"]]),]
	tmp["score"]	<- score
	tmp["duration"] <- duration
	tmp		<- tmp[order (tmp[["name"]]),]
	mark	  	<- c(NA)

# Reads name, duration and scre from CSV
# they are used to match the data from xml, because there is no unique ID that occurs in both documents

	csvfile 		<- list.files("input/", pattern="*.csv")[1]
	csvpath			<- paste("input/", csvfile, sep="")
	csv  			<<- read.csv2(csvpath, header = TRUE, stringsAsFactors = FALSE, encoding = "latin1")
	csv			<<- csv[csv$Name!="Name",]
	colnames(csv)[1:11]	<- c("name", "mail", "Matrikel", "Pruefungsnummer", "score", c(6:10), "duration")
	file.rename(csvpath, paste("backup/", csvfile, sep=""))
	csvdata			<<- data.frame(csv["name"], csv["Matrikel"], csv["Pruefungsnummer"], csv["score"], csv["duration"])
	csvdata 		<<- csvdata[order (csvdata[["name"]], csvdata[["duration"]], csvdata[["score"]]),]
	
	users				<<- data.frame(tmp[["name"]], c(NA), c(NA), tmp[["score"]], mark, tmp["duration"], tmp["id"])
	colnames(users)[1:4]		<<- c("name", "Matrikel", "Pruefungsnummer", "score")
	users				<<- users[order (users[["name"]], users[["duration"]], users[["score"]]),]
	users[["Matrikel"]] 		<<- csvdata[["Matrikel"]]
	users[["Pruefungsnummer"]] 	<<- csvdata[["Pruefungsnummer"]]
	
	lapply(users[["Pruefungsnummer"]], validatePruefungsnummer)
	rownames(users) <<- c(1:nrow(users))
	
	testframe <- data.frame(users["name"], users["score"], users[["duration"]])

	if (is.element(TRUE, duplicated(testframe)))
	{
		warning("Their are students with identical name and score. the assignment of Pruefungsnummer and Matrikel might be wrong")
		return()
	}
	if (!isTRUE(all.equal(users["score"],csvdata["score"])) 
	|| !isTRUE(all.equal(users["duration"], csvdata["duration"]))
	|| !isTRUE(all.equal(users["name"], csvdata["name"])))
	{
		warning("The assignment of Matrikel and Pruefungsnummer might be wrong.")
# 		return()
	}
	
# Reads information about questions from qti document

	question_id_raw <- xpathSApply(qtiDoc, "//item", xmlGetAttr, "ident")
	id 		<- sapply(question_id_raw, function(x) as.numeric(str_sub(x, -4)))
	solution_raw	<- xpathSApply(qtiDoc,"//setvar[.='1']/../displayfeedback", xmlGetAttr, "linkrefid")
	solution 	<- sapply(solution_raw, function(x) as.numeric(str_sub(x, -1)))
	title 	  	<- xpathSApply(qtiDoc, "//item", xmlGetAttr, "title")
	comment_raw 	<- xpathSApply(qtiDoc, "//item/qticomment", xmlValue)
	comment  	<- sapply(comment_raw, function(x) str_sub(x, -2))
	
# 	get metadata from description
#	pattern: "foliensatz, seitenzahl, type diff"
#	example: "oogp_21_qt, 21 – 34, LF"

	type			<- sapply(comment, function(string) getType(string))
	diff_tobe 		<- sapply(comment, function(string) getDifficulty(string))
  
	count			<- xpathSApply(qtiDoc, "//response_label", xmlGetAttr, "ident")
	exam["numberOfAlternatives"] <<- max(count)
	
	opts 			<- sapply(c(0:exam[["numberOfAlternatives"]]), function(x) xpathSApply(qtiDoc, paste("//response_label[@ident=",x,"]/material/mattext", sep=''), xmlValue))
	optsWithTitle		<- data.frame(title, opts)
	hash 			<- apply(optsWithTitle, 1, function(x) digest(x))
  
	questions_new  		<- data.frame (id, solution, title, hash, type, diff_tobe)
	apply(questions_new["id"], 1, function (x) getResults(x, resultsDoc=resultsDoc))
	questions 	   	<<- rbind(questions, questions_new)
	exam[["title"]] 	<<- xpathSApply(qtiDoc, "//assessment", xmlGetAttr, "title")
	exam[["outputPath"]]	<<- paste("./output/", exam[["title"]], "/", sep="")
	dir.create(exam[["outputPath"]], showWarnings = FALSE)
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

secondsToHours <- function (sec)
{
	h <- sec %/% 3600
	min <- (sec-(h*3600)) %/% 60
	sec <- sec - (h*3600) - (min*60)
	
	if (h < 10) h <- paste("0", h, sep="")
	if (min < 10) min <- paste("0", min, sep="")
	if (sec < 10) sec <- paste("0", sec, sep="")
	return(paste(h, min, sec, sep=":"))
}

getResults <- function (id, resultsDoc)
{
	string 		<- paste("//tst_solutions/row[@question_fi=", id, "]", sep="")
	value1 		<- as.numeric(xpathSApply(resultsDoc, string, xmlGetAttr, "value1"))
	active_fi	<- as.numeric(xpathSApply(resultsDoc, string, xmlGetAttr, "active_fi"))
	list 		<- data.frame(value1, active_fi)
	users		<<- users[order (users[["id"]]),]
	list 		<- list[order (list[["active_fi"]]),]
	users[as.character(id)] <<- list[["value1"]]
}

validatePruefungsnummer <- function (x)
{
	if(is.na(x)) {
		warning("Pruefungsnummer is not available")
		return()
	}
	if( x>99999) warning("Pruefungsnummer with 6 digits")
	if( x<10000) warning("Pruefungsnummer with 4 digits")
}