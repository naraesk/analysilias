# Copyright 2014 David Baum <david.baum@uni-leipzig.de>
# 
# This file is part of ilias-analysis.
# 
# ilias-analysis is free software: you can redistibute it and/or modify
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

# require(devtools)
# install_github('ramnathv/rCharts')

import <- function(file)
{       
	if(missing(file)) {
		importFromIlias()
	}
	else {
		path = paste("data/", file, ".RData", sep="")
		load(file= path, envir=.GlobalEnv)
	}
}

importFromIlias <- function () {
	zipfile		<<- list.files("input/", pattern="*.zip")[1]
	zippath 	<<- paste("input/", zipfile, sep="")
	dir.create("tmp", showWarnings = FALSE)
	unzip(zippath, exdir="tmp")
	dir 		<- list.files("tmp")[1] 
	qtiFileName 	<- gsub("tst", "qti", dir)
	resultsFileName <- gsub("tst", "results", dir)
	qtiDoc 	 	<- xmlInternalTreeParse(paste("tmp/", dir, "/", qtiFileName, ".xml", sep=""))
	resultsDoc 	<- xmlInternalTreeParse(paste("tmp/", dir, "/", resultsFileName, ".xml", sep=""))

# Reads user info from the results file

# We use the attributes tries and lastindex to filter only users that actually participated in the test
# tries == 1 means, the user has completed the test
# tries == 0 and lastindex == 27 means, the user stopped at question 27 but has not finished the test
# tries == 0 and listindex == 0 means, the user did not participate. This might happen, if a user opens the wrong test and therefore enters the wrong test password
	id   		<- as.numeric(xpathSApply(resultsDoc, "//tst_active/row[@tries>0 or @lastindex>0]", xmlGetAttr, "active_id"))
	name 		<- xpathSApply(resultsDoc, "//tst_active/row[@tries>0 or @lastindex>0]", xmlGetAttr, "fullname")
	score     	<- as.numeric(xpathSApply(resultsDoc, "//tst_pass_result/row[@workingtime >= 0]", xmlGetAttr, "points"))
	duration_raw	<- as.numeric(xpathSApply(resultsDoc, "//tst_pass_result/row[@workingtime >= 0]", xmlGetAttr, "workingtime"))

	duration	<- sapply(duration_raw, secondsToHours)
	tmp		<- data.frame(id, name)
	tmp		<- tmp[order (tmp[["id"]]),]
	tmp["score"]	<- score
	tmp["duration"] <- duration
	tmp		<- tmp[order (tmp[["name"]]),]
	mark	  	<- c(NA)

# Reads name, duration and score from CSV
# they are used to match the data from xml, because there is no unique ID that occurs in both documents
	csvfile 		<<- list.files("input/", pattern="*.csv")[1]
	csvpath			<<- paste("input/", csvfile, sep="")
	csv  			<- read.csv2(csvpath, header = TRUE, stringsAsFactors = FALSE, encoding = "latin1", skipNul = TRUE)
	csv			<- csv[csv$Name!="Name",]
	csv <- csv[!(csv$Benutzername == ""),] 	# remove empty lines
	colnames(csv)[1:11]	<- c("name", "mail", "Matrikel", "Pruefungsnummer", "score", c(6:10), "duration")
	csvdata			<- data.frame(csv["name"], csv["Matrikel"], csv["Pruefungsnummer"], csv["score"], csv["duration"])
	csvdata 		<- csvdata[order (csvdata[["name"]], csvdata[["duration"]], csvdata[["score"]]),]

	
	users				<<- data.frame(tmp[["name"]], c(NA), c(NA), tmp[["score"]], mark, tmp["duration"], tmp["id"])
	colnames(users)[1:4]		<<- c("name", "Matrikel", "Pruefungsnummer", "score")
	users				<<- users[order (users[["name"]], users[["duration"]], users[["score"]]),]
	users[["Matrikel"]] 		<<- csvdata[["Matrikel"]]
# 	users[["Pruefungsnummer"]] 	<<- csvdata[["Pruefungsnummer"]]
	
	tmp			<- tmp[order (tmp[["id"]]),]
	points 			<<- data.frame(tmp[["id"]])
	colnames(points)	<<- c("id")
	answers			<<- points
	
# Some checks to validate the read data

	rownames(users) <<- c(1:nrow(users))
	
	testAssignment <- data.frame(users["name"], users["score"], users[["duration"]])
	if (is.element(TRUE, duplicated(testAssignment))) {
		warning("Their are students with identical name and score. the assignment of Matrikel might be wrong")
		return()
	}
	if (!isTRUE(all.equal(users["score"], csvdata["score"])) 
	|| !isTRUE(all.equal(users["duration"], csvdata["duration"]))
	|| !isTRUE(all.equal(users["name"], csvdata["name"]))) {
		warning("The assignment of Matrikel might be wrong.")
# 		return()
	}
	
# Reads information about questions from qti document

	question_id_raw <- xpathSApply(qtiDoc, "//item", xmlGetAttr, "ident")
	id		<- sapply(question_id_raw, function(x) as.numeric(str_sub(x,rev(gregexpr("_", x)[[1]])[1] - nchar(x))))
	title 	  	<- xpathSApply(qtiDoc, "//item", xmlGetAttr, "title")
	comment_raw 	<- xpathSApply(qtiDoc, "//item/qticomment", xmlValue)
	comment  	<- sapply(comment_raw, function(x) str_sub(x, -2))
	questionType 	<- xpathSApply(qtiDoc, "//qtimetadatafield/fieldlabel[text() = 'QUESTIONTYPE']/following-sibling::node()", xmlValue)

# 	get metadata from description
#	pattern: "foliensatz, seitenzahl, type diff"
#	example: "oogp_21_qt, 21 – 34, LF"

	type			<- sapply(comment, function(string) getType(string))
	diff_tobe 		<- sapply(comment, function(string) getDifficulty(string))

#  	opts 			<- sapply(c(0:exam[["numberOfAlternatives"]]), function(x) xpathSApply(qtiDoc, paste("//response_label[@ident=",x,"]/material/mattext", sep=''), xmlValue))
# 	optsWithTitle		<- data.frame(title, opts)
# 	hash 			<- apply(optsWithTitle, 1, function(x) digest(x))
	questions  		<<- data.frame (id, title, type, diff_tobe, questionType)
	apply(questions["id"], 1, function (x) getPoints(x, resultsDoc=resultsDoc))
	users			<<- users[order (users[["id"]]),]
	exam[["title"]] 	<<- gsub("/", "", str_replace(xpathSApply(qtiDoc, "//assessment", xmlGetAttr, "title"), "//", ""))
	tmpPath			<- paste("./output/", exam[["title"]], sep="")
	dir.create(tmpPath, showWarnings = FALSE)
	exam[["outputPath"]] 	<<- paste(tmpPath, "/", sep="")
	apply(questions, 1, insertQuestion, qtiDoc = qtiDoc, resultsDoc=resultsDoc)
	questions["numberOfAlternatives"] <<- alternatives
	questions["solution"] <<- solutions
}

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

insertQuestion <- function(row, qtiDoc, resultsDoc) {
	ident <- paste("il_0_qst_", trim(row["id"]), sep="")
	if (row[["questionType"]] == "SINGLE CHOICE QUESTION") {
		numberOfAlternatives <- xpathSApply(qtiDoc, paste("count(//item[@ident='", ident ,"']//response_label)",sep="")) - 1
# 		opts <<- sapply(c(0:numberOfAlternatives), function(x) xpathSApply(qtiDoc, paste("//item[@ident='", ident, "']//response_label[@ident=",x,"]/material/mattext", sep=''), xmlValue))
# 		optsWithTitle		<- data.frame(row[["title"]], opts)
		alternatives <<- append(alternatives, numberOfAlternatives)
		solution_raw	<- xpathSApply(qtiDoc, paste("//item[@ident='", ident ,"']//setvar[.>'0']/../displayfeedback", sep=""), xmlGetAttr, "linkrefid")
		solution 	<- as.numeric(str_sub(solution_raw, -1))
		solutions <<- append(solutions, solution)
		for(i in 0:numberOfAlternatives) {
			text <- xpathSApply(qtiDoc, paste("//item[@ident='", ident ,"']//response_label[@ident='",i,"']/material/mattext", sep=""), xmlValue)
			texts <<- append(texts, text)
		}
		getAnswers(row[["id"]], resultsDoc=resultsDoc)
		return()
	}
# 	TOOD: add question type specific querys
	alternatives <<- append(alternatives,0)
	solutions <<- append(solutions, 0)
	texts <<- append(solutions, "")
}

getType <- function(string) {
	if(grepl("F", string)) return ("Fakt")
	if(grepl("A", string)) return ("Anwendung")
	if(grepl("T", string)) return ("Transfer")
	return("")
}

getDifficulty <- function (string) {
	if(grepl("L", string)) return ("leicht")
	if(grepl("M", string)) return ("mittel")
	if(grepl("S", string)) return ("schwer")
	return("")
}

secondsToHours <- function (sec) {
	h <- sec %/% 3600
	min <- (sec-(h*3600)) %/% 60
	sec <- sec - (h*3600) - (min*60)
	
	if (h < 10) h <- paste("0", h, sep="")
	if (min < 10) min <- paste("0", min, sep="")
	if (sec < 10) sec <- paste("0", sec, sep="")
	return(paste(h, min, sec, sep=":"))
}

getAnswers <- function (id, resultsDoc) {
	avalue <<- vector()
	aid <<- vector()
	apply(answers["id"], 1, function (x) getAnswers2(x, id, resultsDoc=resultsDoc))
	list 		<- data.frame(avalue, aid)
	list 		<- list[order (list[["aid"]]),]
	answers[as.character(id)] <<- list[["avalue"]]
}

getAnswers2 <- function (uid, qid, resultsDoc) {
	string 		<- paste("//tst_solutions/row[@question_fi=", qid, "]", "[@active_fi=", uid, "]", sep="")
	value1		<- max(as.numeric(xpathSApply(resultsDoc, string, xmlGetAttr, "value1")))
	if(identical(value1, numeric(0))) { value1 <- 0 }
	avalue <<- append(avalue, value1)
	aid <<- append(aid, qid)
}
getPoints <- function (id, resultsDoc) {
	qvalue <<- vector()
	qid <<- vector()
	apply(points["id"], 1, function (x) getPoints2(x, id, resultsDoc=resultsDoc))

	list2	<- data.frame(qvalue, qid)
	list2	<- list2[order (list2[["qid"]]),]
	points[as.character(id)] <<- list2[["qvalue"]]
}

getPoints2 <- function (uid, q_id, resultsDoc) {
	string		<- paste("//tst_test_result/row[@question_fi=", q_id, "]", "[@active_fi=", uid, "]", sep="")
	value1		<<- max(as.numeric(xpathSApply(resultsDoc, string, xmlGetAttr, "points")))
	if(identical(value1, numeric(0))) { value1 <<- 0 }
	if(is.infinite(value1)) { value1 <<- 0 }
	qvalue <<- append(qvalue, value1)
	qid <<- append(qid, q_id)
}
