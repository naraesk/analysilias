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

addToPool <- function()
{
	lapply(questions$hash, addQuestionToPool)
}

removeFromPool <- function()
{
	lapply(questions$hash, removeQuestionFromPool)
}

serialise <- function ()
{
	path <- paste("data/", exam[["title"]], ".RData", sep="")
	save(users, questions, exam, file=path)
	write.csv2(users[2:4], file=paste("output/Notenliste ", exam[["title"]], ".csv", sep=""),row.names=FALSE)
	print("Notenliste wurde geschrieben.")
}

printGraphs <- function ()
{

#   Scatter Plot
  lapply(questions$hash, insertTitle)
  
  n <- nPlot(Trennschärfe ~ Schwierigkeitsindex, group="type", data=questions, type="scatterChart")
  n$chart(tooltipContent = "#! function(key, x, y, e) {return e.point.title} !#")
  n$chart(forceY = c(-1, 1))
  n$chart(color = c("green", "blue", "red"))
  n$chart(forceX = c(0, 1))
  n$yAxis(axisLabel = "Trennschärfe")
  n$xAxis(axisLabel = "Schwierigkeitsindex")
  n$xAxis(tickValues = c(0, 0.33, 0.67, 1))
  n$yAxis(tickValues = c(-1, 0, 0.33, 0.67, 1))
  n$save(paste("./output/", exam$title, " - Übersicht.html", sep=""), cdn = T) 

#   Histogramm
  breaks <- c(0, 0.16666, 0.33333, 0.5, 0.66666, 0.83333, 1)
  questions[["Schwierigkeit"]] <- cut(questions[["Schwierigkeitsindex"]], breaks, c(1:6))
  hisdata <- data.frame(table(questions[["Schwierigkeit"]], questions[["type"]]))
  hisdata$label = c("[0, 1/6)", "[1/6, 2/6)", "[2/6, 3/6)", "[3/6, 4/6)", "[4/6, 5/6)", "[5/6, 6/6)")
  his <- nPlot(Freq ~ label, group="Var2", data=hisdata, type="multiBarChart")
  his$chart(color = c('green', 'blue', 'red'))
  his$xAxis(axisLabel = "Schwierigkeitsindex")
  his$save(paste("./output/", exam$title, " - Histogramm.html", sep=""), cdn = T)   

#   Distraktorenanalyse
  q0 <- subset(questions, ! questions[["solution"]] == 0 )
  q1 <- subset(questions, ! questions[["solution"]] == 1 )
  q2 <- subset(questions, ! questions[["solution"]] == 2 )
  q3 <- subset(questions, ! questions[["solution"]] == 3 )
  
  d1 <- data.frame(q0$title, q0[["Distraktor 1"]], c(1))
  d1 <- rename(d1, c("q0.title" = "title", "q0...Distraktor.1..." = "share", "c.1."="Distraktor"))
  
  d2 <- data.frame(q1$title, q1[["Distraktor 2"]], c(2))
  d2 <- rename(d2, c("q1.title" = "title", "q1...Distraktor.2..." = "share", "c.2."="Distraktor"))
  
  d3 <- data.frame(q2$title, q2[["Distraktor 3"]], c(3))
  d3 <- rename(d3, c("q2.title" = "title", "q2...Distraktor.3..." = "share", "c.3."="Distraktor"))
  
  d4 <- data.frame(q3$title, q3[["Distraktor 4"]], c(4))
  d4 <- rename(d4, c("q3.title" = "title", "q3...Distraktor.4..." = "share", "c.4."="Distraktor"))

  df <- d2
  df <- rbind(df, d2)
  df <- rbind(df, d3)
  df <- rbind(df, d4)
  
#   df <- df[order (df[["title"]], df[["share"]]),]
#   df[["order"]] <- c(1:3)
#   testdf <- df
  
  d1 <- dPlot(
	x = "share", 
	y = "title", 
	groups = "Distraktor", 
	data = df, 
	type = 'bar',
	barGap = 0.4,
	bounds = list(x=850,width = 1000, height = 600),
	heights=500)
test <- d1

  
  d1$xAxis(type = "addPctAxis")
  d1$yAxis(type = "addCategoryAxis")
  d1$set(width = 2000, height = 1000)
  d1$save(paste("./output/", exam$title, " - Distraktorenanalyse.html", sep=""), cdn = T)
}

printpool <- function ()
{
	HTMLStart(outdir="./output/", filename="demoR2HTML")
	ftable(pool, row.vars=1)
# 	mosaicplot(  ̃ Class + Survived, Titanic)
	HTMLplot(Height=400)
	HTMLStop()
}

analyse <- function()
{
  score <<- score.multiple.choice(questions[["solution"]],users[5:ncol(users)], totals = TRUE, score=TRUE, short=FALSE, missing = FALSE)
  users["score"] <<- score$scores
  questions["Trennschärfe"] <<- score[["item.stats"]][["r"]]
  questions["Schwierigkeitsindex"] <<- score[["item.stats"]][["mean"]]
  questions["Distraktor 1"] <<- score[["item.stats"]][["0"]]
  questions["Distraktor 2"] <<- score[["item.stats"]][["1"]]
  questions["Distraktor 3"] <<- score[["item.stats"]][["2"]]
  questions["Distraktor 4"] <<- score[["item.stats"]][["3"]]
  questions["Standardabweichung"] <<- score[["item.stats"]][["sd"]]
  
  print("Trennschärfe, Distraktorenanalyse und Schwierigkeitsindex wurden erfolgreich berechnet")
    
  # Notenberechnung 
  
  exam[["minScore"]] <<- mean(users$score) * 0.78
  exam[["maxScore"]] <<- nrow(questions)
  exam[["relScore"]] <<- exam[["maxScore"]] - exam[["minScore"]]
  marks["score"] <<-as.numeric(lapply(marks$percentage, calcScores))
  users["mark"] <<- as.numeric(lapply(users$score, calcMarks))
  exam[["mean"]] <<-  mean(users[["mark"]])
  exam[["rate"]] <<- length(users$mark[users[["mark"]] == 5]) / nrow(users)
}