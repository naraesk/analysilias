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

generateGraphs <- function ()
{
	generateITCGraph()
	generateDistributonOfDifficulty()
	generateDistributionOfGrades()
}

generateITCGraph <- function ()
{
	n <- nPlot(Trennschärfe ~ Schwierigkeitsindex, group="type", data=questions, type="scatterChart")
	n$chart(tooltipContent = "#! function(key, x, y, e) {return e.point.title} !#")
	n$chart(forceY = c(-1, 1))
	n$chart(color = c("green", "blue", "red"))
	n$chart(forceX = c(0, 1))
	n$yAxis(axisLabel = "Trennschärfe")
	n$xAxis(axisLabel = "Schwierigkeitsindex")
	n$xAxis(tickValues = c(0, 0.33, 0.67, 1))
	n$yAxis(tickValues = c(-1, 0, 0.33, 0.67, 1))
	n$save(paste("./output/", exam$title, " - Übersicht.html", sep=""), standalone = TRUE)
}

generateDistributonOfDifficulty <- function ()
{
	breaks <- c(0, 0.16666, 0.33333, 0.5, 0.66666, 0.83333, 1)
	questions[["Schwierigkeit"]] <- cut(questions[["Schwierigkeitsindex"]], breaks, c(1:6))
	hisdata <- data.frame(table(questions[["Schwierigkeit"]], questions[["type"]]))
	hisdata$label = c("[0, 1/6)", "[1/6, 2/6)", "[2/6, 3/6)", "[3/6, 4/6)", "[4/6, 5/6)", "[5/6, 6/6)")
	his <- nPlot(Freq ~ label, group="Var2", data=hisdata, type="multiBarChart")
	his$chart(color = c('green', 'blue', 'red'))
	his$xAxis(axisLabel = "Schwierigkeitsindex")
	his$save(paste("./output/", exam$title, " - Histogramm.html", sep=""), standalone = TRUE) 
}

generateDistributionOfGrades <- function ()
{
	breaks <- c(1, 1.3, 1.7, 2, 2.3, 2.7, 3, 3.3, 3.7, 4, 5)
	markdata<- data.frame(table(factor(users[["mark"]], levels=breaks)))
	markdata$label <- c("1", "1.3", "1.7", "2", "2.3", "2.7", "3", "3.3", "3.7", "4", "5")
	colnames(markdata)[2] <- c("Häufigkeit")
	markgraph <<- nPlot(Häufigkeit ~ Var1, data=markdata, type="multiBarChart")
	markgraph$chart(showControls = FALSE)
	markgraph$chart(reduceXTicks = FALSE)
	markgraph$save(paste("./output/", exam$title, " - Notenverteilung.html", sep=""), standalone = TRUE) 
}


#   Distraktorenanalyse
#   q0 <- subset(questions, ! questions[["solution"]] == 0 )
#   q1 <- subset(questions, ! questions[["solution"]] == 1 )
#   q2 <- subset(questions, ! questions[["solution"]] == 2 )
#   q3 <- subset(questions, ! questions[["solution"]] == 3 )
#   
#   
#   print(length(q0))
#   d1 <- data.frame(q0$title, q0[["Distraktor 1"]], c(1))
#   d1 <- rename(d1, c("q0.title" = "title", "q0...Distraktor.1..." = "share", "c.1."="Distraktor"))
#   df <- d1
# 	
#   if(length(q2) > 0)
#   {
# 	d2 <- data.frame(q1$title, q1[["Distraktor 2"]], c(2))
# 	d2 <- rename(d2, c("q1.title" = "title", "q1...Distraktor.2..." = "share", "c.2."="Distraktor"))
# 	df <- rbind(df, d2)
#   }
#   if(length(q3) > 0)
#   {
# 	d3 <- data.frame(q2$title, q2[["Distraktor 3"]], c(3))
# 	d3 <- rename(d3, c("q2.title" = "title", "q2...Distraktor.3..." = "share", "c.3."="Distraktor"))
# 	df <- rbind(df, d3)
#   }
#   if(length(q4) > 0)
#   {
# 	d4 <- data.frame(q3$title, q3[["Distraktor 4"]], c(4))
# 	d4 <- rename(d4, c("q3.title" = "title", "q3...Distraktor.4..." = "share", "c.4."="Distraktor"))
# 	df <- rbind(df, d4)
#   }
  
#   df <- df[order (df[["title"]], df[["share"]]),]
#   df[["order"]] <- c(1:3)
#   testdf <- df
  
#   d1 <- dPlot(
# 	x = "share", 
# 	y = "title", 
# 	groups = "Distraktor", 
# 	data = df, 
# 	type = 'bar',
# 	barGap = 0.4,
# 	bounds = list(x=850,width = 1000, height = 600),
# 	heights=500)
# test <- d1
# 
#   
#   d1$xAxis(type = "addPctAxis")
#   d1$yAxis(type = "addCategoryAxis")
#   d1$set(width = 2000, height = 1000)
#   d1$save(paste("./output/", exam$title, " - Distraktorenanalyse.html", sep=""), cdn = T)
# }