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

generateGraphs <- function () {
	generateITCGraph()
	generateDistributonOfDifficulty()
	generateDistributionOfGrades()
	generateDistractorGraph()
	
	message("Graphs have been drawn successfully.")
	
	zip(paste("output/", exam[["title"]], ".zip", sep=""), exam[["outputPath"]])
}

generateITCGraph <- function () {
	graph <- nPlot(Trennschärfe ~ Schwierigkeitsindex, group="type", data=questions, type="scatterChart")
	graph$chart(tooltipContent = "#! function(key, x, y, e) {return e.point.title} !#")
	graph$chart(forceY = c(-1, 1))
	graph$chart(color = c("green", "blue", "red"))
	graph$chart(forceX = c(0, 1))
	graph$yAxis(axisLabel = "Item-total correlation")
	graph$xAxis(axisLabel = "Difficulty")
	graph$xAxis(tickValues = c(0, 0.33, 0.67, 1))
	graph$yAxis(tickValues = c(-1, 0, 0.33, 0.67, 1))
	graph$save(paste(exam[["outputPath"]], "Overview.html", sep=""), standalone = TRUE)
}

generateDistributonOfDifficulty <- function () {
	breaks <- c(-0.1, 0.16666, 0.33333, 0.5, 0.66666, 0.83333, 1)
	questions[["Schwierigkeit"]] <<- cut(questions[["Schwierigkeitsindex"]], breaks, c(1:6))
	graphdata <- data.frame(table(questions[["Schwierigkeit"]], questions[["type"]]))
	graphdata$label <- c("[0, 1/6)", "[1/6, 2/6)", "[2/6, 3/6)", "[3/6, 4/6)", "[4/6, 5/6)", "[5/6, 6/6)")
	graph <- nPlot(Freq ~ label, group="Var2", data=graphdata, type="multiBarChart")
	graph$chart(color = c('green', 'blue', 'red'))
	graph$xAxis(axisLabel = "Difficulty")
	graph$save(paste(exam[["outputPath"]], "Distribution of Difficulty.html", sep=""), standalone = TRUE)
}

generateDistributionOfGrades <- function () {
	breaks <- c(1, 1.3, 1.7, 2, 2.3, 2.7, 3, 3.3, 3.7, 4, 5)
	graphdata <- data.frame(table(factor(users[["mark"]], levels=breaks)))
	testgraphdata <<- graphdata
	graphdata$Freq <- graphdata$Freq/sum(graphdata$Freq)*100
	graphdata$label <- c("1", "1.3", "1.7", "2", "2.3", "2.7", "3", "3.3", "3.7", "4", "5")
	colnames(graphdata)[2] <- c("Percent")
	graph <- nPlot(Percent~ Var1, data=graphdata, type="multiBarChart")
	maxY <- max(graphdata$Percent)
	if(maxY < 50) maxY <- 50
	graph$chart(showControls = FALSE)
	graph$chart(reduceXTicks = FALSE)
	graph$chart(showLegend = FALSE)
	graph$yAxis(tickValues = c(0, 10, 20, 30, 40, 50))
	graph$chart(forceY = c(0,maxY))
	graph$yAxis(tickFormat = "#! function(d) {return d + '%'} !#")
	graph$save(paste(exam[["outputPath"]], "Distribution of Grades.html", sep=""), standalone = TRUE)
}

# http://www.jaredlander.com/tag/rcharts/
calculateDistractorShare <- function (x) {

	if(x["Schwierigkeitsindex"] > 0.7) return ()
	id <- x["id"]
	for (i in 0:x["numberOfAlternatives"] ) {
		if(x["solution"] != i) {
			text <- texts[4 * (as.numeric(x["x"]) - 1) + 1]
			share <- sum(answers[id] == i)/(nrow(answers)-sum(answers[id] == x["solution"]))
			if ((share > 0.8) | (share < 0.1)) {
				cat <- i + 5
			} else {
				cat <- i + 1 
			}
			if(share == 0) {
				share <- 0.02
			}
			row <- data.frame(cat, share, x["title"], "bla")
			distractorData  <<- rbind(distractorData , row)
		}
	}
}

generateDistractorGraph <- function () {

	distractorData <<- data.frame(distractor= character(0), share=numeric(0), title = character(0), text = character(0))
	questions["x"] <<- c(1:nrow(questions))
	apply(questions, 1, calculateDistractorShare)
	distractorData <<- rename(distractorData , c("cat" = "distractor", "x..title.." = "title"))
	distractorData <<- distractorData [order (distractorData [["distractor"]]),]
	graph <- dPlot(
		x = "share", 
		y = "title",
		groups = "distractor", 
		data = distractorData, 
		type = 'bar',
		bounds = list(x=850,width = 1000, height = 600),
		heights=500
	)
	graph$xAxis(type = "addPctAxis")
	graph$yAxis(type = "addCategoryAxis", orderRule="title")
	graph$defaultColors("#!d3.scale.ordinal().range(['#0066CC','#00892C','#808080','orange','red','red','red','red']).domain(['1','2','3','4','5','6','7','8'])!#")
	graph$set(width = 2000, height = 1000)
	graph$set(border = 200)

graph$setTemplate(
  afterScript = 
  "<script>
	subChart.series[0].getTooltipText = 
	function (e) {
		var rows = [];
		if (this.categoryFields !== null && this.categoryFields !== undefined && this.categoryFields.length !== 0) {
			this.categoryFields.forEach(function (c, i) {
				if (c !== null && c !== undefined && e.aggField[i] !== null && e.aggField[i] !== undefined) {
					var s = e.aggField[i]
					if (s > 4) {
						s = s - 4
					}
					rows.push(c + (s !== c ? ': ' + s : ''));
				}
			}, this);
		}

		if (this.x) {
			this.x._getTooltipText(rows, e);
		}

		rows.push('Text: ' + lookup[e.cy].text);
		// Get distinct text rows to deal with cases where 2 axes have the same dimensionality
		return rows; //rows.filter(function (elem, pos) { return rows.indexOf(elem) === pos; });
	}
	
	var lookup = {};
		for (var i = 0, len = data.length; i < len; i++) {
			lookup[data[i].title] = data[i];
		}
	</script>")
graph$save(paste(exam[["outputPath"]], "Distractor analysis.html", sep=""), standalone = TRUE)
# 	graph$save(paste(exam[["outputPath"]], "Distractor analysis.html", sep=""), standalone = TRUE)
}