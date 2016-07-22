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

calcGrades <- function (variant = "no") {
  	exam[["maxScore"]] <<- nrow(questions)
  	if (variant == "22%") {
		percentage <- c(0.87, 0.75, 0.67, 0.58, 0.50, 0.42, 0.33, 0.25, 0.12, 0)
		marks <<- data.frame(grades, percentage)
		exam[["minScore"]] <<- 0.78 * mean(users$score)
# TODO: this only works when every questions has the same weight
		if (exam[["minScore"]] > 0.5 * nrow(questions)) exam[["minScore"]] <<- 0.5 * nrow(questions)
		exam[["relScore"]] <<- exam[["maxScore"]] - exam[["minScore"]]
	} else 	{
		exam[["minScore"]] <<- percentage[10] * exam[["maxScore"]]
		print("falsch")
	}
	marks["score"]	<<- sapply(marks$percentage, calcScores, variant=variant)
	users["mark"]   <<- sapply(users$score, findGrade)
	exam[["mean"]]  <<- round(mean(users[["mark"]]), 1)
	exam[["rate"]]  <<- round(length(users$mark[users[["mark"]] == 5]) / nrow(users), 2)
	exam[["sd"]] 	<<- round(sqrt(var(users["mark"])), 3)
	exam[["meanScore"]] <<- round(mean(users[["score"]]),1)
	exam[["scoreSd"]] <<- round(sqrt(var(users["score"])),3)
	
	message("Grades have been calculated successfully.")
}

calcScores <- function (percentage, variant) {
	if(variant == "22%") {
		return (round(percentage * exam[["relScore"]] + exam[["minScore"]]))
	}
	return (round(percentage * exam[["maxScore"]]))
}

findGrade <- function (score) {
	message("start findGrape")
# 	warning(exam[["minScore"]])
	if(score < round(exam[["minScore"]])) return (5)
	return (min(marks$grades[marks$score<=score]))
}