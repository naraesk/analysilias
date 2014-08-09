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

analyze <- function()
{
	score <<- score.multiple.choice(questions[["solution"]], users[8:ncol(users)], totals = TRUE, score = TRUE, short = FALSE, missing = FALSE)

	questions["Trennschärfe"] 		<<- score[["item.stats"]][["r"]]
	questions["Schwierigkeitsindex"] 	<<- score[["item.stats"]][["mean"]]
	questions["Standardabweichung"] 	<<- score[["item.stats"]][["sd"]]
	lapply(c(0:exam[["numberOfAlternatives"]]), function (x) questions[[paste("Distractor", x)]] <<- score[["item.stats"]][[as.character(x)]])
}