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

serialize <- function ()
{
	path <- paste("data/", exam[["title"]], ".RData", sep="")
	save(users, questions, exam, file=path)
	
	generateGradingSchema()
	generateListOfGrades()
}

generateListOfGrades <- function ()
{
	write.csv2(users[, c("Pruefungsnummer", "mark")], file=paste("output/Notenliste ", exam[["title"]], ".csv", sep=""),row.names=FALSE)
	message("list of grades was written successfully")
}

generateGradingSchema <- function()
{
	write.csv2(marks[, c("grades", "score")], file=paste("output/", exam[["title"]], " - Notenschlüssel.csv", sep=""), row.names=FALSE)
	message("Notenschlüssel wurde erfolgreich geschrieben")
}