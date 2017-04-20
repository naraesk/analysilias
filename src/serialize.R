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

serialize <- function () {
	path <- paste("data/", exam[["title"]], ".RData", sep="")
	save(users, questions, exam, file=path)
	
	backupPath <- paste(exam[["outputPath"]], "backup/", sep="")
	dir.create(backupPath, showWarnings = FALSE)
	file.rename(zippath, paste(backupPath, zipfile, sep=""))
	file.rename(csvpath, paste(backupPath, csvfile, sep=""))
	
# 	generate list of grades
	write.csv2(users[, c("Matrikel", "mark")], file=paste(exam[["outputPath"]], "List of Grades", ".csv", sep=""),row.names=FALSE)

# 	generate grading schema
	write.csv2(marks[, c("grades", "score")], file=paste(exam[["outputPath"]], "Grading Schema.csv", sep=""), row.names=FALSE)
	
#	generate general information
	infoFile<-file(paste(exam[["outputPath"]], "Information.txt", sep=""), 'w')
	write(paste("Number of participants: ", nrow(users), sep=""), infoFile)
	write(paste("Failure rate: ", exam[["rate"]], sep=""), infoFile, append = TRUE)
	write(paste("Grade point average: ", exam[["mean"]], sep=""), infoFile, append = TRUE)
	write(paste("Standard deviation: ", exam[["sd"]], sep=""), infoFile, append = TRUE)
	write(paste("Score average: ", exam[["meanScore"]], sep=""), infoFile, append = TRUE)
	write(paste("Standard deviation: ", exam[["scoreSd"]], sep=""), infoFile, append = TRUE)
	write(paste("Corrected questions: ", correctedQuestions, sep=""), infoFile, append = TRUE)
	close(infoFile)
	
	file.copy("README", exam[["outputPath"]])
	
	message("List of grades and grading schema have been written successfully.")
}
