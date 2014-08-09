
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