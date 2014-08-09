calcGrades <- function (percentage, variant)
{
  	exam[["maxScore"]] <<- nrow(questions)
  	if (variant == "22%")
  	{
		percentage <- c(0.87, 0.75, 0.67, 0.58, 0.50, 0.42, 0.33, 0.25, 0.12, 0)
		marks <<- data.frame(grades, percentage)
		exam[["minScore"]] <<- mean(users$score) * 0.78
		if (exam[["minScore"]] > nrow(questions) * 0.5) exam[["minScore"]] <<- 20
		exam[["relScore"]] <<- exam[["maxScore"]] - exam[["minScore"]]
	}
	else
	{
		exam[["minScore"]] <<- percentage[10] * exam[["maxScore"]]
		print("falsch")
	}
	marks["score"]     <<- sapply(marks$percentage, calcScores, variant=variant)
	users["mark"]      <<- sapply(users$score, findGrade)
	exam[["mean"]]     <<- mean(users[["mark"]])
	exam[["rate"]]     <<- length(users$mark[users[["mark"]] == 5]) / nrow(users)
	
	message("Noten wurden erfolgreich berechnet")
}

calcScores <- function (percentage, variant)
{
	if(variant == "22%") return (round(percentage * exam[["relScore"]] + exam[["minScore"]]))
	return (round(percentage * exam[["maxScore"]]))
}

findGrade <- function (score)
{
	if(score < exam[["minScore"]]) return (5)
	return (min(marks$grades[marks$score<=score]))
}