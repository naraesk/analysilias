analyze <- function()
{
	score <<- score.multiple.choice(questions[["solution"]], users[8:ncol(users)], totals = TRUE, score = TRUE, short = FALSE, missing = FALSE)

	questions["Trennschärfe"] 		<<- score[["item.stats"]][["r"]]
	questions["Schwierigkeitsindex"] 	<<- score[["item.stats"]][["mean"]]
	questions["Standardabweichung"] 	<<- score[["item.stats"]][["sd"]]
	lapply(c(0:exam[["numberOfAlternatives"]]), function (x) questions[[paste("Distractor", x)]] <<- score[["item.stats"]][[as.character(x)]])
}

