addToPool <- function()
{
	lapply(questions$hash, addQuestionToPool)
}

removeFromPool <- function()
{
	lapply(questions$hash, removeQuestionFromPool)
}

addQuestionToPool <- function (x)
{
	rowPool <<- as.integer(rownames(subset(pool, id==x)))
	rowQuestions <<- as.integer(rownames(subset(questions, hash==x)))
   
	if(!is.integer(rowPool)) { return(NULL) }

	if(questions[["Schwierigkeitsindex"]][rowQuestions] > 0.67)
	{
		pool[["diff_easy"]][rowPool] <<- pool[["diff_easy"]][rowPool] +1
	}
	else if(questions[["Schwierigkeitsindex"]][rowQuestions] <= 1/3)
	{
		pool[["diff_hard"]][rowPool] <<- pool[["diff_hard"]][rowPool] +1
	}
	else pool[["diff_med"]][rowPool] <<- pool[["diff_med"]][rowPool] +1
  
	if(length(questions[["Trennschärfe"]][rowQuestions]) == 0) {return(NULL)}
  
	if(is.na(questions[["Trennschärfe"]][rowQuestions])) { return(NULL)} 
  
	if(questions[["Trennschärfe"]][rowQuestions] > 0.3)
	{
		pool[["ITC_high"]][rowPool] <<- pool[["ITC_high"]][rowPool] +1
	}
	else if (questions[["Trennschärfe"]][rowQuestions] <= 0)
	{
		pool[["ITC_neg"]][rowPool] <<- pool[["ITC_neg"]][rowPool] +1
	}
	else pool[["ITC_low"]][rowPool] <<- pool[["ITC_low"]][rowPool] +1
}

removeQuestionFromPool <- function(x)
{
	rowPool <- as.integer(rownames(subset(pool, id==x)))
	rowQuestions <- as.integer(rownames(subset(questions, hash==x)))
   
	if(!is.integer(rowPool)) { return(NULL) }

	if(questions[["Schwierigkeitsindex"]][rowQuestions] > 0.67)
	{
		pool[["diff_easy"]][rowPool] <<- pool[["diff_easy"]][rowPool] -1
	}
	else if(questions[["Schwierigkeitsindex"]][rowQuestions] <= 1/3)
	{
		pool[["diff_hard"]][rowPool] <<- pool[["diff_hard"]][rowPool] -1
	}
	else pool[["diff_med"]][rowPool] <<- pool[["diff_med"]][rowPool] -1
  
	if(length(questions[["Trennschärfe"]][rowQuestions]) == 0) {return(NULL)}
  
	if(is.na(questions[["Trennschärfe"]][rowQuestions])) { return(NULL)} 
  
	if(questions[["Trennschärfe"]][rowQuestions] > 0.3)
	{
		pool[["ITC_high"]][rowPool] <<- pool[["ITC_high"]][rowPool] -1
	}
	else if (questions[["Trennschärfe"]][rowQuestions] <= 0)
	{
		pool[["ITC_neg"]][rowPool] <<- pool[["ITC_neg"]][rowPool] -1
	}
	else pool[["ITC_low"]][rowPool] <<- pool[["ITC_low"]][rowPool] -1
}