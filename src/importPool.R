qtiDoc 	 <- xmlInternalTreeParse("./qti.xml")

solution_raw    <- xpathApply(qtiDoc,"//setvar[.='1']/../displayfeedback", xmlGetAttr, "linkrefid")
solution 	  <- as.integer(lapply(solution_raw, function(x) str_sub(x, -1)))
title 	  <- as.character(xpathApply(qtiDoc, "//item", xmlGetAttr, "title"))

opt0		  <- as.character(xpathApply(qtiDoc, "//response_label[@ident=0]/material/mattext", xmlValue))
opt1		  <- as.character(xpathApply(qtiDoc, "//response_label[@ident=1]/material/mattext", xmlValue))
opt2		  <- as.character(xpathApply(qtiDoc, "//response_label[@ident=2]/material/mattext", xmlValue))
opt3		  <- as.character(xpathApply(qtiDoc, "//response_label[@ident=3]/material/mattext", xmlValue))

comment_raw 	  <- as.character(xpathApply(qtiDoc, "//qticomment", xmlValue))
comment  	  <- as.character(lapply(comment_raw, function(x) str_sub(x, -2)))
type		  <- as.character(lapply(comment, function(string) getType(string)))
diff_tobe	  <- as.character(lapply(comment, function(string) getDifficulty(string)))
ITC_neg = c(0)
ITC_low = c(0)
ITC_high = c(0)
diff_easy = c(0)
diff_med = c(0)
diff_hard = c(0)

opts <- data.frame(title, solution, opt0, opt1, opt2, opt3)
id <- apply(opts, 1, function(x) digest(x))

pool_new <- data.frame(id, title, solution, type, ITC_neg, ITC_low, ITC_high, diff_easy, diff_med, diff_hard, diff_tobe, stringsAsFactors = FALSE)
pool	 <- rbind(pool, pool_new)
print("Daten wurden erfolgreich eingelesen")

getType <- function(string)
{
  if(grepl("F", string)) return ("Fakt")
  if(grepl("A", string)) return ("Anwendung")
  if(grepl("T", string)) return ("Transfer")
}

getDifficulty <- function (string)
{
  if(grepl("L", string)) return ("leicht")
  if(grepl("M", string)) return ("mittel")
  if(grepl("S", string)) return ("schwer")
}

# save(pool, file="data/Pool EI.RData")