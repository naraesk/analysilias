source("src/helper.R") # don't call any of these functions
source("src/functions.R")

# There exist two ways for import data (see below)
# In both cases, three objects are created:
# users:		data of students (ID, points, mark, answers)
# questions:	ID, title, solution of the questions, and some metrics.
# exam: 		general information about the exam (title, average points, ... )
# All further functions will access these objects.

# Variant 1: Import Data from ILIAS export file and related pool
# Just download the export file of your exam from ILIAS and place the zip file into the input folder
import()
# load("data/Pool OOGP.RData")

# Variant 2:: Import of an already converted exam
# import("SP WS 1213")

# calculate some metrics (ITC, difficulty of item, distractor analysis) and marks
# TODO: calculation of marks has to be more flexible
analyse()

# create some diagrams into output folder
# Scatter plot (x: difficulty, y: ITC)
# Histogram of distribution of difficulty
# Diagram for distractor analysis

printGraphs()

# The serialize function does several things:
# – save the relevant objects as RData into the data folder
# — create csv file with user id and mark
# TODO: print csv with overview of marks and points

serialise() 

pool[["type"]] <- "F"

# The pool contains all questions of your ILIAS pool
# addToPool adds the analysis results to this pool to enable an overall analysis
# removeFromPool does the opposite, e.g. when added analysis was incorrect

addToPool()
# removeFromPool()

# TODO: serialise Pool
save(pool, file="data/Pool EVA.RData")

cleanup()

test <- da