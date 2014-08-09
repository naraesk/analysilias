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

source("src/ilias.R")

grades 	   <- c(1.0,  1.3,  1.7,  2.0,  2.3,  2.7,  3.0,  3.3,  3.7,  4.0)
percentage <- c(0.87, 0.75, 0.67, 0.58, 0.50, 0.42, 0.33, 0.25, 0.12, 0)
marks 	   <- data.frame(grades, percentage)

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
analyse("22%")

# create some diagrams into output folder
# Scatter plot (x: difficulty, y: ITC)
# Histogram of distribution of difficulty
# Diagram for distractor analysis

generateGraphs()

# The serialize function does several things:
# – save the relevant objects as RData into the data folder
# — create csv file with user id and mark
# TODO: print csv with overview of marks and points

serialize()

# The pool contains all questions of your ILIAS pool
# addToPool adds the analysis results to this pool to enable an overall analysis
# removeFromPool does the opposite, e.g. when added analysis was incorrect

# addToPool()
# removeFromPool()

# TODO: serialise Pool
# save(pool, file="data/Pool EVA.RData")

cleanup()