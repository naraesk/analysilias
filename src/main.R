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

grades 	   <- c(1.0,  1.3,  1.7,  2.0,  2.3,  2.7,  3.0,  3.3,  3.7,  4.0)
percentage <- c(0.95, 0.9, 0.85, 0.8, 0.75, 0.7, 0.65, 0.6, 0.55, 0.5)

source("src/ilias.R")

# Variant 1: Import Data from ILIAS export file and related pool
# Just download the export file of your exam from ILIAS and place the zip file into the input folder
import()

correct("13564")

# Variant 2:: Import of an already converted exam
# loads the appropriate RData object from the data subdir
# import("SP WS 1213")

# calculate some metrics (ITC, difficulty of item, distractor analysis)
analyze()

# calculates the grade schema as well es the grades
# if you pass "22%" as an option, the "22% rule" of uni leipzig will be applied.

calcGrades("22%")

# create some diagrams into output folder
# Scatter plot (x: difficulty, y: ITC)
# Histogram of distribution of difficulty
# Diagram for distractor analysis

generateGraphs()

# The serialize function does several things:
# – save the relevant objects as RData into the data folder
# – create csv file with user id and mark
# – create wsv file with grading schema

serialize()

# The pool contains all questions of your ILIAS pool
# addToPool adds the analysis results to this pool to enable an overall analysis
# removeFromPool does the opposite, e.g. when added analysis was incorrect

# addToPool()
# removeFromPool()

# TODO: serialise Pool
# save(pool, file="data/Pool EVA.RData")

cleanup()