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

# Import downloaded csv and zip file from ILIAS
import()

# Sometimes a question of an exam contains an error

# The question with the Ilias ID 279477 is considered correct for all participants, regardless of their answer.
# correctQuestion("279477")

# Use this, if there are multiple correct answers to a question
# The question with the Ilias ID 279479 io considered correct for all participants who have chosen option 2 or the original correct answer
# The index is 0 based, so valid values are 0, 1,2, and 3!
# The order corresponds to the order of possible answers in the Ilias question pool 
# correctDistractor("279479", 1)

# Use this to set a new correct answer. 
# For answer 3 you now get points, for all other answers not!
# The index is 0 based, so valid values are 0, 1,2, and 3!
# The order corresponds to the order of possible answers in the Ilias question pool 
# changeSolution("279480", 3)

# calculate some metrics (ITC, difficulty of item, distractor analysis)
analyze()

# calculates the grade schema as well es the grades
# if you pass "22%" as an option, the "22% rule" of uni leipzig will be applied.

calcGrades("22%")
#calcGrades()

# create some diagrams into output folder
# Scatter plot
# Histogram of distribution of difficulty
# Diagram for distractor analysis

generateGraphs()

# The serialize function does several things:
# – save the relevant objects as RData into the data folder
# – create csv file with user id and mark
# – create wsv file with grading schema

serialize()

cleanup()
