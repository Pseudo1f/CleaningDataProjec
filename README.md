---
title: "README"
author: "CourseraStudent"
date: "Sunday, September 27, 2015"
output: html_document
---

To load the data just run (file_path is the path to where you downloaded the file)
     data <- read.table(file_path, header = TRUE)
     View(data)

The run_analysis script takes in seven text files and as output produces a tidy data set
The data come from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
A description of the original data is http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones.

The 7 files used are X_train, y_train, subject_train, X_test, y_test, subject_test and features.
Please see the above link for a full description but basically the data collected is on 30 people doing 
one of six exercises. Many different measurements were taken for each person, exercise pair.
The X data contain those measurements. The y data contain the exercises for each mesurements. The
subject data contain the subjects for each measurement. And the features data tell you the name of each
measurement. Note that participants were just randomly assigned to the train or test group but the structure
of both of these are the same

The script assumes that you have extracted the zip file into your working directory. It then does five things:

1. Merges the train and test data.
     To do this it reads in the files, merges the data on subjects and exercises, and even adds in the
     column variables. Note that this last part may be getting a bit ahead of ourselves as under some
     interpretations this is what is supposed to be done in part 4 but I decided to do it in this step
2. Extracts only measurements for means and standard deviations
     Here I had to make some decisions as to what columns to pick since it wasn't absolutely clear what
     means qualified. I took a wide view and decided to include anything that has "mean"" in the title
3. Renames the exercises to a descriptive name
     The original data only has numbers for each exercise. this is changed to a variable that makes sense
     in Englihs
4. Relabels some of the measurements with better name
     This is a step which is perhaps a bit contentious. As I noted in 1., I actually already brought in the
     column variable names, and I think they are pretty descriptive. That being said, I decided, just for 
     clarity, to rename the "Acc" that shows up in names into "Acceleration". This is probably unnecessary
     but I did it just in case
5. Produce a final tidy data set with just the averages
     Pretty straight forward -- grouping by subject and by exercise I then summarize the data by taking the
     mean of each column. Note that my form of the tidy data set has 180 observations (6 exercises * 
     30 individuals) and 88 variables (which means 86 measurements). Some people may claim that the tidy data
     should have exactly one mean per observation. I won't strongly disagree but I would note that this set
     would be extremely long (and narrow) and for visual purposes I prefer the wider version. Plus, I would
     argue that an observation corresponds to a (person, exercise) pair and not necessarily to a (person, 
     exercise, measurement) triplet.

Finally, credit to David Hood (https://class.coursera.org/getdata-032/forum/thread?thread_id=26) for many 
helpful suggestions for presentation of this readme.