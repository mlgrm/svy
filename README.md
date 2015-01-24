# svy
tools for importing surveys from ODK and analysing them in R

combine the form specification in json with the csv data output to create a data frame with one column for each question.  also, some automatic summary functions for each question type.

Note: this has already been done for formhub (an ODK gui) by prabhasp with his formhub.R.  his also retrieves the data from the formhub server.  so you should probably look there first.  i couldn't get it to work for my surveys, so here's my go.
