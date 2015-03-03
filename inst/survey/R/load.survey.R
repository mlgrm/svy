library(xlsx)
load.survey <- function(form,data){
  svy <- read.xlsx(form,sheetName="survey")
  chc <- read.xlsx(form,sheetName="choices")
  data <- read.csv(data)
  svy <- svy[!is.na(recs$type),]
  gname <- NULL
  names
  apply(svy,1,function(rec){
    switch(rec$type
           "begin group"={
             gname <<- rec$name
             
           })
    
  })
}