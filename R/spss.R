library(foreign)
write.spss.svy <- function(x,dat,sps){
  df <- as.data.frame(lapply(x,function(c)
    structure(c,class=class(c)[!class(c)%in%c("AsIs","svq")])),
    stringsAsFactors=FALSE)
  write.foreign(df,dat,sps,"SPSS")
  invisible(df)
}