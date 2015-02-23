# getCurlOptionsConstants()
txt <- getURI(sh@cellsfeed, curl=sh@connection, 
              .opts=list(httpheader=c(Authorization=paste0("GoogleLogin auth=",
                                                           sh@connection@auth))))

txt <- getURI(paste0(sh@cellsfeed,"?empty=true"), curl=sh@connection, 
              .opts=list(httpheader=c(Authorization=paste0("GoogleLogin auth=",
                                                           sh@connection@auth))))



# txt <- sub("\\sxlmns="," xmlns:=",txt)

x <- xmlParse(txt,asText=TRUE)

# entries <- x$children$feed$children[names(x$children$feed$children)=="entry"]

getCellVals <- function(sheet)
  as.data.frame(sapply(c(row="row",col="col",val="inputValue"),function(a){
    v <- unlist(getNodeSet(sheet,paste0("//gs:cell/@",a)))
    all.is.numeric(v,what="vector")
  }))



edf <- xmlToDataFrame(entries)

sapply(entries,function(e)c(split(sub(".*")))

saveXML(sh.xml,"data/sh.xml")
