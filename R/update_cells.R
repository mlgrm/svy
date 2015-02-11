options(svy.entry.template=paste(readLines(file("data/entry_template.xml")),
                                 collapse="\n"))

options(svy.feed.template=paste(readLines(file("data/feed_template.xml")),
                                collapse="\n"))

updateCells <- function(sheet, vals) {
  h <- basicTextGatherer()
  curlPerform(
    url=paste0(sheet@cellsfeed,"/batch"),
    postfields=newFeed(sheet,vals),
    writefunction=h$update,
    httpheader=c(
      Authorization=paste0("GoogleLogin auth=", sheet@connection@auth),
      "Content-Type"="application/atom+xml",
      curl=sheet@connection,
      verbose=TRUE
    )
  )
  h$value()
}

newEntry <- function(sheet,r,c,val,id){
  fmt <- getOption(
    "svy.entry.template",
    paste(readLines(file("data/entry_template.xml")),collapse="\n"))
  cellurl <- sprintf("%s/R%dC%d",sheet@cellsfeed,r,c)
  sprintf(fmt,id,cellurl,cellurl,r,c,val)
}

newFeed <- function(sheet,vals,ids=rownames(vals)){
  fmt <- getOption(
    "svy.feed.template",
    paste(readLines(file("data/feed_template.xml")),collapse="\n"))
  entries <- mapply(newEntry,r=vals$r,c=vals$c,val=vals$val,id=ids,
                    MoreArgs = list(sheet=sheet))
  sprintf(fmt,sheet@cellsfeed,paste(entries,collapse=""))
}