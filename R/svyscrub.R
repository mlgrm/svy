library(jsonlite)

# readDat <- function(data,form,
#                     language="English",
#                     sep="."){
#   dat <- read.csv(data,na.strings=c("n/a","NA","skip",""))
#   code <- fromJSON(form)
#   .digest(code,dat,language,sep=sep)
# }
#
# .digest <- function(code,dat,language,prefix="",sep){
#   label=code$label[[language]]
#   if(prefix!="") prefix <- paste0(prefix,sep,code$name) else prefix <- code$name
#   colname <- prefix
#
#   browser(expr=(code$name=="A"))
#   dat <- switch(
#     code$type,
#     # if it's a survey, just reapply to each row
#     survey=by(code$children,code$children$name,
#               .digest,dat,language,prefix="",sep,simplify=FALSE),
#
#     # if it's a group, update the prefix and get the children
#     group=by(code$children[[1]],code$children[[1]]$name,
#              .digest,dat,language,prefix,sep,simplify=FALSE),
#
#     # we'll do these later
#     start=rep(NA,ncol(dat)),
#     end=rep(NA,ncol(dat)),
#     today=rep(NA,ncol(dat)),
#     deviceid=rep(NA,ncol(dat)),
#     subscriberid=rep(NA,ncol(dat)),
#     imei=rep(NA,ncol(dat)),
#     phonenumber=rep(NA,ncol(dat)),
#
#     # native types
#     note=rep(NA,ncol(dat)),
#     integer=as.numeric(dat[,colname]),
#     date=as.Date(dat[,colname]),
#     text=as.character(dat[,colname]),
#     "select one"={
#       r <- factor(
#         dat[,colname],
#         levels=code$children[[1]]$name,
#         labels=code$children[[1]]$label[[language]]
#       )
#     },
#     "select all that apply"={
#       r <- as.logical(
#         as.matrix(
#           dat[,paste(prefix,
#                      code$children[[1]]$label[[language]],
#                      sep=sep)]
#         )
#       )
#     },
#     calculate=as.numeric(dat[,colname]),
#     stop(paste("unrecognized type in json file:",code$type))
#   )
#
#   class(dat) <- c(code$type,"survey",class(dat))
#
#   attributes(dat) <- list(
#     name=code$name,
#     label=label
#     )
#
#   dat
#
#
# }

load.svy <- function(data,form=sub("_[0-9_]+.csv",".json", data),
                     update.fun=identity){
  form <- fromJSON(form)
  dat <- cleandat(read.csv(data,na.strings=c("n/a","NA","","skip")))
  dat <- update.fun(dat)
  dat <- cleandat(dat)
  dat <- extract(form$children,dat)
  dat <- as.data.frame(dat, stringsAsFactors=FALSE)
#   colnames(dat) <- sapply(dat,function(c){
#     lbl <- attributes(c)$label
#     if(is.null(lbl)) attributes(c)$name else getlabels(c)
#   })
  dat
}

extract <- function(df,dat,group=NULL){
  nm <- factor(df$name,levels=df$name)
  l <- by(df,nm,function(r){
    name <- r$name
    if(r$type=="group")
      return(extract(r$children[[1]],dat,group=c(group,r$name)))
    fn <- make.names(paste("extract",r$type))
    if(exists(fn, mode="function"))
      f <- match.fun(fn) else f <- extract.unknown
    c <- f(r,dat,group=group)
#     browser(expr=r$type=="text")
    attributes(c) <- c(attributes(c),r,list(group=group))
    list(c)
  },
  simplify=FALSE)
  do.call(c, l)
}

getcol <- function(name,dat,group=NULL){
  cn <- paste(c(group,name), collapse=getOption("odksvy.separator","."))
  if(cn %in% colnames(dat)) dat[,cn] else rep(NA,nrow(dat))
}

getlabels <- function(l,lang=getOption("odksvy.default.lang","English")){
  if(is.null(l)) return("")
  if(is.character(l) && length(l)==1) return(l)
  if(is.data.frame(l)) return(l[[lang]])
  if(is.list(l)) return(sapply(l,function(e) if(is.list(e)) e[[lang]] else e))
  getlabels(attributes(l)$label)
}

getlabel <- function(...) getlabels(...)[1]

extract.unknown <- function(r,dat,group=NULL){
  cn <- paste(c(group,r$name),collapse=getOption("odksvy.separator","."))
  if(cn %in% colnames(dat)) dat[,cn] else rep(NA,nrow(dat))
}

extract.text <- function(r,dat,group=NULL){
  as.character(getcol(r$name,dat,group))
}

extract.date <- function(r,dat,group=NULL){
  col <- getcol(r$name,dat,group)
  if(is.factor(col)) as.Date(levels(col)[col]) else
    as.Date(col)
}

extract.time <- function(r,dat,group=NULL){
  col <- getcol(r$name,dat,group)
  if(is.factor(col)) as.POSIXct(levels(col)[col]) else
    as.POSIXct(col)
}

extract.integer <- function(r,dat,group=NULL){
  col <- getcol(r$name,dat,group)
  if(is.factor(col)) as.integer(levels(col)[col]) else
    as.integer(col)
}

extract.numeric <- function(r,dat,group=NULL){
  col <- getcol(r$name,dat,group)
  if(is.factor(col)) as.numeric(levels(col)[col]) else
    as.numeric(col)
}


extract.select.one <- function(r,dat,group=NULL,
                               lang=getOption("odksvy.default.lang","English")){
  col <- getcol(r$name,dat,group)
  lbl <- getlabels(r$children[[1]][["label"]],lang)
  factor(col, levels=r$children[[1]]$name, labels=lbl, ordered = TRUE)
}

extract.select.all.that.apply <-
  function(r,dat,group=NULL,
           lang=getOption("odksvy.default.lang","English")){
  nm <- r$children[[1]]$name
  cn <- paste(paste(c(group,r$name),collapse=getOption("odksvy.separator",".")),
              nm, sep=getOption("odksvy.separator","."))
  mat <- as.matrix(dat[,cn])
  colnames(mat) <- NULL
  attr(mat,"choices") <- getlabels(r$children[[1]]$label,lang)
  I(mat)
}

extract.today <- extract.date
extract.start <- extract.time
extract.end <- extract.time
extract.deviceid <- extract.text
extract.imei <- extract.text
extract.note <- extract.text
extract.calculate <- extract.text
extract.decimal <- extract.numeric

tree <- function(l,prefix=NULL){
  if(class(l)[1]=="list")
    invisible(mapply(tree,l,lapply(names(l),function(n)c(prefix,n)))) else
      cat(paste(prefix,collapse=":"),class(l),"\n")
}

summary.svy <- function(dat,lang=getOption("odksvy.default.lang","English")){
  attr.str <- function(obj,a){
    r <- attr(obj,a)
    str_or_empty(r)
  }
  str_or_empty <- function(r)if(is.null(r)) "" else r[1]

  s <- list()
  s$short <- sub("^.*\\.([^\\.]+)$","\\1",colnames(dat))
  s$label <- sapply(dat,getlabel)
  s$groups <- sapply(dat,function(c)paste(attr(c,"group"),collapse=", "))
  s$name <- sapply(dat,attr.str,"name")
  s$type <- sapply(dat,attr.str,"type")
  s$class <- sapply(dat,attr.str,"class")
  s$summary <- sapply(dat,function(c){
    s <- summary(c)
    paste(names(s),s,sep=": ",collapse="; ")
  })
  s$colname <- colnames(dat)

  as.data.frame(s,stringsAsFactors = FALSE, row.names=1:ncol(dat))
}

attributes.svy <- function(s)lapply(s,attributes)
apply.attr.svy <- function(a,s){
  a <- lapply(a,function(a1){if(!is.null(a1$dim))a1$dim[1] <- nrow(s);a1})
  as.data.frame(mapply(function(a1,s1){attributes(s1) <- a1;s1},a,s))
}

apply.attr.svyq <- function(a,q){
  attributes(q) <- sapply(names(a),
                            function(n){
                              if(is.null(attributes(q)[[n]])) a[[n]] else
                                attributes(q)[[n]]
                            },
                            simplify=FALSE, USE.NAMES=TRUE)
  q
}

pres.svy <- function(s,f,...){
  a <- attributes.svy(s)
}

pres.svyq <- function(q,f,...){
  a <- attributes(q)
  res <- f(q,...)
  attributes(res) <- sapply(names(a),
                            function(n)
                              if(is.null(attributes(res)[n])) a[[n]] else
                                attributes(res)[[n]],
                            simplify=FALSE, USE.NAMES=TRUE)
  res
}

as.svy <- function(dat,tmp){
  att <- attributes.svy(tmp)
  as.data.frame(mapply(apply.attr.svyq,att,dat))
}

as.svyq <- function(col, label, ...) switch(
  class(col)[1],
  integer=as.svyq.integer(col,label,...),
  numeric=as.svyq.numeric(col,label,...),
  factor=as.svyq.factor(col,label,...),
  ordered=as.svyq.factor(col,label,...),
  stop("unrecognized vector class")
  )

as.svyq.factor <- function(col, label, choices=levels(col)){
  ordered(col,levels=choices)
  attributes(col)$label <- label
  attributes(col)$type <- "select one"
  col
}

as.svyq.numeric <- function(col, label){
  attributes(col)$label <- label
  attributes(col)$type <- "decimal"
  col
}

as.svyq.integer <- function(col, label){
  attributes(col)$label <- label
  attributes(col)$type <- "integer"
  col
}
