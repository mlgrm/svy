library(XML)
library(RCurl)
ns <- c(
  x='http://www.w3.org/2005/Atom',
  openSearch='http://a9.com/-/spec/opensearchrss/1.0/',
  batch='http://schemas.google.com/gdata/batch',
  gs='http://schemas.google.com/spreadsheets/2006'
)

ns2 <- ns
names(ns2)[1] <- ""

xmlns <- ns

names(xmlns) <- paste0("xmlns:",names(ns))
names(xmlns)[1] <- "xmlns"


txt <- getURI(paste0(sh@cellsfeed,""), curl=sh@connection, 
              .opts=list(httpheader=c(Authorization=paste0("GoogleLogin auth=",
                                                           sh@connection@auth))))

x <- xmlRoot(xmlParse(txt,asText=TRUE))
entries <- getNodeSet(x,"//x:entry",ns)




feed <- newXMLNode("feed",namespace=ns2)
addChildren(feed,x[["id"]])
entry <- newXMLNode("entry",parent=feed)
newXMLNode()

xml.print

# addChildren(t$value(),xmlNode("id",xmlTextNode("http://barratt.pro")))
