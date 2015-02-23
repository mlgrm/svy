library(RGoogleDocs)
source("R/getdocs.R")

options(GoogleAuth=getGoogleAuth(readRDS("~/svy/data/passwd")["login"],
                                 readRDS("~/svy/data/passwd")["passwd"],
                                 service="wise"))
options(GoogleDocsConnection=getGoogleDocsConnection(getOption("GoogleAuth")))
con <- getOption("GoogleDocsConnection")
wb <- getWorkbook("unhcr")
sh <- getWorksheets(wb,con)[["types"]]
df <- getSheet(wb,"simple")
