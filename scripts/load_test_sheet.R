
options(GoogleAuth=getGoogleAuth("rauth@barratt.pro","...",service="wise"))
options(GoogleDocsConnection=getGoogleDocsConnection(getOption("GoogleAuth")))
con <- getOption("GoogleDocsConnection")
wb <- getWorkbook("unhcr")
sh <- getWorksheets(wb,con)[["types"]]
df <- getSheet(wb,"simple")
