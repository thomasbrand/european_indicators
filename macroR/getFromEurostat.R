library(gdata)
library(lubridate)
library(reshape)

convertToDate <- function(data) {
  x <- as.character(data[,"time"])
  if ( nchar(x[1]) == 4) {
    yr <- as.numeric(x)
    t <- origin + years(yr-1970)
  }
  if ( nchar(x[1]) == 6) {
    yr <- as.numeric(substr(x,1,4))
    mon <- (as.numeric(substr(x,6,6))-1)*3
    t <- origin + years(yr-1970) + months(mon)
  }
  data[,"time"] <- t
  data
}

#var<-"ilc_li02"

getFromEurostat <- function(var) {
  url <- paste("http://epp.eurostat.ec.europa.eu/NavTree_prod/everybody/BulkDownloadListing?sort=1&downfile=data%2F",var,".tsv.gz",sep="")
  #  download.file(url, "savedFromEurostat.tsv.gz")
  u <- url
  conn <- gzcon(url(u))
  raw <- textConnection(readLines(conn))
  close(conn)
  data <- read.table(raw, header=TRUE, sep="\t", na.strings=": ", dec=".", stringsAsFactors=FALSE)
  close(raw)
  # Convert ": " to NA's
  data[data==": "] <- NA
  # Remove X from dates
  names(data) <- sub("X","",names(data))
  
  for ( col in 2:length(names(data)) ) {
    data[,col] <- sub("f","",(data[,col]))
    data[,col] <- as.numeric(data[,col])
  }
  
  # Keep only country abbreviation
  #print(data[1,1])
  data[,1] <- trim(data[,1])
  #print(data[1,1])
  data[,1+length(names(data))] <- paste(var)
  #substr(data[,1],start=1,stop=nchar(data[,1])-2) #cr?ation d'une nouvelle colonne
  data[,1] <- substr(data[,1],start= nchar(data[,1])-1, stop=nchar(data[,1]) )  
  #print(data[1,1])
  if (var=="tipsfs10") data<-data[duplicated(data[,1]),] else data<-data[!duplicated(data[,1]),]
  names(data)[1] <- "country"
  names(data)[length(names(data))] <- "variable"
  
  data <- melt(data, variable_name="time")
  convertToDate(data)
  }
