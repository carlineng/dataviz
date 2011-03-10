library(XML)

prefix = "http://www.whatwepayfor.com/api/"

getBudgetAccount <- function(year="", income="", exacttax="", fn="", subfn="", agency="", bureau="", account="", adjInflYear="", type="", sortby="", sortdir="", filing="", selfEmployed="", showExtra="", showChange="") {

   # Test usage of this function:
   # test = getBudgetAccount(year=2009, income=50000, agency=3815)

   # Generate the API query:
   query = paste(prefix,"getBudgetAccount?",sep="")

   params <- list("year"=year,"income"=income,"exacttax"=exacttax,"function"=fn,
   "subfunction"=subfn,"agency"=agency,"bureau"=bureau,"account"=account,
   "adjustInflationYear"=adjInflYear,"type"=type,"sortby"=sortby,
   "sortdir"=sortdir,"filing"=filing,"selfEmployed"=selfEmployed,
   "showExtra"=showExtra,"showChange"=showChange)

   arguments = pasteArgs(params)
   query = paste(query, arguments, sep="")

   # Get the result of the query
   xmlResult <- xmlRoot(xmlTreeParse(query))
         
   # Get attribute values from the api call
   result <- xmlToDataframe(xmlResult)
	    
   return(result)
}

getBudgetAggregate <- function(year="", income="", exacttax="", fn="", subfn="", agency="", bureau="", account="", adjInflYear="", type="", sortby="", sortdir="", filing="", selfEmployed="", showExtra="", showChange="", group="") {

   # Test usage of this function:
   # test = getBudgetAggregate(year=2009, income=50000, agency=3815)

   # Generate the API query:
   query = paste(prefix,"getBudgetAggregate?",sep="")

   params <- list("year"=year,"income"=income,"exacttax"=exacttax,"function"=fn,
   "subfunction"=subfn,"agency"=agency,"bureau"=bureau,"account"=account,
   "adjustInflationYear"=adjInflYear,"type"=type,"sortby"=sortby,
   "sortdir"=sortdir,"filing"=filing,"selfEmployed"=selfEmployed,
   "showExtra"=showExtra,"showChange"=showChange, "group"=group)

   arguments = pasteArgs(params)
   query = paste(query, arguments, sep="")

   # Get the result of the query
   xmlResult <- xmlRoot(xmlTreeParse(query))
         
   # Get attribute values from the api call
   result <- xmlToDataframe(xmlResult)
	    
    return(result)
}

getReceiptAccount <- function(year="", exacttax="", categ="", subcateg="", agency="", bureau="", account="", adjInflYear="", type="", sortby="", sortdir="", filing="", selfEmployed="", showExtra="", showChange="") {

   # Test usage of this function:
   # test = getBudgetAccount(year=2009, income=50000, agency=3815)

   # Generate the API query:
   query = paste(prefix,"getReceiptAccount?",sep="")
   params <- list("year"=year,"exacttax"=exacttax,"category"=categ,
   "subcategory"=subcateg,"agency"=agency,"bureau"=bureau,"account"=account,
   "adjustInflationYear"=adjInflYear,"type"=type,"sortby"=sortby,
   "sortdir"=sortdir,"filing"=filing,"selfEmployed"=selfEmployed,
   "showExtra"=showExtra,"showChange"=showChange)

   arguments = pasteArgs(params)
   query = paste(query, arguments, sep="")

   # Get the result of the query
   xmlResult <- xmlRoot(xmlTreeParse(query))
         
   # Get attribute values from the api call
   result <- xmlToDataframe(xmlResult)
	    
   return(result)
}

getReceiptAggregate <- function(year="", exacttax="", categ="", subcateg="", agency="", bureau="", account="", adjInflYear="", type="", sortby="", sortdir="", filing="", selfEmployed="", showExtra="", showChange="", group="") {
   # Test usage of this function:
   # test = getBudgetAccount(year=2009, income=50000, agency=3815)

   # Generate the API query:
   query = paste(prefix,"getReceiptAggregate?",sep="")

   params <- list("year"=year,"exacttax"=exacttax,"category"=categ,
   "subcategory"=subcateg,"agency"=agency,"bureau"=bureau,"account"=account,
   "adjustInflationYear"=adjInflYear,"type"=type,"sortby"=sortby,
   "sortdir"=sortdir,"filing"=filing,"selfEmployed"=selfEmployed,
   "showExtra"=showExtra,"showChange"=showChange,"group"=group)

   arguments = pasteArgs(params)
   query = paste(query, arguments, sep="")
   # Get the result of the query
   xmlResult <- xmlRoot(xmlTreeParse(query))
   # Get attribute values from the api call
   result <- xmlToDataframe(xmlResult)
   return(result)
}

getPopulation <- function(startYear, endYear=startYear) {
   query = paste(prefix,"getPopulation?",sep="")
   years = startYear:endYear
   result <- getYearsFrame(query, years)
   return(result)
}

getGDP <- function(startYear, endYear=startYear) {
   query = paste(prefix,"getGDP?",sep="")
   years = startYear:endYear
   result <- getYearsFrame(query, years)
   return(result)
}

getDebt <- function(startYear, endYear=startYear) {
   query = paste(prefix,"getDebt?",sep="")
   years = startYear:endYear
   result <- getYearsFrame(query,years)
   return(result)
}

getTaxRates <- function(startYear, endYear=startYear, type="") {
   query = paste(prefix,"getTaxRates?",sep="")
   if(nchar(type) > 0) {
      query = paste(query,"type=",type,"&",sep="")
   }
   years = startYear:endYear
   result <- getYearsFrame(query,years)
   return(result)
}

getInflation <- function(startYear, endYear=startYear) {
   query = paste(prefix,"getInflation?",sep="")
   years = startYear:endYear
   result <- getYearsFrame(query,years)
   return(result)
}

getYearsFrame <- function(query, years) {
   result = data.frame()
   for(i in years) {
      thisQuery = paste(query, "year=", i, sep="")
      xmlResult <- xmlRoot(xmlTreeParse(thisQuery))
      newResult <- yearToFrame(xmlResult, i)
      result <- rbind(result,newResult)
   }
   return(result)
}

pasteArgs <- function(params) {
   arguments = ""
   for(name in names(params)) {
      if(nchar(params[[name]]) > 0) {
      # If the argument isn't empty, add it to the arg list
      arguments = paste(arguments, name, "=", params[[name]], "&", sep="")
      }
   }
   # Remove the trailing '&'
   arguments = substr(arguments, 1, nchar(arguments) - 1)
   return(arguments)
}

yearToFrame <- function(xmlDoc, year) {
   result <- xmlSApply(xmlDoc, function(x) xmlAttrs(x))
   result <- t(result)
   row.names(result) <- year
   result <- as.data.frame(result)
   return(result)
}

xmlToDataframe <- function(xmlDoc) {
   result <- xmlSApply(xmlDoc, function(x) xmlAttrs(x))
   result <- t(result)
   row.names(result) <- 1:nrow(result)
   result <- as.data.frame(result)
   return(result)
}
