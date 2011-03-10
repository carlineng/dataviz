source("queryAPI.R")

library(plyr)
library(ggplot2)

load("scratch.RData")

# Scratchwork just to play with the data
# Data ranges from 1984-2010

# Most granular level of data
budgetAcct <- getBudgetAccount()
# Aggregate spending levels
budgetAgg <- getBudgetAggregate()

receiptAcct <- getReceiptAccount()
receiptAgg <- getReceiptAggregate()

# Find uniques:

# account, agency, bureau
acct.unique <- unique(budgetAcct["account"]) # 1580 unique vals
agency.unique <- unique(budgetAcct["agency"]) # 26 unique vals
bureau.unique <- unique(budgetAcct["bureau"]) # 326 unique vals
# function, subfunction
fn.unique <- unique(budgetAcct["function"])
subfn.unique <- unique(budgetAcct["subfunction"])
# accountID, agencyID, bureauID, functionID, subfunctionID
acct.id.unique <- unique(budgetAcct["accountID"])
agency.id.unique <- unique(budgetAcct["agencyID"])
bureau.id.unique <- unique(budgetAcct["bureauID"])
fn.id.unique <- unique(budgetAcct["function"])
subfn.id.unique <- unique(budgetAcct["subfunction"])
# category, subcategory
cat.unique <- unique(receiptAcct["category"])
subcat.unique <- unique(receiptAcct["subcategory"])
# Get pop/GDP/debt/inflation
population <- getPopulation(startYear=1984, endYear=2010)
GDP <- getGDP(startYear=1984, endYear=2010)
debt <- getDebt(startYear=1984, endYear=2010)
inflation <- getInflation(startYear=1984, endYear=2010)

# getTaxRates() function is broken. Need to split it into 2 parts.
# 84-93 has 11 columns, 94-10 has 10 columns
taxRates.84.93 <- getTaxRates(startYear=1984, endYear=1993, type=0)
taxRates.84.93 <- rbind(taxRates.84.93,getTaxRates(startYear=1984,endYear=1993,type=1))
taxRates.84.93 <- rbind(taxRates.84.93,getTaxRates(startYear=1984,endYear=1993,type=2))
taxRates.84.93 <- rbind(taxRates.84.93,getTaxRates(startYear=1984,endYear=1993,type=3))

taxRates.94.10 <- getTaxRates(startYear=1994, endYear=2010, type=0)
taxRates.94.10 <- rbind(taxRates.94.10,getTaxRates(startYear=1994,endYear=2010,type=1))
taxRates.94.10 <- rbind(taxRates.94.10,getTaxRates(startYear=1994,endYear=2010,type=2))
taxRates.94.10 <- rbind(taxRates.94.10,getTaxRates(startYear=1994,endYear=2010,type=3))

