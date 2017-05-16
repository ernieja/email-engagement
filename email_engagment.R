library(data.table)
library(dplyr)
library(plyr)
library(ggplot2)
library(reshape2)


## Prep data for analysis


memory.limit(19000)

eng_raw <- fread("2016PreviousMonths.csv", header = T, sep=",", quote="", showProgress = T, data.table = T, na.strings = c("NULL","",NA,"null"))


# create index column numbering drops in ascending order for each address
setkey(engDT, "email")
setorder(memberEng, email, dropdate) 
memberEng[,n_row := seq_along(dropdate), by = email]


memberEng[,c("ek_email","esp","engagementdate","engagementtime","grossleadrevenueamount")] <- NULL
memberEngDT <- memberEng[tradename != "mortgage"] # mortgage was a third-party drop that we don't want to include


# number engaged drops for each email address  
setkey(memberEngDT,"email")
memberEngDT[, engaged := ifelse(opencount == 1 | clickcount == 1 | leadcount == 1, 1, 0), by = email]
memberEngDT[,engCounter := seq_len(.N)-1, by=.(email,rleid(engaged))]


## reorganize data by drop number


# send counts per drop
drops.freq <- data.table(xtabs(~ n_row, memberEngDT))

# action count per drop
drops.eng <- memberEngDT[, .(sumEng = sum(engaged)), by = n_row]

# lead count per drop
drops.lead <- memberEngDT[, .(sumLead = sum(leadcount)), by = n_row]

# combine tables and calculate action/lead rates
drops <- cbind(drops.freq, drops.eng, drops.lead, actionRate = drops.eng$sumEng / drops.freq$N, 
               leadRate = drops.lead$sumLead/drops.freq$N)
names(drops) <- c("n_row", "sentCount", "dropN", "actionCount", "n_row", "leadCount", "actionRate", "leadRate")
drops[,n_row := NULL]

rm(drops.eng)
rm(drops.freq)
rm(drops.lead)



## find outliers using the standard 1.5 IQR method


sent_outliers <- data.table(sentCount = boxplot.stats(drops$sentCount)$out)
dropsNew <- drops[!sent_outliers, on = "sentCount"]









## materials for knitr


fwrite(drops, "drops_final.csv")

memberEngDT[2:3] <- list(NULL)
memberEngDT[5:10] <- list(NULL)
memberEngDT[5:16] <- list(NULL)
memberEngDT[5:7] <- list(NULL)


memberEngDT[c(3)] <- colwise(as.Date)(memberEngDT[c(3)])
memberEngDT[,c(1,2,4)] <- colwise(as.factor)(memberEngDT[,c(1,2,4)])
summary <- summary(memberEngDT[1:4])


saveRDS(summmary, "summary.rds")
saveRDS(memberEng,"engagement_final.rds")
