library(data.table)
library(dplyr)
library(plyr)
library(ggplot2)
library(reshape2)


## Prep data for analysis


memory.limit(19000)


eng.raw <- fread("all_engagement_final.csv")

# create index column numbering drops in ascending order for each address
setkey(eng.raw, "email")
setorder(eng.raw, email, dropdate) 
eng.raw[,n_row := seq_along(dropdate), by = email]


eng.raw[,c("ek_email","esp","engagementdate","engagementtime","grossleadrevenueamount")] <- NULL
eng.DT <- eng.raw[tradename != "mortgage"] # mortgage was a third-party drop that we don't want to include

rm(eng.raw)

# number engaged drops for each email address  
setkey(eng.DT,"email")
eng.DT[, engaged := ifelse(opencount == 1 | clickcount == 1 | leadcount == 1, 1, 0), by = email]
eng.DT[,engCounter := seq_len(.N)-1, by=.(email,rleid(engaged))]


## reorganize data by drop number


drops.freq <- data.table(xtabs(~ n_row, eng.DT))


drops.eng <- eng.DT[, .(sumEng = sum(engaged)), by = n_row]


drops.lead <- eng.DT[, .(sumLead = sum(leadcount)), by = n_row]


# merge tables and calculate action/lead rates
drops.data <- cbind(drops.freq, drops.eng, drops.lead, actionRate = drops.eng$sumEng / drops.freq$N, 
               leadRate = drops.lead$sumLead/drops.freq$N)
names(drops.data) <- c("n_row", "sentCount", "dropN", "actionCount", "n_row", "leadCount", "actionRate", "leadRate")
drops.data[,n_row := NULL]

rm(drops.eng)
rm(drops.freq)
rm(drops.lead)



## remove outliers using standard 1.5 IQR


sent_outliers <- data.table(sentCount = boxplot.stats(drops.data$sentCount)$out)
dropsNew <- drops.data[!sent_outliers, on = "sentCount"]
fwrite(dropsNew, "drops_final.csv")




## materials for knitr


final <- data.frame(eng.DT)
final[2:3] <- list(NULL)
final[5:10] <- list(NULL)
final[5:16] <- list(NULL)
final[5:7] <- list(NULL)


final[c(3)] <- colwise(as.Date)(final[c(3)])
final[,c(1,2,4)] <- colwise(as.factor)(final[,c(1,2,4)])
summary <- summary(final[1:4])

saveRDS(summmary, "summary.rds")

