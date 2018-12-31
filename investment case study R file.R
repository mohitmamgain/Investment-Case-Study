# SET WORKING DIRECTORY
# CHECKPOINTS 1
companies <- read.delim("companies.txt", sep="\t")
rounds2 <- read.csv("rounds2.csv")
# unique key for each company
names(companies)[1] <- "company_permalink"

library(dplyr)
library(tidyr)
# all the words in companies and rounds2 are converted into lowecase as part of data prepearation.
rounds2 <- mutate_all(rounds2, funs(tolower))
companies <- mutate_all(companies, funs(tolower))

# unique companies are present in rounds2
length(unique(rounds2$company_permalink))

# unique companies are present in companies
length(unique(companies$company_permalink))

# merge on rounds2 and companies is done so that now we have only one data frame to work on.
master_frame <- merge(rounds2, companies, by = "company_permalink", all = TRUE)

# to match answers in excel also have use write command to convert into excel file
write.csv(master_frame,"master_frame.csv", row.names = FALSE)


# CHECKPOINTS 2
# filter the funding round type to venture, seed, angel and private equity
# and used aggregate command to calculate average invested amount in these funding type
master_frame$raised_amount_usd <- as.numeric(as.character(master_frame$raised_amount_usd))
master_frame_aggregate <-filter(master_frame,master_frame$funding_round_type=="venture" | master_frame$funding_round_type=="angel" | master_frame$funding_round_type=="seed" | master_frame$funding_round_type=="private_equity")
aggregate(master_frame_aggregate$raised_amount_usd, list(master_frame_aggregate$funding_round_type), mean, na.rm = TRUE)


# As the avg. funding amount of venture type = USD 73,908,593 and 
# considering that sparks Funds wants to invest b/w 5 to 15 million USD per investment, 
# investment type which is most suitable for it = VENTURE.

# CHECKPOINT 3
# want to calculate the top9 countries with highest amount of investment for venture funding type

# narrow down the data frame for venture funding type
master_frame_venture <- filter(master_frame, master_frame$funding_round_type == "venture")
write.csv(master_frame_venture,"final.csv", row.names = FALSE)
as.factor(master_frame_venture$country_code)

# created top9 data frame and have used aggregate function for highest amount of inversmtent by country
top9 <- aggregate(master_frame_venture$raised_amount_usd, list(master_frame_venture$country_code), sum, na.rm = TRUE)
# arrange the amount in descending order
top9 <- arrange(top9, desc(x))
# removed the black spaces from country column as data cleaning
top9 <- top9[!(is.na(top9$Group.1) | top9$Group.1==""), ]
# select first 9 rows as we want top 9 countries
top9 <- arrange(top9, desc(x))[1:9,]
names(top9)[1] <- "country"
names(top9)[2] <- "Total investment"


# CHECKPOINT 4

mapping <- read.csv("mapping.csv", na.strings = c("", "NA"))
# used gsub function to replace "0" with "na" as part of data cleaning
mapping$category_list <- gsub("0", "na", mapping$category_list)

mapping <- mutate_all(mapping, funs(tolower))
# changes the name to column for ease of access
names(mapping)[2] <- "Automotive_sports"
names(mapping)[4] <- "Cleantech_Semiconductors"
names(mapping)[8] <- "News_Search_and_Messaging"
names(mapping)[10] <- "Social_Finance_Analytics_Advertising"

# convert wide data format to long data format using gather function
mapping_new <- gather(mapping, sector, value, Automotive_sports : Social_Finance_Analytics_Advertising)
mapping_new <- mapping_new[!(mapping_new$value == 0),]
mapping_new <- mapping_new[, -3]
mapping_new <- na.omit(mapping_new)
names(mapping_new)[1] <- "primary_sector"

# have used seperate function to seperate the primary sector form categories list
master_frame <- separate(master_frame, category_list, into=c("primary_sector", "secondary_sector", "tertiary_sector" ), sep="\\|")
master_frame <- master_frame[, -10:-11]

mapping_merge_sector <- merge(mapping_new,master_frame, by = "primary_sector", all = FALSE)


# CHECKPOINT 5

# filter the data frame D1 for venture funding, country = usa and amount b/w 5-15 million usd
D1 <- filter(mapping_merge_sector, mapping_merge_sector$funding_round_type == "venture", mapping_merge_sector$country_code == "usa", mapping_merge_sector$raised_amount_usd >= 5000000 & mapping_merge_sector$raised_amount_usd <= 15000000)
# filter the data frame D1 for venture funding, country = gbr and amount b/w 5-15 million usd
D2 <- filter(mapping_merge_sector, mapping_merge_sector$funding_round_type == "venture", mapping_merge_sector$country_code == "gbr", mapping_merge_sector$raised_amount_usd >= 5000000 & mapping_merge_sector$raised_amount_usd <= 15000000)
#filter the data frame D1 for venture funding, country = ind and amount b/w 5-15 million usd
D3 <- filter(mapping_merge_sector, mapping_merge_sector$funding_round_type == "venture", mapping_merge_sector$country_code == "ind", mapping_merge_sector$raised_amount_usd >= 5000000 & mapping_merge_sector$raised_amount_usd <= 15000000)
write.csv(D1,"D1.csv", row.names = FALSE)
write.csv(D2,"D2.csv", row.names = FALSE)
write.csv(D3,"D3.csv", row.names = FALSE)

# to count the no. of investment in differnet sector for usa
D1_1 <- aggregate(D1$raised_amount_usd, list(D1$sector), FUN = length)
D1_1 <- arrange(D1_1, desc(x))[1:3,]
names(D1_1)[1] <- "sector"
names(D1_1)[2] <- "Count of investment in usa"
# total amount of investment in different secotr for usa
D1_2 <- aggregate(D1$raised_amount_usd, list(D1$sector), FUN = sum, na.rm = TRUE)
D1_2 <- arrange(D1_2, desc(x))[1:3,]
names(D1_2)[1] <- "sector"
names(D1_2)[2] <- "Total amount invested"


D1 <- merge(D1,D1_1, by = "sector", all = TRUE)
D1 <- merge(D1, D1_2, by = "sector", all = TRUE)
## to count the no. of investment in differnet sector for gbr
D2_1 <- aggregate(D2$raised_amount_usd, list(D2$sector), FUN = length)
D2_1 <- arrange(D2_1, desc(x))[1:3,]
names(D2_1)[1] <- "sector"
names(D2_1)[2] <- "Count of investment in gbr"

# # total amount of investment in different secotr for gbr
D2_2 <- aggregate(D2$raised_amount_usd, list(D2$sector), FUN = sum, na.rm = TRUE)
D2_2 <- arrange(D2_2, desc(x))[1:3,]
names(D2_2)[1] <- "sector"
names(D2_2)[2] <- "Total amount invested"

D2 <- merge(D2,D2_1, by = "sector", all = TRUE)
D2 <- merge(D2, D2_2, by = "sector", all = TRUE)

# to count the no. of investment in differnet sector for gbr
D3_1 <- aggregate(D3$raised_amount_usd, list(D3$sector), FUN = length)
D3_1 <- arrange(D3_1, desc(x))[1:3,]
names(D3_1)[1] <- "sector"
names(D3_1)[2] <- "Count of investment in ind"
# # total amount of investment in different secotr for ind
D3_2 <- aggregate(D3$raised_amount_usd, list(D3$sector), FUN = sum, na.rm = TRUE)
D3_2 <- arrange(D3_2, desc(x))[1:3,]
names(D3_2)[1] <- "sector"
names(D3_2)[2] <- "Total amount invested"

D3 <- merge(D3,D3_1, by = "sector", all = TRUE)
D3 <- merge(D3, D3_2, by = "sector", all = TRUE)

# company for highest amount of investment for topmost sector in usa
D1_top1 <- filter(D1, sector == "Others")
D1_top1 <- select(D1_top1, company_permalink, funding_round_code, raised_amount_usd)
D1_top1 <- aggregate(D1_top1$raised_amount_usd, list(D1_top1$company_permalink), FUN = sum, na.rm = TRUE)
D1_top1 <- arrange(D1_top1, desc(x))


# company for highest amount of investment for second topmost sector in usa
D1_top2 <- filter(D1, sector == "Social_Finance_Analytics_Advertising")
D1_top2 <- select(D1_top2, company_permalink, funding_round_code, raised_amount_usd)
D1_top2 <- aggregate(D1_top2$raised_amount_usd, list(D1_top2$company_permalink), FUN = sum, na.rm = TRUE)
D1_top2 <- arrange(D1_top2, desc(x))

# company for highest amount of investment for topmost sector in gbr
D2_top1 <- filter(D2, sector == "Others")
D2_top1 <- select(D2_top1, company_permalink, funding_round_code, raised_amount_usd)
D2_top1 <- aggregate(D2_top1$raised_amount_usd, list(D2_top1$company_permalink), FUN = sum, na.rm = TRUE)
D2_top1 <- arrange(D2_top1, desc(x))

# company for highest amount of investment for second topmost sector in gbr
D2_top2 <- filter(D2, sector == "Social_Finance_Analytics_Advertising")
D2_top2 <- select(D2_top2, company_permalink, funding_round_code, raised_amount_usd)
D2_top2 <- aggregate(D2_top2$raised_amount_usd, list(D2_top2$company_permalink), FUN = sum, na.rm = TRUE)
D2_top2 <- arrange(D2_top2, desc(x))

# company for highest amount of investment for topmost sector in ind
D3_top1 <- filter(D3, sector == "Others")
D3_top1 <- select(D3_top1, company_permalink, funding_round_code, raised_amount_usd)
D3_top1 <- aggregate(D3_top1$raised_amount_usd, list(D3_top1$company_permalink), FUN = sum, na.rm = TRUE)
D3_top1 <- arrange(D3_top1, desc(x))

# company for highest amount of investment for second topmost sector in ind
D3_top2 <- filter(D3, sector == "Social_Finance_Analytics_Advertising")
D3_top2 <- select(D3_top2, company_permalink, funding_round_code, raised_amount_usd)
D3_top2 <- aggregate(D3_top2$raised_amount_usd, list(D3_top2$company_permalink), FUN = sum, na.rm = TRUE)
D3_top2 <- arrange(D3_top2, desc(x))


