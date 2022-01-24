##########
library("qdap")
library("magrittr")
library(dplyr)
AAPL <- read.csv("apleaa.csv")
#############
##data transformation
tweet <- read.csv("tweet.csv")
comp <- read.csv("company_tweet.csv")

tweet$post_date <- as.POSIXct(tweet$post_date,origin = "1970-01-01",tz = "GMT")
tweet$post_date <- as.Date(tweet$post_date)
tweet$tweet_id[1]
tickers <- comp[comp$ticker_symbol == "AAPL",]
dataset <- tweet[tweet$post_date >= "2018-01-01" & tweet$post_date <= "2019-12-31", ]
write.csv(tickers, "apple.csv")
write.csv(dataset, "apple2.csv")
##########
###data cleaned and merged with tableau prep
##########
###sentiment analysis
AAPL <- AAPL %>% arrange(post_date)
table <- AAPL[,c("post_date","body")]
final <- polarity(table[,2], table[,1])

winter<-as.data.frame(final[2])
write.csv(winter,"winterc2a.csv")
##########
###import stock data of AAPL
library(dplyr)
stock<-read.csv("AAPL.csv")
data <- read.csv("winterc2a.csv")
####polarity table
data <- data[,2:5]
data$group.1 <- as.Date(data$group.1, format = "%m/%d/%Y")
data$group.1 <- sort(data$group.1)
colnames(data) <- c("Date","total.sentences","wc","avg.polarity")

####stock
stock <- stock[stock$Date >= "2016-01-01",]
stock$Date <- as.Date(stock$Date)
#####merge
table <- merge(stock,data, by = "Date",all.x = T)
omittable <- na.omit(table)
plot(omittable$Date, omittable$Close, type = "l")
plot(omittable$Date, omittable$avg.polarity, type = "l")
#####there might be lags exist, creating lag variables
####lag 1 month
df1m <- data.frame(lag1 = matrix(ncol = 1, nrow = 30))
m1 <- as.data.frame(table[1:1401,10])
colnames(m1) <- c("lag1")
new1 <- rbind(df1m,m1)
lag1 <- cbind(table, new1)
####lag 2 month
df2m <- data.frame(lag2 = matrix(ncol = 1, nrow = 60))
m2 <- as.data.frame(table[1:1371,10])
colnames(m2) <- c("lag2")
new2 <- rbind(df2m,m2)
lag2 <- cbind(lag1, new2)
####lag 3 month
df3m <- data.frame(lag3 = matrix(ncol = 1, nrow = 90))
m3 <- as.data.frame(table[1:1341,10])
colnames(m3) <- c("lag3")
new3 <- rbind(df3m,m3)
lag3 <- cbind(lag2, new3)
####lag 4 month
df4m <- data.frame(lag4 = matrix(ncol = 1, nrow = 120))
m4 <- as.data.frame(table[1:1311,10])
colnames(m4) <- c("lag4")
new4 <- rbind(df4m,m4)
lag4 <- cbind(lag3, new4)
###omit na
final <- lag4[lag4$Date >= "2018-01-02" & lag4$Date <= "2020-06-23", ]
lag1table <- final[,c("Date","Close","lag1")]
lag1table <- na.omit(lag1table)
lag4table <- final[,c("Date","Close","lag4")]
lag4table <- na.omit(lag4table)
####regression
reg1 <- lm(Close ~ avg.polarity, data = table)
summary(reg1)
reg2 <- lm(Close ~ lag1, data = lag1table)
summary(reg2)
reg4 <- lm(Close ~ lag4, data = lag4table)
summary(reg4)
reg <- lm(Close ~ avg.polarity+lag1+lag2+lag3+lag4, data = final)
summary(reg)
###mutual table(exclude na for all variables)
mutual <- final[final$Date >= "2018-06-25" & final$Date <= "2019-12-31", ]
regm <- lm(Close ~ avg.polarity+lag1+lag2+lag3+lag4, data = mutual)
summary(regm)
###remove data after 2019/5/31
remov <- final[final$Date >= "2018-01-02" & final$Date <= "2019-5-31", ]
regr <- lm(Close ~ avg.polarity+lag1+lag2+lag3+lag4, data = mutual)
summary(regr)
lag1tabler <- remov[,c("Date","Close","lag1")]
lag1tabler <- na.omit(lag1tabler)
reg1r <- lm(Close ~ lag1, data = lag1tabler)
summary(reg1r)



