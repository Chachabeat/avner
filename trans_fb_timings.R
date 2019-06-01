library(data.table)
library(ggplot2)

avner <- fread("/Users/barrysun/Downloads/avner_data.csv")
fb_data <- fread("/Users/barrysun/Downloads/sep_nov_avner_fb.csv")

# Clean amounts and transaction dates
avner[, Amount := as.numeric(gsub(",", "", Amount))]
avner[, Transaction_Date := as.POSIXct(`Transaction Date`, format = "%d/%m/%Y %H:%M:%S")]
avner[, Transaction_Date := as.Date(Transaction_Date)]

# Clean Facebook data - likes, engagement, reach
fb_data <- fb_data[, 1:12]
fb_data[, Date := as.Date(Date, format = "%m/%d/%y")]

fb_data[, Total_Likes := as.integer(`Lifetime Total Likes`)]

fb_data[`Daily New Likes` == "", `Daily New Likes` := "0"]
fb_data[`Daily Unlikes` == "", `Daily Unlikes` := "0"]
fb_data[, Daily_Likes := as.integer(`Daily New Likes`) - as.integer(`Daily Unlikes`)]

fb_data[, Daily_Engaged := as.integer(`Daily Page Engaged Users`)]
fb_data[, Weekly_Engaged := as.integer(`Weekly Page Engaged Users`)]

fb_data[, Daily_Reach := as.integer(`Daily Total Reach`)]
fb_data[, Weekly_Reach := as.integer(`Weekly Total Reach`)]

fb_data <- na.omit(fb_data)

cols <- unlist(lapply(fb_data, is.numeric))
cols <- names(fb_data)[cols]
cols <- c("Date", cols)

fb_data <- fb_data[, cols, with = F]
#fb_data[rowSums(is.na(fb_data)) > 0,]

# Merge FB and transaction data
sep_nov <- merge(fb_data, avner[, .(Transaction_Date, Amount, `Promote Page to Facebook`, `Event Name`, `Event Location`, `Event Year`)]
                 , by.x = "Date", by.y = "Transaction_Date", all.x = T, sort = T)

ggplot(sep_nov, aes(x = Date, y = Weekly_Reach)) +
  geom_line() +
  facet_wrap(vars(`Event Name`))
