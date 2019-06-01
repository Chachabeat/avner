library(data.table)
library(caret)

avner <- fread("/Users/barrysun/Downloads/avner_data.csv")

avner[, Amount := as.numeric(gsub(",", "", Amount))]

# Test regression
regr <- avner[, .(Amount, Event_ID = `Event Identifier`, Event_Name = `Event Name`, Location = `Event Location`, Post_Code = `Post Code Cleaned`, Message = `Message Flag`, `Fundraising Page Identifier`)]
regr[, `:=`(Event_ID = factor(Event_ID), Location = factor(Location), Post_Code = as.numeric(Post_Code), Message = factor(Message))]
regr[is.na(Post_Code), Post_Code := 0]

regr_model <- glm(Amount ~ Event_ID, regr[Amount > 0], family = Gamma)
regr_line <- predict(regr_model, regr)

model <- train(
  Amount ~ Event_ID + Post_Code + Message + Location + `Fundraising Page Identifier`, 
  regr,
  method = "lm",
  trControl = trainControl(
    method = "cv", 
    number = 10,
    verboseIter = TRUE
  )
)

# Mt Kilimajaro event is the biggest success
avner[, .(No._Donations = .N, Avg = mean(Amount)), `Event Location`]
avner[order(`Event Year`), .(No._Donations = .N, Avg = mean(Amount)), `Event Year`]

# Try clustering
clusters <- list()
tss <- 0
for(i in 1:15){  
  clusters[[i]] <- kmeans(regr[, Amount], i, nstart = 20)
  tss[i] <- clusters[[i]]$tot.withinss
}

plot(tss) # use 3 clusters?

clusters <- kmeans(regr[, Amount], 3, nstart = 20)
plot(clusters$cluster)

#PCA
pr_out <- prcomp(avner[, .(`Event Identifier`, Amount)], scale = T)

# Map out donations
SydneyMap <- get_map("Sydney", zoom = 10)
ggmap(SydneyMap) + geom_point(aes(x = lon, y = lat, colour = as.factor(`Post Code Cleaned`)), data = avner[`Event Location` == "Sydney"])
