library(data.table)
library(ggmap)

avner <- fread("/Users/barrysun/Downloads/avner_data.csv")

avner[, Amount := as.numeric(gsub(",", "", Amount))]

# Mt Kilimajaro event is the biggest success
avner[, .(No._Donations = .N, Avg = mean(Amount)), `Event Location`]
avner[order(`Event Year`), .(No._Donations = .N, Avg = mean(Amount)), `Event Year`]

# Try clustering
clusters <- list()
tss <- 0
for(i in 1:15){  
  clusters[[i]] <- kmeans(avner[, Amount], i, nstart = 20)
  tss[i] <- clusters[[i]]$tot.withinss
}

plot(tss) # use 3 clusters?

clusters <- kmeans(avner[, Amount], 3, nstart = 20)

#PCA
pr_out <- prcomp(avner[, .(`Event Identifier`, Amount)], scale = T)


# Map out donations
SydneyMap <- get_map("Sydney", zoom = 10)
ggmap(SydneyMap) + geom_point(aes(x = lon, y = lat, colour = as.factor(`Post Code Cleaned`)), data = avner[`Event Location` == "Sydney"])
