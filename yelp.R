#' ---
#' title: "Yelp Dataset Challenge"
#' date: "Apr. 21, 2015"
#' ---
rm(list = ls())

data.url = "..."

###------------------------- part 1: business -------------------------###
## load dataset for business, user, checkin, tip
load.data <- function(data.url, fname = "business", header = TRUE) {
  dataset <- read.csv(paste(data.url, fname, ".csv", sep = ""), header = header)
  ## check missing data
  print(paste("Missing data of", fname, ".csv:" , any(is.na(dataset))), sep = " ")
  return(dataset)
}

## fill missing data with proportionally occurred values
fill.buz.na <- function(buz) {
  buz.mat = as.matrix(buz)
  for (i in 1:ncol(buz)) {
    freq = as.data.frame(table(buz.mat[,i]))
    freq = freq[freq[[1]] != "",]
    freq = freq[!is.na(freq[[1]]),]
    total = sum(freq[[2]])
    total
    rate = freq[[2]] / total
    rate  
    na.idx = which(buz.mat[,i] == "")
    na.idx = c(na.idx, which(is.na(buz.mat[,i])))
    for (j in na.idx) {
      random = runif(1)
      random
      k = 1
      accml.rate = rate[1]
      while(random > accml.rate && k <= length(rate)) {
        k = k + 1
        accml.rate = accml.rate + rate[k]
      }
      buz[j,i] = freq[[1]][k]
    }
  }
  return(buz)
}

## load business
buz = load.data(data.url, "business")
dim(buz)

## extract business in Las Vegas
buz.lasv = subset(buz, city == 'Las Vegas')
dim(buz.lasv)
## extract business in Karlsruhe
buz.karl = subset(buz, city == 'Karlsruhe')
dim(buz.karl)
## extract business in Edinburgh
buz.edin = subset(buz, city == 'Edinburgh')
dim(buz.edin)
## extract business in Waterloo
buz.wtl = subset(buz, city == 'Waterloo')
dim(buz.wtl)
## extract business in Montreal
buz.mont = subset(buz, city == 'Montreal')
dim(buz.mont)
## extract business in Pittsburgh
buz.pitt = subset(buz, city == 'Pittsburgh')
dim(buz.pitt)

## missing value of business
buz.na = buz.pitt[1:100,]
image(buz.na == "", main = "Missing Values", xlab = "Business", ylab = "Variable", 
      xaxt = "n", yaxt = "n", bty = "n")
axis(1, seq(0, 1, length.out = nrow(buz.na)), 1:nrow(buz.na), col = "white", cex = 0.2)
axis(2, seq(0, 1, length.out = ncol(buz.na)), names(buz.na), col = "white", las = 2, cex=0.1)
## fill missing values
buz.pitt = fill.buz.na(buz.pitt)
buz.pitt[1:2,]


###------------------------- part 2: business feature selection -------------------------###
## installation note: requires Java installed on your machine and the rJava package
require(FSelector)
buz.pitt.sel = buz.pitt[, grepl("attribute", names(buz.pitt))]
buz.pitt.sel = cbind(categories = buz.pitt$categories, buz.pitt.sel)
stars = buz.pitt$stars
buz.pitt.sel = cbind(stars, buz.pitt.sel)
## select business of categories: Restaurant
buz.pitt.sel = buz.pitt.sel[grep("Restaurant", buz.pitt.sel$categories),]
buz.pitt.sel = subset(buz.pitt.sel, select=-c(categories))
dim(buz.pitt.sel)
## Feature Ranking - calculate weights for each attribute using some function
weights = chi.squared(stars ~ ., buz.pitt.sel)
print(weights)
top.attr = cutoff.k(weights, 50)
## print the results
f = as.simple.formula(top.attr, "stars")
print(f)
## generate wordcloud
library(wordcloud)
library(stringr)
rownames(weights) = str_replace_all(rownames(weights), "attributes.", "")
wordcloud(rownames(weights), weights$attr_importance * 100, 
          min.freq = 1, rot.per = .5,
          colors = c("blue", "red", "black", "purple"))


###------------------------- part 3: classification of business stars -------------------------###
library(cvTools)
library(car)
buz.cv = buz.pitt.sel
buz.cv$stars[buz.cv$stars <= 3] = 0
buz.cv$stars[buz.cv$stars > 3] = 1
fold <- 10
set.seed(121121)
cvgroup <- cvFolds(nrow(buz.cv), K = fold)
errors = dim(fold)
precissions = dim(fold)
recalls = dim(fold)
fscores= dim(fold)
probs = NULL
actuals = NULL
## 10-fold cross-validation
for (i in (1:fold)) { 
  train <- which(cvgroup$which != i)
  test <- which(cvgroup$which == i)
  ## logistic model
  m1 <- glm(stars ~ latitude + review_count +
              attributes.Alcohol +
              attributes.Take.out +
              attributes.Noise.Level +
              attributes.Wi.Fi +
              attributes.Has.TV +
              attributes.Attire +
              attributes.Good.For.Kids +
              attributes.Caters +
              attributes.Outdoor.Seating +
              attributes.Ambience.classy +
              attributes.Parking.lot +
              attributes.Good.For.brunch +
              attributes.Waiter.Service +
              attributes.Parking.street +
              attributes.Ambience.hipster +
              attributes.Good.For.dinner +
              attributes.Good.For.breakfast +
              attributes.Parking.garage +
              attributes.Accepts.Credit.Cards +
              attributes.Takes.Reservations +
              attributes.Ambience.trendy +
              attributes.Delivery +
              attributes.Ambience.intimate +
              attributes.Good.For.Groups +
              attributes.Ambience.romantic +
              attributes.Ambience.upscale +
              attributes.Ambience.casual, 
            family = binomial(link = "logit"), data = buz.cv, subset = train)
  prob <- predict(m1, buz.cv[test,], type = c("response"))
  predicted <- recode(prob, "0.5:1 = 1; else = 0")
  actual = buz.cv[test,]$stars
  ## confusion matrix
  cm = table(actual, predicted)
  cm
  error = (cm[1,2] + cm[2,1]) / nrow(buz.cv[test,])
  error
  errors[i] = error
  if ((cm[1,1]) + (cm[2,1]) == 0) {
    precission = 0
  } else {
    precission = (cm[1,1]) / ((cm[1,1]) + (cm[2,1]))
  }
  precissions[i] = precission
  
  if ((cm[1,1]) + (cm[1,2]) == 0) {
    recall=0
  } else {
    recall = (cm[1,1]) / ((cm[1,1]) + (cm[1,2]))
  }
  recalls[i] = recall
  fscore = (2 * precission * recall) / (precission + recall)
  fscores[i] = fscore
  probs = c(probs, prob)
  actuals = c(actuals, actual)
}
summary(m1)
## error and accuracy
avg.error1 = mean(errors)
avg.precission1 = mean(precissions)
avg.recall1 = mean(recalls)
avg.fscore1 = mean(fscores)
## review the above performances
Rowname = c("m1")
Columnname = c("Error Rates","Precission","Recall","F1 Scores")
m1 = matrix(c(avg.error1, avg.precission1, avg.recall1, avg.fscore1), 
            nrow = 1, ncol = 4, byrow = TRUE, dimnames = list(Rowname, Columnname))
m1
## ROC
library(ROCR)
result = data.frame(probs,actuals)
pred = prediction(result$probs,result$actuals)
perf = performance(pred, "tpr","fpr")
plot(perf, main="Model 1 - ROC") 


###------------------------- part 4: business map (Pittsburgh) -------------------------###
library(ggmap)
map <- get_map(location = c(-79.970, 40.445), zoom = 13, maptype = "roadmap")
buz.pitt.1 = buz.pitt[buz.pitt$stars == 2,]
points(buz.pitt.1$longitude, buz.pitt.1$latitude, pch=21, bg=colors, cex=buz.pitt.1$stars, lwd=.4)
ggmap(map) + 
  geom_point(aes(x = longitude, y = latitude, size = sqrt(stars)), 
             data = buz.pitt, colour = buz.pitt$stars, cex=buz.pitt$stars) 


###------------------------- part 5: checkin map -------------------------###
library(ggmap)
library(plyr)
## load checkin
checkin = load.data(data.url, "checkin")
dim(checkin)
## count the checkins by business and hours
checkin.by.buz <- function(checkin, buz, hour.from, hour.to) { 
  ## combine checkin_info attributes only
  checkin.info = c()
  for (i in 1:ncol(checkin)) {
    colname = colnames(checkin)[i]
    if (grepl("checkin_info", colname)) {
      substr = unlist(strsplit(colname, "[.]"))
      hour = as.numeric(substr[2])
      if (hour >= hour.from && hour <= hour.to) {
        checkin.info = cbind(checkin.info, checkin[,i])
      }
    } 
  }
  ## count checkin by row
  count = rep(c(0), each = nrow(checkin.info))
  for (i in 1:nrow(checkin.info)) {
    count[i] = count[i] + sum(checkin.info[i,], na.rm = T)
  }
  business_id = as.character(checkin$business_id)
  checkin.map = cbind(checkin.info, business_id = business_id, count = count)
  checkin.map = data.frame(checkin.map)
  checkin.map = subset(checkin.map, business_id %in% buz.pitt$business_id)
  ## append latidude and longitude
  lat.lon = buz
  lat.lon$business_id = as.character(lat.lon$business_id)
  lat.lon$latitude = as.character(lat.lon$latitude)
  lat.lon$longitude = as.character(lat.lon$longitude)
  lat = mapvalues(checkin.map$business_id, from = lat.lon$business_id, to = lat.lon$latitude)
  checkin.map = cbind(checkin.map, latitude = lat)
  checkin.map$latitude = as.numeric(as.character(checkin.map$latitude))
  lon = mapvalues(checkin.map$business_id, from = lat.lon$business_id, to = lat.lon$longitude)
  checkin.map = cbind(checkin.map, longitude = lon)
  checkin.map$longitude = as.numeric(as.character(checkin.map$longitude))
  checkin.map = checkin.map[checkin.map$count == 0,]
  return(checkin.map)
}

## map business latitude and longitude for checkin - morning
checkin.hour.am = checkin.by.buz(checkin, buz.pitt, 9, 12)
checkin.hour.am$count = as.numeric(as.character(checkin.hour.am$count))
map <- get_map(location = c(-79.970, 40.445), zoom = 13, maptype = "roadmap")
ggmap(map) + 
  geom_point(aes(x = longitude, y = latitude, size = sqrt(count)), 
             data = checkin.hour.am, colour = "magenta",
             alpha = 1, size = 5, shape = 21) 

## map business latitude and longitude for checkin - afternoon
checkin.hour.pm = checkin.by.buz(checkin, buz.pitt, 12, 18)
checkin.hour.pm$count = as.numeric(as.character(checkin.hour.pm$count))
map <- get_map(location = c(-79.970, 40.445), zoom = 13, maptype = "roadmap")
ggmap(map) + 
  geom_point(aes(x = longitude, y = latitude, size = sqrt(count)), 
             data = checkin.hour.pm, colour = "magenta",
             alpha = 1, size = 5, shape = 21) 

## map business latitude and longitude for checkin - evening
checkin.hour.pm = checkin.by.buz(checkin, buz.pitt, 18, 23)
checkin.hour.pm$count = as.numeric(as.character(checkin.hour.pm$count))
map <- get_map(location = c(-79.970, 40.445), zoom = 13, maptype = "roadmap")
ggmap(map) + 
  geom_point(aes(x = longitude, y = latitude, size = sqrt(count)), 
             data = checkin.hour.pm, colour = "magenta",
             alpha = 1, size = 5, shape = 21) 


###------------------------- part 6: checkin -------------------------###
library(plotrix)
## load checkin
checkin = load.data(data.url, "checkin")
dim(checkin)
## count the checkins by week days: 0 - Sunday; 6 - Saturday
checkin.by.day <- function(checkin, date = 0) {
  count = rep(c(0), each = 24)
  for (i in 1:ncol(checkin)) {
    colname = colnames(checkin)[i]
    if (grepl("checkin_info", colname)) {
      substr = unlist(strsplit(colname, "[.]"))
      if (substr[3] == date) {
        index = as.numeric(substr[2]) + 1
        count[index] = count[index] + sum(checkin[,i], na.rm = T)
      }
    } 
  }
  return(count)
}

## plot clock cycle
## color for Sunday ~ Saturday: black, red, green, blue, cyan, magenta, yellow
plot.clock <- function(checkin) {
  plot.new()
  for (i in 0:6) {
    par(new=T)
    count = checkin.by.day(checkin, date = i)
    p1 = clock24.plot(count, 0:23, show.grid = T, labels = 0:23, 
                      point.col = c(i), line.col = c(i+1), lwd = 3,
                      rp.type = "symbols", main = "24 Clock Plot",
                      mar = c(2,2,4,2))    
  }  
}

## filter checkin in Pittsburgh
checkin.pitt = subset(checkin, business_id %in% buz.pitt$business_id)
dim(checkin.pitt)
plot.clock(checkin.pitt)
legend("bottomleft", c("Sunday","Monday","Tuesday","Wednesday","Thurday","Friday","Saturday"), 
       cex = 0.8, 
       col=c(1,2,3,4,5,6,7), 
       pch=26:27, horiz=T,lwd=3)

## filter checkin in Las Vegas
checkin.lasv = subset(checkin, business_id %in% buz.lasv$business_id)
dim(checkin.lasv)
plot.clock(checkin.lasv)

## filter checkin in Karl
checkin.karl = subset(checkin, business_id %in% buz.karl$business_id)
dim(checkin.karl)
plot.clock(checkin.karl)

## filter checkin in Edinburgh
checkin.edin = subset(checkin, business_id %in% buz.edin$business_id)
dim(checkin.edin)
plot.clock(checkin.edin)

## filter checkin in Waterloo
checkin.wtl = subset(checkin, business_id %in% buz.wtl$business_id)
dim(checkin.wtl)
plot.clock(checkin.wtl)

## filter checkin in Montreal
checkin.mont = subset(checkin, business_id %in% buz.mont$business_id)
dim(checkin.mont)
plot.clock(checkin.mont)

user = load.data(data.url, "user")
dim(user)


###------------------------- part 7: review -------------------------###
## load review
review0 = load.data(data.url, "review_0")
dim(review0)
review0$text = as.character(review0$text)
## filter reviews for businesses in Pittsburgh
review.pitt = subset(review0, business_id %in% buz.pitt$business_id)
## filter reviews by businesses' ratings
review.pitt = subset(review.pitt, as.numeric(as.character(review.pitt$stars)) == 1)
dim(review.pitt)
## select frequently reviewed businesses
review.pitt.idx = sort(table(review.pitt$business_id), decreasing = T)[1:10]
review.pitt.idx = names(review.pitt.idx)
review.pitt.idx
review.pitt = subset(review.pitt, business_id %in% review.pitt.idx)
dim(review.pitt)
## delete carriage returns in review text
library(stringr)
review.pitt$text = str_replace_all(review.pitt$text, "[\n*]", "")


###------------------------- part 8: review text mining -------------------------###
## load libraries
Sys.setenv(NOAWT= "true") ## work around for the stemmer issue
library(tm)
library(lsa)
library(ggplot2)

## prepare corpus
corpus = Corpus(VectorSource(review.pitt$text))
corpus = tm_map(corpus, content_transformer(tolower)) ## convert text to lower case
inspect(corpus[1:3])  
corpus = tm_map(corpus, removePunctuation) ## remove punctuations
inspect(corpus[1:3])
corpus = tm_map(corpus, removeNumbers) ## remove numbers
inspect(corpus[1:3])
corpus = tm_map(corpus, function(x) removeWords(x, stopwords("english"))) ## remove stopwords
inspect(corpus[1:3]) 
corpus = tm_map(corpus, stemDocument, language = "english") ## stemming
inspect(corpus[1:3])
corpus  # check corpus

## generate wordcloud
library(wordcloud)
## manually remove words from the cloud
corpus = tm_map(corpus,removeWords,c("good","like"))
wordcloud(corpus, scale=c(5,0.5), max.words=200, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))

td.mat = as.matrix(TermDocumentMatrix(corpus))
td.mat[1:3,]
m = as.matrix(td.mat)
## calculate the frequency of words
v = sort(rowSums(m), decreasing=TRUE) 
words = names(v)
wc = data.frame(word=words, freq=v)
wc[1:300,]
wordcloud(wc$word, wc$freq, min.freq = 80, random.order=FALSE, 
          rot.per=0.35, use.r.layout=FALSE, colors=brewer.pal(8, "Dark2"))
c = wc[wc == "pizza",]
c = rbind(c, wc[wc == "beer",])
c = rbind(c, wc[wc == "sandwich",])
c = rbind(c, wc[wc == "chees",])
c = rbind(c, wc[wc == "burger",])
c = rbind(c, wc[wc == "pancak",])
c = rbind(c, wc[wc == "salad",])
c = rbind(c, wc[wc == "servic",])
c = rbind(c, wc[wc == "dessert",])
c = rbind(c, wc[wc == "wine",])
c = rbind(c, wc[wc == "place",])
c = rbind(c, wc[wc == "food",])
c = rbind(c, wc[wc == "time",])
c = rbind(c, wc[wc == "order",])
c = rbind(c, wc[wc == "friend",])
c = rbind(c, wc[wc == "sauc",])

## run mds
td.mat = TermDocumentMatrix(corpus)
td.mat[1:3,]

dim(td.mat) ## dimension of term-doc matrix

dist.mat = dist(t(td.mat)) ## compute distance matrix
dist.mat  ## check distance matrix

## generate mds plot
doc.mds = cmdscale(dist.mat, k = 2)
data = data.frame(x = doc.mds[, 1], y = doc.mds[, 2], 
                  business = review.pitt$business_id, 
                  id = row.names(review.pitt))
ggplot(data, aes(x = x, y = y, color=business)) + 
  geom_point() + 
  geom_text(aes(x = x, y = y - 0.2, label = id))

## TFIDF weighting
td.mat.w = TermDocumentMatrix(corpus, control = list(weighting = weightTfIdf))
dist.mat = dist(t(td.mat.w))
doc.mds = cmdscale(dist.mat, k = 2)
data = data.frame(x = doc.mds[, 1], y = doc.mds[, 2], 
                  business = review.pitt$business_id, 
                  id = row.names(review.pitt))
ggplot(data, aes(x = x, y = y, color = business)) + geom_point()

## run mds by lsa package
lsa.space = lsa(td.mat.w,dims = 3)  ## create LSA space
dist.mat = dist(t(as.textmatrix(lsa.space)))  ## compute distance matrix
dist.mat  ## check distance mantrix
doc.mds = cmdscale(dist.mat, k = 2)
data = data.frame(x = doc.mds[, 1], y = doc.mds[, 2], 
                  business = review.pitt$business_id, 
                  id = row.names(review.pitt))

## generate 2D plot
ggplot(data, aes(x = x, y = y, color=business)) + 
  geom_point() + 
  geom_text(aes(x = x, y = y - 0.2, label = id))

## generate 3D plot
library(scatterplot3d)
doc.mds = cmdscale(dist.mat, k = 3)
doc.mds = doc.mds[1:(nrow(doc.mds)-2),]
colors = rep(seq(1,10,length.out=10), each = 135)
scatterplot3d(doc.mds[, 1], doc.mds[, 2], doc.mds[, 3], color = colors, 
              pch = 16, main = "Semantic Space Scaled to 3D", 
              xlab = "x", ylab = "y", zlab = "z", type = "h")


###------------------------- part 9: build network -------------------------###
library(plyr)
## map business id and business name for dataset review
review = subset(review0, business_id %in% buz.pitt$business_id)
y = mapvalues(review$business_id, from = buz$business_id, to = as.character(buz$name))
buz.user = cbind(review, name = y)
buz.user = buz.user[c("user_id", "name", "business_id", "stars")]
dim(buz.user)
buz.user[1:3,]

## select businesses which are frequently reviewed
buz.freq = sort(table(review$business_id), decreasing = T)
buz.freq.rname = rownames(buz.freq[1:30])
buz.user = subset(buz.user, business_id %in% buz.freq.rname)
dim(buz.user)

## create a business-to-business co-rating network
library(igraph)
g = graph.data.frame(buz.user, directed = T)
mat = as.matrix(get.adjacency(g))
m2 = t(mat) %*% mat
buz.idx = which(colSums(m2) > 0)
buz.mat = m2[buz.idx, buz.idx]
## co-star with self does not count
diag(buz.mat) = 0  
buz.idx = which(colSums(buz.mat) > 0)
buz.mat = buz.mat[buz.idx, buz.idx]
dim(buz.mat)
buz.mat[1:3,]
row.names = rownames(buz.mat)
rownames(buz.mat) = c(1:30)
colnames(buz.mat) = c(1:30)
rownames(buz.mat)[order(colSums(buz.mat), decreasing = T)[1:5]]
g = graph.adjacency(buz.mat, weighted = T, mode = "undirected", diag = F)
g1 = delete.edges(g,E(g) [weight<10])
g2 = delete.vertices(g1, which(degree(g1) == 0))
set.seed(1)
plot(g2, layout = layout.fruchterman.reingold, vertex.label = V(g2)$name)
set.seed(1)
plot(g2, layout = layout.fruchterman.reingold, vertex.label.cex = 0.8)

## get modularity-based community
fc = fastgreedy.community(g2)
modularity(fc)
membership(fc)
set.seed(1)
plot(fc, g2, main = "modularity community", layout = layout.fruchterman.reingold, 
     vertex.label.cex = 1)
dendPlot(fc)

##### central degree #####
deg = degree(g2)
deg
## the top-5 nodes with highest degrees
top = order(deg, decreasing = T)[1:5]  
top

## size node by degree
V(g2)$size = abs(deg) * 1
V(g2)$color = "green"
V(g2)$label.color = "blue"
V(g2)$label.cex = 0.5
E(g2)$color = "black"
## highlight the top-5 nodes
V(g2)[top]$label.color = "black"  
V(g2)[top]$label.cex = 1
V(g2)[top]$color = "Skyblue"
set.seed(1)
plot(g2, layout = layout.lgl)
title("degree centrality")

## compute node closeness centrality
clo = closeness(g2)
clo
top = order(clo, decreasing = T)[1:5]

## size node by closeness
V(g2)$size = abs(clo)^2 * 5e+06
V(g2)$color = "blue"
V(g2)$label.color = "black"
V(g2)$label.cex = 0.2
## highlight the top-5 nodes
V(g2)[top]$label.color = "red"  
V(g2)[top]$label.cex = 1
set.seed(1)
plot(g2, layout = layout.lgl)
title("closeness")

## compute node betweenness centrality
bet = betweenness(g2)
bet
top = order(bet, decreasing = T)[1:5]

## size node by betweenness
V(g2)$size = abs(bet) * 0.1
V(g2)$color = "blue"
V(g2)$label.color = "black"
V(g2)$label.cex = 0.2
## highlight the top-5 nodes
V(g2)[top]$label.color = "red"
V(g2)[top]$label.cex = 1
set.seed(1)
plot(g2, layout = layout.lgl)
title("betweenness")

## compute pagerank
pg = page.rank(g2)$vector
pg
top = order(pg,decreasing=T)[1:5]

## size node by pagerank
V(g2)$size = abs(pg) * 250
V(g2)$label.color = "black"
## highlight the top-5 nodes
V(g2)[ top ]$label.color = "red"
set.seed(1)
plot(g2)
title("PageRank")


###------------------------- part 10: generate recommendation -------------------------###
library(igraph)
library(recommenderlab)

## map business id and business name for dataset review
review = review0
y = mapvalues(review$business_id, from = buz$business_id, to = as.character(buz$name))
buz.user = cbind(review, name = y)
buz.user = buz.user[c("user_id", "name", "business_id", "stars")]
buz.user[1:3,]
buz.user = data.frame(from = buz.user$user_id, to = buz.user$name, weight = buz.user$stars)
buz.user$weight = as.numeric(buz.user$weight)

## create a business-to-business co-reviewing network
g = graph.data.frame(buz.user)
mat = as.matrix(get.adjacency(g))
mat.w = get.adjacency(g, attr = "weight")
buz.idx = which(colSums(mat) >= 2)
user.idx = which(rowSums(mat) >= 2)
rmat = mat.w[user.idx, buz.idx]
dim(rmat)

m = as.matrix(rmat)
m = as(m, "realRatingMatrix")
dim(m)
## IBCF
r = Recommender(m[1:2500], method = "IBCF")
r
names(getModel(r))
getModel(r)$topN
recom = predict(r, m[2500:2600], n = 5)
recom
as(recom, "list")
recom3 = bestN(recom, n = 3)
recom3
as(recom3, "list")
## UBCF
r = Recommender(m[1:500], method = "UBCF")
r
names(getModel(r))
recom = predict(r, m[501:526], n = 5)
recom
## Popular
r = Recommender(m[1:500], method = "Popular")
r
names(getModel(r))
recom = predict(r, m[501:526], n = 5)
recom
## Random
r = Recommender(m[1:500], method = "Random")
r
names(getModel(r))
recom = predict(r, m[501:526], n = 5)
recom
## evaluation
e = evaluationScheme(m, method = "cross", k = 4, given = 5, goodRating = 3)
e
r1 = Recommender(getData(e, "train"), "Random")
r2 = Recommender(getData(e, "train"), "Popular")
r3 = Recommender(getData(e, "train"), "UBCF")
r4 = Recommender(getData(e, "train"), "IBCF")
p1 = predict(r1, getData(e, "known"), type = "ratings")
p2 = predict(r2, getData(e, "known"), type = "ratings")
p3 = predict(r3, getData(e, "known"), type = "ratings")
p4 = predict(r4, getData(e, "known"), type = "ratings")
error = rbind(calcPredictionAccuracy(p1, getData(e, "unknown")), 
              calcPredictionAccuracy(p2, getData(e, "unknown")),
              calcPredictionAccuracy(p3, getData(e, "unknown")),
              calcPredictionAccuracy(p4, getData(e, "unknown")))
rownames(error) = c("Random", "Popular", "UBCF", "IBCF")
error = t(error)
error

## comparison of several recommender algorithms
scheme = evaluationScheme(m, method = "cross", k = 4, given = 5, 
                          goodRating = 3)
scheme

## use evaluate() with a list of algorithms together with their parameters
## instead of a single method name.
algorithms = list('random items' = list(name = "RANDOM", param = NULL), 
                  'popular items' = list(name = "POPULAR", param = NULL), 
                  'user-based CF' = list(name = "UBCF", param = list(method = "Cosine", minRating = 5)), 
                  'item-based CF' = list(name = "IBCF", param = NULL))
## run algorithms
results = evaluate(scheme, algorithms, n = c(1, 3, 5, 10))
results
names(results)

plot(results, annotate = c(1, 2, 3, 4), legend = "topleft")
plot(results, "prec/rec", annotate = 2, ylim = c(0, 0.2), legend = "topleft")
