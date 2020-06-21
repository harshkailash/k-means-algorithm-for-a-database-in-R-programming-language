library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(stringr)
library(tm)
library(igraph)
library(networkD3)

system("ls ../input")
isis <- read_csv('50tweets.csv')
tweet <- as.character(isis$tweets)
#removing links
tweet = gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", " ", tweet)
#retweet
tweet = gsub("(RT|via)((?:\\b\\W*@\\w+)+)", " ", tweet)
# removing hashtags
tweet = gsub("#\\w+", " ", tweet)
# removing @people
tweet = gsub("@\\w+", " ", tweet)
#removing punctuations
tweet = gsub("[[:punct:]]", " ", tweet)
#removing numbers
tweet = gsub("[[:digit:]]", " ", tweet)
#removing emojis
tweet<-str_replace_all(tweet,"[^[:graph:]]"," ")
tweet <- str_replace_all(tweet,'https'," ")
tweet <- str_replace_all(tweet,'amp'," ")
# removing non-english characters
tweet1 <- grep('tweet',iconv(tweet,'latin1','ASCII',sub='tweet'))
tweet<-tweet[-tweet1]
#removing spaces
tweet = gsub("[ \t]{2,}", " ", tweet)
tweet = gsub("^\\s+|\\s+$", "", tweet)
tweet = tolower(tweet)
corp <- Corpus(VectorSource(tweet))
corp <- tm_map(corp,removeWords,c(stopwords('english'),stopwords('SMART'),'required','responded'))
tdm <- TermDocumentMatrix(corp) 
freq.terms <- findFreqTerms(tdm,lowfreq=250)
term.freq <- rowSums(as.matrix(tdm))
term.freq <- subset(term.freq, term.freq >= 250)
df <- data.frame(term = names(term.freq), freq = term.freq)
library(ggplot2)
png('frequent_terms_isis.png')
ggplot(df, aes(x = term, y = freq)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Count") + coord_flip()
dev.off()

iraq <- findAssocs(tdm,'iraq',0.08)
iraq <- as.data.frame(iraq)
names<- rownames(iraq)
values <- iraq$iraq
df_iraq <- data.frame(term=names,value=values)
ggplot(df_iraq, aes(x = names, y = values)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Associations of the word Iraq") + coord_flip()

soldiers <- findAssocs(tdm,'soldiers',0.08)
soldiers <- as.data.frame(soldiers)
names<- rownames(soldiers)
values <- soldiers$soldiers
df_soldiers <- data.frame(term=names,value=values)
ggplot(df_soldiers, aes(x = names, y = values)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Associations of the word Soldiers") + coord_flip()

syria <- findAssocs(tdm,'syria',0.08)
syria <- as.data.frame(syria)
names<- rownames(syria)
values <- syria$syria
df_syria <- data.frame(term=names,value=values)
ggplot(df_syria, aes(x = names, y = values)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Associations of the word Syria") + coord_flip()

is<-findAssocs(tdm,'isis',0.08)
is <- as.data.frame(is)
names<- rownames(is)
values <- is$isis
df_is <- data.frame(term=names,value=values)
ggplot(df_is, aes(x = names, y = values)) + geom_bar(stat = "identity") + xlab("Terms") + ylab("Associations of the word ISIS") + coord_flip()


library(igraph)
tdm_new <- removeSparseTerms(tdm,0.98)
m2 <- as.matrix(tdm_new)
distMatrix <- dist(scale(m2))
fit<-hclust(distMatrix,method = 'ward.D')
png('cluster_dendogram_isis.png')
plot(fit)
dev.off()
#Kmean clustering of tweets
m3 <- t(m2)
set.seed(123)
k<-8
kmeansResult <- kmeans(m3, k)
round(kmeansResult$centers,digit=3)
for(i in 1:k){
  cat(paste(" cluster ", i, ": ", sep=""))
  s <- sort(kmeansResult$centers[i,], decreasing=T)
  cat(names(s)[1:3], "\n")
  
}
library(igraph)
m4 <- m2
m4[m4>=1]<- 1
termmatrix <- m4 %*% t(m4)
g <- graph.adjacency(termmatrix,weighted = T , mode = 'undirected')
g <- simplify(g)
V(g)$label <- V(g)$name
V(g)$degree <- degree(g)
set.seed(100)
layout1 <- layout.fruchterman.reingold(g)
plot(g , layout = layout1,main='Network of Terms')
library(networkD3)
isis$username<- as.character(isis$username)
isis$tweets <- as.character(isis$tweets)
isis$tweets <- gsub("(f|ht)(tp)(s?)(://)(.*)[.|/](.*)",'',isis$tweets)
#here we get tweets which are retweets
rt_patterns = grep("(RT|via)((?:\\b\\W*@\\w+)+)",isis$tweets, ignore.case=TRUE)
isis_sub <- isis[rt_patterns,]
u <- isis_sub$username
isis_sub$tweets <- gsub(':','',isis_sub$tweets)
isis_sub$tweets <- strsplit(isis_sub$tweets, " ")
at_who <- lapply(isis_sub$tweets, function(xx)xx[grepl("@[[:alnum:]]", xx)])
#here we remove the @ character
at_who <- str_extract_all(at_who,'(?<=@)\\w+')
u = unlist(u)
at_who <- unlist(at_who)
df <- cbind(u,at_who)
df <- unique(df)

df_df <- as.data.frame(df)

simpleNetwork(df_df,charge = -500 , opacity = 0.6, zoom = T, fontSize = 10) %>% saveNetwork(file='network_isis.html')

# Any results you write to the current directory are saved as output.
