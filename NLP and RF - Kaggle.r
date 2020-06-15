#Importing the dataset
dataset_train = read.csv("../input/train.csv")
dataset_test = read.csv("../input/train.csv")

# Combining the dataset
dataset_train2 = dataset_train[,-5]
combi = rbind(dataset_train2,dataset_test)

# Cleaning the texts
#install.packages('tm')
library(tm)

#install.packages('SnowballC')
library(SnowballC)

# Creating the corpus
corpus = VCorpus(VectorSource(combi$text))
corpus = tm_map(corpus, content_transformer(tolower))
corpus = tm_map(corpus, removeNumbers)
corpus = tm_map(corpus, removePunctuation)
corpus = tm_map(corpus, removeWords, stopwords())
corpus = tm_map(corpus, stemDocument)
corpus = tm_map(corpus, stripWhitespace)

Spc <- content_transformer(function(x, pattern) gsub(pattern, " ", x))
corpus <- tm_map(corpus, Spc, "â") 
corpus <- tm_map(corpus, Spc, "@")
corpus <- tm_map(corpus, Spc, "<")
corpus <- tm_map(corpus, Spc, "~")
corpus <- tm_map(corpus, Spc, "#")
corpus <- tm_map(corpus, Spc, "Y")
corpus <- tm_map(corpus, Spc, "ð")
corpus <- tm_map(corpus, Spc, "®")
corpus <- tm_map(corpus, Spc, "???")
corpus <- tm_map(corpus, Spc, "T")
corpus <- tm_map(corpus, Spc, "%")
corpus <- tm_map(corpus, Spc, "ã")
corpus <- tm_map(corpus, Spc, ">")
corpus = tm_map(corpus, stripWhitespace)

# Creating the Bag of Words model
dtm = DocumentTermMatrix(corpus)
dtm = removeSparseTerms(dtm, 0.999)
combidataset = as.data.frame(as.matrix(dtm))

# Splitting the data for model building
combitrain = combidataset [1:7613,]
combitest = combidataset [7614:10876,]

combitrain$target_final = dataset_train$target

str(combitrain)
combitrain$target_final = as.factor(combitrain$target_final)


# Applying Random Forest model Training set
library(randomForest)
model = randomForest(x = combitrain[,-1570],
                          y = combitrain$target_final,
                          ntree = 10)


# Predicting the Test set results
Pred = predict(model,combitest)
sample_submission = read.csv(file.choose())
submit<-data.frame(id = sample_submission$id, target = Pred)

# writing the data to Submit file
#write.csv(submit, file = "Submission.csv",row.names=FALSE)
                           
# Public Score - 0.76
