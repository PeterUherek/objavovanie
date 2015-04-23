library(RMySQL)
library(yaml)
library(tm)
library(lattice)
library(DAAG)
library(slam)
library(reshape2)
library(ggplot2)

db_config = yaml.load_file("db_config.yml")
mydb = dbConnect(MySQL(), host=db_config$db$host, dbname=db_config$db$name, user=db_config$db$user, password=db_config$db$pass)

dbGetQuery(mydb, "SET NAMES 'cp1250'")
rs = dbSendQuery(mydb, "SELECT visits, body_lematized
                        FROM articles 
                        WHERE body_lematized != 'Error' AND date(published_at) = '2009-10-07' limit 1000")
data = fetch(rs, n=-1)

doc.vec <- VectorSource(data[[1]])

doc.corpus <- Corpus(doc.vec)

doc.corpus <- tm_map(doc.corpus, removeWords, stopwords)
summary(doc.corpus)
TDM <- TermDocumentMatrix(doc.corpus, control = list(weighting = weightTfIdf))
DTM <- DocumentTermMatrix(doc.corpus, control = list(weighting = weightTfIdf))

#Treba nastavit hodnotu kolko slov vymazeme a zoberieme len tie najviac vyskytujuce sa 
DTM.common = removeSparseTerms(DTM, 0.9)
dim(DTM)
dim(DTM.common)

DTM.dense <- as.matrix(DTM.common)

DTM.dense<- cbind(DTM.dense, visits = data$visits)
 
prepared_data <- as.data.frame(DTM.dense)

# 3 fold cross-validation
CVlm(df=prepared_data,form.lm=formula(visits ~  vec + utorok ), m=3) 

#TDM.dense = melt(TDM.dense, value.name = "count")
#TDM.common = removeSparseTerms(TDM, 0.99)
#c<-inspect(DTM.common[1,1])
#plot(c[1,],ylab="Výskyt slova", xlab="Slovo")

