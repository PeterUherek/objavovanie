library(RMySQL)
library(yaml)
library(tm)
library(lattice)
library(DAAG)
library(slam)
library(reshape2)
library(ggplot2)
library(boot)

formula_factory <- function(prepared_data){
  
  formula_string <- 'visits ~'
  vector_of_words <- colnames(prepared_data)
  
  length_without_visits <- length(vector_of_words)-1 
  
  for (i in 1:length_without_visits){
    
    x <- toString(vector_of_words[[i]])
    if(i != length_without_visits) { x <- paste(x,' +',sep="") } 
    formula_string <- paste(formula_string, x, sep = " ")
  }  
  return (formula_string)
}


db_config = yaml.load_file("C:/Users/Peter/Documents/GitHub/objavovanie/db_config.yml")
mydb = dbConnect(MySQL(), host=db_config$db$host, dbname=db_config$db$name, user=db_config$db$user, password=db_config$db$pass)

dbGetQuery(mydb, "SET NAMES 'cp1250'")
rs = dbSendQuery(mydb, "SELECT a.uvisits, a.words, CONCAT_WS(' ',a.body_lematized, c.name,s.name,e.name) AS body_lematized
                        FROM articles a
                         LEFT JOIN sme_categories c ON a.id_category=c.id
                 LEFT JOIN sme_sections s ON s.id=a.id_section
                 LEFT JOIN sections e ON e.id=a.id_section
                 WHERE a.body_lematized != 'Error' 
                 AND  date(a.published_at) >= '2009-10-07' AND date(a.published_at) < '2009-10-20'
                 AND a.body_lematized != ''
                 AND a.uvisits != 0
                 AND a.words != 0
                 AND a.uvisits < 10000 ")
data = fetch(rs, n=-1)

doc.vec <- VectorSource(data$body_lematized)

doc.corpus <- Corpus(doc.vec)

doc.corpus <- tm_map(doc.corpus, removeWords, stopwords)
summary(doc.corpus)
#TDM <- TermDocumentMatrix(doc.corpus, control = list(weighting = weightTfIdf))
DTM <- DocumentTermMatrix(doc.corpus, control = list(weighting = weightTfIdf))

#Treba nastavit hodnotu kolko slov vymazeme a zoberieme len tie najviac vyskytujuce sa 
DTM.common = removeSparseTerms(DTM, 0.996)
dim(DTM)
dim(DTM.common)

DTM.dense <- as.matrix(DTM.common)

DTM.dense<- cbind(DTM.dense, uvisits = log(data$uvisits))
DTM.dense<- cbind(DTM.dense, wordz = log(data$words)) 
prepared_data <- as.data.frame(DTM.dense)

#formula_string <- formula_factory(prepared_data)

# 10 fold cross-validation
regression <- CVlm(df=prepared_data,form.lm=formula(uvisits ~ .), m=10,plotit = 'Observed', legend.pos = 'right') 


list_of_error = c()
list_of_square = c()

for(i in 1:length(regression$Predicted)){
  origin_value = exp(regression$uvisits[[i]])
  predicted_value = exp(regression$Predicted[[i]])
  e = abs(origin_value - predicted_value)
  list_of_error <- c(list_of_error, e)
}
e_mean=mean(list_of_error)
for(i in 1:length(list_of_error)){
  e1 = list_of_error[[i]] - e_mean
  e2 = (e1^2)
  list_of_square <-c(list_of_square, e2)
}
min(list_of_error)
max(list_of_error)
mean(list_of_error)
median(list_of_error)
MSE = sqrt(sum(list_of_square)/length(list_of_square))
MSE
#Deviance is a measure of goodness of fit of a generalized linear model. 
#Or rather, it’s a measure of badness of fit–higher numbers indicate worse fit.
d = 0
for (i in 1:length(regression$Predicted) ){
if(1000 < list_of_error[[i]]){
  d= d+1
  
}
}
d

vysledok = glm(uvisits ~ .,data=prepared_data)
#(cv.err <- cv.glm(prepared_data, vysledok)$delta)
(cv.err.10 <- cv.glm(prepared_data, vysledok, K = 10)$delta)



#TDM.dense = melt(TDM.dense, value.name = "count")
#TDM.common = removeSparseTerms(TDM, 0.99)
#c<-inspect(DTM.common[1,1])
#plot(c[1,],ylab="Výskyt slova", xlab="Slovo")

