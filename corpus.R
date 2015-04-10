library(RMySQL)
library(tm)
library(slam)
library(reshape2)
library(ggplot2)

mydb = dbConnect(MySQL(), user='root', password='asdf456', dbname='sme2', host='localhost',CharSet='utf8')
dbGetQuery(mydb, "SET NAMES 'cp1250'")
rs = dbSendQuery(mydb, "SELECT body_prepared FROM sme2.articles where visits > 5000 and body_prepared != ''")
data = fetch(rs, n=-1)

TEXTFILE = "C:/Users/Peter/Documents/GitHub/objavovanie/slovak_stopwords.txt"
stopwords = readLines(TEXTFILE,encoding='UTF-8')
print(stopwords)


doc.vec <- VectorSource(data[[1]])

doc.corpus <- Corpus(doc.vec)

doc.corpus <- tm_map(doc.corpus, removeWords, stopwords)
summary(doc.corpus)
TDM <- TermDocumentMatrix(doc.corpus)
DTM <- DocumentTermMatrix(doc.corpus)

TDM.dense <- as.matrix(TDM)
TDM.dense = melt(TDM.dense, value.name = "count")


ggplot(TDM.dense, aes(x = Docs, y = Terms, fill = log10(count))) +
     geom_tile(colour = "white") +
       scale_fill_gradient(high="#FF0000" , low="#FFFFFF")+
       ylab("") +
       theme(panel.background = element_blank()) +
       theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
