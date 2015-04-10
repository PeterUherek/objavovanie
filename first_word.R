library(RMySQL)
library(httr)
library(tm)


be_lematized <-function(text){
  print(text)
  r <- POST("http://text.fiit.stuba.sk:8080/lematizer/services/lemmatizer/lemmatize/fast", body = text,content_type('text/plain'),verbose())
  body_lematized <- httr::content(r,encoding='UTF-8')
  return(body_lematized)
}

be_updated <-function(text,id,mydb){
  query <- sprintf("UPDATE sme2.articles SET first_words = '%s' where id= %d",text,id)
  dbGetQuery(mydb, "SET NAMES 'utf8'")
  dbGetQuery(mydb, query)
}


mydb = dbConnect(MySQL(), user='root', password='asdf456', dbname='sme2', host='localhost',CharSet='utf8')
dbGetQuery(mydb, "SET NAMES 'cp1250'")
rs = dbSendQuery(mydb, "SELECT body_no_html,id FROM sme2.articles where visits > 5000 and body_lematized = '' and body_no_html != '' limit 20")
data = fetch(rs, n=-1)

ids = data$id

doc.vec <- VectorSource(data$body_no_html)
doc.corpus <- Corpus(doc.vec)

for (i in 1:length(doc.corpus)){
  doc.corpus <- tm_map(doc.corpus, tolower)
  first = strsplit(as.character(doc.corpus[[i]]), " ")[[1]]
  print(first[[1]])
}

