library(RMySQL)
library(httr)
library(tm)

(removeCharacter <- content_transformer(function(x, pattern) gsub(pattern, "", x)))

be_lematized <-function(text){
  r <- POST("http://text.fiit.stuba.sk:8080/lematizer/services/lemmatizer/lemmatize/fast", body = text,content_type('text/plain'),add_headers(tools = "all",disam="true"))
  body_lematized <- httr::content(r,encoding='UTF-8')
  print(r$status)
  if (r$status==500)
  {
    return ("Error")
  }
  return(body_lematized)
}

be_updated <-function(text,id,mydb){
  query <- sprintf("UPDATE sme2.articles SET body_lematized = '%s' where id= %d",text,id)
  dbGetQuery(mydb, "SET NAMES 'utf8'")
  dbGetQuery(mydb, query)
}

big_database_update <-function(stopwords,mydb){
  
  dbGetQuery(mydb, "SET NAMES 'cp1250'")
  rs = dbSendQuery(mydb, "SELECT body_no_html,id FROM sme2.articles where body_lematized = 'Error' and body_no_html != ''")
  data = fetch(rs, n=-1)
  ids = data$id
  print(ids)
  
  doc.vec <- VectorSource(data$body_no_html)
  doc.corpus <- Corpus(doc.vec)
  
  #(beLematized <- content_transformer(function(x) httr::content(POST("http://text.fiit.stuba.sk:8080/lematizer/services/lemmatizer/lemmatize/fast", body = x,content_type('text/plain')),encoding='UTF-8')))
  #doc.corpus <- tm_map(doc.corpus, beLematized)
  doc.corpus <- tm_map(doc.corpus, removeCharacter, "[]\\?!\"\'#$%&(){}+*/:;,.„“–_´`|~\\[<=>@\\^-]")
  doc.corpus <- tm_map(doc.corpus, tolower)
  doc.corpus <- tm_map(doc.corpus, removeNumbers)
  doc.corpus <- tm_map(doc.corpus, stripWhitespace)
  doc.corpus <- tm_map(doc.corpus, removeWords,stopwords) 
  doc.corpus <- tm_map(doc.corpus, stripWhitespace)
  
 
  for (i in 1:length(doc.corpus)){
    if(doc.corpus[[i]]==" "){ doc.corpus[[i]] = "No_body"}
    if(doc.corpus[[i]]!=""){ doc.corpus[[i]]<-be_lematized(doc.corpus[[i]]) }
  }
  
  doc.corpus <- tm_map(doc.corpus, removeWords,stopwords) 
  doc.corpus <- tm_map(doc.corpus, stripWhitespace)
  
  for (i in 1:length(doc.corpus)){
    be_updated(doc.corpus[[i]],ids[i],mydb)
  }
}

TEXTFILE = "C:/Users/Peter/Documents/GitHub/objavovanie/slovak_stopwords.txt"
stopwords = readLines(TEXTFILE,encoding='UTF-8')

mydb = dbConnect(MySQL(), user='root', password='asdf456', dbname='sme2', host='localhost',CharSet='utf8')
dbGetQuery(mydb, "SET NAMES 'cp1250'")

rs = dbSendQuery(mydb, "select count(*) from articles where body_lematized = 'Error' and body_no_html != ''")
count_raw = fetch(rs, n=-1)
print (count_raw)
all_count = strtoi(count_raw)
count = all_count/50


for (i in 1:count){
  
  print(i)
  big_database_update(stopwords,mydb)
}