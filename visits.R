library(RMySQL)

mydb = dbConnect(MySQL(), user='root', password='asdf456', dbname='sme2', host='localhost',CharSet='utf8')
dbGetQuery(mydb, "SET NAMES 'cp1250'")
for (den in 11:20 ) {
  print(den)
q <- sprintf("select g.sme_id,count(g.sme_id) as visits from (select v.sme_id from visits as v join sme2.articles a on v.sme_id=a.id where date(a.published_at) = '2009-10-%d' and v.happened_at >= a.published_at and v.happened_at <= DATE_ADD(a.published_at,INTERVAL 1 DAY) group by v.sme_id,v.cookie) as g group by g.sme_id",den)
rs = dbSendQuery(mydb,q)
data = fetch(rs, n=-1)
data$visits[[1]]
length(data$sme_id)

for(i in 1:length(data$sme_id)) {
  print(i)
  query <- sprintf("UPDATE sme2.articles SET uvisits = %d where id= %d",data$visits[[i]],data$sme_id[[i]])
  dbGetQuery(mydb, "SET NAMES 'utf8'")
  dbGetQuery(mydb, query)
}
}