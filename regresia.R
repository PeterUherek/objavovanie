library(RMySQL)
library(dplyr)

mydb = dbConnect(MySQL(), user='root', password='asdf456', dbname='sme2', host='localhost')
rank <-c()
vec_of_times <- c()
rs = dbSendQuery(mydb, "SELECT id FROM sme2.articles where visits > 5000 and id_category = 11")
data = fetch(rs, n=-1)

for (article_id in data$id){
print(article_id)
output <- myfunction3(article_id,vec_of_times,rank,mydb)
rank <- output[[1]]
vec_of_times <- output[[2]] 
}

plot(vec_of_times,rank)
res=lm(rank~vec_of_timetys)
abline(res)


myfunction3 <- function(article_id,vec_of_times,rank,mydb){
date <- myfunction(article_id,mydb)
r <- c(1:length(date))
rank <- append(rank,r)
vec_of_times <- myfunction2(date,r,vec_of_times)
output <- list(rank,vec_of_times)
return (output)
}

myfunction <- function(article_id,mydb){
rs = dbSendQuery(mydb, sprintf("select distinct(cookie), happened_at  from sme2.visits where sme_id = %d",article_id))
data = fetch(rs, n=-1)
date <- as.POSIXct(data[[2]], format = "%Y-%m-%d %H:%M:%S")
return(date)
}

myfunction2 <- function(date,r,vec_of_times){
	for(n in r){
		different_time <- difftime(date[n],date[1],units='sec')
		vec_of_times <- append(vec_of_times,different_time)
	}
return(vec_of_times)
}





