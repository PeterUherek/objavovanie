library(RMySQL)
library(yaml)

VISITS_MINIMUM = 5000

db_config = yaml.load_file("db_config.yml")
mydb = dbConnect(MySQL(), host=db_config$db$host, dbname=db_config$db$name, user=db_config$db$user, password=db_config$db$pass)
run(mydb)
dbDisconnect(mydb)

run <- function(mydb) {
  #Inicializovanie vektorov
  rank <-c() # Zoznam hodnot celkoveho poctu pristupov, ktore sa mapuju na casy pristupov. 
  vec_of_times <- c() #Casy pristupov
  
  rs = dbSendQuery(mydb, sprintf("SELECT id FROM articles as a where date(a.published_at) = '2009-10-07' limit 2", VISITS_MINIMUM))
  data = fetch(rs, n=-1)
  
  # Prechadzanie vsetkych clankov pre dany den
  for (article_id in data$id){
    print(article_id)
    
    output <- get_rank_and_time(article_id,vec_of_times,rank,mydb)
    rank <- output[[1]]
    vec_of_times <- output[[2]] 
  }
  
  # Generovanie obrazku
  plot(vec_of_times,rank)
  # Generovanie regresie
  res=lm(rank~vec_of_times)
  # Zobrazenie regresie v obrazku
  abline(res)
}

get_rank_and_time <- function(article_id,vec_of_times,rank,mydb){
  date <- get_date(article_id,mydb)
  #Vyrobenie pomocneho vekora. Hodnoty tvori zoznam poctu pristupov,
  #ktory sa bude mapovat na vec_of_vectors. 
  r <- c(1:length(date))
  
  #Priadnie pomocneho zoznamu poctu pristupov do celkoveho zoznamu pristupov.
  rank <- append(rank,r)
  vec_of_times <- get_time(date,r,vec_of_times)
  output <- list(rank,vec_of_times)
  return (output)

}


get_date <- function(article_id,mydb){
  rs = dbSendQuery(mydb, sprintf("select distinct(v.cookie), v.happened_at from sme2.visits as v join sme2.articles a on v.sme_id=a.id where v.sme_id = %d and v.happened_at >= a.published_at and v.happened_at <= DATE_ADD(a.published_at,INTERVAL 1 DAY)",article_id))
  data = fetch(rs, n=-1)
  #Formatovanie casu pre vsetky pristupy
  date <- as.POSIXct(data[[2]], format = "%Y-%m-%d %H:%M:%S")
  return(date)
}

get_time <- function(date,r,vec_of_times){
  for(n in r){
    #Vypocet doby pristupu v sekundach od casu publikovania clanku
    different_time <- difftime(date[n],date[1],units='sec')
    #Pridanie novej hodnoty do zoznamu.
    vec_of_times <- append(vec_of_times,different_time)
  }
  return(vec_of_times)
}





