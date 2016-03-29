getProgram<-function(con,postaja, komponenta, usporedno) UseMethod("getProgram")
getSiroviPodatak<-function(con,program, pocetak, kraj) UseMethod("getSiroviPodatak")
getKomponenta<-function(con,formula) UseMethod("getKomponenta")
getKomponente<-function(con,vrsta) UseMethod("getKomponente")
getPostaja<-function(con,nacionalna_oznaka) UseMethod("getPostaja")
getPostaje<-function(con) UseMethod("getPostaje")



aqdb<-function(){
  if ( !require(RPostgreSQL)) stop("Nije instaliran RPostgreSQL")
  user<-"kraljevic"
  host<-"uran.gric.dhz.hr"
  dbname<-"aqdb"
  password<-"C6bm7@AqDb"
  con<-function(){
    dbConnect(dbDriver("PostgreSQL"), user=user, host=host, dbname=dbname, password=password)
  }
  class(con)<-append(class(con),'aqdb')
  con
}

getProgram.aqdb<-function(con,postaja, komponenta, usporedno){
  con<-con()
  sql<-paste0("SELECT * FROM program_mjerenja pm JOIN postaja po on po.id = pm.postaja_id JOIN komponenta k on k.id = pm.komponenta_id WHERE po.nacionalna_oznaka = '",postaja,"' AND k.formula = '",komponenta,"' AND usporedno_mjerenje=",usporedno)
  program<-dbGetQuery(con, sql)
  dbDisconnect(con)
  program
  
}

getSiroviPodatak.aqdb<-function(con,program, pocetak, kraj){
  con<-con()
  sql<-paste0("SELECT * FROM podatak_sirovi WHERE program_mjerenja_id = ", program, " AND vrijeme > '",pocetak, "' AND vrijeme <= '",kraj, "' ORDER BY vrijeme")
  podaci<-dbGetQuery(con, sql)
  dbDisconnect(con)
  podaci
}

getKomponenta.aqdb<-function(con,formula){
  con<-con()
  sql<-paste0("SELECT * FROM komponenta k WHERE k.formula = '",formula,"'")
  program<-dbGetQuery(con, sql)
  dbDisconnect(con)
  program
}

getKomponente.aqdb<-function(con,vrsta){
  con<-con()
  sql<-paste0("SELECT * FROM komponenta k")
  if ( !missing(vrsta)){
    sql<-paste0(" WHERE k.vrsta = '",vrsta,"'")
  }  
  komponente<-dbGetQuery(con, sql)
  dbDisconnect(con)
  komponente
}

getPostaja.aqdb<-function(con,nacionalna_oznaka){
  con<-con()
  sql<-paste0("SELECT * FROM postaja p WHERE po.nacionalna_oznaka = '",nacionalna_oznaka,"'")
  program<-dbGetQuery(con, sql)
  dbDisconnect(con)
  program
}

getPostaje.aqdb<-function(con){
  con<-con()
  sql<-paste0("SELECT * FROM postaja")
  program<-dbGetQuery(con, sql)
  dbDisconnect(con)
  program
}

