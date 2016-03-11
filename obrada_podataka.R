require(zoo)
require(lubridate)
require(RPostgreSQL)
require(data.table)
LDLFLAG<-2048
KONTROLAFLAG<-1024
OBUHVATFLAG<-524288
TZ<-'CET'

trim<-function(x){
  gsub("^\\s+|\\s+$","",x)
}

fn<-function(k) {
  function(t) {
    paste0(t,'.',k)
  }
}

getProgramID<-function(postaja, komponenta, usporedno){
  con<-dbConnect(dbDriver("PostgreSQL"), user = "kraljevic", host="uran.gric.dhz.hr", dbname="aqdb", password="C6bm7@AqDb")
  sql<-paste0("SELECT * FROM program_mjerenja pm JOIN postaja po on po.id = pm.postaja_id JOIN komponenta k on k.id = pm.komponenta_id WHERE po.nacionalna_oznaka = '",postaja,"' AND k.formula = '",komponenta,"' AND usporedno_mjerenje=",usporedno)
  program<-dbGetQuery(con, sql)
  dbDisconnect(con)
  program[1]
  
}

getSiroviPodatak<-function(program, pocetak, kraj){
  con<-dbConnect(dbDriver("PostgreSQL"), user = "kraljevic", host="uran.gric.dhz.hr", dbname="aqdb", password="C6bm7@AqDb")
  sql<-paste0("SELECT * FROM podatak_sirovi WHERE program_mjerenja_id = ", program, " AND vrijeme > '",pocetak, "' AND vrijeme <= '",kraj, "' ORDER BY vrijeme")
  podaci<-dbGetQuery(con, sql)
  dbDisconnect(con)
  podaci
}

getKomponenta<-function(komponenta){
  con<-dbConnect(dbDriver("PostgreSQL"), user = "kraljevic", host="uran.gric.dhz.hr", dbname="aqdb", password="C6bm7@AqDb")
  sql<-paste0("SELECT * FROM komponenta k WHERE k.formula = '",komponenta,"'")
  program<-dbGetQuery(con, sql)
  dbDisconnect(con)
  program
}

status_agr<-function(statusi) {
  i=0
  for ( n in statusi) {
    i=bitwOr(i,n)
  }
  i
}


napraviNevaljan<-function(pod, od, do) UseMethod("napraviNevaljan")
napraviNevaljan.default<-function(pod, od, do) pod

napraviValjan<-function(pod, od, do) UseMethod("napraviValjan")
napraviValjan.default<-function(pod, od, do) pod

primijeniKorekciju <- function(pod, koef) UseMethod("primijeniKorekciju")
primijeniKorekciju.default<-function(pod, koef) pod

broj_ispod_dl<- function(pod) UseMethod("broj_ispod_dl")
broj_ispod_dl.default<- function(pod) pod

agregiraj <- function(pod) UseMethod("agregiraj")
agregiraj.default <- function(pod) pod


# podaci<-function(pocetak, kraj, postaja, komponenta, usporedno=0 ) {
#   pocetak<-as.POSIXct(pocetak, tz='CET')
#   kraj<-as.POSIXct(kraj, tz='CET')
#   konv_vm<-getKomponenta(komponenta)$konv_v_u_m
#   pod<-getSiroviPodatak(getProgramID(postaja, komponenta,usporedno), pocetak, kraj)
#   pod<-read.zoo(pod[,-c(1,2,7,8)], format = '%Y-%m-%d %H:%M', tz='GMT', FUN=as.POSIXct, index=1)
#   obj<-list(pocetak=pocetak, kraj=kraj, postaja=postaja, komponenta=komponenta, konv_vm=konv_vm, koeficijenti=NA, minutni=pod, korigirani=NA, satni=NA)
#   class(obj)<-append(class(obj),"podaci")
#   obj
# }
# 
# ucitajKoeficijente.podaci<-function(obj,fajl) {
#   obj$koeficijenti<-read.zoo(fajl, format = '%d.%m.%Y %H:%M', tz='CET', FUN=as.POSIXct, header=TRUE, index=1, sep=';')
#   obj$koeficijenti$B<-obj$koeficijenti$B*obj$konv_vm
#   obj$koeficijenti$Sr<-obj$koeficijenti$Sr*obj$konv_vm
#   obj$koeficijenti$DL<-3.33*obj$koeficijenti$Sr/obj$koeficijenti$A
#   primijeniKorekciju(obj)
# }
# 
# 
# 
# 
#   
# primijeniKorekciju.podaci<-function(pod, koef=NA) {
#   df<-merge(pod$minutni, pod$koeficijenti)
#   df$A<-na.approx(df$A, rule=2)
#   df$B<-na.approx(df$B, rule=2)
#   df$DL<-na.locf(df$DL, na.rm=F)
#   df$vrijednostKor<-df$vrijednost*df$A+df$B
#   df[df$vrijednostKor< (-df$DL),'status']<-bitwOr(df[df$vrijednostKor< (-df$DL),'status'],LDLFLAG)
#   time(df)=with_tz(time(df),tz="CET")  
#   pod$korigirani<-df[time(df) > pod$pocetak & time(df)<=pod$kraj,c('vrijednostKor','status')]
#   names(pod$korigirani)[1]<-'vrijednost'
#   pod
# }
# 
# 
# 
# broj_ispod_dl.podaci<-function(pod) {
#   length(pod$korigirani$status[bitwAnd(pod$korigirani$status,LDLFLAG)==LDLFLAG])
# }
# 
# agregiraj.podaci<-function(pod) {
#   niz<-as.POSIXct(seq(0,unclass(pod$kraj)[1]-unclass(pod$pocetak)[1], by=3600 ), origin=pod$pocetak)
#   niz<-zoo(niz, niz)
#   df<-merge(pod$korigirani, niz)
#   df$niz2<-na.locf(df$niz,fromLast=T)
#   ddt<-data.table(df)
#   ddt<-ddt[,list(vrijednost=mean(vrijednost[status<KONTROLAFLAG], na.rm=T), suma=sum(vrijednost[status<KONTROLAFLAG],na.rm=T),broj=length(status[status<KONTROLAFLAG]), status=status_agr(status)),by='niz2']
#   ddt[broj<45]$status <- bitwOr(ddt[broj<45]$status,OBUHVATFLAG)
#   ddt[vrijednost==-999]$status <- OBUHVATFLAG
#   ff<-zoo(ddt, as.POSIXct(ddt$niz2,origin='1970-01-01'))
#   pod$satni<-ff
#   pod  
# }
# 
# 
# plot.podaci<-function(pod,xrange=NA, korigirani=T, nekorigirani=F, satni=T,  ...){
#   if ( missing(xrange) )  { 
#     xrange <- range(pod$pocetak, pod$kraj)
#   }
#   podopseg<-function(df, uvjet) {
#     print(uvjet)
#     df[df$status<uvjet &time(df)>= xrange[1] & time(df)<= xrange[2],]
#   }
#   yrange<-NA
#   l<-list()
#   if ( korigirani ) {
#     l$red<-podopseg(pod$korigirani, KONTROLAFLAG)
#     yrange<-range(yrange, range(l$red$vrijednost), na.rm=T)
#   }
#   if ( nekorigirani ) {
#     l$green<-podopseg(pod$minutni, KONTROLAFLAG)
#     yrange<-range(yrange, range(l$green$vrijednost), na.rm=T)
#   }
#   if ( satni ) {
#     l$blue<-podopseg(pod$satni, OBUHVATFLAG)
#     yrange<-range(yrange, range(l$blue$vrijednost),na.rm=T)
#   }
# 
#   tmp<-plot(xrange, yrange, type='n')
#   tmp<-lapply(names(l), function(x) lines(l[[x]]$vrijednost, col=x))  
# }
# 
# 
# write.satni<-function(pod, fname) {
#   write.zoo(pod$satni[,-c(1)], file=fname, col.names = T, sep=';')
# }
#
# Radi samo za SO2, CO i O3. Ne radi za NOx i PM sa Grimma
#
#UPOTREBA
# 0. odrediti pocetak i kraj
# pocetak<-as.POSIXct('2014-12-31')
# kraj <- as.POSIXct('2016-01-02')
# 1. ucitati podatke
#     plitvice_ozon<-podaci(pocetak,kraj,'PLJ01','O3')
#
# 2. ucitati korekcijske koeficijente
#    plitvice_ozon<-ucitajKoeficijente(plitvice_ozon,'so2_koef.csv')
# datoteka sa korekcijskim koeficijentima mora biti oblika:
# vrijeme primjene od;A;B;Sr
# 12.12.2014 11:45;0.959;3.83;0.45
# 13.01.2015 11:40;0.91;11.27;0.6
#
# 3. agregirati podatke
#    plitvice_ozon<-agregiraj(plitvice_ozon)
#
# 4. spremiti podatke
#    write.satni(pod, 'ime_fajla')
#
# Crtanje:
# plot(plitvice_ozon, xrange=NA, korigirani=T, nekorigirani=F, satni=T)
# moze se odabrati opseg za crtanje npr:
# xrange = range(as.POSIXct('2015-02-01'),as.POSIXct('2015-04-02'))
# moze se odrediti hoce li se crtati korigirani minutni, nekorigirani minutni, satni
# xrange, korigirani,nekorigirani, satni su opcionalni argumenti i podrazumijevanje vrijednosti su:
# xrange= range(pocetak, kraj)
# korigirani=T
# nekorigirani=F
# satni=T
#
#
# za proglasiti opseg nevaljan
# plitvice_ozon<-napraviValjan(plitvice_ozon, as.POSIXct('2015-03-08'), as.POSIXct('2015-03-10'))
# za proglasiti opseg valjan
# plitvice_ozon<-napraviNevaljan(plitvice_ozon, as.POSIXct('2015-03-08'), as.POSIXct('2015-03-10'))
# nakon izmjene valjanosti potrebno je ponovno provesti agregaciju da bi se izracunali novi satni podaci
#
# broj_ispod_dl(plitvice_ozon) vraca broj podataka ispod -DL
#
