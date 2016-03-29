library(zoo)
source("dohvat_podataka.R")
LDLFLAG<-2048
KONTROLAFLAG<-1024
OBUHVATFLAG<-524288
TZ<-'CET'


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


trim<-function(x){
  gsub("^\\s+|\\s+$","",x)
}

fn<-function(k) {
  function(t) {
    paste0(t,'.',k)
  }
}


status_agr<-function(statusi) {
  i=0
  for ( n in statusi) {
    i=bitwOr(i,n)
  }
  i
}

podacinox<-function(pocetak, kraj, postaja, usporedno=0) {
  komplist<-c("NO","NOx","NO2")
  obj<-podaci(pocetak, kraj, postaja, komplist, usporedno)
  obj$noxkomponenta<-obj$komponenta
  obj$komponenta<-obj$komponenta[-3]
  class(obj)<-append(class(obj),"podataknox")
  obj
}

podacipm<-function(pocetak, kraj, postaja, usporedno=0) {
  komplist<-c("PM10","PM2.5","PM1")
  podaci(pocetak, kraj, postaja, komplist, usporedno)
}


podaci<-function(pocetak, kraj, postaja, komplist, usporedno=0) {
  pocetak<-as.POSIXct(pocetak, tz=TZ)
  kraj<-as.POSIXct(kraj, tz=TZ)
  db<-aqdb()
  komponente<-lapply(komplist,FUN=function(k) getKomponenta(db,k))
  names(komponente)<-komplist
  
  readfun<-function(x) {
    program_id<-getProgram(db,postaja, x, usporedno)[1]
    df<-read.zoo(getSiroviPodatak(db,program_id,pocetak, kraj)[,-c(1,2,6,7,8,9,10)], format = '%Y-%m-%d %H:%M', tz='GMT', FUN=as.POSIXct, index=1)
    names(df)<-c(paste0("vrijednost.",x), paste0("status.",x))
    df
  }
  pod <- lapply(komplist, readfun)
  names(pod)<-komplist
  pod<-do.call(merge, pod)
  obj<-list(pocetak=pocetak, kraj=kraj, postaja=postaja, komponenta=komponente, koeficijenti=NA, minutni=pod, korigirani=NA, satni=NA)
  class(obj)<-append(class(obj),"podatak")
  obj
}

primijeniKorekciju.podataknox<-function(pod, koef) {
  print("korekcija:nox")
  df<-pod$korigirani
  pod$komponenta<-pod$noxkomponenta
  komponente<-pod$komponenta
  
  nox<-df[,'vrijednost.NOx']/komponente$NOx$konv_v_u_m
  no<- df[,'vrijednost.NO']/komponente$NO$konv_v_u_m
  
  df[,'vrijednost.NO2']<-komponente$NO2$konv_v_u_m*(nox-no)
  df[,'status.NO2']<-bitwOr(df[,'status.NOx'],df[,'status.NO'])

  df$Sr.NO2<-df[,'Sr.NOx']
  tf<-!is.na(df[,'vrijednost.NO2']) & (df[,'vrijednost.NO2'] < (-df[,'Sr.NO2']))
  df[tf,'status.NO2']<- bitwOr(df[tf,'status.NO2'],LDLFLAG)
  pod$korigirani<-df
  NextMethod()
}

primijeniKorekciju.podatak<-function(pod, koef) {
  if (!require(lubridate)) stop("Nije instaliran paket lubridate")
  print("korekcija:multi")
  df<-merge(pod$minutni,koef)
  idx<-index(df)
  for(k in pod$komponenta) {
    f<-fn(k$formula)
    df[,f('A')]<-na.approx(df[,f('A')],rule=2)
    df[,f('B')]<-na.approx(df[,f('B')],rule=2)
    df[,f('Sr')]<-na.locf(df[,f('Sr')], na.rm=F)
    df[,f('vrijednost')]<-df[,f('vrijednost')]*df[,f('A')]+df[,f('B')]
    tf<-!is.na(df[,f('vrijednost')]) & (df[,f('vrijednost')] < (-df[,f('Sr')]))
    df[tf,f('status')]<- bitwOr(df[tf,f('status')],LDLFLAG)
  }
  time(df)=with_tz(time(df),tz=TZ)  
  pod$korigirani<-df[time(df) > pod$pocetak & time(df)<=pod$kraj,]
  NextMethod("primijeniKorekciju",pod)
}

napraviNevaljan.podatak<-function(pod, od, do, komplist) {
  if ( missing(do) ) {
    do <- od
  }
  od<-as.POSIXct(od, tz=TZ)  
  do<-as.POSIXct(do, tz=TZ)  
  if ( missing(komplist)) {
    komplist<-sapply(pod$komponenta, function(x) x$formula)
  }
  for(i in komplist) {
    f<-fn(i)
    tf<-time(pod$korigirani)>=od & time(pod$korigirani)<= do
    pod$korigirani[tf,f('status')]<-bitwOr(pod$korigirani[tf,f('status')],KONTROLAFLAG)
  }
  pod
}

napraviValjan.podatak<-function(pod, od, do, komplist) {
  mask<-bitwNot(KONTROLAFLAG)
  if ( missing(do) ) {
    do <- od
  }
  od<-as.POSIXct(od, tz=TZ)  
  do<-as.POSIXct(do, tz=TZ)  
  
  if ( missing(komplist)) {
    komplist<-sapply(pod$komponenta, function(x) x$formula)
  }
  for(i in komplist) {
    f<-fn(i)
    tf<-time(pod$korigirani)>=od & time(pod$korigirani)<= do
    pod$korigirani[tf,f('status')]<-bitwAnd(pod$korigirani[tf,f('status')],mask)
  }
  pod
}

ucitajKoeficijente<-function(obj,fajl) {
  df<-read.csv(fajl, header=TRUE, sep=';')
  for(komp in obj$komponenta) {
    f<-fn(komp$formula)
    if(f('A') %in% colnames(df)){
      df[,f('B')]<-df[,f('B')] * komp$konv_v_u_m
      df[,f('Sr')]<-3.33*komp$konv_v_u_m*df[,f('Sr')]/df[,f('A')]
    } else {
      df[,f('A')]<-1
      df[,f('B')]<-0
      df[,f('Sr')]<-3.33*komp$konv_v_u_m
    }
  }
  df<-read.zoo(df, format = '%d.%m.%Y %H:%M', tz=TZ, FUN=as.POSIXct, index=1)
  primijeniKorekciju(obj, df)
}

broj_ispod_dl.podatak<-function(pod) {
  ispod_dl_komponenta<-function(komp){
    f<-fn(komp$formula)
    length(pod$korigirani[bitwAnd(pod$korigirani[,f('status')],LDLFLAG)==LDLFLAG,f('status')])
  }
  list<-lapply(pod$komponenta, ispod_dl_komponenta)
  names(list)<-sapply(pod$komponenta, function(x) x$formula)
  list
}

agregiraj.podatak<-function(pod) {
  if ( !require(data.table) ) stop("Nije instaliran paket data.table")

  niz<-as.POSIXct(seq(0,unclass(pod$kraj)[1]-unclass(pod$pocetak)[1], by=3600 ), origin=pod$pocetak)
  niz<-zoo(niz, niz)
  df<-merge(pod$korigirani, niz)
  df$niz2<-na.locf(df$niz,fromLast=T)
  ddt<-data.table(df)
  
  agregiraj_komponentu<-function(k){
    komp<-k$formula
    f<-fn(komp)
    tf<-ddt[,get(f('status'))]<KONTROLAFLAG
    na<-!is.na(ddt[,get(f('status'))])
    
    jj<-ddt[na&tf,list(mean(get(f('vrijednost')),na.rm = F),sum(get(f('vrijednost')),na.rm = T),length(get(f('vrijednost')))), by='niz2']
    ss<-ddt[na,status_agr(get(f('status'))),by='niz2']
    satni<-merge(jj,ss, by='niz2')
    names(satni)[-1]<-c(paste0('vrijednost.',komp),paste0('sum.',komp),paste0('n.',komp),paste0('status.',komp))
    satni<-zoo(satni, as.POSIXct(satni$niz2,origin='1970-01-01'))
    satni[satni[,f('n')]<45,f('status')] <- bitwOr(satni[satni[,f('n')]<45,f('status')],OBUHVATFLAG)
    subset(satni,select=-c(niz2))
  }
  svi<-do.call(merge, lapply(pod$komponenta, agregiraj_komponentu))
  pod$satni<-svi
  pod  
}


plot.podatak<-function(pod,xrange=NA, korigirani=T, nekorigirani=F, satni=T,  ...){
  
  par(mfrow=c(length(pod$komponenta),1)) 
  if ( missing(xrange) )  { 
    xrange <- c(pod$pocetak, pod$kraj)
  }
  xrange<-as.POSIXct(xrange, tz=TZ)  
  for(k in pod$komponenta) {
    f<-fn(k$formula)
    
    
    podopseg<-function(df, uvjet) {
      df[(df[,f('status')] < uvjet) & time(df)>= xrange[1] & time(df)<= xrange[2],]
    }
    yrange<-NA
    l<-list()
    if ( korigirani ) {
      l$red<-podopseg(pod$korigirani, KONTROLAFLAG)
      yrange<-range(yrange, range(l$red[,f('vrijednost')]), na.rm=T)
    }
    if ( nekorigirani ) {
      l$green<-podopseg(pod$minutni, KONTROLAFLAG)
      yrange<-range(yrange, range(l$green[,f('vrijednost')]), na.rm=T)
    }
    if ( satni ) {
      l$blue<-podopseg(pod$satni, OBUHVATFLAG)
      yrange<-range(yrange, range(l$blue[,f('vrijednost')]),na.rm=T)
    }
    tmp<-plot(xrange, yrange, type='n', xlab = "vrijeme", ylab = paste0("c(",k$formula,")"))
    tmp<-lapply(names(l), function(x) lines(l[[x]][,f('vrijednost')], col=x))
  }
}

#write.satni<-function(pod, fname) {
#  write.zoo(pod$satni[,-c(1)], file=fname, col.names = T, sep=';')
#}
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
