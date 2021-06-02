import_libs <- function(){
  library(dplyr)
  library(jsonlite)  
  
}



#Hatali guzergahi duzeltiyor
Guzergah_cozucu<- function(gps,isdebug=FALSE){
  #json cozumlemesi
  gps <- gps %>% mutate(dt_old = dt)
  gps2<-gps
  if(!isdebug){
    gps2 <- gps2 %>% mutate(X1 = row_number(),dt = as.POSIXct(dt),rt=as.POSIXct(rt),coor = loc[[2]]) %>%
      mutate(index=X1) %>%
      mutate(lng = lapply(coor,function(x) x[1]) %>% unlist(),lat = lapply(coor,function(x) x[2]) %>% unlist()) %>%
      select(-c("coor"))
  }
  gps2<-gps2 %>% mutate(index=X1)
  gps2<-gps2 %>% select(X1,dt,rt,lat,lng,spd,index)
  #Zaman sapmalarini kontrol etme
  deneme<-gps2 %>% 
    mutate(ffd=c(diff(dt),0)) %>% 
    mutate(ffr = c(diff(rt),0)) %>%
    mutate(X1 = row_number())
  
  #Anomaliyi Saptama (Zaman Kaymalarını Kontrol Ederek)
  dtmean = mean(deneme$ffd,na.rm=TRUE)
  dtsd = sd(deneme$ffd,na.rm=TRUE)
  deneme<-deneme %>% 
    mutate(ano_dt = abs(mean(deneme$ffd,na.rm=TRUE)-deneme$ffd)/sd(deneme$ffd,na.rm=TRUE)>1.5) %>% 
    mutate(ano_rt = abs(mean(deneme$ffr,na.rm=TRUE)-deneme$ffr)/sd(deneme$ffr,na.rm=TRUE)>1.5) %>% select(X1,ffd,ffr,ano_dt,ano_rt) 
  ano_dt <- deneme %>% filter(ano_dt == TRUE) %>% slice(nrow(.):1)
  ano_rt <- deneme %>% filter(ano_rt == TRUE) %>% slice(nrow(.):1)
  defect_rate <- deneme %>%   
    mutate(dffd = c(0,diff(ffd)),dffr=c(0,diff(ffr))) %>% 
    mutate(pat = as.numeric(dffr==0)) %>% summarize(rate = (sum(pat)/n())*100) %>% as.integer()
  
  if(defect_rate<90 & defect_rate>65){
    zaman = ano_dt$ffd[1]
    gps2<-dogrukayipguzergah(gps2,zaman)
    count=nrow(gps2)
  }else{
    i<-1
    dt_uzunluk<- nrow(ano_dt)+1
    count <- 0
    while (i!=dt_uzunluk) {
      if(!yeraltikontrol(gps2,ano_dt,i,deneme)){
        kayip<-kayipguzergah(deneme,ano_dt,gps2,i)
        count <- count+nrow(kayip)
        if(all(is.na(kayip))){
          i=i+1
        }else{
          zaman = (as.integer(ano_dt$ffd[i]))/(as.integer(count(kayip)))
          dogrukayip<-dogrukayipguzergah(kayip,zaman)
          #zamanduzeltme fonksiyonu buraya eklenecek
          dogrukayip<-zamanduzeltme(dogrukayip)
          gps2<-yerdegistirme(gps2,dogrukayip,kayip)
          #Bu 3 if yerine tek bir kontrol mekanizmasi
          #(kayipin son elemanından kucuk ilk ano_dt'ye atlama )
          if(nrow(ano_dt)>1 & i != nrow(ano_dt) & kayip[nrow(kayip),]$X1<ano_dt[(i+1),]$X1){
            i=i+1
          }
          if(nrow(ano_dt)>1 & i != nrow(ano_dt) & kayip[nrow(kayip),]$X1<ano_dt[(i+1),]$X1){
            i=i+1
          }
          if(nrow(ano_dt)>1 & i != nrow(ano_dt) & kayip[nrow(kayip),]$X1<ano_dt[(i+1),]$X1){
            i=i+1
          }
          i=i+1
        }
      }else{
        i=i+1
      }
    }
  }
  
  
  gps <- gps[gps2$index,]
  # gps$dt <- gps2$dt
  gps$dt <- gps2$dt
  if(isdebug){
    gps <- gps %>% mutate(coor = loc[[2]]) %>%
      mutate(lng = lapply(coor,function(x) x[1]) %>% unlist(),lat = lapply(coor,function(x) x[2]) %>% unlist()) %>%  select(-c("coor"))  
  }
  change = (count/nrow(gps))*100
  ret <- list("data"=gps,"rate"=change,"count"=count)
  return(ret)
  
}




kayipguzergah <-function(deneme,ano_dt,gps,i){
  start <- as.integer(ano_dt$X1[i])
  if(!is.na(deneme$ffr[start+2]))
    start <- start+2
  else if(!is.na(deneme$ffr[start+1]))
    start <- start+1
  flag <- TRUE
  flag2 <- FALSE
  d <- filter(gps,X1==start)
  start <- start-1
  len_ano_dt <- nrow(ano_dt)
  while(flag){
    if(start<=3) 
      break
    if(!flag2 & i!=len_ano_dt & start==ano_dt[i+1,]$X1) 
      i<- i+1
    if(as.integer(deneme$ffr[start])==0 & as.integer(deneme$ffr[start-1])==0
       &as.integer(deneme$ffr[start-2])==0){
      flag2=TRUE
    }
    if(!flag2){
      d = rbind(d,filter(gps,X1==start))
      start=start-1
    }else if(flag2 & as.integer(deneme$ffr[start])!=0 & as.integer(deneme$ffr[start-1])!=0 & as.integer(deneme$ffr[start-2])!=0){
      if((len_ano_dt != i) & abs(start-ano_dt$X1[i+1]) < 10){
        
        d = rbind(d,filter(gps,X1==start))
        start=start-1
      }else{
        flag=FALSE
        
        d = rbind(d,filter(gps,X1==start))
        if(!is.na(deneme$ffr[start-1])) d = rbind(d,filter(gps,X1==start-1))
        if(!is.na(deneme$ffr[start-2])) d = rbind(d,filter(gps,X1==start-2))
        if(!is.na(deneme$ffr[start-3])) d = rbind(d,filter(gps,X1==start-3))
        if(!is.na(deneme$ffr[start-4])) d = rbind(d,filter(gps,X1==start-4))
      }
    }else{
      d = rbind(d,filter(gps,X1==start))
      start=start-1
    }
  }
  if(!flag2) 
    d<-NULL #0 pattern'e hic rastlanmamistir
  kayip <- as.data.frame(d)
  return(kayip)
}




dogrukayipguzergah<-function(kayip,zaman){
  #Ortalama Bir Zaman Bulma
  noktalar <- as.integer(count(kayip))
  
  #Noktalar Arasi Mesafe Datasını Olusturma
  msf<- kayip %>% 
    mutate(range = kayip$spd*(zaman/3.6)+((kayip$spd*(zaman/3.6)))/4) %>% 
    select(X1,lng,lat,range) 
  longs<- kayip %>% 
    select(lng)
  lats<- kayip %>% 
    select(lat)
  x<-mapply(earth.dist,rowwise(longs),rowwise(lats),msf$lng,msf$lat)
  msfdata <- as.data.frame(x)
  name <- kayip %>%
    select(X1)
  name <- as.list(name)
  colnames(msfdata)<-unlist(name)
  rownames(msfdata)<-unlist(name)
  msfdata <-msfdata %>% 
    rbind(range = msf$range)
  
  #Mesafe Datasinin cozumlenerek sonuca ulaşilmasi (Noktalari temsil eden kolonlardaki maksimum sayınınsatir ismi o noktadan sonra gelen noktayi temsil ediyor)
  msfdata2 <- msfdata %>% mutate_all(list(~last(.)-.)) 
  msfdata2 <- msfdata2[-nrow(msfdata),]
  rownames(msfdata2) <- colnames(msfdata)
  liste <-c()
  banli <-c()
  id =1
  uzunluk = as.integer(count(msfdata2))
  diag(msfdata2)<--9999
  id=uzunluk
  for(i in 1:uzunluk)
  {
    liste=c(liste,id)
    if(id>uzunluk){
      break
    }
    banli=c(banli,names(msfdata2[id]))
    temp<-msfdata2 %>% select(id) %>% subset(!rownames(msfdata2)%in%banli)
    if(i==uzunluk) break
    index=as.integer(which(msfdata2[,id]%>% subset(!rownames(msfdata2)%in%banli)== max(temp))[1])
    isim=row.names(msfdata2[,id,drop=FALSE]%>% subset(!rownames(msfdata2)%in%banli))[index]
    id=as.integer(which(rownames(msfdata2[,id,drop=FALSE]) ==isim))
    
  }
  liste<-rev(liste)
  dogrukayip<-kayip[liste,]
  return(dogrukayip)
}




#Long-Lat Koordinat Sisteminde Mesafe Bulma Fonksiyonu
#https://conservationecology.wordpress.com/2013/06/30/distance-between-two-points-in-r/
earth.dist <- function (long1, lat1, long2, lat2){
  rad <- pi/180
  a1 <- lat1 * rad
  a2 <- long1 * rad
  b1 <- lat2 * rad
  b2 <- long2 * rad
  dlon <- b2 - a2
  dlat <- b1 - a1
  a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
  c <- 2 * atan2(sqrt(a), sqrt(1 - a))
  R <- 6378.145
  d <- R * c
  d <- d*1000
  return(d)
}




#Duzeltilmis guzergahın geri dataframe'e eklenmesi
yerdegistirme<-function(gps,dogrukayip,kayip){
  tersdogru<-dogrukayip[dim(dogrukayip)[1]:1,]
  bas=as.integer(kayip$X1[dim(kayip)[1]]) 
  son=as.integer(kayip$X1[1])
  gps2<-gps
  gps2[bas:son,]<-tersdogru
  return(gps2)
}




#Aracin yeraltindan cikip cikmadigini saptama(Data'da islenmemesi gereken kisim)
#Fazla zaman alabilir duzeltilmesi gerekli
#Belli bir range icerisinde kontrol etmesi gerekiyor (Simdilik 100)
yeraltikontrol<-function(gps,ano_dt,i,deneme){
  start = as.integer(ano_dt$X1[i])
  if(start<= mean(gps$X1)){
    kontroldata<-gps[start:as.integer(nrow(gps)),]
    kontrol=start+100
  }
  else{
    kontroldata<-gps[1:start,]
    kontrol=start-100
  }
  farktablosu <- kontroldata %>% 
    mutate(flng = c(diff(lng),0)) %>% 
    mutate(flat = c(diff(lat),0)) %>% 
    mutate(fspd = c(diff(spd),0)) %>% 
    select(X1,flng,flat,fspd)
  
  result=FALSE
  flag=FALSE
  if(is.na(farktablosu$flng[2])&is.na(farktablosu$flat[2])&is.na(farktablosu$fspd[2])){
    return(result)
  }
  else if(farktablosu$flng[2]==0 & farktablosu$flat[2]==0 & farktablosu$fspd[2]==0){
    result=TRUE
    flag=TRUE
  }
  start=start+1
  k=3
  while (flag) {
    if(start==kontrol+100){
      flag=FALSE}
    if(is.na(farktablosu$flng[k])&is.na(farktablosu$flat[k])&is.na(farktablosu$fspd[k])){
      break
    }
    if(farktablosu$flng[k]==0 & farktablosu$flat[k]==0 & farktablosu$fspd[k]==0){
      k=k+1
      start=start+1
    }
    else if(farktablosu$flng[k]!=0| farktablosu$flat[k]!=0|farktablosu$fspd[k]!=0){
      result=FALSE
      break
    }
    
  }
  return(result)
}




zamanduzeltme<-function(dogrukayip){
  time <- (abs(as.numeric(difftime(dogrukayip[1,]$dt,dogrukayip[(nrow(dogrukayip)-1),]$dt)))*60)-1 
  
  time_const <- dogrukayip %>% 
    mutate(dlong = c(lead(lng)[1:(n()-1)],0),
           dlat  = c(lead(lat)[1:(n()-1)],0),
           mv    = (spd+c(lead(spd)[1:(n()-1)],0))/2
    ) %>% 
    mutate(dis  =  (earth.dist(lng,lat,dlong,dlat))) %>% 
    mutate(td    = (((dis)/mv)*3.6)) %>%
    slice(1:(nrow(.)-1)) %>% 
    summarize(time_const = sum(td)/time)
  
  time_const <- time_const$time_const
  
  
  tt<-dogrukayip %>% 
    mutate(dlong = c(lead(lng)[1:(n()-1)],0),
           dlat  = c(lead(lat)[1:(n()-1)],0),
           mv    = (spd+c(lead(spd)[1:(n()-1)],0))/2
    ) %>% 
    mutate(dis  =  (earth.dist(lng,lat,dlong,dlat))) %>% 
    mutate(td    = round(((((dis)/mv)*3.6))*time_const) ) %>%
    select(X1,dt,spd,td) %>%
    slice(1:(n()-1) )
  
  
  timeadd<-cumsum(tt %>% select(td))
  first_time <- tt[1,]$dt
  correct_time<-rep(first_time,each=nrow(dogrukayip))
  correct_time[2:length(correct_time)]<-(correct_time[2:length(correct_time)]+unlist(timeadd))
  
  dogrukayip <- dogrukayip %>% 
    mutate(dt = correct_time)
  
  return(dogrukayip)
}
