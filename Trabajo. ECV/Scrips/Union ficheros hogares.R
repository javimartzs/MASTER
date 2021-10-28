library(tidyverse)
library(dineq)

### 2008 ----
h<-read_csv(paste("Ficheros/esudb08h.csv", sep = ""))
d<-read_csv(paste("Ficheros/esudb08d.csv", sep = ""))

ecv08.hog <- left_join(d,h, by =  c('DB030' = 'HB030'))
save(ecv08.hog, file = "ecv8.Hogar.RData")

### 2009 ----
h<-read_csv(paste("Ficheros/esudb09h.csv", sep = ""))
d<-read_csv(paste("Ficheros/esudb09d.csv", sep = ""))

ecv09.hog <- left_join(d,h, by =  c('DB030' = 'HB030'))
save(ecv09.hog, file = "ecv9.Hogar.RData")

### 2010 ----
h<-read_csv(paste("Ficheros/esudb10h.csv", sep = ""))
d<-read_csv(paste("Ficheros/esudb10d.csv", sep = ""))

ecv10.hog <- left_join(d,h, by =  c('DB030' = 'HB030'))
save(ecv10.hog, file = "ecv10.Hogar.RData")

### 2011 ----
h<-read_csv(paste("Ficheros/esudb11h.csv", sep = ""))
d<-read_csv(paste("Ficheros/esudb11d.csv", sep = ""))

ecv11.hog <- left_join(d,h, by =  c('DB030' = 'HB030'))
save(ecv11.hog, file = "ecv11.Hogar.RData")

### 2012 ----
h<-read_csv(paste("Ficheros/esudb12h.csv", sep = ""))
d<-read_csv(paste("Ficheros/esudb12d.csv", sep = ""))

ecv12.hog <- left_join(d,h, by =  c('DB030' = 'HB030'))
save(ecv12.hog, file = "ecv12.Hogar.RData")

### 2013 ----
h<-read_csv(paste("Ficheros/esudb13h.csv", sep = ""))
d<-read_csv(paste("Ficheros/esudb13d.csv", sep = ""))

ecv13.hog <- left_join(d,h, by =  c('DB030' = 'HB030'))
save(ecv13.hog, file = "ecv13.Hogar.RData")

### 2014 ----
h<-read_csv(paste("Ficheros/esudb14h.csv", sep = ""))
d<-read_csv(paste("Ficheros/esudb14d.csv", sep = ""))

ecv14.hog <- left_join(d,h, by =  c('DB030' = 'HB030'))
save(ecv14.hog, file = "ecv14.Hogar.RData")

### 2015 ----
h<-read_csv(paste("Ficheros/esudb15h.csv", sep = ""))
d<-read_csv(paste("Ficheros/esudb15d.csv", sep = ""))

ecv15.hog <- left_join(d,h, by =  c('DB030' = 'HB030'))
save(ecv15.hog, file = "ecv15.Hogar.RData")

### 2016 ----
h<-read_csv(paste("Ficheros/esudb16h.csv", sep = ""))
d<-read_csv(paste("Ficheros/esudb16d.csv", sep = ""))

ecv16.hog <- left_join(d,h, by =  c('DB030' = 'HB030'))
save(ecv16.hog, file = "ecv16.Hogar.RData")

### 2017 ----
h<-read_csv(paste("Ficheros/esudb17h.csv", sep = ""))
d<-read_csv(paste("Ficheros/esudb17d.csv", sep = ""))

ecv17.hog <- left_join(d,h, by =  c('DB030' = 'HB030'))
save(ecv17.hog, file = "ecv17.Hogar.RData")

### 2018 ----
h<-read_csv(paste("Ficheros/esudb18h.csv", sep = ""))
d<-read_csv(paste("Ficheros/esudb18d.csv", sep = ""))

ecv18.hog <- left_join(d,h, by =  c('DB030' = 'HB030'))
save(ecv18.hog, file = "ecv18.Hogar.RData")

### 2019 ----
h<-read_csv(paste("Ficheros/esudb19h.csv", sep = ""))
d<-read_csv(paste("Ficheros/esudb19d.csv", sep = ""))

ecv19.hog <- left_join(d,h, by =  c('DB030' = 'HB030'))
save(ecv19.hog, file = "ecv19.Hogar.RData")


for (i in 8:19) {
  load(paste("ecv",i,".Hogar.RData", sep = ""))
}

ecv08.19.hog <- ecv08.hog %>% full_join(ecv09.hog) %>%full_join(ecv10.hog) %>% full_join(ecv11.hog) %>% 
  full_join(ecv12.hog) %>% full_join(ecv13.hog) %>% full_join(ecv14.hog) %>% full_join(ecv15.hog) %>% 
  full_join(ecv16.hog) %>% full_join(ecv17.hog) %>% full_join(ecv18.hog) %>% full_join(ecv19.hog)


save(ecv08.19.hog, file = "ecv08.19.Hogar.RData")




