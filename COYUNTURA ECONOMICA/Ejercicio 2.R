library(pwt10)
library(tidyverse)
library(knitr)
library(kableExtra)
library(TTR)
library(dineq)
library(gglorenz)
data("pwt10.0")

## Ejercicio 1-------
df <- pwt10.0 %>% filter(year %in% 1970:2017) %>% select(country, year, rgdpo, emp, csh_i, pop) %>% group_by(country) %>% 
  mutate(prod.trab = rgdpo/emp)

df <- df %>% group_by(country) %>% 
  mutate(tasa.crec.pob = ROC(pop, n=1))

df1 <- df %>% select(country,
                     tasa.inversion=csh_i,
                     tasa.crec.pob,
                     prod.trab) %>% group_by(country) %>% 
  summarise(media.inv = mean(tasa.inversion, na.rm = T),
            media.pob = mean(tasa.crec.pob, na.rm = T))

df2 <- df %>% select(country, prod.trab, year) %>% filter(year == 2017)

ej1 <- left_join(df1,df2)

ggplot(ej1, aes(media.inv, prod.trab)) + geom_point()

ggplot(ej1, aes(media.pob, prod.trab)) + geom_point()



## Ejercicio 2------
df <- pwt10.0 %>% filter(isocode %in% c("FRA", "USA")) %>% select(country, year, rgdpo, emp) %>% group_by(country) %>% 
  mutate(prod.trab = rgdpo/emp)

ggplot(df, aes(year, prod.trab, color = country)) + geom_line()

## Ejercicio 3------
df <- pwt10.0 %>% filter(isocode %in% c("JPN", "USA")) %>% select(country, year, rgdpo, emp) %>% group_by(country) %>% 
  mutate(prod.trab = rgdpo/emp)

ggplot(df, aes(year, prod.trab, color = country)) + geom_line()


## Ejercicio 4------
df <- pwt10.0 %>% filter(isocode %in% c("JPN")) %>% select(country, year, rgdpo, emp, csh_i)
ggplot(df, aes(year, csh_i)) + geom_line()


## Ejercicio 5-----
df <- pwt10.0 %>% filter(isocode %in% c("HKG", "TWN", "KOR", "SGP")) %>% select(country, year, rgdpo, emp) %>% group_by(country) %>% 
  mutate(prod.trab = rgdpo/emp)

ggplot(df, aes(year, prod.trab, color = country)) + geom_line()


## Ejercicio 6------
df <- pwt10.0 %>% filter(isocode %in% c("HKG", "TWN", "KOR", "SGP")) %>% select(country, year, csh_i) 

ggplot(df, aes(year, csh_i, color = country)) + geom_line()


## Ejercicio 7-----
df <- pwt10.0 %>% filter(year %in% 1995:2017) %>% select(country, year, rgdpo, emp, csh_i, pop) %>% group_by(country) %>% 
  mutate(prod.trab = rgdpo/emp)

df <- df %>% group_by(country) %>% 
  mutate(tasa.crec.pob = ROC(pop, n=1))

df1 <- df %>% select(country,
                     tasa.inversion=csh_i,
                     tasa.crec.pob,
                     prod.trab) %>% group_by(country) %>% 
  summarise(media.inv = mean(tasa.inversion, na.rm = T),
            media.pob = mean(tasa.crec.pob, na.rm = T))

df2 <- df %>% select(country, prod.trab, year) %>% filter(year == 2017)
ej1 <- left_join(df1,df2)

ej1 <- ej1 %>% group_by(country) %>% mutate(estimacion = (media.inv/media.pob)^((1/3)/(1-(1/3))),
                                            relativa = prod.trab/126350.858,
                                            estimacion.rel = estimacion/5.109561) %>% 
  mutate(estimacion = log(estimacion),
         estimacion.rel = log(estimacion.rel))


ggplot(ej1, aes(relativa, estimacion.rel)) + geom_point()


## Ejercicio 8-----
df <- pwt10.0 %>% filter(year == 2017) %>% select(country, year, rgdpo, emp, cn) %>% group_by(country) %>% 
  mutate(prod.trab = rgdpo/emp)

df <- df %>% group_by(country) %>% 
  mutate(estimacion = (cn/prod.trab)^((1/3)/(1-(1/3))))


ej1 <- ej1 %>% group_by(country) %>% mutate(estimacion = (media.inv/media.pob)^((1/3)/(1-(1/3))),
                                            relativa = prod.trab/126350.858,
                                            estimacion.rel = estimacion/5.109561) %>% 
  mutate(estimacion = log(estimacion),
         estimacion.rel = log(estimacion.rel))


ggplot(ej1, aes(relativa, estimacion.rel)) + geom_point()


