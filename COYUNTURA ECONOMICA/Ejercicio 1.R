library(pwt10)
library(tidyverse)
library(knitr)
library(kableExtra)
library(TTR)
library(dineq)
library(gglorenz)

pwt10.0
#1--------
df <- pwt10.0 %>% filter(year == 2017) %>% select(country, year, rgdpo, emp) %>% 
  mutate(prod.trab = rgdpo/emp,
         relativa = prod.trab/126350.858) %>% 
  arrange(desc(prod.trab))

df1 <- df[1:10,]
df <- df %>% arrange(prod.trab)
df2 <- df[1:10,] %>% arrange(desc(prod.trab))
ej1 <- full_join(df1,df2)

ej1 <- ej1 %>% select(country,prod.trab,relativa) %>% kable(booktabs = TRUE,format = "latex",
                    caption = "paises mas y menos ricos", align = "c") %>% 
  row_spec(10, hline_after = T)

rm(list=ls())
#2--------
df <- pwt10.0 %>% filter(year == c(1970,2017)) %>% select(country, year, rgdpo, emp) %>% 
  mutate(prod.trab = rgdpo/emp) %>% group_by(country) %>% 
  mutate(tasa.var = ROC(prod.trab, n = 1, type = "discrete")*100) %>% arrange(desc(tasa.var))

df1 <- df[1:10,]
df <- df %>% arrange(tasa.var)
df2 <- df[1:10,] %>% arrange(desc(prod.trab))

mean(df$tasa.var, na.rm = T)

df3 <- df[71:80,]

ej2 <- full_join(df1,df2)
ej2.1 <- df3

ej2 <- ej2 %>% select(country,tasa.var) %>% kable(booktabs = TRUE,format = "latex",
                      caption = "paises que mas y menos crecieron 1970-2017", align = "c") %>% 
  row_spec(10, hline_after = T)

ej2.1 <- ej2.1 %>% select(country,tasa.var) %>% kable(booktabs = TRUE,format = "latex",
                          caption = "Paises que crecieron en torno a la media 1970-2017", align = "c") 

rm(list=ls())
#3--------
df <- pwt10.0 %>% filter(year == c(1970,2017)) %>% select(country, year, rgdpo, emp) %>% 
  mutate(prod.trab = rgdpo/emp) %>% group_by(country) %>% 
  mutate(tasa.var = ROC(prod.trab, n = 1, type = "discrete")*100) %>% arrange(desc(tasa.var))

ggplot(df, aes(tasa.var))+ geom_histogram(bins = 30)

rm(list=ls())
#4-------

df <- pwt10.0 %>% filter(year %in% 1970:2017) %>% select(country, year, rgdpo, emp, csh_x, csh_m) %>% 
  mutate(prod.trab = rgdpo/emp, netas = csh_x+csh_m) 

df1 <- df %>% group_by(country) %>% 
  mutate(tasa.var.prod = ROC(prod.trab, n = 1, type = "discrete")*100,
         tasa.var.x = ROC(csh_x, n = 1)*100,
         tasa.netas = ROC(netas, n=1)*100) 


ggplot(df1, aes(tasa.var.x, tasa.var.prod))+ geom_point()+ geom_smooth(method = "lm")+
  theme_bw()

  

rm(list=ls())
#5------
df1970 <- pwt10.0 %>% filter(year == 1970) %>% select(country, year, rgdpo, emp) %>% 
  mutate(prod.trab1970 = rgdpo/emp)

df07 <- pwt10.0 %>% filter(year == 2017) %>% select(country, year, rgdpo, emp) %>% 
  mutate(prod.trab2007 = rgdpo/emp) 

df1 <- left_join(df1970, df07, by="country") %>% select(country, prod.trab1970, prod.trab2007)

df1 %>% ggplot(aes(prod.trab2007, prod.trab1970)) + geom_point(size = 7, alpha = 0.6, color = "blue")+
ylim(0,150000) + xlim(0,150000)+
  theme_bw()



rm(list=ls())
#6-------
df <- pwt10.0 %>% filter(year %in% c(1970, 1980, 1990, 2000, 2010, 2017)) %>% select(country, year, rgdpo, emp) %>% 
  group_by(year, country) %>% mutate(prod.trab = rgdpo/emp) %>% na.omit(prod.trab)

ggplot(df)+ geom_histogram(aes(df$prod.trab), fill = "darkblue", alpha = 0.7)+xlim(0,150000)+
  facet_wrap(df$year, scales = "free_x")+
  theme_bw()


rm(list=ls())


#7-------
df <- pwt10.0 %>% filter(year %in% c(1970, 1980, 1990, 2000, 2010, 2017)) %>% select(country, year, rgdpo, emp) %>% 
  group_by(year) %>% mutate(prod.trab = rgdpo/emp) %>% na.omit(prod.trab)

df.gini <- df %>% group_by(year) %>% summarise(gini = gini.wtd(prod.trab, weights = NULL))  %>% 
  pivot_wider(names_from = year, values_from = gini)


ggplot(df, aes(x=prod.trab))+ stat_lorenz()+
  facet_wrap(df$year, scales = "free_x")+
  geom_abline(data = df, aes(intercept=0, slope=1), color="darkred", size=0.8)+
  geom_hline(data = df, aes(yintercept = 0.5), alpha = 0.6, linetype = 2)+
  theme_bw()

rm(list=ls())

#8-------
df <- pwt10.0 %>% filter(isocode %in% c("NOR", "NGA")) %>% select(country, year, rgdpo, emp) %>% 
  group_by(year) %>% mutate(prod.trab = rgdpo/emp) %>% na.omit(prod.trab) %>% select(country, year, prod.trab)

df <- df %>% pivot_wider(names_from = country, values_from = prod.trab) %>% mutate(ratio = (Norway/Nigeria))

ggplot(df, aes(year, ratio)) + geom_line()+
  theme_bw()


