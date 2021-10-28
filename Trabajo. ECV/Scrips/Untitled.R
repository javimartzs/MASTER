library(tidyverse)
library(ariamsita)
library(mapSpain)
library(pwt10)
library(viridis)
library(viridisLite)
library(dineq)
library(stargazer)
library(sjPlot)
library(haven)

r<-read_csv(paste("Ficheros/datos_2014/esudb14r.csv", sep = ""))
h<-read_csv(paste("Ficheros/datos_2014/esudb14h.csv", sep = ""))
d<-read_csv(paste("Ficheros/datos_2014/esudb14d.csv", sep = ""))
p<-read_csv(paste("Ficheros/datos_2014/esudb14p.csv", sep = ""))

per <- left_join(r, p, by =  c('RB030' = 'PB030'))%>%
  mutate(id_hh = as.integer(str_sub(RB030,1,-3)))
hog <- left_join(d,h, by =  c('DB030' = 'HB030'))
ecv14 <- left_join(per, hog, by = c("id_hh" = "DB030"))


r<-read_csv(paste("Ficheros/datos_2017/esudb17r.csv", sep = ""))
h<-read_csv(paste("Ficheros/datos_2017/esudb17h.csv", sep = ""))
d<-read_csv(paste("Ficheros/datos_2017/esudb17d.csv", sep = ""))
p<-read_csv(paste("Ficheros/datos_2017/esudb17p.csv", sep = ""))

per <- left_join(r, p, by =  c('RB030' = 'PB030'))%>%
  mutate(id_hh = as.integer(str_sub(RB030,1,-3)))
hog <- left_join(d,h, by =  c('DB030' = 'HB030'))
ecv17 <- left_join(per, hog, by = c("id_hh" = "DB030"))


r<-read_csv(paste("Ficheros/datos_2020/esudb20r.csv", sep = ""))
h<-read_csv(paste("Ficheros/datos_2020/esudb20h.csv", sep = ""))
d<-read_csv(paste("Ficheros/datos_2020/esudb20d.csv", sep = ""))
p<-read_csv(paste("Ficheros/datos_2020/esudb20p.csv", sep = ""))

per <- left_join(r, p, by =  c('RB030' = 'PB030'))%>%
  mutate(id_hh = as.integer(str_sub(RB030,1,-3)))
hog <- left_join(d,h, by =  c('DB030' = 'HB030'))
ecv20 <- left_join(per, hog, by = c("id_hh" = "DB030"))

ecv <- ecv14 %>% full_join(ecv17) %>% full_join(ecv20)

rm(list = setdiff(ls(), c("ecv")))

save(ecv, file = "ecv.RData")
load("ecv.RData")

#REGIMEN DE TENENCIA-------

df1 <- ecv %>% select(id_hh, ccaa=DB040, regimen=HH021, año=DB010, pesos = RB050)

df1 %>% group_by(año) %>% summarise(sum(pesos))

df1 <- df1 %>% mutate(regimen = case_when(
  regimen == 1 ~ "En propiedad sin hipoteca",
  regimen == 2 ~ "En propiedad con hipoteca",
  regimen == 3 ~ "En alquiler a precio de mercado",
  regimen == 4 ~ "En alquiler a precio inferior al de mercado",
  regimen == 5 ~ "En cesión gratuita"))

df1 <- df1 %>% 
  group_by(regimen, año) %>% 
  summarise(n = sum(pesos)) %>% 
  group_by(año) %>% 
  mutate(pct = n/sum(n)*100) %>% 
  mutate(pct = round(pct, 2)) 

df1$año <- as.factor(df1$año)

df1 %>% ggplot(aes(x=regimen, y = pct, fill = año)) + geom_bar(stat = "identity", position=position_dodge() , width = 0.5)+
  labs(title = "Porcentaje de personas según régimen de tenencia de la vivienda",
       caption = "Elaborado a partir de la ECV",
       x = "",
       y = "%")+
  scale_fill_manual(values = c("royalblue3", "seagreen3", "salmon3"))+
  theme_ariamsita()+
  theme(axis.title.x = element_blank(),
        axis.text.x = element_text(face = "bold", size = 10),
        axis.text.y = element_text(face = "bold", size = 10),
        plot.title = element_text(size = 15),
        legend.title = element_blank(),
        legend.position = c(0.21, 0.75),
        legend.direction = "horizontal",
        legend.text = element_text(size = 12))
  

#ALQUILER POR CCAA en 2020-----

df2 <- ecv %>% select(id_hh, ccaa=DB040, regimen=HH021, año=DB010, pesos = RB050) %>% filter(año == 2020)

df2 <- df2 %>% mutate(regimen = ifelse(regimen %in% 3:4, "En alquiler", "En propiedad"))

df2 <- df2 %>% 
  group_by(regimen, ccaa) %>% 
  summarise(n = sum(pesos)) %>% 
  group_by(ccaa) %>% 
  mutate(pct = n/sum(n)*100) %>% 
  mutate(pct = round(pct, 2)) 

df2$pct.lab <- paste0(df2$pct, "%")

plot <- mapSpain::esp_get_ccaa()

df2 <- df2 %>% left_join(plot, by = c("ccaa" = "nuts2.code"))

df2 <- df2 %>% filter(regimen == "En alquiler")

df2 %>% ggplot() + 
  geom_sf(aes(fill = pct, geometry = geometry), color = "white",lwd = 0.3)+
  scale_fill_viridis(option = "turbo", name = "%")+
  geom_sf_label(aes(label = pct.lab, geometry = geometry),fill = "white", alpha = 0.5,size = 3,label.size = 0)+
  labs(title = "Porcentaje de personas que viven en alquiler por CCAA en 2020")+
  theme_minimal() +
  theme(
    axis.line = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks = element_blank(),
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = "white", color = NA),
    panel.background = element_rect(fill = "white", color = NA),
    legend.background = element_rect(fill = "white", color = NA),
    panel.border = element_blank(),
    plot.title = element_text(size = 15, hjust = 0.5, face = "bold"),
    legend.title = element_text(color = "black", size = 10),
    legend.text = element_text(color = "black", size = 10, hjust = 0),
    legend.position = c(0.2, 0.6))


# Modelo logit binomial-----

load("ecv.RData")

df3 <- ecv %>% 
  select(año = DB010, renta = vhRentaa, HX240,tamaño.hogar = HX040,
         cohorte = RB080, sexo = RB090, situacion = RB210,
         estudios = PE040, regimen = HH021, urbanizacion = DB100, RB050) %>% mutate(renta = renta/HX240,
                                                                             decil = ntiles.wtd(renta, n = 100, RB050),
                                                                             renta = log(renta)) %>% 
  filter(cohorte %in% 1955:1994, renta >0, año == 2020) %>% 
  mutate(cohorte = case_when(
    cohorte %in% 1955:1964 ~ "1955-1964",
    cohorte %in% 1965:1974 ~ "1965-1974",
    cohorte %in% 1975:1984 ~ "1975-1984",
    cohorte %in% 1985:1994 ~ "1985-1994"))
  

df3 <- df3 %>% mutate(regimen = ifelse(regimen %in% 1:2, "1", "0"),
                      estudios = ifelse(estudios>=500, 1, 0))

df3$regimen <- as.factor(df3$regimen)
df3$cohorte <- as.factor(df3$cohorte)


model <- glm(family=binomial(link="logit"), data = df3,
             regimen ~ cohorte + decil)

plot_model(model,type = "pred",
           terms=c("Percentil[all]", "cohorte[all]")) + geom_line(size = .5) +
  labs(title = "Probabilidad de tener una vivienda en propiedad por percentil de renta en 2020",
       subtitle = "Por cohorte de edad",
       x = "Percentil de Ingreso Equivalente", 
       y = "")+
  theme_ariamsita()+
  theme(plot.title = element_text(size = 15),
        axis.title.x = element_text(size = 13),
        axis.text = element_text(size = 12, color = "black"),
        legend.position = "bottom",
        legend.title = element_blank())


stargazer(model,type = "text",title = "Logit estimation, Log Odds")
stargazer2(model, odd.ratio = T, type = "text",title = "Logit estimation, Odds ratios")


