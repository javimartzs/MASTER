library(tidyverse)
library(dineq)
library(spatstat)

load("Ficheros/ecv08.19.Personas.RData")


df <- ecv08.19 %>% select(año = DB010,
                          pesos = RB050,
                          ccaa = DB040,
                          vhRentaa,
                          HX240,
                          HX040,
                          regimen = HH021) %>% group_by(año) %>% mutate(IngresoEquiv = vhRentaa/HX240,
                                                      IngresoPC = vhRentaa/HX040,
                                                      decil = ntiles.wtd(IngresoEquiv, n = 10, pesos))

df %>% filter(año == 2019) %>% group_by(decil) %>%summarise(min(IngresoEquiv))

df1 <- df %>% group_by(año) %>% summarise(`Renta media UC` = weighted.mean(IngresoEquiv, pesos),
                                          `Renta media PC` = weighted.mean(IngresoPC, pesos),
                                          `Renta mediana UC` = weighted.median(IngresoEquiv, pesos),
                                          `Renta mediana PC` = weighted.median(IngresoPC, pesos)) %>% 
  pivot_longer(cols = c(2,4), names_to = "renta", values_to = "value")

df1 %>% ggplot(aes(año, value, color = renta)) + geom_line()+
  scale_x_continuous(breaks = seq(2008,2019,by=1))


df <- df %>% mutate(regimen = case_when( 
  regimen == 1 ~ "Propiedad sin hipoteca", 
  regimen == 2 ~ "Propiedad con hipoteca", 
  regimen == 3 ~ "Alquiler pm",
  regimen == 4 ~ "Alquiler reducido", 
  regimen == 5 ~ "Cesión gratuita"))

df1 <- df %>% filter(!is.na(regimen), año == 2019) %>% 
  group_by(decil, regimen) %>% 
  summarise(n = sum(pesos)) %>% 
  group_by(decil) %>% 
  mutate(pct= n/sum(n)) %>% 
  mutate(decil:=as.factor(decil))

df1 %>% ggplot(aes(x=decil, y= pct, fill = regimen)) + geom_bar(stat = "identity", position = "stack", )


