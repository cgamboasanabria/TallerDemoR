library(tidyverse)
Def2017 <- read.csv("http://www.deis.msal.gov.ar/wp-content/uploads/2019/01/DefWeb17.csv")


temp = tempfile(fileext = ".xlsx")
download.file("http://www.deis.msal.gov.ar/wp-content/uploads/2019/01/DescDef1.xlsx", 
              destfile=temp, 
              mode='wb')

library(readxl)
Provincias <- read_xlsx(temp, sheet = "PROVRES")
CodMuer <- read_xlsx(temp, sheet = "CODMUER")

str(Def2017)
str(Provincias)

Def2017j <- Def2017 %>%  
  left_join(Provincias %>% mutate(PROVRES = as.integer(CODIGO)), by = c("PROVRES")) %>% 
  left_join(CodMuer, by = c("CAUSA" = "CODIGO")) %>% 
  rename(PROV = VALOR.x, CAUSA_descr = VALOR.y)

Def2017j <- Def2017j %>% separate(col = CAUSA, sep = 1, into = c("CAP", "TIT"))

Def2017j %>% group_by(GRUPEDAD, CAUSA_descr) %>% summarise(N = sum(CUENTA)) %>% top_n(1, N)

library("wpp2019")

?wpp2019

data(tfr)

ncol(tfr)

Arg_y_el_Mundo <-  tfr %>% 
  filter(name %in% c("World", "Argentina")) %>%
  gather(Period, TFR, -c(1, 2, ncol(tfr))) %>% 
  mutate(Anio = as.numeric(substr(Period, start = 1, stop = 4)) + 2.5)

ggplot(Arg_y_el_Mundo) + 
  geom_point(aes(x = Anio, y = TFR, color = name))

library(HMDHFDplus)
getHMDcountries()
getHMDitemavail(CNTRY = "ESP", username = userTaller, password = passTaller)
dataESPm = readHMDweb(CNTRY = "ESP", 
                      username = userTaller, password = passTaller, 
                      item = "bltper_1x5")

dataESPm <- read.csv("Día4/Data/dataESPm.csv")

head(dataESPm)

dataESPf = readHFDweb(CNTRY = "ESP", 
                      username = userHFD, password = passHFD,
                      item = "asfrRR")






edad_en_t_anio <- function(x, t = 25){
  x + t
}


edad_en_t_anio(5, 30)


edad_en_t_anio(10, 5)
edad_en_t_anio(x = 5, t = 10)


edades = c(59, 58, 32, 30, 25, 25)
edad_en_t_anio(edades, 10)


edad_en_t_anios <- function(x, t = 25){
  return(x + t)
}

q_desde_m <- function(x, m, n, a){  # incluyo todos los argumentos sin valor por default
  
  q = m*n/(1+(n-a)*m)               # aplico la operación principal
  
  q[length(q)] = 1                  # me aseguro que TODOS fallezcan
  
  q_result = data.frame(x, q)       # organizo el resultado como data.frame
  
  return(q_result)                  # con return, doy noticia de qué objeto devuelvo
}

dataESPm_simple = dataESPm %>% filter(Year == 2015) %>% select(Age, mx, ax) 
q_ejemplo_simple <- q_desde_m(x = dataESPm_simple$Age, m = dataESPm_simple$mx, n = 1, a = dataESPm_simple$ax)

















