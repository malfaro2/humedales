source("code/packages.R")

# Read Data

INH_ConHum <- read_csv("data_raw/INH_ConHum.csv")
INH_FactInf <- read_csv("data_raw/INH_Fact_Infl.csv")

ConHum <- INH_ConHum %>% 
  select(Formulario:IBE)
View(ConHum)

FactInf <- INH_FactInf %>% 
  filter(!is.na(Formulario)) 
View(FactInf)

save(ConHum, FactInf, file= "data_clean/data_clean.Rdata")
