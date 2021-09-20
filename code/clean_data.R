source("code/packages.R")

# Read Data

INH_ConHum <- read_csv("data_raw/INH_ConHum.csv")
INH_FactInf <- read_csv("data_raw/INH_Fact_Infl.csv")
vars <- read_csv("data_raw/INH_General.xls - INH_General.csv")
index  <- read_excel("data_raw/WetlandValues_Index.xlsx")
ConHum_complete  <- read_excel("data_raw/ConHum.xlsx")

## Wetland condition (ConHum)
## correct 0s [others = ]

unique(ConHum_complete$Otros)

ConHum <- ConHum_complete %>%
  mutate(damage = case_when(is.na(Otros)!=TRUE | Fuego_Quem == 1 ~ 1,
                             TRUE ~ 0))
ConHum <- ConHum %>% 
  select(c(Formulario:Cultivado,damage)) 
View(ConHum)

ConHum[ConHum$Formulario == 3289,][1,"Formulario"]<-32891
ConHum[ConHum$Formulario == 7700,][1,"Formulario"]<-77001
ConHum[ConHum$Formulario == 8585,][1,"Formulario"]<-85851

reps<- ConHum %>%  filter(Formulario == 3289|
                            Formulario == 7700 | 
                            Formulario == 8585)    

## Check zeroes
ConHum %>% 
  filter(BuenEstado==0 & Drenado ==0 & Gan_Presen ==0 & Plantas_In == 0 &
           Seco==0 & Artificial==0 & Sediment ==0 & Restaur == 0 & Proc_Resta==0 &
           Contamin == 0 & Colmat == 0 & Cultivado ==0 )%>% 
  summary()

### Influence Factors (FactInf)
FactInf <- INH_FactInf %>% 
  filter(!is.na(Formulario)) 
View(FactInf)

FactInf[FactInf$Formulario == 3289,][1,"Formulario"]<-32891
FactInf[FactInf$Formulario == 7700,][1,"Formulario"]<-77001
FactInf[FactInf$Formulario == 8585,][1,"Formulario"]<-85851

reps1 <- FactInf %>%  filter(Formulario == 3289|
                            Formulario == 7700 | 
                            Formulario == 8585)    

vars

vars[vars$Formulario == 3289,][1,"Formulario"]<-32891
vars[vars$Formulario == 7700,][1,"Formulario"]<-77001
vars[vars$Formulario == 8585,][1,"Formulario"]<-85851

reps2 <- vars %>%  filter(Formulario == 3289|
                            Formulario == 7700 | 
                            Formulario == 8585)    

index_all <- index %>% 
  filter(!is.na(ID)) 
View(index)

reps3 <- index_all %>%  filter(ID == 3289|
                      ID == 7700 | 
                      ID == 8585)    

index_all[index_all$ID == 3289,][1,"ID"]<-32891
index_all[index_all$ID == 7700,][1,"ID"]<-77001
index_all[index_all$ID == 8585,][1,"ID"]<-85851


dim(index_all);dim(ConHum); dim(FactInf); dim(vars)

save(ConHum, FactInf, index_all, vars, file= "data_clean/data_clean.Rdata")
