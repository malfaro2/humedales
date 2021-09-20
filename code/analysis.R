source(file='code/packages.R')
load(file='data_clean/cluster_ConHum.Rdata')
load(file='data_clean/cluster_FactInf.Rdata')
load(file='data_clean/data_clean.Rdata')
clustersConHum2  <- read.csv(file='data_clean/cluster_ConHum.csv')
clustersFactInf2 <- read.csv(file='data_clean/cluster_FactInf.csv')

names(index_all)[1]<- "Formulario"

all <- clustersConHum2  %>% 
  full_join(ConHum , by="Formulario") %>% 
  full_join(clustersFactInf2 , by="Formulario") %>% 
  left_join(FactInf, by="Formulario") %>%  
  left_join(vars, by="Formulario") %>% 
  left_join(index_all, by="Formulario") %>% 
  select(Formulario:groups5.x, groups5.y, BuenEstado,IV, CI, HI, VI, 
         asp,tipo_hum,nat_art,Perimetro_m,Area_Has) 


names(all) <- c("ID","Name","cCH5", "cFI5", "BE", "IV","CI", "HI", "VI","asp","tipo_hum",
                "nat_art","Perimetro_m","Area_Has")
head(all)

all %>%  
  group_by(cCH5, cFI5)%>%
  summarise(mean=mean(VI)) %>% 
  spread(cCH5, mean)%>%
  kable()

all%>%
  group_by(cCH5, cFI5)%>%
  summarise(n=n())%>%
  spread(cCH5, n)%>%
  kable()

all %>%  
  filter(cCH5 ==1) %>% 
  group_by(cFI5, BE)%>%
  summarise(n = n()) %>% 
  spread(cFI5,n)%>%
  kable()


dataset <- ConHum %>% select(Formulario, BuenEstado) %>% 
  full_join(clustersConHum2, by="Formulario") %>% 
  full_join(clustersFactInf2, by="Formulario") %>% 
  left_join(FactInf, by="Formulario") %>%  
  left_join(vars, by="Formulario") %>% 
  left_join(index_all, by="Formulario") %>% 
  select(Formulario, BuenEstado, groups5.x, groups5.y, IV, CI, HI, VI, asp,
         tipo_hum,nat_art,Perimetro_m,Area_Has,
         Bosques, Gan_Extens, Sabanas,Pesca,
         Acuacult,Moluscos,Turism_Com) 

names(dataset) <- c("ID","IBE","cCH5", "cFI5", "IV","CI", "HI", "VI","asp","tipo_hum",
                "nat_art","Perimetro_m","Area_Has","Bosques","Gan_Extens",
                "Sabanas","Pesca","Acuacult","Moluscos" , "Turism_Com")

mod1 <- lm(VI ~ as.factor(cCH5) + as.factor(cFI5),data = dataset)
summary(mod1)
plot(mod1)
extract_eq(mod1)

aa<-aov(mod1)
plot(TukeyHSD(aa, "as.factor(cCH5)"))
plot(TukeyHSD(aa, "as.factor(cFI5)"))

mod2 <- lm(IV ~ IBE,data = dataset)
summary(mod2)

mod3 <- lm(IV ~ scale(Area_Has),data = dataset)
summary(mod3)

mod4 <- lm(IV ~ scale(Perimetro_m),data = dataset)
summary(mod4)

mod5 <- glm(IBE ~ scale(Area_Has)+ Bosques + Gan_Extens + Sabanas + Pesca,
               data = dataset, family = "binomial")
mod6 <- glm(IBE ~ Bosques + Gan_Extens + Sabanas + Pesca,
            data = dataset, family = "binomial")
mod7 <- glm(IBE ~ Gan_Extens + Sabanas + Pesca,
            data = dataset, family = "binomial")
mod8 <- glm(IBE ~ Sabanas + Pesca,
            data = dataset, family = "binomial")
summary(mod5)$aic;summary(mod6)$aic;summary(mod7)$aic;summary(mod8)$aic

confint(mod5)
exp(coef(mod5))
exp(cbind(OR = coef(mod5), confint(mod5)))

#Now we can say that having forest (influenciado por) a wetland, 
#increases the odds of a wetland area being in good condition 
#(versus not being in good condition) by a factor of 5.99. 
#Sabana's factor is 2.54 and fishing 4.31.The area, and
# extensive cattle raising decrease that same odds.


mod9 <- glm(VI ~ scale(Area_Has)+ Bosques + Gan_Extens + Sabanas + Pesca +
              asp+ tipo_hum + nat_art,
            data = dataset, family = "gaussian")
mod10 <- glm(VI ~ scale(Area_Has)+ Bosques + Gan_Extens + Sabanas + Pesca +
              asp+ nat_art,
            data = dataset, family = "gaussian")
mod11 <- glm(VI ~ Bosques + Gan_Extens + Sabanas + Pesca +
              asp+ tipo_hum + nat_art,
            data = dataset, family = "gaussian")
mod12 <- glm(VI ~ Bosques + Gan_Extens + Sabanas + Pesca +
              asp+  nat_art,
            data = dataset, family = "gaussian")
summary(mod9)$aic;summary(mod10)$aic;summary(mod11)$aic;summary(mod12)$aic

### changes missing in here: set up as a baseline: Inside protected area, 
### palustrine and natural

dataset <- dataset %>% 
  mutate(tipo_hum = factor(tipo_hum, levels=c("Palustre","Lacustre","Estuarino"),
       labels=c("0Palustre","1Lacustre","2Estuarino")),
       nat_art = factor(nat_art, levels=c("Natural", "Artificial"),
       labels=c("0Natural", "1Artificial")))
mod11 <- glm(VI ~ Bosques + Gan_Extens + Sabanas + Pesca +
               asp+ tipo_hum + nat_art,
             data = dataset, family = "gaussian")

summary(mod11)
plot(mod11)








