## Descriptives


load(file= "data_clean/data_clean.Rdata")

names(ConHum)
names(FactInf)
names(index_all)
names(vars)


index_all %>% group_by(Wetland_Type) %>% summarise(mean = mean(VI),
                                     sd = sd(VI))
