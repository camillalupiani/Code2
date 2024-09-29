rm(list=ls()); gc()
install.packages("foreign")
library(foreign)
library(data.table)
library(dplyr)
library(stringr)
library(tidyr)
library(ggplot2)
library(padr)
install.packages("writexl")  # Installa il pacchetto se non l'hai ancora fatto
library(writexl)



dt <- fread("/Users/camillalupiani/Library/CloudStorage/OneDrive-UniversitàCommercialeLuigiBocconi/Dropbox/R/Tesi/databasefinale.csv")

dt[, date_2 := paste(str_sub(Date,1, 4), str_sub(Date, 5, 6), "01", sep = "-")]
dt[, date := as.Date(date_2)]

#primo metodo per grafico
ggplot()+ theme_light()+
  geom_line(data = dt, aes(x = as.Date(date), y = shadowrate), color = "red")+
  geom_line(data = dt, aes(x = as.Date(date), y = mro), color = "black")+
  labs(title = "ciao", x = "Date")+
  scale_x_date(date_breaks = "2 years", date_labels = "%y-%m")

ggplot()+ theme_light()+
  geom_line(data = dt, aes(x = as.Date(date), y = Hicp), color = "red")+
  geom_line(data = dt, aes(x = as.Date(date), y = quart_hicp_gap), color = "black")+
  labs(title = "ciao", x = "Date")+
  scale_x_date(date_breaks = "2 years", date_labels = "%y-%m")


#grafico per tasso di interesse con shadowrate
dt_melt <- melt(dt, id.vars = "date")
dt_melt <- dt_melt[variable %in%c("Hicp", "quart_hicp_gap")]

ggplot()+ theme_light()+
  geom_line(data = dt_melt[!is.na(value)], aes(x = as.Date(date), y = as.numeric(value), color = variable))+
  scale_color_manual(labels = c("Hicp", "Inflation gap"), values = c("darkred", "darkblue"))+
  labs(x = "Date", color = "Legend:")

dt_infl <- melt(dt, id.vars = "date")
dt_infl <- dt_infl[variable %in%c("Hicp")]

ggplot()+ theme_light()+
  geom_line(data = dt_infl[!is.na(value)], aes(x = as.Date(date), y = as.numeric(value), color = variable))+
  scale_color_manual(labels = c("Inflation"), values = c("darkblue"))+
  labs(x = "Date", color = "Legend:",y="Percentage")

#grafico per inflazione
dt_graf <- melt(dt, id.vars = "date")
dt_graf <- dt_graf[variable %in%c("Hicp", "mro","shadowrate")]

ggplot() +
  theme_light() +
  geom_line(data = dt_graf[!is.na(value)], aes(x = as.Date(date), y = as.numeric(value), color = variable)) +
  scale_color_manual(labels = c("Inflation", "Eonia", "Shadow Rate"), values = brewer.pal(n = 4, name = "PRGn")) +
  labs(x = "Date", color = "Legend:", y = "Rate")

#grafico tassi di interesse
dt_rates <- melt(dt, id.vars = "date")
dt_rates<- dt_rates[variable %in%c("mro", "Eonia")]
ggplot()+ theme_light()+
  geom_line(data = dt_rates[!is.na(value)], aes(x = as.Date(date), y = as.numeric(value), color = variable))+
  scale_color_manual(labels = c("mro", "Eonia"), values = brewer.pal(n = 4, name = "RdGy"))+
  labs(x = "Date", color = "Legend:", y= "Rate")

#grafico con m2
dt_m2 <- melt(dt, id.vars = "date")
dt_m2<- dt_m2[variable %in%c("mro", "M2")]
ggplot()+ theme_light()+
  geom_line(data = dt_m2[!is.na(value)], aes(x = as.Date(date), y = as.numeric(value), color = variable))+
  scale_color_manual(labels = c("mro", "M2"), values = brewer.pal(n = 4, name = "RdGy"))+
  labs(x = "Date", color = "Legend:", y= "Rate")

#grafico con debito
dt_debt <- meld(dt, id.vars= "date")
dt_debt<- dt_debt[variable %in%c("mro", "gov_debt_percentage_gdp")]
ggplot()+ theme_light()+
  geom_line(data = dt_m2[!is.na(value)], aes(x = as.Date(date), y = as.numeric(value), color = variable))+
  scale_color_manual(labels = c("Mro", "Percentuale del debito pubblico rispetto al PIL"), values = brewer.pal(n = 4, name = "RdGy"))+
  labs(x = "Date", color = "Legend:", y= "Rate")

#grafico con commodity
dt_commodity <- melt(dt, id.vars= "date")
dt_commodity<- dt_commodity[variable %in%c("mro", "commodity_price_change")]
ggplot()+ theme_light()+
  geom_line(data = dt_m2[!is.na(value)], aes(x = as.Date(date), y = as.numeric(value), color = variable))+
  scale_color_manual(labels = c("Mro", "Commodity Inflation"), values = brewer.pal(n = 5, name = "BrBG"))+
  labs(x = "Date", color = "Legend:", y= "Rate")

dt_dummy_filled <- fill(dt, c(b_output_gap))
dt_dummy_filled <- fill(dt_dummy_filled, c(b_hicp_gap))
dt_dummy_filled <- fill(dt_dummy_filled, c(f_output_gap))
dt_dummy_filled <- fill(dt_dummy_filled, c(f_hicp_gap))
dt_dummy_filled <- fill(data, c(gov_debt_percentage_gdp))

write_xlsx(dt_dummy_filled, "data.xlsx")




setnames(dt, "HICP+1 GAP", "hicp_plus_1")
dt_dummy_filled <- fill(dt, c(hicp_plus_1))
