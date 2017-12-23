library(pwt9)
library(WDI)
library(cshapes)
library(ggplot2)
library(dplyr)
library(readr)

# first, we need a panel data that cover the states in a given period

#load Andreas Beger's function to create country-year data frame
source("./r_code/R_function_for_state_panel.R")

####creat state panel data frame from 1989 to 2016
state.panel <- state_panel("1989", "2016", by="year")

##create country.name
library(countrycode)
state.panel <- state.panel %>% 
       dplyr::mutate(country.name = countrycode(.$ccode, "cown","country.name"),
                     year = format(.$date, format="%Y")) %>% 
       dplyr::select(ccode, country.name, year) %>% 
       dplyr::mutate(year = as.numeric(year))


##now get some IVs

# polity IV
library(foreign)
if (!file.exists("raw_data/polity.Rdata")){
       polity <- read.spss("http://www.systemicpeace.org/inscr/p4v2016.sav")
       save(polity, file = "raw_data/polity.Rdata")
} else {
       load(file = "raw_data/polity.Rdata")
}

polity <- polity %>% 
       as.data.frame(.) %>% 
       dplyr::select(ccode, year, polity2) %>% 
       dplyr::filter(year >=1989 & year <=2016)
save(polity, file = "clean_data/polity.Rdata")

state.panel <- left_join(state.panel, polity, by = c("ccode", "year"))

#get some socialeconomic variables for WDI
#WDIsearch("GDP per capita")
#WDIsearch("population")

wdi <- WDI(country = "all", 
           indicator = c("SP.POP.TOTL","NY.GDP.PCAP.CD",
                         "NY.GDP.MKTP.KD.ZG","MS.MIL.TOTL.TF.ZS"),
           start = 1989, end = 2016, extra = FALSE)
##Note:
#Armed forces personnel (% of total labor force) 
#GDP growth (annual %) from The World Bank: Data.
#NY.GDP.PCAP.CD: GDP per capita (current US$) from The World Bank: Data.
#Population, total

wdi <- wdi %>% dplyr::rename(pop = SP.POP.TOTL,
                             gdppc = NY.GDP.PCAP.CD,
                             gdpgrowth = NY.GDP.MKTP.KD.ZG,
                             miliper= MS.MIL.TOTL.TF.ZS) %>%  
       dplyr::mutate(ccode = countrycode(.$iso2c, "iso2c","cown"))  
save(wdi, file = "raw_data/wdi.RData")              

load("raw_data/wdi.RData")
wdi <- wdi %>%
       select(ccode, year, pop, gdppc, gdpgrowth, miliper) %>% 
       dplyr::filter(!is.na(ccode))
save(wdi, file = "clean_data/wdi.RData")              

state.panel <- left_join(state.panel, wdi, by = c("ccode", "year"))

##download GED data: this is spatial data frame
#check if it is already downloaded
list.files("raw_data")

if (!file.exists("raw_data/ged171.Rdata")){
       ged_url = "http://ucdp.uu.se/downloads/ged/ged171-RData.zip"
       temp <- tempfile()
       download.file(ged_url, temp)
       unzip(zipfile = temp, exdir = "raw_data")
} else {
       load(file = "raw_data/ged171.Rdata")
}

load(file = "raw_data/ged171.Rdata")
#clean GED data: 1989-2016
ged = ged171@data

ged <- ged %>%
       select(year, gwnoa, type_of_vi) %>% 
       dplyr::mutate(state_vio = ifelse(type_of_vi ==1, 1, 0),
                     nonstate_vio = ifelse(type_of_vi ==2, 1, 0),
                     oneside_vio = ifelse(type_of_vi ==3, 1, 0)) 

ged <- ged %>% group_by(gwnoa, year) %>% 
       summarise(state_vio =sum(state_vio),
                 nonstate_vio = sum(nonstate_vio),
                 oneside_vio = sum(oneside_vio)) %>% 
       mutate(vio = 1)

ged$gwnoa <-  as.numeric(as.character(ged$gwnoa))       
state.panel <- left_join(state.panel, ged, by = c("ccode" = "gwnoa", "year"))


state.panel <- state.panel %>% 
       dplyr::mutate_at(vars(state_vio, nonstate_vio,
                             oneside_vio, vio),
                        funs(ifelse(is.na(.), 0, .)))

save(state.panel, file = "clean_data/state.panel.RData")

