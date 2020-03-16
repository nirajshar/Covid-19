library(ggplot2)
library(caTools)
library(dplyr)
library(openxlsx)

covid19 <- read.xlsx('COVID-19.xlsx')
head(covid19)
View(covid19)
dim(covid19)
str(covid19)

# Curate data
covid19$Lat    <- as.numeric(as.character(covid19$Lat))
covid19$Long   <- as.numeric(as.character(covid19$Long))
colnames(covid19)[1] <- "Country"
colnames(covid19)[2] <- "State"
covid19$Date <- as.Date(covid19$Date, origin = "1899-12-30")
covid19$Last_Update_Date <- as.Date(covid19$Last_Update_Date, origin = "1899-12-30")
head(covid19)

# Method 1 : Plot on Map
library("ggmap")
library(maptools)
library(maps)

png(file="covid19-map1.jpg")
map("world", fill=TRUE, col="white", bg="lightblue", ylim=c(-180, 180),xlim = c(-180,180))
points(covid19$Long,covid19$Lat, col="red", cex=0.1,pch=16)
dev.off()

# Method 2 : Plot on MAP
theme_set(theme_bw())
library("sf")
library("rnaturalearth")
library("rnaturalearthdata")
library(rgeos)
world <- ne_countries(scale = "medium", returnclass = "sf")
class(world)                 

png(file="covid19-map2.jpg")
ggplot(data = world) +
  geom_sf() +
  xlab("Longitude") + ylab("Latitude") +
  ggtitle("World map", subtitle = paste0("(", length(unique(covid19$Country )), " countries)")) +
  geom_point(data=covid19, aes(x=Long, y=Lat), color="red", alpha=0.5)
dev.off()

# Number of Cases
  library(DT)
  str(covid19)
  country_wise <- covid19 %>% group_by(Country, Case_Type) %>%  
                  dplyr::summarize(Count = max(Cases)) %>% arrange(desc(Count))
  datatable(country_wise)

# Infected countries
  Infected_Countries <- subset(country_wise, Case_Type == 'Confirmed')
  ggplot(Infected_Countries, aes(x="", y=Count, fill=Country)) + 
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0)

# Top 10 Infected Countries
  top_10_Inf_countries <- head(Infected_Countries,10)
  ggplot(top_10_Inf_countries, aes(x="", y=Count, fill=Country)) + 
    geom_bar(stat="identity", width=1) +
    coord_polar("y", start=0) +
    scale_fill_manual(values=c("#808000"	,"#808080"	, "#800000"	,  "#000080"	, "#800080"	,
      "#55DDE0", "#33658A", "#2F4858", "#F6AE2D", "#F26419", "#999999")) 
  
# Plot Confirmed cases
covid19_confirmed <- subset(country_wise , Case_Type == 'Confirmed')
highest_confirmed <- covid19_confirmed %>% arrange(desc(Count)) %>% 
  select(Country,Count)

head(highest_confirmed,10) %>% 
  ggplot(aes(reorder(Country,-Count),Count,fill=Country)) + 
  geom_bar(stat = "identity") + 
  ylab("Number of confirmed Cases")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("Country") + 
  ggtitle("Top 10 countries with Confirmed Covid19 Cases") + 
  guides(fill=F)

# Plot Death Toll cases
datatable(country_wise)
covid19_deaths <- subset(country_wise , Case_Type == 'Deaths')
highest_deaths <- covid19_deaths %>% arrange(desc(Count)) %>% 
  select(Country,Count)

head(highest_deaths,10) %>% 
  ggplot(aes(reorder(Country,-Count),Count,fill=Country)) + 
  geom_bar(stat = "identity") + 
  ylab("Number of Deaths")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("Country") + 
  ggtitle("Top 10 countries with Deaths Covid19 Cases") + 
  guides(fill=F)

# Plot Recovered cases
datatable(country_wise)
covid19_recovered <- subset(country_wise , Case_Type == 'Recovered')
highest_recovered <- covid19_recovered %>% arrange(desc(Count)) %>% 
  select(Country,Count)

head(highest_recovered,10) %>% 
  ggplot(aes(reorder(Country,-Count),Count,fill=Country)) + 
  geom_bar(stat = "identity") + 
  ylab("Number of Recovered cases")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("Country") + 
  ggtitle("Top 10 countries with Recovered Covid19 Cases") + 
  guides(fill=F)

# Plot Active cases
datatable(country_wise)
covid19_active <- subset(country_wise , Case_Type == 'Active')
highest_active <- covid19_active %>% arrange(desc(Count)) %>% 
  select(Country,Count)

head(highest_active,10) %>% 
  ggplot(aes(reorder(Country,-Count),Count,fill=Country)) + 
  geom_bar(stat = "identity") + 
  ylab("Number of Active")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("Country") + 
  ggtitle("Top 10 countries with Active Covid19 Cases") + 
  guides(fill=F)

# Plot New Active cases
covid19_newCases <- subset(covid19 , Case_Type == 'Active' & 
                             Date >= as.Date(Sys.Date(),format='%Y-%m-%d')-10)

country_wise_newCases <- covid19_newCases %>% group_by(Country, Case_Type) %>%  
  dplyr::summarize(Count = max(Cases)) %>% arrange(desc(Count))

View(country_wise_newCases)
highest_newCases <- country_wise_newCases %>% arrange(desc(Count)) %>% 
  select(Country,Count)

head(highest_newCases,10) %>% 
  ggplot(aes(reorder(Country,-Count),Count,fill=Country)) + 
  geom_bar(stat = "identity") + 
  ylab("Number of New Cases")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  xlab("Country") + 
  ggtitle("Top 10 countries with New Covid19 Cases") + 
  guides(fill=F)

# Spread in other countries except china
View(covid19)
top_10 <- head(unique(country_wise$Country),10)

top_10_countries <- subset(covid19 , Country %in% top_10[-1] & Case_Type == 'Confirmed') 

  top_10_countries %>%
  ggplot(aes(x=Date, y=Cases, group=Country, color=as.factor(Country))) +
  geom_line() + 
  geom_point()

# Spread in China
  CH <- subset(covid19 , Country == 'China' & Case_Type == 'Confirmed')
  View(CH)
  
  CH %>%
    ggplot(aes(x=Date, y=Cases, group=State, color=as.factor(State))) +
    geom_line() + 
    geom_point()
  
  CH_Province <- CH %>% group_by(State, Case_Type) %>% 
                        summarise(Count=max(Cases)) %>%
                        arrange(desc(Count))
  View(CH_Province)
  
  head(CH_Province,10) %>%
  ggplot(aes(reorder(State,-Count), y=Count)) +
    geom_bar(stat = "identity")

# Province in China having highest spread
  CH_Hubei <- subset(covid19 , Country == 'China' & State == 'Hubei' & Case_Type == 'Confirmed')
  View(CH_Hubei)  

  ggplot(data=CH_Hubei,
         aes(x=Date, y=Cases, colour=State)) +
    geom_line()  
    
# Top 10 infected countries wrt death, Recovery & Active cases
  top_10 <- head(unique(country_wise$Country),10)
  conf_inf <- subset(covid19 , Country %in% top_10 & Case_Type == 'Confirmed')
  conf_rec <- subset(covid19 , Country %in% top_10 & Case_Type == 'Recovered')
  conf_dea <- subset(covid19 , Country %in% top_10 & Case_Type == 'Deaths')
  conf_act <- subset(covid19 , Country %in% top_10 & Case_Type == 'Active')
  
  (conf_inf_grp <- conf_inf %>% group_by(Country) %>% summarise(Conf_Count = max(Cases)) %>%
                              arrange(desc(Conf_Count)))

  (conf_rec_grp <- conf_rec %>% group_by(Country) %>% summarise(Conf_Count = max(Cases)) %>%
      arrange(desc(Conf_Count)))

  (conf_dea_grp <- conf_dea %>% group_by(Country) %>% summarise(Conf_Count = max(Cases)) %>%
      arrange(desc(Conf_Count)))
  
  (conf_act_grp <- conf_act %>% group_by(Country) %>% summarise(Conf_Count = max(Cases)) %>%
      arrange(desc(Conf_Count)))
  
  colnames(conf_inf_grp)[1] <- "Country"
  colnames(conf_rec_grp)[1] <- "Country"
  colnames(conf_dea_grp)[1] <- "Country"
  colnames(conf_act_grp)[1] <- "Country"
  
  conf_grp1 <-merge(x = conf_inf_grp, y = conf_rec_grp, by = "Country")
  conf_grp2 <-merge(x = conf_dea_grp, y = conf_act_grp, by = "Country")
  conf_grp3 <-merge(x = conf_grp1, y = conf_grp2, by = "Country")
  
  View(conf_grp3)
  colnames(conf_grp3)[2] <- "Confirmed"
  colnames(conf_grp3)[3] <- "Recovered"
  colnames(conf_grp3)[4] <- "Deaths"
  colnames(conf_grp3)[5] <- "Active"
  
  library(reshape2)
  library(ggplot2)
  
  df <- melt(conf_grp3, "Country")
  
  ggplot(df, aes(fill=variable, y=value, x=Country)) + 
    geom_bar(position="stack", stat="identity")
  
# Fatality rate
  conf_grp3
  
  conf_grp3_Per <-   conf_grp3
  conf_grp3_Per$Percent_Death <- round((conf_grp3_Per$Deaths / conf_grp3_Per$Confirmed) * 100,2)
  conf_grp3_Per
  
# Spread Rate
  conf_grp3_Per$Percent_Spread <- round(conf_grp3_Per$Active /  (conf_grp3_Per$Confirmed + 
                                                             (conf_grp3_Per$Active - 
                                     (conf_grp3_Per$Confirmed - conf_grp3_Per$Recovered - conf_grp3_Per$Deaths)
                                   )) *100,2)
  
# Recovery Rate
  conf_grp3_Per$Percent_Recovery <- round(conf_grp3_Per$Recovered / (conf_grp3_Per$Confirmed + 
                                        (conf_grp3_Per$Active - 
                                           (conf_grp3_Per$Confirmed - conf_grp3_Per$Recovered - conf_grp3_Per$Deaths)
                                        )) *100,2)

    
  datatable(conf_grp3_Per)
  
  melt_data <- data.frame(conf_grp3_Per$Country,conf_grp3_Per$Deaths,conf_grp3_Per$Percent_Recovery) 
  colnames(melt_data)[1] <- "Country"
  colnames(melt_data)[2] <- "Deaths"
  colnames(melt_data)[3] <- "Recovery"
  
  df2 <- melt(melt_data, "Country")
  ggplot(df2, aes(fill=variable, y=value, x=Country)) + 
    geom_bar(position="stack", stat="identity")
  
  