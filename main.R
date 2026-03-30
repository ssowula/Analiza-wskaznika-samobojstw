# BIBLIOTEKI 
library(dplyr)
library(ggplot2)
library(nortest)
library(knitr)
library(scales)

# WCZYTYWANIE DANYCH
df_suicide <- read.csv('dane_samobojstwa.csv')
df_country_by_continent <- read.csv('Countries by continents.csv')
df_alcohol <- read.csv('dane_alkohol.csv')
df_oop<-read.csv('dane_oop.csv')
df_dghe <- read.csv('dane_dghe.csv')
df_pkb <- read.csv('dane_pkb.csv')

# WYBÓR POTRZEBNYCH DO ANALIZY KOLUMN

kraje_i_kontynenty <- na.omit(df_country_by_continent)
names(kraje_i_kontynenty)[1] = "continent"
names(kraje_i_kontynenty)[2] = "country"

suicide <- data.frame("country" = df_suicide$Location, "year" = df_suicide$Period, "sex" = df_suicide$Dim1, "suicide" = df_suicide$FactValueNumeric)

alcohol <- data.frame("country" = df_alcohol$Location, "year" = df_alcohol$Period, "sex" = df_alcohol$Dim1, "alcohol" = df_alcohol$FactValueNumeric)

oop<- data.frame("country" = df_oop$Location, "year" = df_oop$Period, "oop" = df_oop$FactValueNumeric)

dghe <- data.frame("country" = df_dghe$Location, "year" = df_dghe$Period, "dghe" = df_dghe$FactValueNumeric)

pkb <- data.frame("country" = df_pkb$Entity, "year" = df_pkb$Year, "pkb" = df_pkb$GDP.per.capita..PPP..constant.2021.international...)

# WSTĘPNE CZYSZCZENIE DANYCH

suicide <- na.omit(suicide)
alcohol <- na.omit(alcohol)
oop<-na.omit(oop)
dghe <- na.omit(dghe)
pkb<-na.omit(pkb)

# ŁĄCZENIE I DOPASOWYWANIE TABEL 

help_suicide <- kraje_i_kontynenty
help_suicide <- left_join(kraje_i_kontynenty, suicide ,by=c('country'))

suicide_only <- na.omit(help_suicide)

help_alcohol <- left_join(help_suicide,alcohol,by=c('country','sex','year'))
suicide_alcohol <- na.omit(help_alcohol)

suicide_oop <- left_join(help_suicide,oop,by=c("country", "year"))
suicide_oop <- na.omit(suicide_oop)
suicide_oop <- suicide_oop %>%
  filter(sex=="Both sexes")

suicide_dghe <- left_join(help_suicide,dghe,by=c("country","year"))
suicide_dghe <- na.omit(suicide_dghe)
suicide_dghe <- suicide_dghe %>%
  filter(sex=="Both sexes")

suicide_pkb <- left_join(help_suicide,pkb,by=c("country","year"))
suicide_pkb<-na.omit(suicide_pkb)
suicide_pkb <- suicide_pkb%>%
  filter(sex=="Both sexes")

# ZDEFINIOWANIE PALETY KOLORÓW

continent_colors <- c(
  "Europe" = "#0085C7",
  "Asia" = "#F4C300",
  "Africa" = "#000000",
  "Oceania" = "#009F3D",
  "North America" = "#DF0024",
  "South America" = "#FF7F00"
)

# I. Zapoznanie się z danymi, interpretacja statystyk opisowych, filtrowanie i grupowanie danych

str(suicide)
kable(summary(suicide))
sd(suicide$suicide)

ggplot(suicide,aes(x=suicide))+
  geom_boxplot()+
  labs(title="Wykres pudełkowy dla wskaźnika samobójstw",
       x="Wartość wskaźnika")+
  theme_light(base_size = 15)

## 1a) Na którym kontynencie i w jakim kraju wskaźnik osiąga wartość najwyższą, a w którym najniższą?
## 1b) Czy płeć ma znaczenie?
### UJĘCIE KONTYNENTALNE

### UJĘCIE KRAJOWE

x<-suicide_only %>% 
  group_by(country,sex)%>%
  summarise(mean_country_suicide = round(mean(suicide),2)) %>%
  arrange(desc(mean_country_suicide))

kable(x%>%
        filter(sex=="Both sexes")%>%
        head(10))

kable(x%>%
        filter(sex=="Male")%>%
        head(15))

kable(suicide_only%>%
        filter(country %in% c("Lithuania","Japan"), sex=="Male",year == 2011))

kable(x%>%
        filter(sex=="Female")%>%
        head(10))

kable(x %>% 
        filter(sex == "Both sexes") %>%
        arrange(mean_country_suicide)%>%
        head(10))

kable(x %>% 
        filter(sex == "Male") %>%
        arrange(mean_country_suicide)%>%
        head(10))

kable(x %>% 
        filter(sex == "Female") %>%
        arrange(mean_country_suicide)%>%
        head(10))

## 1c) Jak to wygląda jeśli chodzi o Polskę?

kable(suicide_only %>%
        filter(country == "Poland") %>%
        group_by(country,sex) %>%
        summarise(poland_mean_suicide = round(mean(suicide),3))%>%
        head(10))

poland<-suicide_only %>%
  filter(country == "Poland") %>%
  group_by(country,sex)

male_poland <- poland%>%
  filter(sex=="Male")

female_poland <- poland%>%
  filter(sex=="Female")

male_poland %>%
  select(suicide)%>%
  pull()%>%
  shapiro.test()

female_poland%>%
  select(suicide)%>%
  pull()%>%
  shapiro.test()

x <- male_poland$suicide
y <- female_poland$suicide

wilcox.test(x,y,alternative="g",paired = F)

# II. Interpretacja danych historycznych
## 2a) Jak zmieniał się wskaźnik samobójstw na przestrzeni lat w ujęciu kontynentalnym?

wykres_historyczny <- suicide_only %>%
  filter(sex=="Both sexes")%>%
  group_by(continent,sex,year) %>%
  summarise(mean_continent_year = round(mean(suicide),2))

wykres_historyczny %>%
  ggplot(aes(x=year,y=mean_continent_year,col=continent)) +
  geom_line(size = 1) +
  geom_point(size=2.5)+
  scale_color_manual(values = continent_colors)+
  labs(title = "Zmiana wskaźnika samobójstw dla obu płci na przestrzeni lat",
       x="Rok",
       y="Współczynnik śmiertelności z powodu samobójstw",
       fill="kontynent")+
  theme_light(base_size = 15)

# III. Badanie zależności + wykresy

## 3a) Czy istnieje zależność między średnim spożyciem alkoholu (wśród osób pijących) w kraju a samobójstwami?

### Ujęcie ogólne
x<-suicide_alcohol%>%
  filter(sex=="Both sexes")%>%
  group_by(continent,sex)

cor(x$suicide,x$alcohol,method = "spearman")

### MĘŻCZYŹNI

x<-suicide_alcohol%>%
  filter(sex=="Male")%>%
  group_by(continent,sex)

cor(x$suicide,x$alcohol,method="spearman")

x%>%
  ggplot()+
  geom_point(mapping=aes(x=alcohol,y=suicide,col=continent),alpha = 0.7)+
  scale_color_manual(values = continent_colors)+
  labs(
    title="Zależność między wskaźnikiem spożywanego alkoholu a wskaźnikiem samobójstw",
    x="Średnie dzienne spożycie na osobę w gramach wśród osób pijących",
    y="Współczynnik śmiertelności z powodu samobójstw"
  )+ 
  facet_wrap(vars(continent),ncol=2)+
  theme_light(base_size=15)

x<-suicide_alcohol%>%
  filter(sex=="Male",continent=="Europe")%>%
  group_by(continent,sex)

cor(x$suicide,x$alcohol,method="spearman")

x%>%
  ggplot()+
  geom_point(mapping=aes(x=alcohol,y=suicide,col=continent),alpha = 0.7)+
  scale_color_manual(values = continent_colors)+
  labs(
    title="Zależność między wskaźnikiem spożywanego alkoholu a wskaźnikiem samobójstw",
    x="Średnie dzienne spożycie na osobę w gramach wśród osób pijących",
    y="Współczynnik śmiertelności z powodu samobójstw"
  )+
  theme_light(base_size = 15)

### KOBIETY

x<-suicide_alcohol%>%
  filter(sex=="Female")%>%
  group_by(continent,sex)

cor(x$suicide,x$alcohol,method = 'spearman')

kable(alcohol %>%
        group_by(sex)%>%
        summarise(mean_alcohol = round(mean(alcohol),2)))

## 3b) Czy w krajach, gdzie obywatele płacą za leczenie z własnej kieszeni, wskaźnik samobójstw jest wyższy?<br>

cor(suicide_oop$suicide,suicide_oop$oop,method = 'spearman')

x<- suicide_oop %>%
  filter(year %in% c(2000,2010,2020))

x%>%
  ggplot()+
  geom_point(mapping=aes(x=oop,y=suicide, col=continent), alpha = 0.7)+
  scale_color_manual(values = continent_colors)+
  labs(title="Relacja między wskaźnikiem samobójstw a wydatkami na opiekę zdrowotną\nz własnej kieszeni",
       x="Roczne wydatki na opiekę zdrowotną [USD]",
       y="Współczynnik śmiertelności z powodu samobójstw")+
  facet_wrap(vars(year))+
  scale_x_continuous(labels=label_number())+
  theme_light(base_size = 15)

## 3c) Czy w krajach z wyższymi wydatkami rządowymi na opiekę zdrowotną wskaźnik samobójstw jest niższy?<br>

cor(suicide_dghe$suicide,suicide_dghe$dghe,method='spearman')

suicide_dghe%>%
  filter(year %in% c(2000,2010,2020))%>%
  ggplot()+
  geom_point(mapping=aes(x=dghe,y=suicide, col=continent), alpha = 0.7) +
  scale_color_manual(values = continent_colors)+
  labs(title="Relacja między wskaźnikiem samobójstw a wydatkami rządowymi\nna opiekę zdrowotną",
       x="Roczne rządowe wydatki na opiekę zdrowotną w przeliczeniu na osobę [USD]",
       y="Współczynnik śmiertelności z powodu samobójstw")+
  facet_wrap(vars(year))+
  scale_x_continuous(labels=label_number())+
  theme_light(base_size = 15)

## 3d) Czy istnieje zależność między PKB per capita a samobójstwami?<br>

cor(suicide_pkb$pkb,suicide_pkb$suicide,method='spearman')

suicide_pkb%>%
  filter(year %in% c(2000,2010,2020))%>%
  ggplot()+
  scale_color_manual(values = continent_colors)+
  geom_point(mapping=aes(x=pkb,y=suicide, col=continent), alpha = 0.7)+
  labs(title="Relacja między PKB per capita a wskaźnikiem samobójstw",
       x="PKB per capita [USD]",
       y="Współczynnik śmiertelności z powodu samobójstw")+
  facet_wrap(vars(year))+
  scale_x_continuous(labels=label_number())+
  theme_light(base_size=15)