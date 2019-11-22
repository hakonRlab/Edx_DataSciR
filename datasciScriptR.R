


###DataCamp
seq(12,93,2)

data("murders")
pop <- murders$population
pop<-sort(murders$population) ##sorterer i stigende rekkefølge
pop <- order(murders$population)##vektor med radnummerene for å få en sortert vektor
pop<-rank(murders$population)#rangsjeringen av radene i orginal vektoren

which.min(murders$population)
which.max(murders$population)
i <- which.min(murders$population)
states <- murders$state
states[i]##indexing

ranks <- rank(murders$population)
my_df <- data.frame(name=states,rank=ranks)
ind <- order(murders$population)
my_df1 <- data.frame(states=states[ind],ranks=ranks[ind])

data("na_example")
str(na_example)
mean(na_example)
ind<-is.na(na_example)
sum(ind)

mean(na_example[!ind])

murder_rate <- murders$total/murders$population*100000
ind <- which(murders$state=="Massachusetts")
murder_rate[ind]

ind <- match(c("New York","Florida","Texas"),murders$state)
ind
murder_rate[ind]

c("Boston","Dakota","Washington")%in%murders$state

murder_rate <- murders$total/murders$population*100000
low <- murder_rate<1
ind <- murders$region=="Northeast" & low
murders$state[ind]  

#By default, the data.frame() function turns character into factors.To avoid this,we utilize
#the stringAsFactors argumnet and set it equal to false
grades <- data.frame(names=c("John","Juan","Jean","Yao"),
                     exame_1=c(95,80,90,85),
                     exame_2=c(90,85,85,90),
                     stringsAsFactors=FALSE)
class(grades$names)

library(dplyr)  
murders <- mutate(murders,rate=total/population*100000,rank=rank(-rate))
filter(murders,rate<=0.71)#filter:selekterer rader
new_table <- select(murders,state,region,rate)#selct:selekterer kolonner
no_florida <- filter(murders,state!="Florida")#kan bruke != for å fjerne rader
#using the pip operator %>% 
murders%>%select(state,region,rate)%>%filter(rate<=0.71)

murders %>% filter(region %in% c("Northeast","West") & rate<1) %>% select(state,rate,rank)

##Generalform of an if - else statment
if(boolean condition){
  expressions
} else{alternatove expressions
}
###an example that tells us which state, if any, have a murder rare less that 0.5
murder_rate <- murders$total/murders$population*100000
ind <- which.min(murder_rate)
if(murder_rate[ind]<0.5){
  print(murders$state[ind])
} else{
  print("no state has murder rate that low")
}
# the ifelse() function is particularly useful on vectors
a <- c(0,1,2,-4,5)
result <- ifelse(a > 0, 1/a, NA)

# the ifelse() function is also helpful for replacing missing values
data(na_example)
no_nas <- ifelse(is.na(na_example), 0, na_example) 
sum(is.na(no_nas))

# the any() and all() functions evaluate logical vectors
z <- c(TRUE, TRUE, FALSE)
any(z)
all(z)

# the general form of a function
my_function <- function(VARIABLE_NAME){
  perform operations on VARIABLE_NAME and calculate VALUE
  VALUE
}

# functions can have multiple arguments as well as default values
avg <- function(x, arithmetic = TRUE){
  n <- length(x)
  ifelse(arithmetic, sum(x)/n, prod(x)^(1/n))
}
####General form for-loop
for (i in range of values){
  operation that use i, which is changing across the range of values
}

compute_s_n <- function(n){
  x <- 1:n
  sum(x)
}
m <- 25
#create an empty vector
s_n <- vector(length=m)
for(n in 1:m){
  s_n[n] <- compute_s_n(n)
}
#Functions that are typically used instead of for loops in R
#the apply family
apply()
sapply()
tapply()
mapply()
#other functions that are widely used are
split()
cut()
quantile()
reduce()
identical()
unique()


######exercise
install.packages("dslabs")
library(dslabs)
data(movielens)
str(movielens)

levels(movielens$genres)
nlevels(movielens$genres)


x <- c(2, 43, 27, 96, 18)
min(x)
which.min(x)

name <- c("Mandi","Amy","Nicole","Olivia")
distance<-c(0.8,3.1,2.8,4.0)
time <- c(10,20,40,50)
timeh <- time/60
speed <- distance/timeh

data(heights)
optioins(digits=3) #reports 3 significant digitss for all answers

mh <- mean(heights$height)
ind <-heights$height>mh 
sum(ind)
fabovav<-filter(heights,height>mh & sex=="Female")
ind<-heights$sex=="Female"
mean(ind)
min(heights$height)
ind <- match(50,heights$height)
heights$sex[ind]
max(heights$height)
x<-min(heights$height):max(heights$height)
sum(!x %in% heights$height )
heights2<-mutate(heights,ht_cm=height*2.54)
heights2[18,]
mean(heights2$ht_cm)
females <- filter(heights2,sex=="Female")
mean(females$ht_cm)

data(olive)
head(olive)
plot(olive$palmitic,olive$palmitoleic)
hist(olive$eicosenoic)
boxplot(palmitic~region,data=olive)

########

##Data Science: Visulization

#Empirical Cumulative Distribution Function (eCDF)


# make a table of category proportions
prop.table(table(heights$sex))

####ggplot2
library(tidyverse)
library(ggthemes)
library(ggrepel)

library(dslabs)
data(murders)

p <- murders %>% ggplot() + geom_point(aes(x=population/10^6,y=total))
r <- murders %>% summarize(rate=sum(total)/sum(population)*10^6) %>% pull(rate)

ggplot(murders) + 
  geom_abline(intercept=log10(r),lty=2,color="darkgrey")+
  geom_point(aes(x=population/10^6,y=total,col=region),size=3)+
  geom_text(aes(population/10^6,total,label=abb),nudge_x=0.075)+
  scale_x_continuous(trans="log10")+
  scale_y_continuous(trans="log10")+
  xlab("Population in millions (log scale)")+
  ylab("Total numbers od murders (log scale)")+
  ggtitle("US Gun Murders in 2010")+
  scale_color_discrete(name="Region")+
  theme_economist()

murders %>% 
  ggplot(aes(population/10^6,total,label=abb))+
  geom_abline(intercept=log10(r),lty=2,color="darkgrey")+
  geom_point(aes(col=region),size=3)+
  geom_text_repel()+
  scale_x_log10()+
  scale_y_log10()+
  xlab("Population in millions (log scale)")+
  ylab("Total numbers of Murders (log scale)")+
  ggtitle("US Gun Murders in 2010")+
  scale_color_discrete(name="Region")+
  theme_economist()

data("heights")

p <- heights %>% filter(sex=="Male") %>% 
  ggplot(aes(x=height))
p+geom_histogram(binwidth = 1,fill="blue",col="black")+
  xlab("Male height in inches")+
  ggtitle("Histogram")

p+geom_density(fill="blue")+
  theme_classic()

p <- heights %>% filter(sex=="Male") %>% 
  ggplot(aes(sample=height))
p+geom_qq()

# QQ-plot against a normal distribution with same mean/sd as data
params <- heights %>%
  filter(sex == "Male") %>%
  summarize(mean = mean(height), sd = sd(height))
p + geom_qq(dparams = params) +
  geom_abline()

#QQ-plot of scaled data against the standard normal distribution
heights %>% 
  ggplot(aes(sample=scale(height)))+
  geom_qq()+
  geom_abline()

library(gridExtra)
p <- heights %>% filter(sex=="Male") %>% ggplot(aes(x=height))
p1 <- p+geom_histogram(binwidth = 1,fill="blue",col="black")
p2 <- p+geom_histogram(binwidth = 2,fill="blue",col="black")
p3 <- p+geom_histogram(binwidth = 3,fill="blue",col="black")

grid.arrange(p1,p2,p3,ncol=3,nrow=1)

###dplyr
library(tidyverse)
library(dslabs)
data("heights")

s <- heights %>% 
  filter(sex=="Male") %>% 
  summarize(average=mean(height),standard_deviation=sd(height))
s
s$average
s$standard_deviation

heights %>% 
  filter(sex=="Male") %>% 
  summarize(median=median(height),
            minimum=min(height),
            maximum=max(height))
#with the fuction summarize, we can only call functions that return a single value

data("murders")
#calculate US murder rate, generating a data frame
us_murder_rate <-  murders %>% 
  summarize(rate=sum(total)/sum(population)*10^5)
us_murder_rate
#extracting the numeric US murder rate with the dot operator
us_murder_rate %>% .$rate
us_murder_rate <-  murders %>% 
  summarize(rate=sum(total)/sum(population)*10^5) %>% 
  .$rate

###group_by converts a data frame to a grouped data frame
heights %>% 
  group_by(sex) %>% 
  summarize(average=mean(height),standard_deviation=sd(height))

murders <- murders %>% 
  mutate(murder_rate=total/population*10^5)
murders %>% 
  group_by(region) %>% 
  summarize(median_rate=median(murder_rate))


### Sorting data tables

murders %>% arrange(population) %>% head()
murders %>% arrange(murder_rate) %>% head()
murders %>% arrange(desc(murder_rate)) %>% head()
murders %>% arrange(region,murder_rate) %>% head()
#show the top 10 states with heighest murder rate, not ordered by rate
murders %>% top_n(10,murder_rate)
murders %>% arrange(desc(murder_rate)) %>% top_n(10)

#DataCamp exercise
install.packages("NHANES")
library(NHANES)
data(NHANES)
library(dslabs)
data(na_example)
mean(na_example)
sd(na_example)
mean(na_example,na.rm=TRUE)
sd(na_example,na.rm=TRUE)

tab <- NHANES %>% filter(Gender=="female",AgeDecade==" 20-29") %>% 
  summarize(average=mean(BPSysAve),std=sd(BPSysAve))
ref <- NHANES %>% filter(Gender=="female",AgeDecade==" 20-29") %>% 
  summarize(average=mean(BPSysAve,na.rm=TRUE),std=sd(BPSysAve,na.rm=TRUE))
ref  
ref_avg <- NHANES %>% filter(Gender=="female",AgeDecade==" 20-29") %>% 
  summarize(average=mean(BPSysAve,na.rm=TRUE),std=sd(BPSysAve,na.rm=TRUE)) %>% 
  .$average
ref_avg
ref_minmax <- NHANES %>% filter(Gender=="female",AgeDecade==" 20-29") %>% 
  summarize(min=min(BPSysAve,na.rm=TRUE),max=max(BPSysAve,na.rm=TRUE))
ref_minmax

NHANES %>% filter(Gender=="female") %>% 
  group_by(AgeDecade) %>% 
  summarize(average=mean(BPSysAve,na.rm=TRUE),std=sd(BPSysAve,na.rm=TRUE))

race <- NHANES %>% filter(Gender=="male",AgeDecade==" 40-49") %>% 
  group_by(Race1) %>% 
  summarize(average=mean(BPSysAve,na.rm=TRUE),standard_deviation=sd(BPSysAve,na.rm=TRUE)) %>% 
  arrange(desc(average,standard_deviation))
    
#####Gapminder.org

libray(dslabs)
data("gapminder")
head(gapminder)

# compare infant mortality in Siri Lanka and Turkey
gapminder %>% 
  filter(year==2015 & country %in% c("Sri Lanka","Turkey")) %>% 
  select(country,infant_mortality)

# basic scatterplot of life expectancy versus fertility
ds_theme_set() # set plot theme 
filter(gapminder,year==1962) %>% 
  ggplot(aes(fertility,life_expectancy))+
  geom_point()
#add color as continet
filter(gapminder,year==1962) %>% 
  ggplot(aes(fertility,life_expectancy,color=continent))+
  geom_point()

#Faceting makes multiple side-by-side plots stratified by some variable (up to two variables)

filter(gapminder,year %in% c(1962,2012)) %>% 
  ggplot(aes(fertility,life_expectancy, col=continent))+
  geom_point()+
  facet_grid(continent~year)

filter(gapminder,year %in% c(1962,2012)) %>% 
  ggplot(aes(fertility,life_expectancy, col=continent))+
  geom_point()+
  facet_grid(.~year)

years <- c(1962,1980,1990,2000,2012)
continents <- c("Europe","Asia")

gapminder %>% 
filter(year %in% years & continent %in% continents) %>% 
  ggplot(aes(fertility,life_expectancy, col=continent))+
  geom_point()+
  facet_wrap(~year)

#Time series plots

gapminder %>% 
  filter(country=="United States") %>% 
  ggplot(aes(year,fertility))+
  geom_point()

gapminder %>% 
  filter(country=="United States") %>% 
  ggplot(aes(year,fertility))+
  geom_line()

#Multiple time series
countries <- c("South Korea","Germany")
gapminder %>% 
  filter(country %in% countries) %>% 
  ggplot(aes(year,fertility))+
  geom_line()
#Need to tell ggplot that there are two groups
gapminder %>% 
  filter(country %in% countries) %>% 
  ggplot(aes(year,fertility,group=country))+
  geom_line()

gapminder %>% 
  filter(country %in% countries) %>% 
  ggplot(aes(year,fertility,col=country))+
  geom_line()
#Adding text lables to plot  
lables <- data.frame(country=countries,x=c(1975,1965),y=c(60,72))  
gapminder %>% 
  filter(country %in% countries) %>% 
  ggplot(aes(year,life_expectancy,col=country))+
  geom_line()+
  geom_text(data=lables,aes(x,y,label=country),size=5)+
  theme(legend.position="none")

# Transforamtion
gapminder <- gapminder %>% 
  mutate(dollars_per_day=gdp/population/365)

past_year <- 1970
gapminder %>% 
  filter(year==past_year &!is.na(gdp)) %>% 
  ggplot(aes(dollars_per_day))+
  geom_histogram(binwidth=1,color="black")

gapminder %>% 
  filter(year==past_year &!is.na(gdp)) %>% 
  ggplot(aes(log2(dollars_per_day)))+
  geom_histogram(binwidth=1,color="black")
#or
gapminder %>% 
  filter(year==past_year &!is.na(gdp)) %>% 
  ggplot(aes(dollars_per_day))+
  geom_histogram(binwidth=1,color="black")+
  scale_x_continuous(trans="log2")
gapminder %>% 
  filter(year==past_year &!is.na(gdp)) %>% 
  ggplot(aes(log10(population)))+
  geom_histogram(binwidth=0.5,color="black")
  scale_x_continuous(trans="log10")
  
# Stratify and boxplot
length(levels(gapminder$region))  

past_year <- 1970
p <- gapminder %>% 
  filter(year==past_year &!is.na(gdp)) %>% 
  ggplot(aes(region,dollars_per_day))
p+geom_boxplot()+
  theme(axis.text.x=element_text(angle=90,hjust=1))
#reorder function
fac <- factor(c("Asia","Asia","West","West","West"))
levels(fac)

value <- c(10,11,12,6,4)
fac <- reorder(fac,value,FUN=mean)
levels(fac)

#reorder by median income and color by continent

p <- gapminder %>% 
  filter(year==past_year & !is.na(gdp)) %>% 
  mutate(region=reorder(region,dollars_per_day,FUN=median)) %>% #reodrder
  ggplot(aes(region,dollars_per_day,fill=continent))+ # color by continent
  geom_boxplot()+
  theme(axis.text.x=element_text(angle=90,hjust=1))+
  xlab("")

p+scale_y_continuous(trans="log2") #log2 scale y-axis

p+scale_y_continuous(trans="log2")+ geom_point(show.legend = FALSE) #add data points
  
#Comparing distribution

#define western countries
west <- c("Western Europe","Northern Europe","Southern Europe","Northern America","Australia and New Zealand")
#facet by West vs developing
gapminder %>% 
  filter(year==past_year & !is.na(gdp)) %>% 
  mutate(group=ifelse(region %in% west,"West","Developing")) %>% 
  ggplot(aes(dollars_per_day))+
  geom_histogram(binwidth = 1,color="black")+
  scale_x_continuous(trans="log2")+
  facet_grid(.~group)
#facet by West/developing and year
present_year <- 2010
gapminder %>% 
  filter(year %in% c(past_year,present_year) & !is.na(gdp)) %>% 
  mutate(group=ifelse(region %in% west,"West","Developing")) %>% 
  ggplot(aes(dollars_per_day))+
  geom_histogram(binwidth = 1,color="black")+
  scale_x_continuous(trans="log2")+
  facet_grid(year~group)
#define countreis that have data available in both years
country_list_1 <- gapminder %>% 
  filter(year==past_year & !is.na(dollars_per_day)) %>% .$country
country_list_2 <- gapminder %>% 
  filter(year==present_year & !is.na(dollars_per_day)) %>% .$country
country_list <- intersect(country_list_1,country_list_2)
gapminder %>% 
  filter(year %in% c(past_year,present_year) & country %in% country_list )%>% 
  mutate(group=ifelse(region %in% west,"West","Developing")) %>% 
  ggplot(aes(dollars_per_day))+
  geom_histogram(binwidth = 1,color="black")+
  scale_x_continuous(trans="log2")+
  facet_grid(year~group)

p <- gapminder %>% 
  filter(year %in% c(past_year,present_year) & country %in% country_list )%>% 
  mutate(region=reorder(region,dollars_per_day,FUN=median)) %>% 
  ggplot()+
  theme(axis.text.x=element_text(angle=90, hjust=1))+
  xlab("")+ scale_y_continuous(trans="log2")
p+ geom_boxplot(aes(region,dollars_per_day,fill=continent))+
  facet_grid(year~.)
#arrange matching boxplots next to each other,colored by year  
p+geom_boxplot(aes(region,dollars_per_day,fill=factor(year)))


##Density plot -area under each curve andds to 1

gapminder %>% filter(year==past_year & country %in% country_list) %>% 
  mutate(group=ifelse(region %in% west, "West","Developing")) %>% 
  group_by(group) %>% 
  summarize(n=n()) %>% 
  knitr::kable()
#smooth density plots -variable counts on y-axis
p <- gapminder %>% 
  filter(year %in% c(past_year,present_year) & country %in% country_list) %>% 
  mutate(group=ifelse(region %in% west, "West","Developing")) %>% 
  ggplot(aes(dollars_per_day,y=..count..,fill=group))+
  scale_x_continuous(trans="log2")
p+geom_density(alpha=0.2)+
  facet_grid(year~.)
p+geom_density(alpha=0.2,bw=0.75)+
  facet_grid(year~.)

#Add new region groups with case_when
gapminder <- gapminder %>% 
  mutate(group=case_when(
    .$region %in% west ~"West",
    .$region %in% c("Eastern Asia","South-Eastern Asia")~"East Asia",
    .$region %in% c("Carribbean","Central America","South America")~"Latin America",
    .$continent =="Africa" & .$region !="Northern Africa"~"Sub-Saharan Africa",
    TRUE~"Others"
  ))
#reorder factor levels
gapminder <- gapminder %>% 
  mutate(group=factor(group,levels=c("Others","Latin America","East Asia","Sub-Saharan Africa","West")))
p <- gapminder %>% 
  filter(year %in% c(past_year,present_year) & country %in% country_list) %>% 
  ggplot(aes(dollars_per_day,fill=group))+
  scale_x_continuous(trans="log2")
p+geom_density(alpha=0.2,bw=0.75,position="stack")+
  facet_grid(year~.)

#weighted stacked desinty plot
gapminder %>% 
  filter(year %in% c(past_year, present_year) & country %in% country_list) %>% 
  group_by(year) %>% 
  mutate(weight = population/sum(population*2)) %>% 
  ungroup() %>% 
  ggplot(aes(dollars_per_day, fill= group, weight= weight))+
  scale_x_continuous(trans="log2")+
  geom_density(alpha=0.2, bw=0.75,position="stack")+
  facet_grid(year~.)

## The Ecological fallacy is assuming that conclusions made from the average of a group apply to all members of that group
#add additional case
gapminder <- gapminder %>% 
  mutate(group1=case_when(
    .$region %in% west ~"The West",
    .$region %in% "Northern Africa"~"Northern Africa",
    .$region %in% c("Eastern Asia","South-Eastern Asia")~"East Asia",
    .$region =="Southern Asia"~"Southern Asia",
    .$region %in% c("Carribbean","Central America","South America")~"Latin America",
    .$continent =="Africa" & .$region !="Northern Africa"~"Sub-Saharan Africa",
    .$region %in% c("Melanesia","Micronesia","Polynesia")~"Pacific Islands"))

#define a data frame with group average income and average infant survival rate

surv_income <- gapminder %>% 
  filter(year %in% present_year & !is.na(gdp) & !is.na(infant_mortality) & !is.na(group1)) %>% 
  group_by(group1) %>% 
  summarize(income=sum(gdp)/sum(population)/365 ,
              infant_survival_rate=1-sum(infant_mortality/1000*population)/sum(population))
surv_income %>%  arrange(income)  

surv_income %>% ggplot(aes(income,infant_survival_rate,label=group1,color=group1))+
  scale_x_continuous(trans="log2",limit=c(0.25,150))+
  scale_y_continuous(trans="logit",limit=c(0.895,0.9981),
                     breaks=c(0.85,0.90,0.95,0.99,0.995,0.998))+
  geom_label(size=3,show.legend=FALSE)
  
###Show the data
heights %>%  ggplot(aes(sex,height)) + geom_point()
#jitter, alpha blended point plot  
heights %>% ggplot(aes(sex,height))+ geom_jitter(width=0.1,alpha=0.2)  

color_blind_friendly_cols <- c("#999999", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

p1 <- data.frame(x = 1:8, y = 1:8, col = as.character(1:8)) %>%
  ggplot(aes(x, y, color = col)) +
  geom_point(size = 5)
p1 + scale_color_manual(values = color_blind_friendly_cols)  
  
##Slope chart and Bland-Altman plot, consider using these when comparing one variable at two different
#time points, especially for small numbers of observations

west <- c("Western Euorpe","Northern Europe","Southern Europe","Northern America","Australia and New Zealnad")
dat <- gapminder %>% 
  filter(year %in% c(2010,2015) & region %in% west & !is.na(life_expectancy) & population>10^7)

dat %>% 
  mutate(location = ifelse(year == 2010,1,2),
         location=ifelse(year==2015 & country %in% c("United Kingdom","Portugal"),
                         location + 0.22,location),
         hjust=ifelse(year == 2010,1,0)) %>% 
  mutate(year=as.factor(year)) %>% 
  ggplot(aes(year,life_expectancy,group=country))+
  geom_line(aes(color=country),show.legend = FALSE)+
  geom_text(aes(x=location,label=country,hjust=hjust),show.legend = FALSE)+
  xlab("")+
  ylab("Life Expectancy")
  
#Bland-Altman plot
library(ggrepel)
dat %>% 
  mutate(year=paste0("life_expectancy_",year)) %>% 
  select(country,year,life_expectancy) %>% spread(year,life_expectancy) %>% 
  mutate(average=(life_expectancy_2015 + life_expectancy_2010)/2,
         difference=life_expectancy_2015 - life_expectancy_2010) %>% 
  ggplot(aes(average, difference,label=country))+
  geom_point()+
  geom_text_repel()+
  geom_abline(lty=2)+
  xlab("Average of 2010 and2015")+
  ylab("Difference between 2015 and 2010")

#reduce the number of digits locally using "round" or "signif"
#Reduce the number of significant digits globally by setting an option.For example, "option(digits=3)
#will cause all future computations that session to have 3 digits

#We encode categorical variables with color and shape.These shapes can be controlled with "shape"

#sequential colors are suited for data that goes from high to low
library(RColorBrewer)
display.brewer.all(type="seq")
#Diverging colors are used to represent values that diverge from the center
display.brewer.all(type="div")

#Tile plot of measles rate by year and state
library(dslabs)
data(us_contagious_diseases)
str(us_contagious_diseases)
#assign dat to the per 10 000 rate of measles, removing Alaska and Hawaii and adjusting for weeks reportin
the_disease <- "Measles"
dat <- us_contagious_diseases %>% 
  filter(!state %in% c("Hawaii","Alaska") & disease==the_disease) %>% 
  mutate(rate=count/population *10000 * 52/weeks_reporting) %>% 
  mutate(state=reorder(state,rate))
#plot disease rate per year in California
dat %>% filter(state=="California" & !is.na(rate)) %>% 
  ggplot(aes(year,rate))+
  geom_line()+
  ylab("Cases per 10,000")+
  geom_vline(xintercept = 1963, col="blue")
#tile plot of disease rate by state and year
dat %>% ggplot(aes(year,state,fill=rate))+
  geom_tile(color="grey50")+
  scale_x_continuous(expand=c(0,0))+
  scale_fill_gradientn(colors=RColorBrewer::brewer.pal(9, "Reds"), trans="sqrt")+
  geom_vline(xintercept=1963, col="blue")+
  theme_minimal() + theme(panel.grid=element_blank())+
  ggtitle(the_disease)+
  ylab("")+
  xlab("")
#Line plot of measles rate by year
#compute US average measles rate by year
avg <- us_contagious_diseases %>% 
  filter(disease==the_disease) %>% group_by(year) %>% 
  summarize(us_rate=sum(count,na.rm=TRUE)/sum(population,na.rm=TRUE)*10000)
#make a line plot of measles rate by year by state
dat %>% 
  filter(!is.na(rate)) %>% 
  ggplot()+
  geom_line(aes(year,rate,group=state),color="grey50",show.legend = FALSE,alpha=0.2,size=1)+
  geom_line(mapping=aes(year,us_rate),data=avg,size=1,col="black")+
  scale_y_continuous(trans = "sqrt", breaks= c(5, 25, 125, 300))+
  ggtitle("Cases per 10,000 by state")+
  xlab("")+
  ylab("")+
  geom_text(data=data.frame(x=1955,y=50),
            mapping=aes(x,y,label="US average"),color="black")+
  geom_vline(xintercept=1963,col="blue")

us_contagious_diseases %>% filter(!is.na(population)) %>% 
        group_by(year, disease) %>%
        summarize(rate = sum(count)/sum(population)*10000) %>%
        ggplot(aes(year, rate, color=disease))+ 
        geom_line()  

#Titanic Survival exercises

options(digits = 3) 
library(tidyverse)
install.packages("titanic")
library(titanic)

titanic <- titanic_train %>% 
  select(Survived,Pclass,Sex,Age,SibSp,Parch,Fare) %>% 
  mutate(Survived=factor(Survived),
         Pclass=factor(Pclass),
         Sex=factor(Sex))
str(titanic)
?titanic_train

titanic %>% 
  group_by(Sex) %>% 
  ggplot(aes(Age,fill=Sex))+
  geom_density(alpha=0.2)+
  facet_grid(Sex~.)
titanic %>% 
  group_by(Sex) %>% 
  ggplot(aes(Age,y=..count..,fill=Sex))+
  geom_density(alpha=0.2)+
  facet_grid(Sex~.)  

params <- titanic %>% 
  filter(!is.na(Age)) %>% 
  summarize(mean=mean(Age),sd=sd(Age))
titanic %>% 
  ggplot(aes(sample=Age))+
  geom_qq(dparams=params)+
  geom_abline()

titanic %>% 
  ggplot(aes(Sex,fill=Survived))+
  geom_bar(position = position_dodge())

titanic %>% 
  ggplot(aes(Age,fill=Survived))+
  geom_density(alpha=0.2)

titanic %>% 
  filter(Fare>0) %>% 
  ggplot(aes(Survived,Fare))+
  geom_boxplot()+
  scale_y_continuous(trans="log2")+
  geom_jitter()

titanic %>% 
  ggplot(aes(Pclass,fill=Survived))+
  geom_bar()
titanic %>% 
  ggplot(aes(Pclass,fill=Survived))+
  geom_bar(position=position_fill())
titanic %>% 
  ggplot(aes(Survived,fill=Pclass))+
  geom_bar(position=position_fill())

titanic %>% 
  ggplot(aes(Age,y=..count..,fill=Survived))+
  geom_density(alpha=0.2)+
  facet_grid(Sex~Pclass)

###Phx125.3 Probability

#the rep function and the sample function

beads <- rep(c("red","blue"),times=c(2,3))
beads

sample(beads,1)  #sample 1 bead at random

#Monte Carlo simulation
B <- 10000 #number of times to draw 1 bead
events <- replicate(B,sample(beads,1))
tab <- table(events)
tab
prop.table(tab)

events <- sample(beads,B,replace=TRUE)
prop.table(table(events))

#Important note on seeds in R 3.5 and R 3.6
set.seed(1) #r 3.5
set.seed(1,sample.kind="Rounding") #will make R 3.6 generate a seed as in R 3.5

# An important application of the mean function
# to find the probability of drawing a blue bead at random, you can run:
mean(beads=="blue")

#Introducing paste and expand.grid
number <- "Three" 
suit <- "Hearts"
paste(number,suit)#joins strings and insterts a space in between

#joining vectors element-wise with paste
paste(letters[1:5], as.character(1:5))
#generating combinations of 2 vectors with expand.grid
expand.grid(pants=c("blue","black"),shirt=c("white","grey","plaid"))

#Generating a deck of cards
suits <- c("Diamonds","Clubs","Hearts","Spades")
numbers <- c("Ace","Deuce","Three","Four","Five","Six","Seven","Eight","Nine","Ten","Jack","Queen","King")
deck <- expand.grid(number=numbers,suit=suits)
deck <- paste(deck$number,deck$suit)

#probabilit of drawing a king
kings <- paste("King",suits)
mean(deck %in% kings)

#Permutations and combinations
library(gtools)
permutations(5,2) #ways to choose 2 numbers on order from 1:5

all_phone_numbers <- permutations(10,7,v=0:9)
n <- nrow(all_phone_numbers)
index <- sample(n,5)
all_phone_numbers[index,]

permutations(3,2) # order matters
combinations(3,2) # order noes not matter

#probablity of drawing a second king given that one king is drawn
hands <- permutations(52,2,v=deck)
first_hand <- hands[,1]
second_hand <- hands[,2]
sum(first_hand %in% kings)
sum(first_hand %in% kings & second_hand %in% kings) / sum(first_hand %in% kings)

#Probability of natural 21 in blackjack
aces <- paste("Ace",suits)
facecard <- c("King","Queen","Jack","Ten")
facecard <- expand.grid(number=facecard,suit=suits)
facecard <- paste(facecard$number,facecard$suit)
hands <- combinations(52,2,v=deck) # all possible hands
#probability of a natrual 21 given that the ace is listed first in "combinations"
mean(hands[,1] %in% aces & hands[,2] %in% facecard)
#probability of natural 21 checking for both ace first and ace second
mean(hands[,1] %in% aces & hands[,2] %in% facecard | (hands[,2]%in% aces & hands[,1] %in% facecard))

#Monte Carlo simulation of natural 21 in blackjack
hand <- sample(deck,2)
hand
#code for B=10000 hands of blackjack
B <- 10000
results <- replicate(B, {
  hand<- sample(deck,2)
  (hand[1] %in% aces & hand[2] %in% facecard | (hand[2]%in% aces & hand[1] %in% facecard))
})
mean(results)

#The Birthday problem
#duplicated takes a vector and returns a vector of the same length with TRUE for any elements that have appeared previously in that vector

n <- 50
bdays <- sample(1:365,n,replace=TRUE)
any(duplicated(bdays)) # check if any bdays are duplicated

#Monte Carlo simulation with B=10000 replicates
B <- 10000
results <- replicate(B,{
  bdays <- sample(1:365,n,replace=TRUE)
  any(duplicated(bdays))
})
mean(results)

###sapply
#Some functions automatically apply element-wise to vectors, such as sqrt and *
#However, other functions do not operate element-wise by default.This includes functions we define ourself
#The function sapply(x,f) allows any other function f to be applied element-wise to the vector x

#function to calculate probability of shared bday across n people

compute_prob <- function(n,B=10000){
  same_day <-replicate(B,{
    bdays <- sample(1:365,n,replace=TRUE)
  any(duplicated(bdays))
})
mean(same_day)
}
n <- seq(1,60)

#Element-wise operation over vectors and sapply
x <- 1:10
sqrt(x)   #sqrt operates on each element of the vector
y <- seq(1:10)
y*x       # * operates element-wise on both vectors
compute_probe(n)   #does not iterate over the vector n without sapply
saplly(x,sqrt)     # this is equivalent to sqrt(x)

prob <- sapply(n,compute_prob)   # element-wise application of compute_probe to n
plot(n,prob)

#Computing bday problem probabilities with sapply

#function for computing exact probalitity of shared birthdays for any n

exact_prob <- function(n){
  prob_unique <- seq(365,365-n+1)/365  #vector of fractions for mult. rule
               1-prod(prob_unique)     #calculate prob of no shared bdays and subtract from 1
}
#sapply function element-wise to vector of n values
eprob <- sapply(n,exact_prob)
#plotting Monte Calro results and exact probabilities on same graph

plot(n,prob)  # plot Monte Carlo results
lines(n,eprob, col="red")   # add line for exact prob

#How many Monte Carlo experiments are enough?
#The lager the number of Monte Carlo replicates B, the more accurate the estimate
#One practical approach is to try many sizes for B and look for size provide stable estimates

B <- 10^seq(1,5,len=100)  # defines vector of many B values
compute_prob <- function(B,n=22){   #function to run Monte Carlo simulation with each B
  same_day <- replicate(B,{
    bdays <- sample(1:365,n,replace=TRUE)
    any(duplicated(bdays))
  })
    mean(same_day)  
}
prob <- sapply(B,compute_prob) #sapply cumpute_prob to amny values of B
plot(log10(B),prob,type="l")  # plot a line graph of estimates


##The Monty Hall problem
#Monte Carlo simulation of stick strategy

B <- 10000
stick <- replicate(B,{
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat")) #puts prizes in random order
  prize_door <- doors[prize=="car"]  #note which door has prize
  my_pick <- sample(doors,1) #note which door is chosen
  show <- sample(doors[!doors %in% c(my_pick,prize_door)],1) #open door with no prize that isn't prize
  stick <- my_pick #stick wiht original door
  stick==prize_door #test wether the original door has the prize
})
mean(stick)  #probability of choosing prize door when sticking

#Monte Carlo simulation of switch strategy
switch <- replicate(B,{
  doors <- as.character(1:3)
  prize <- sample(c("car","goat","goat")) #puts prizes in random order
  prize_door <- doors[prize=="car"]  #note which door has prize
  my_pick <- sample(doors,1) #note which door is chosen
  show <- sample(doors[!doors %in% c(my_pick,prize_door)],1) #open door with no prize that isn't prize
  switch <- doors[!doors %in% c(my_pick,show)] #switch to the door that wasn't chosen first or opened
  switch==prize_door #test wether the original door has the prize
})
mean(switch)  #probability of choosing prize door when sticking

###Exercise descrite probability
runners <- c("Jamaica", "Jamaica", "Jamaica", "USA", "Ecuador", "Netherlands", "France", "South Africa")
B<-10000
medals<-seq(1:3)
set.seed(1)
Olympic<-
  replicate(B,{
  winner<-sample(runners,3,replace=FALSE)
  winner1<-all(winner=="Jamaica")
}) 
mean(Olympic)

#Restaurant manger exericise
6*nrow(combinations(6,2))*2
6*nrow(combinations(6,2))*3
6*nrow(combinations(6,3))*3

n <- seq(1:12)
dinner <- function(n){
  supper <- n*nrow(combinations(6,2))*3
}             
din <- sapply(n,dinner)
data.frame(entress=n, combos=din) %>% 
  filter(din>365) %>% 
  min(.$entress)

n <- seq(2:12)
side <- function(x){
    6*nrow(combinations(x,2))*3
}             
side_nr <- sapply(2:12,side)
data.frame(entress=2:12, combos=side_nr) %>% 
  filter(side_nr>365) %>% 
    min(.$entress)

##Esophageal cancer study exercise
data("esoph")
head(esoph)
sum(nrow(esoph))
all_cases <- sum(esoph$ncases)
all_controls <- sum(esoph$ncontrols)

highAlc <- esoph %>% 
  filter(alcgp=="120+")
sum(highAlc$ncases)/sum(highAlc$ncontrols+highAlc$ncases)

lowAlc <- esoph %>% 
  filter(alcgp=="0-39g/day")
sum(lowAlc$ncases)/sum(lowAlc$ncontrols+lowAlc$ncases)


smokecase <- esoph %>% 
  filter(tobgp=="30+")
sum(smokecase$ncases)/all_cases
sum(smokecase$ncontrols)/all_controls

sum(highAlc$ncontrols)/sum(all_controls)

smokeAlc <- esoph %>% 
  filter(tobgp=="30+" & alcgp=="120+")
sum(smokeAlc$ncases)/all_cases
smokeAlc <- esoph %>% 
  filter(tobgp=="30+" | alcgp=="120+")
sum(smokeAlc$ncases)/all_cases
sum(smokeAlc$ncontrols)/all_controls


# Section 2
###Continous Probabiltiy
#The cumulative distribution function (CDF) is a distribution function for continuous data x that
#reports the proportion of the data below a for all values of a

#The CDF is the probability function for continous varibales

x <- heights %>% filter(sex=="Male") %>% pull(height)

f <- function(a) mean(x<=a)
1-f(70) #probability of  male taller than 70 inches

#Theoretical distribution

pnorm(a,avg,s) #gives the value of the cumulative distribution function F(a) for the normal distribution 
#defined by  avg and standard diviation s

# we can estimate the probalitity that a male is taller than 70.5 inches using

1-pnorm(70.5,mean(x),sd(x))

#Discretization and the normal appoximation
#plot distribution of exact heights in data
plot(prop.table(table(x)),xlab="a= Height in inches", ylab= "Pr(x=a)")

#probabilities in actual data over length 1 ranges containing an integer
mean(x<=68.5) - mean(x<=67.5)
#probabilities in normal apporximation match well
pnorm(68.5,mean(x),sd(x)) - pnorm(67.5,mean(x),sd(x))

#probabilities in actual data over other ranges don´t mach normal approx as well
mean(x<=70.9) - mean(x<=70.1)
pnorm(70.9,mean(x),sd(x)) - pnorm(70.1,mean(x),sd(x))

##Probability Density
#the probability of a singel value is not defined for a continuous distribution
#the quantity with the most similar interpretation to the probability of a single value is the probability density function f(x)
#the probability density f(x) is defined such as the integral of f(x) over a range gives the CDF of taht range
#in R, the probability density function for the normal distribution is given by dnorm
#Note that dnorm gives the dinsity function and pnorm gives the distribution function, which is the integral of the densoty function

#Generating normally distributed random numbers for Monte Carlo simulations
# rnorm(n,avg,s) generates n random numnbers from the normal distribution with the avg and standard deviation s

x <- heights %>% filter(sex=="Male") %>% pull(height)
n <- length(x)
avg <- mean(x)
s <- sd(x)
simulate_heights <- rnorm(n,avg,s)

#plot distributio of simulated_heights
data.frame(simulate_heights=simulate_heights) %>% 
  ggplot(aes(simulate_heights))+
  geom_histogram(color="black",binwidth=2)

#Monte Carlo simulation of probability of tallest person being over 7 feet
B <- 1000
tallest <- replicate(B,{
  simulate_heights <- rnorm(800,avg,s) #generate 800 normally distributed random heights
  max(simulate_heights) #determine the tallest height
})
mean(tallest>=7*12) # propotion of the times that tallest person exceeded 7 feet

#other continuous distributions
#you may encounter other continuous distributions (Student t, chi-square,exponetial, gamme, beta, etc...)
#R provides functions for density (d), quantile (q), probability distribution (p) and random number generation (r) for amny of these distributions
#Each distribution has a macthing ebbreviatio (for example, norm or t) that is paired with the related function abbreviation (d,p,q,r)to
#to create appropriate functions
#for example, use rt to generate random numbers for a Monte Carlo simulation using the Student t distribution

#plotting the normal distribution with dnorm

x <- seq(-4,4, length=100)
data.frame(x,f=dnorm(x)) %>% 
  ggplot(aes(x,f))+
  geom_line()

##Assessment Continuous probability
set.seed(16)
act_score <- rnorm(10000,20.9,5.7)
mean(act_score)
sd(act_score)
sum(act_score>=36)
sum(act_score>30)/10000
mean(act_score>30)
sum(act_score<10)/10000
mean(act_score<=10)

x <- 1:36
f_x <- dnorm(x,20.9,5.7)
plot(x,f_x)
data.frame(x,f_x) %>% 
  ggplot(aes(x,f_x))+
  geom_line()

act_scoreZ <- (act_score - 20.84012)/5.675237
mean(act_scoreZ>2)
act_scoreZ==2
mean(act_score)+2*sd(act_score)
qnorm(0.975,mean(act_score),sd(act_score))

#Q4a
f <- function(a) {mean(act_score<=a)}
sapply(x,f)

cdf <- sapply(1:36,function(x){
  mean(act_score<=x)
})
min(which(cdf>=0.95))
#Q4b
qnorm(0.95,20.9,5.7)
#Q4c
p <- seq(0.01,0.99,0.01)
sample_quantiles <- quantile(act_score,p)
names(sample_quantiles[max(which(sample_quantiles<26))])
#Q4d
theoretical_quantile <- qnorm(p,20.9,5.7)
plot(theoretical_quantile,sample_quantiles)


###Random Variables

##Modelling a random variable
#Define random variable x to be 1 if blue, 0 otherwise

beads <- rep(c("red","blue"),times=c(2, 3))
x <-  ifelse(sample(beads, 1) == "blue", 1, 0)

##Sampleing Models
#A sampling model models the random behavior of a process as the sampling of draws from a urn
#the probability distribution of a random varibale is the probability of the observed value falling in any given interval
#the average of amny draws of a random varible is called the expected value
#the standard deviation of many drwas of a random variable is called is standard error

#Monte Carlo simulation:chance of casino losing money on roullette
#sampling model 1: define urn, then sample

color <- rep(c("Black","Red","Green"),c(18,18,2)) #define the urn for the sample model
n <- 1000
x <- sample(ifelse(color=="Red",-1,1),n,replace=TRUE) #1000 draws from urn,-1 if red, elsse +1
x[1:10] #first 10 outcomes

#sampling model 2: define urn inside sample function by noting probabilities
x <- sample(c(-1,1),n,replace=TRUE,prob=c(9/19,10/19)) #1000 independent draws
s <- sum(x) #total winnings = sum of draws
s

#We use the sampling model to run a Monte Carlo simulation and use the results to estimate the probability of the casinp losing money
n <- 1000 #number of roulette players
B <- 10000 # number of Monte Carlo experiments

S<- replicate(B, {
  x <- sample(c(-1,1), n ,replace=TRUE, prob=c(9/19, 10/19)) #simulate 1000 roulette spins
  sum(x)
})
mean(S<0) #probability of the casino losing money

#we can plot a histogram of the observed values of S as well as the normal density curve based on the mean and standard deviation of S
library(tidyverse)
s <- seq(min(S),max(S),length=100) # sequence of 100 values across range of S
normal_density <- data.frame(s=s,f=dnorm(s,mean(S),sd(S))) #generate normal density for S
data.frame(S=S) %>%  # make data frame od S for hisotgram
ggplot(aes(S,..density..))+
  geom_histogram(color="black",binwidth=10)+
  ylab("Probability")+
  geom_line(data=normal_density,mapping=aes(s,f),color="blue")


###Distributions vs probability distributions
#distribution F(a), what proportion of the list is less than or equal to a.
# the random variable a has a distribution function, and we can define
# F(a) as what is the probability that X is less than or equal to a.

##Notation for random variables
#Captial letters denote random variables (X) and lowercase letters denote observed values(x).
#In the noation Pr(X=x), we are asking how frequently the random variable X is equal to the value x.
#For example, if x=6, this statement becomes Pr(X=6).


##Central Limit Theorem
#The Central Limit Theorem (CLT) says that the distribution of the sum of a random variable is approximated by the normal distribution
#CLT,when the number of the draws is large, the sum of the independent draws is approximately normal.
#The expected value of the random variable, E[X]=u, is the average of the values in the urn.
#This represents the expectation of one draw.
#The standard error of the one draw of a random varibale is the standard deviation of the values in the urn.
#The standard error of the sum of independent draws of a random variable is the square roor of the number of draws times standard deviation of the urn.
#Equations
#These equations apply to the case where there are only two outcomes, 𝑎 and 𝑏 with proportions 𝑝 and 1−𝑝 respectively. The general principles above also apply to random variables with more than two outcomes.
#Expected value of a random variable: 
  #𝑎𝑝+𝑏(1−𝑝)
#Expected value of the sum of n draws of a random variable: 
  #n×(𝑎𝑝+𝑏(1−𝑝))

#Standard deviation of an urn with two values: 
#|b-a|*sqrt(p*(1-p))
#Standard error of the sum of n draws of a random variable:
 # sqrt(n)*|b-a|*sqrt(p*(1-p))


### Averages and proportions
#Property 1: The expected value of the sum of random variables is the sum of the
#expected values of the individual random variables.
#Property 2: The expected vaule of a random variable times a non-random constant is
#the expected value times that non-random constant. E[aX]=a*E[X]
#Property 3: the square of the standard error of the sum of independent random
#variables is the sum of the square of the standard error of each random varible.
#Note that the square of the standard error is referred to as the variance in statisitcal
#textbooks. Variance[X]=SE[X]^2.
#Property 4: The standard error of a random varibale times a non-random constant is the
#standard error times the non-random constant. SE[aX]=a*SE[X].
#The standard error of the average of independent draws from the same urn is the standard deviation
#of the urn,S, divided by the square root of n.
#Property 5: If X is a noramlly distributed random variable, then if a and b are
#non-random constant, aX+b is also a normally distributed random variable.

#Average of multiple draws of a random variable
#The expected value of average of multiple draws from an urn is the expected value of the urn (𝜇).
#The standard deviation of the average of multiple draws from an urn is the standard deviation of the urn divided by the square root of the number of draws (𝜎/𝑛√).
#The sum of multiple draws of a random variable
#The expected value of the sum of 𝑛 draws of random variable is 𝑛 times its original expected value:
  #E[𝑛𝑋]=𝑛𝜇
#The standard error of the sum of 𝑛 draws of random variable is 𝑛√ times its original standard error:
  #SE[𝑛𝑋]=𝑛√𝜎

###Law of large numbers, or the law of averages
#When n is very large, the average of the draws converges to the average of the urn.

###How large is large in CLT (central limit theorem)
#in many sircumastances, as few as 30 draws is enough to make the CLT useful.
#NB:Note that when the probability of success is very small, we need a large sample size.
#In cases with low probability and large sample size without normal distribution on
#can use the Poisson distribution.

##Question 1 & 2
options(digits = 3)
(1*1/5)+(-0.25*4/5)
n <- 44
p <- 1/4
a <- 1
b <- 0
mu <- n*a*p + b*(1-p)
mu
mu*44
sigma <- sqrt(n) * abs(b-a) * sqrt(p*(1-p))
sigma

1-pnorm(30,11,sigma)
sum(S>8)

set.seed(21)
B <- 10000
S <- replicate(B,{
x <- sample(c(1,-0.25),44,replace=TRUE,prob=c(1/5,4/5))
sum(x)
})
mean(S>=8)

i <- seq(0.25,0.95,0.05)
sample_function <- function(h){
  mu <- n*a*h + b*(1-h)
  sigma <- sqrt(n) * abs(b-a) * sqrt(p*(1-h))
  x <- 1-pnorm(35,mu,sigma)
  return(x)
}

xx <- sapply(i,sample_function)
xx <- data.frame(i,xx)
#answer2c
p <- seq(0.25, 0.95, 0.05)
exp_val <- sapply(p, function(x){
  mu <- n * a*x + b*(1-x)
  sigma <- sqrt(n) * abs(b-a) * sqrt(x*(1-x))
  1-pnorm(35, mu, sigma)
})

min(p[which(exp_val > 0.8)])

##Qustion3
#a
p <- 5/38
a <- 6
b <- -1
mu <- a*p + b*(1-p)
mu

#b
sigma <-abs(b-a) * sqrt(p*(1-p))
sigma
#c
mu
#d
sigma/sqrt(500)
#e
mu*500
#f
sigma*sqrt(500)
#g
pnorm(0,-39.5,52.9)

###the big short: Interest rates explained
#Code:Interest rate sampling model
n <- 1000
loss_per_foreclosure <- -200000
p <- 0.02
defaults <- sample(c(0,1),n,prob=c(1-p,p),replace=TRUE)
sum(defaults*loss_per_foreclosure)
#Code:Interest rate Monte Carlo simulation
B <- 10000
losses <- replicate(B,{
  defaults <- sample(c(0,1),n,prob=c(1-p,p),replace=TRUE)
  sum(defaults*loss_per_foreclosure)
})
#Code:Plotting expected losses
library(tidyverse)
data.frame(losses_in_millions=losses/10^6) %>% 
  ggplot(aes(losses_in_millions))+
  geom_histogram(binwidth=0.6,col="black")
#Code:Expected value and standard error of the sum of 1000 loans
n*(p*loss_per_foreclosure + (1-p)*0) #expected value
sqrt(n)*abs(loss_per_foreclosure)*sqrt(p*(1-p)) #standard error
#Code:Calculating interes rate for 1% probability of losing money
l <- loss_per_foreclosure
z <- qnorm(0.01)
x <- -l*(n*p-z*sqrt(n*p*(1-p)))/(n*(1-p)+z*sqrt(n*p*(1-p)))
x  #required profit when loan is not a forclosure
x/180000  #interest rate
loss_per_foreclosure*p +x*(1-p) #expected value of the profit per loan
n*(loss_per_foreclosure*p + x*(1-p))  #expected value of the profit over n loans
#Monte Carlos simulation for 1% probability of losing money
B <- 100000
profit <- replicate(B,{
  draws <- sample(c(x,loss_per_foreclosure),n,prob=c(1-p,1),replace=TRUE)
  sum(draws)
})
mean(profit)  #expected value of the profit over n loans
mean(profit<0)  #probability of losing money

###The Big Short
#Code:Expected value with higher default rate and interest rate
p <- 0.04
loss_per_foreclosure <-  -200000
r <- 0.05
x <- r*180000
loss_per_foreclosure*p + x*(1-p)
#Code:Calculating number of loans for desired probability of losing money
z <- qnorm(0.01)
l <- loss_per_foreclosure
n <- ceiling((z^2*(x-1)^2*p*(1-p))/(1*p + x*(1-p))^2)
n #nomber of loans required
n*(loss_per_foreclosure*p + (1-p)) #expected profit over n loans

#Code:Monte Carlo simulation with default probability
B <- 10000
p <- 0.04
x <- 0.05*180000
profit <- replicate(B,{
  draws <- sample(c(x,loss_per_foreclosure),n,prob=c(1-p,1),replace=TRUE)
  sum(draws)
})
mean(profit)
#Code:Monte Carlo simulation with unknow default probability
p <- 0.04
x <- 0.05*180000
profit <- replicate(B,{
  new_p <- 0.04 + samples(seq(-0.01,0.01,length=100),1)
  draws <- sample(c(x,loss_per_foreclosure),n,prob=c(1-new_p,new_p),replace=TRUE)
  sum(draws)
})
mean(profit) #expected profit
mean(profit<0)  #probability of losing money
mean(profit< -10000000) #probability of losing over $10 milloin

#############

###PH125.4x Data Science:Interference and Modeling

##Sampling Model parameters and Estimates
#In sampeling model, the collection of elemnets in the urn is called the population
#A parameter is a number that summerizes data for an entire population
#A sample is observed data from a subset of the population
#An estimate is a summary of the observed data about a parameter that we believe is inforamtive. 
#It is a data-driven guess of the population parameter
#We want  to perdict the proportion of the blue beads in the urn, the paramater p. The proportion of red beads
#in the urn is 1-p and the spread is 2p-1.
#The sample proption is a random varible.Sampling gives random results drawn from the population distribution.

library(tidyverse)
library(dslabs)
take_poll(25)  #draw 25 beads

#Many common data science tasks can be framed as estimating a parameter form a sample.
#We illustrate statistical inference by walking through the process to estimate p. From the
#estimate of p, we can easily calculate an estimate of the spread, 2p-1.
#Consider the random variable X that is 1 if a blue bead is chosen and 0 if a red bead
#is chosen. The propotrion of blue beads i N draws is the average of the draws X1,....Xn.
#X(over score) is the sample average. In statistics, a bar on top of a symbol denotes that average.
#X(overscore) is a random variable because it is the average of random draws- each
#time we take a sample, X(overscore) is different
#The number of blue beads drawn in N draws, NX, is N times the proprion of values in the urn.
#However, we do not know the true proprtion:we are trying to estimate this parameter p.
##Polling versus forcasting
#A poll taken in advance of an election estimates p for that moment, not for the election day.
#In order to predict election results, forcasters try to use early estimates of p to predcit
#p on elction day.
##Properties of our estimate
#When interpreting values of 𝑋¯, it is important to remember that 𝑋¯ is a random variable
#with an expected value and standard error that represents the sample proportion of positive events.
#The expected value of 𝑋¯is the parameter of interest p. This followsfrom the fact that𝑋¯ 
#is the sum of independent draws of the random variable times the constant 1/N.
#      E(𝑋¯ =p)
#As the number of draws N increases, the standard error of our estimates 𝑋¯ decreses.
#The standard error of the average of 𝑋¯ over N draws is:
#  SE(𝑋¯)=sqrtp(1-p)/N
#In theory, we can get more accurate estimates of p increasing N. In pratice, there are
#limits on the size of N due to cost, as well as other factors we discuss later.
#We can also use other random, variable equations to determone the expected value of the sum
#of draws E(S) and standard error of the sum of draws SE(S).
#     E(S)=Np
#     SE(S)=sqrt(Np(1-p))
##Central limit theorem in practice
#Becasue 𝑋¯is the sum of random draws divided by a constant, the distribution of 𝑋¯
#is approximately normal.
#We can convert 𝑋¯to a standrad normal variable Z.
#   Z=𝑋¯-E(𝑋¯)/SE(𝑋¯)
#The probability that 𝑋¯is within 0.01 of the actual value of p is:
#  Pr(Z≤0.01/sqrt(p(1-p)/N))-Pr(Z≤-0.01/sqrt(p(1-p)/N))

##Confidence intervals
#Code: Monte Carlo simulation of CI
#Note that to compute the exact 95% CI, we would use qnorm(0,975)*SE_hat insted of 2*SE_hat

p <- 0.45
N <- 1000
X <- sample(c(0,2),size=N,replace=TRUE,prob=c(1-p,p)) #generate N obesrvations
X_hat <- mean(X)  #calculate X_hat
SE_hat <- sqrt(X_hat*(1-X_hat)/N) #calculate SE_hat, SE of the mean of N obseravtions
c(X_hat-2*SE_hat,X_hat+2*SE_hat)  #build interval of 2*SE above and below mean

#Code:Solving for z with qnorm
z <- qnorm(0.995) #calculate z to solve for 99% CI
pnorm(qnorm(0.995)) #demonstrate that qnorm give the z value for a given probalility
pnorm(qnorm(1-0.995)) #demostrating symmetry of 1-qnorm
pnorm(z) - pnorm(-z)  #demostrating that this z value gives correct probalility for interval

#Code: Monte Carlo simulation
B <- 10000
inside <- replicate(B, {
  X <- sample(c(0,1), size=N,replace=TRUE,prob=c(1-p,p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  between(p,X_hat-2*SE_hat,X_hat+2*SE_hat) #TRUE if p in CI
})
mean(inside)

##Power
#if we are trying to predict the results of an election, then a CI that includes a spread of 0 (a tie) is not helpful
#A CI that includes a spread of 0 does not implya close election, it means that the sample size is to small
#Power is the probability of detecting an effect when there is a true effect to find. The power increases as
#sample size increases, because larger sample size means smaller standard error

#Code:CI for the spread with sample size of 25
N <- 25
X_hat <- 0.48
(2*X_hat-1) + c(-2,2)*2*sqrt(X_hat*(1-X_hat)/sqrt(N))
(2*X_hat-1) + c(-2,2)*2*sqrt(X_hat*(1-X_hat)/N)  #??? usikker på sqrt(N)

##p-value
#The H0 is the hypothesis that there is no effect. In this case, the H0 is that the spread is 0, or p=0.5-
#The p-value is that probability of detecting an effect of a cretain size or larger when the H0 is true.
#We can convert the probability of seeing an observed value under the H0 into a strandard normal random variable.
#we compute the value of z that corresonds to the observed results, and then use that z to compute the p-value.
#If a 95% CI does not include our observed value, then the p.value must be smaller than 0.05.
#It is preferable to report CI instead of p-values, as CI give information about hte size of the
#estimate and p-values do not.

#Code:Computing a p-value for observed spread of 0.02
N <- 100  #sample size
z <- sqrt(N)* 0.02/0.5  #spread of 0.02
1 - (pnorm(z) - pnorm(-z))

##Poll aggregators
#Poll aggregators combine the results of many polls to simulate poll with a large sample size and therefore
#generate more precise estimates than individual polls.
#Polls can be simulated with a Monto Carlo simulation and used to construct an estimate of the spread and CI.
#The actual data science exercise of forcasting elections involoves more complex statistical modelling,
#but these underlying ideas still apply

#Code:Simulating polls
#Note that to compute the exact 95% CI,we would use qnorm(0.975)*SE_hat instead of 2*SE_hat
d <- 0.039
Ns <- c(1298,533,1342,897,774,254,812,324,1291,1056,2172,516)
p <- (d+1)/2
#Calculate CI of the spread
confidence_intervals <- sapply(Ns, function(N) {
  X <- sample(c(0,1), size=N,replace=TRUE, prob=c(1-p,p))
  X_hat <- mean(X)
  SE_hat <- sqrt(X_hat*(1-X_hat)/N)
  2*c(X_hat ,X_hat-2*SE_hat,X_hat+2*SE_hat)-1
}) 
#generate a data frame storing results
polls <- data.frame(poll=1:ncol(confidence_intervals),
                    t(confidence_intervals),sample_size=Ns)
names(polls) <- c("poll","estimate","low","high","sample_size")
polls
#Code:Calculating the spread of combined polls
#Note that to compute the exact 95% CI,we would use qnorm(0.975)*SE_hat instead of 2*SE_hat
d_hat <- polls %>% 
  summarize(avg=sum(estimate*sample_size)/sum(sample_size)) %>% 
  .$avg
p_hat <- (1+d_hat)/2
moe <- 2*1.96*sqrt(p_hat*(1-p_hat)/sum(polls$sample_size))
moe
round(d_hat*100,1)
round(moe*100,1)

##Poll data and pollster bias
#Code:Generating simulated poll data
library(dslabs)
data(polls_us_election_2016)
names(polls_us_election_2016)

#keep only national polls from the week before the election with a grade considered reliable
polls <- polls_us_election_2016 %>% 
  filter(state=="U.S." & enddate>="2016-10-31" & (grade %in% c("A+", "A","A-","B+") | is.na(grade)))

#add spread estimate
polls <- polls %>% 
  mutate(spread=rawpoll_clinton/100-rawpoll_trump/100)

#commpute estimated spread for combined polls
d_hat <- polls %>% 
  summarize(d_hat=sum(spread*samplesize)/sum(samplesize)) %>% 
  .$d_hat

#compute margin of error
p_hat <- (d_hat+1)/2
moe <- 1.96*2*sqrt(p_hat*(1-p_hat)/sum(polls$samplesize))

#histogram of the spread
polls %>% 
  ggplot(aes(spread))+
  geom_histogram(color="black", binwidth=0.01)

#Code:investigating poll data and pollster
#number of polls per pollster in week before election
polls %>% group_by(pollster) %>% 
  filter(n()>=6) %>% 
  ggplot(aes(pollster,spread))+
  geom_point()+
  theme(axis.text.x=element_text(angle=90,hjust=1))

#standard error within each pollster
polls %>% group_by(pollster) %>% 
  filter(n()>=6) %>% 
  summarize(se=2*sqrt(p_hat*(1-p_hat)/median(samplesize)))

##Data driven models
#Collect last result before the election for each pollster
one_poll_per_pollster <- polls %>% group_by(pollster) %>% 
  filter(enddate==max(enddate)) %>%   #keep latest poll
  ungroup()
#histogram of spread estimates
results <- one_poll_per_pollster %>% 
  summarise(avg=mean(spread), se=sd(spread)/sqrt(length(spread))) %>% 
  mutate(start=avg-1.96*se,end=avg+1.96*se)
round(results*100,1)


##Bayes´Theorem
#Bayes´Theorem states that the probability of event A happening given event B is equal to the probability
#of both A and B divided by the probability of event B

#Code:Monte Carlo simulation of Cystic fibrosis test probability
prev <- 0.00025 #disease prevelance
N <- 100000 #number of tests
outcome <- sample(c("Disease","Healthy"),N,replace=TRUE,prob=c(prev,1-prev))
N_D <- sum(outcome=="Disease")  #nnumber with the disease
N_H <- sum(outcome=="Healthy")  #number healthy

#for each person, randomly determine if test is + or -
accuracy <- 0.99
test <- vector("character",N)
test[outcome=="Disease"] <- sample(c("+","-"),N_D,replace=TRUE,prob=c(accuracy,1-accuracy))
test[outcome=="Healthy"] <- sample(c("+","-"),N_H,replace=TRUE,prob=c(accuracy,1-accuracy))
table(outcome,test)

###Finding a missing airplane
Pr_A <- 0.2
Pr_B <- 0.6
Pr_C<-0.15
Pr_D <- 0.05
Pr_notFound_inarea <- 0.1
Pr_notFound_diffarea <-1
options(digits=3)
#Q1
Pr_notFound_inarea*0.5/Pr_A

##Election forcasting
polls <- polls_us_election_2016 %>% 
  filter(state=="U.S." & enddate>="2016-10-31" & (grade %in% c("A+", "A","A-","B+") | is.na(grade))) %>% 
  mutate(spread=rawpoll_clinton/100-rawpoll_trump/100)
one_poll_per_pollster <- polls %>% group_by(pollster) %>% 
  filter(enddate==max(enddate)) %>%   #kee lastet poll
  ungroup()
results <- one_poll_per_pollster %>% 
  summarise(avg=mean(spread), se=sd(spread)/sqrt(length(spread))) %>% 
  mutate(start=avg-1.96*se,end=avg+1.96*se)
#Code:Computing the posterior mean, standard error, cerdible interval and probability
mu <- 0
tau <- 0.035
sigma <- results$se
Y <- results$avg
B <- sigma^2/(sigma^2+tau^2)

posterior_mean <- B*mu+(1-B)*Y
posterior_se <- sqrt(1/(1/sigma^2+ 1/tau^2))

posterior_mean
posterior_se

#95% credible interval
posterior_mean+c(-1.96,1.96)*posterior_se
#probability of d > 0
1-pnorm(0,posterior_mean,posterior_se)

###Mathematical representations of models
#Code:simulated data with Xj=d+ej
J <- 6
N <- 2000
d <- 0.021
p <- (d+1)/2
X <- d +  rnorm(J,0,2*sqrt(p*(1-p/N)))

#Code:simulated data with Xi,j=d+ei,j
I <- 5
J <- 6
N <- 2000
d <- 0.021
p <- (d+1)/2
X <- sapply(1:I, function(i){
  d + rnorm(J,0,2*sqrt(p*(1-p/N)))
})

#Code:simulated data for Xi,j=d + hi +ei,j
I <- 5
J <- 6
N <- 2000
d <- 0.021
p <- (d+1)/2
h <- rnorm(I,0,0.025) #assume standard error of pollster-to-pollster variability is 0.025
X <- sapply(1:I, function(i){
  d + rnorm(J,0,2*sqrt(p*(1-p/N)))
})

#Code:Calculating probability of d>0 wiht general bias
#Note that sigma now includes an estimate of the variability due to general bias alfa_b=0.025
mu <- 0
tau <- 0.035
sigma <- sqrt(results$se^2 + 0.025^2)
Y <- results$avg
B <- sigma^2/(sigma^2 + tau^2)

posterior_mean <- B*mu +(1-B)*Y
posterior_se <- sqrt(1/(1/sigma^2 + tau^2))

1-pnorm(0,posterior_mean,posterior_se)

###Predicting the electoral college

#Code:Top 5 states ranked by electoral votes

results_us_election_2016  %>% arrange(desc(electoral_votes)) %>% top_n(5,electoral_votes)

#Code:Computing the average and standard deviation for each state
results <- polls_us_election_2016 %>% 
  filter(state !="U.S." & !grepl("CD","state") &
  enddate >= "2016-10-31" & (grade %in% c("A+", "A", "A-", "B+") | is.na(grade))) %>% 
  mutate(spread=rawpoll_clinton/100 - rawpoll_trump/100) %>% 
  group_by(state) %>% 
  summarize(avg=mean(spread), sd=sd(spread), n=n()) %>% 
  mutate(state=as.character(state))

#10 closest races = battle states
results %>% arrange(abs(avg))

#join electrol college votes and results
results <-  left_join(results,results_us_election_2016, by="state")

#states with no polls: note Rhode Island and District of Columbia = Democrat
results_us_election_2016 %>% filter(!state %in% results$state)

#assigns sd to state with just one poll as median of other sd values
results <- results %>% 
  mutate(sd=ifelse(is.na(sd), median(results$sd, na.rm=TRUE),sd))

#Code: calculate the posterior mean and standard error
mu <- 0
tau <- 0.02
results %>% mutate(sigma=sd/sqrt(n),
B=sigma^2/(sigma^2 + tau^2),
posterior_mean=B*mu + (1-B)*avg,
posterior_se= sqrt(1/(1/sigma^2 +1/tau^2))) %>% 
  arrange(abs(posterior_mean))

#Code: Monte Carlo simulation of election Night results ( no general bias)
mu <- 0
tau <- 0.02
clinton_EV <- replicate(1000, {
  results %>% mutate(sigma = sd/sqrt(n),
        B = sigma^2/ (sigma^2 + tau^2),
        posterior_mean = B*mu + (1-B)*avg,
        posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
        simulated_result=rnorm(length(posterior_mean),posterior_mean,posterior_se),
        clinton= ifelse(simulated_result > 0, electoral_votes,0)) %>% #award votes if clinton wins state
    summarize(clinton=sum(clinton)) %>%  #total votes for clinton
    pull(clinton) + 7 #7 votes for Rhode Island and DC
})
mean(clinton_EV > 269) # over 269 votes wins election

#histogram of outcomes
data.frame(clinton_EV) %>% 
  ggplot(aes(clinton_EV))+
  geom_histogram(binwidth=1)+
  geom_vline(xintercept=269)

#Code: Monte Carlo simulation including general bias
mu <- 0
tau <- 0.02
bias_sd <- 0.03
clinton_EV_2 <- replicate(1000, {
  results %>% mutate(sigma = sqrt(sd^2/(n) + bias_sd^2),    # added bias_sd term
                     B = sigma^2/ (sigma^2 + tau^2),
                     posterior_mean = B*mu + (1-B)*avg,
                     posterior_se = sqrt( 1 / (1/sigma^2 + 1/tau^2)),
                     simulated_result = rnorm(length(posterior_mean), posterior_mean, posterior_se),
                     clinton = ifelse(simulated_result > 0, electoral_votes, 0)) %>%    # award votes if Clinton wins state
    summarize(clinton = sum(clinton)) %>%    # total votes for Clinton
    .$clinton + 7    # 7 votes for Rhode Island and DC
})
mean(clinton_EV_2 > 269)    # over 269 votes wins election

##Forcasting
Code: Variability across one pollster

# select all national polls by one pollster
one_pollster <- polls_us_election_2016 %>%
  filter(pollster == "Ipsos" & state == "U.S.") %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100)

# the observed standard error is higher than theory predicts
se <- one_pollster %>%
  summarize(empirical = sd(spread),
            theoretical = 2*sqrt(mean(spread)*(1-mean(spread))/min(samplesize)))
se

# the distribution of the data is not normal
one_pollster %>% ggplot(aes(spread)) +
  geom_histogram(binwidth = 0.01, color = "black")

#Code: Trend across time for several pollsters

polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  mutate(spread = rawpoll_clinton/100 - rawpoll_trump/100) %>%
  ggplot(aes(enddate, spread)) +
  geom_smooth(method = "loess", span = 0.1) +
  geom_point(aes(color = pollster), show.legend = FALSE, alpha = 0.6)

#Code: Plotting raw percentages across time

polls_us_election_2016 %>%
  filter(state == "U.S." & enddate >= "2016-07-01") %>%
  select(enddate, pollster, rawpoll_clinton, rawpoll_trump) %>%
  rename(Clinton = rawpoll_clinton, Trump = rawpoll_trump) %>%
  gather(candidate, percentage, -enddate, -pollster) %>%
  mutate(candidate = factor(candidate, levels = c("Trump", "Clinton"))) %>%
  group_by(pollster) %>%
  filter(n() >= 10) %>%
  ungroup() %>%
  ggplot(aes(enddate, percentage, color = candidate)) +
  geom_point(show.legend = FALSE, alpha = 0.4) +
  geom_smooth(method = "loess", span = 0.15) +
  scale_y_continuous(limits = c(30, 50))

###The t-Distribution
#Code:Calculating 95% CI with the t-distribution
#Given s as an estimate of sigma, the Z=X-d/s/sqrt(N) follows a t-distribution with N-1 degrees of freedom.
#We can determine CI using the t-distribution instead of the normal distribution by calculating the desired quantile
#with the function qt

z <- qt(0.975,nrow(one_poll_per_pollster)-1)
one_poll_per_pollster %>% 
  summarize(avg=mean(spread),moe=z*sd(spread)/sqrt(length(spread))) %>% 
  mutate(start=avg-moe,end=avg+moe)

#quantile from t-distribution versus normal distribution
qt(0.975,14)  #14=nrow(one_poll_per_pollster) - 1
qnorm(0.975)

###Association tests
#The Fisher test
#Two-by-two table

#Code: Research funding rates example

# load and inspect research funding rates object
library(tidyverse)
library(dslabs)
data(research_funding_rates)
research_funding_rates

# compute totals that were successful or not successful
totals <- research_funding_rates %>%
  select(-discipline) %>%
  summarize_all(funs(sum)) %>%
  summarize(yes_men = awards_men,
            no_men = applications_men - awards_men,
            yes_women = awards_women,
            no_women = applications_women - awards_women)

# compare percentage of men/women with awards
totals %>% summarize(percent_men = yes_men/(yes_men + no_men),
                     percent_women = yes_women/(yes_women + no_women))

#Code: Two-by-two table and p-value for the Lady Tasting Tea problem

tab <- matrix(c(3,1,1,3), 2, 2)
rownames(tab) <- c("Poured Before", "Poured After")
colnames(tab) <- c("Guessed Before", "Guessed After")
tab

# p-value calculation with Fisher's Exact Test
fisher.test(tab, alternative = "greater")

###Chi-square test
#Code: Chi-squared test

# compute overall funding rate
funding_rate <- totals %>%
  summarize(percent_total = (yes_men + yes_women) / (yes_men + no_men + yes_women + no_women)) %>%
  .$percent_total
funding_rate

# construct two-by-two table for observed data
two_by_two <- tibble(awarded = c("no", "yes"),
                     men = c(totals$no_men, totals$yes_men),
                     women = c(totals$no_women, totals$yes_women))
two_by_two

# compute null hypothesis two-by-two table
tibble(awarded = c("no", "yes"),
       men = (totals$no_men + totals$yes_men) * c(1-funding_rate, funding_rate),
       women = (totals$no_women + totals$yes_women) * c(1-funding_rate, funding_rate))

# chi-squared test
chisq_test <- two_by_two %>%
  select(-awarded) %>%
  chisq.test()
chisq_test$p.value

#Code: Odds ratio

# odds of getting funding for men
odds_men <- (two_by_two$men[2] / sum(two_by_two$men)) /
  (two_by_two$men[1] / sum(two_by_two$men))

# odds of getting funding for women
odds_women <- (two_by_two$women[2] / sum(two_by_two$women)) /
  (two_by_two$women[1] / sum(two_by_two$women))

# odds ratio - how many times larger odds are for men than women
odds_men/odds_women

#Code: p-value and odds ratio responses to increasing sample size

# multiplying all observations by 10 decreases p-value without changing odds ratio
two_by_two %>%
  select(-awarded) %>%
  mutate(men = men*10, women = women*10) %>%
  chisq.test()


########################

#######  
#  Data Science:Productivity tools
# HarvardX:PH125.5x
#######


