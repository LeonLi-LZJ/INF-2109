library(tidyverse)
library(psych)
library(broom)
library(gridExtra)
library(scales)


### Import the Data ###
yrd_1 <- read.csv("https://raw.githubusercontent.com/LeonLi-LZJ/INF-2178/master/YDR_statistical_data_2004-2018.csv",header = T) # read the csv document
yrd_1 <- filter(yrd_1,Year>2006 & Year<2015) # select data from 2007 to 2014
colnames(yrd_1) # check the column names of the dataset

# change the type of Treatment variable in to factor
yrd_1$Treatment <- as.factor(yrd_1$Treatment)
class(yrd_1$Treatment)


### Descriptive statistics ###
# "describe" fuction tells about the mean,median, 25th, 75th quartiles,
# min and max of continuous variables.
describe(yrd_1)

# we can also use "skim" in "skimr" package to take a brief look.
skimr::skim(yrd_1)

# collect city data in Treatment == 1 and Treatment == 0
city_data <- yrd_1 %>% select(ï»¿City, Treatment)
city_1 <- filter(city_data, Treatment == 1)
city_0 <- filter(city_data, Treatment == 0)

# collect city data by Treatment and Time, to analysis the descriptive features of denpendent variables
city_data_1 <- yrd_1 %>% select(ï»¿City, Treatment,GDPpercapita,GDPPCRate,FAI,IS,NOE,RSCG)
city_1 <- filter(city_data_1, Treatment == 1)
city_0 <- filter(city_data_1, Treatment == 0)
describe(city_1)
describe(city_0)

# Some nice plots - Distribution 
a <- ggplot(yrd_1,aes(GDPPCRate,fill=Treatment))+
  labs(x="Growth Rate of GDP Per Capita")+
  scale_fill_discrete(name="Cities",labels=c("No HSR","HSR"))+
  scale_x_continuous(labels = dollar_format(prefix = "",suffix="%"))+
  geom_density()
b <- ggplot(yrd_1,aes(GDPpercapita,fill=Treatment))+
  labs(x="GDP Per Capita")+
  scale_fill_discrete(name="Cities",labels=c("No HSR","HSR"))+
  scale_x_continuous(labels = dollar_format(prefix = "£¤",suffix="T",scale = 0.001))+
  geom_density()
c <- ggplot(yrd_1,aes(IS,fill=Treatment))+
  labs(x="Industry Structure (IS)")+
  scale_fill_discrete(name="Cities",labels=c("No HSR","HSR"))+
  geom_density()
d <- ggplot(yrd_1,aes(FAI,fill=Treatment))+
  labs(x="Fixed-Assest Investment (FAI)")+
  scale_fill_discrete(name="Cities",labels=c("No HSR","HSR"))+
  scale_x_continuous(labels = 
                       dollar_format(prefix ="£¤",suffix="B",scale=0.1))+
  geom_density()
e <- ggplot(yrd_1,aes(NOE,fill=Treatment))+
  labs(x="Numer of Employees (NOE)")+
  scale_fill_discrete(name="Cities",labels=c("No HSR","HSR"))+
  scale_x_continuous(labels = 
                       dollar_format(prefix ="",suffix="M",scale=0.01))+
  geom_density() 
f <- ggplot(yrd_1,aes(RSCG,fill=Treatment))+
  labs(x="Retal Sales of Consumer Goods (RSCG)")+
  scale_fill_discrete(name="Cities",labels=c("No HSR","HSR"))+
  scale_x_continuous(labels = 
                       dollar_format(prefix ="£¤",suffix="B",scale=0.1))+
  geom_density()
grid.arrange(b,a,c,d,f,e,ncol=3)

###Some nice plots - Time Series
b <- yrd_1 %>%
  mutate(type=if_else(Treatment == 1, "City along HSR","City not along HSR"))%>%
  ggplot(aes(x=Year,y=GDPPCRate))+
  geom_point(alpha=0.5,size=1.5)+
  scale_y_continuous(labels = dollar_format(prefix="",suffix = "%"))+
  labs(x="Year",y="Growth Rate of GDP Per Capita")+
  facet_wrap(vars(type),nrow = 2)+
  theme_light()+
  geom_vline(xintercept = 2010.5, linetype = "dashed")

a <- yrd_1 %>%
  mutate(type=if_else(Treatment == 1,  "City along HSR","City not along HSR"))%>%
  ggplot(aes(x=Year,y=GDPpercapita))+
  geom_point(alpha=0.5,size=1.5)+
  scale_y_continuous(labels = dollar_format(prefix = "£¤",suffix="T",scale = 0.001))+
  labs(x="Year",y="GDP Per Capita")+
  facet_wrap(vars(type),nrow = 2)+
  theme_light()+
  geom_vline(xintercept = 2010.5, linetype = "dashed")

c<- yrd_1 %>%
  mutate(type=if_else(Treatment == 1,  "City along HSR","City not along HSR"))%>%
  ggplot(aes(x=Year,y=IS))+
  geom_point(alpha=0.5,size=1.5)+
  scale_y_continuous()+
  labs(x="Year",y="Industry Structure (IS)")+
  facet_wrap(vars(type),nrow = 2)+
  theme_light()+
  geom_vline(xintercept = 2010.5, linetype = "dashed")


d<- yrd_1 %>%
  mutate(type=if_else(Treatment == 1,  "City along HSR","City not along HSR"))%>%
  ggplot(aes(x=Year,y=FAI))+
  geom_point(alpha=0.5,size=1.5)+
  scale_y_continuous(labels = 
                       dollar_format(prefix ="£¤",suffix="B",scale=0.1))+
  labs(x="Year",y="Fixed-Assets Investment (FAI)")+
  facet_wrap(vars(type),nrow = 2)+
  theme_light()+
  geom_vline(xintercept = 2010.5, linetype = "dashed")

e <- yrd_1 %>%
  mutate(type=if_else(Treatment == 1,  "City along HSR","City not along HSR"))%>%
  ggplot(aes(x=Year,y=RSCG))+
  geom_point(alpha=0.5,size=1.5)+
  scale_y_continuous(labels = 
                       dollar_format(prefix ="£¤",suffix="B",scale=0.1))+
  labs(x="Year",y="Retal Sales of Consumer Goods (RSCG)")+
  facet_wrap(vars(type),nrow = 2)+
  theme_light()+
  geom_vline(xintercept = 2010.5, linetype = "dashed")

f <- yrd_1 %>%
  mutate(type=if_else(Treatment == 1,  "City along HSR","City not along HSR"))%>%
  ggplot(aes(x=Year,y=NOE))+
  geom_point(alpha=0.5,size=1.5)+
  scale_y_continuous(labels = 
                       dollar_format(prefix ="",suffix="M",scale=0.01))+
  labs(x="Year",y="Numer of Employees (NOE)")+
  facet_wrap(vars(type),nrow = 2)+
  theme_light()+
  geom_vline(xintercept = 2010.5, linetype = "dashed")

grid.arrange(a,b,c,d,e,f,ncol=3)

### T test ###
# collect data from GDPPCRate in treatment == 1
Rate_group1 <- 
  yrd_1 %>% 
  filter(Treatment == 1) %>% 
  select(GDPPCRate) %>% 
  as.vector() %>% 
  unlist()

# collect data from GDPPCRate in treatment == 0
Rate_group2 <- 
  yrd_1 %>% 
  filter(Treatment == 0) %>% 
  select(GDPPCRate) %>% 
  as.vector() %>% 
  unlist()

# conduct t-test with GDPPCRate in two treatment group
t.test(Rate_group1,Rate_group2)

# collect data from GDPpercapita in treatment == 1
PC_group1 <-
  yrd_1 %>% 
  filter(Treatment == 1) %>% 
  select(GDPpercapita) %>% 
  as.vector() %>% 
  unlist()

# collect data from GDPpercapita in treatment == 0
PC_group2 <-
  yrd_1 %>% 
  filter(Treatment == 0) %>% 
  select(GDPpercapita) %>% 
  as.vector() %>% 
  unlist()

# conduct t-test with GDPpercapita in two treatment group
t.test(PC_group1,PC_group2)

# T-test about GDPPCRate in Year 2017, 2011, 2014
# collect data from GDPPCRate in treatment == 1 in Year 2007
Rate_group1_2007 <- 
  yrd_1 %>% 
  filter(Treatment == 1) %>% 
  filter(Year == 2007) %>%
  select(GDPPCRate) %>% 
  as.vector() %>% 
  unlist()

# collect data from GDPPCRate in treatment == 0 in Year 2007
Rate_group2_2007 <- 
  yrd_1 %>% 
  filter(Treatment == 0) %>% 
  filter(Year == 2007) %>%
  select(GDPPCRate) %>% 
  as.vector() %>% 
  unlist()

# conduct t-test with GDPPCRate in two treatment group in Year 2007
t.test(Rate_group1_2007,Rate_group2_2007)

# collect data from GDPPCRate in treatment == 1 in Year 2011
Rate_group1_2011 <- 
  yrd_1 %>% 
  filter(Treatment == 1) %>% 
  filter(Year == 2011) %>%
  select(GDPPCRate) %>% 
  as.vector() %>% 
  unlist()

# collect data from GDPPCRate in treatment == 0 in Year 2011
Rate_group2_2011 <- 
  yrd_1 %>% 
  filter(Treatment == 0) %>% 
  filter(Year == 2011) %>%
  select(GDPPCRate) %>% 
  as.vector() %>% 
  unlist()

# conduct t-test with GDPPCRate in two treatment group in Year 2011
t.test(Rate_group1_2011,Rate_group2_2011)


# collect data from GDPPCRate in treatment == 1 in Year 2014
Rate_group1_2014 <- 
  yrd_1 %>% 
  filter(Treatment == 1) %>% 
  filter(Year == 2014) %>%
  select(GDPPCRate) %>% 
  as.vector() %>% 
  unlist()

# collect data from GDPPCRate in treatment == 0 in Year 2014
Rate_group2_2014 <- 
  yrd_1 %>% 
  filter(Treatment == 0) %>% 
  filter(Year == 2014) %>%
  select(GDPPCRate) %>% 
  as.vector() %>% 
  unlist()

# conduct t-test with GDPPCRate in two treatment group in Year 2014
t.test(Rate_group1_2014,Rate_group2_2014)

# T-test about GDPpercapita in Year 2017, 2011, 2014
# collect data from GDPpercapita in treatment == 1 in Year 2007
PC_group1_2007 <-
  yrd_1 %>% 
  filter(Treatment == 1) %>% 
  filter(Year == 2007) %>%
  select(GDPpercapita) %>% 
  as.vector() %>% 
  unlist()

# collect data from GDPpercapita in treatment == 0 in Year 2007
PC_group2_2007 <-
  yrd_1 %>% 
  filter(Treatment == 0) %>% 
  filter(Year == 2007) %>%
  select(GDPpercapita) %>% 
  as.vector() %>% 
  unlist()

# conduct t-test with GDPpercapita in two treatment group in Year 2007
t.test(PC_group1_2007,PC_group2_2007)

# collect data from GDPpercapita in treatment == 1 in Year 2011
PC_group1_2011 <-
  yrd_1 %>% 
  filter(Treatment == 1) %>% 
  filter(Year == 2011) %>%
  select(GDPpercapita) %>% 
  as.vector() %>% 
  unlist()

# collect data from GDPpercapita in treatment == 0 in Year 2011
PC_group2_2011 <-
  yrd_1 %>% 
  filter(Treatment == 0) %>% 
  filter(Year == 2011) %>%
  select(GDPpercapita) %>% 
  as.vector() %>% 
  unlist()

# conduct t-test with GDPpercapita in two treatment group in Year 2011
t.test(PC_group1_2011,PC_group2_2011)

# collect data from GDPpercapita in treatment == 1 in Year 2014
PC_group1_2014 <-
  yrd_1 %>% 
  filter(Treatment == 1) %>% 
  filter(Year == 2014) %>%
  select(GDPpercapita) %>% 
  as.vector() %>% 
  unlist()

# collect data from GDPpercapita in treatment == 0 in Year 2014
PC_group2_2014 <-
  yrd_1 %>% 
  filter(Treatment == 0) %>% 
  filter(Year == 2014) %>%
  select(GDPpercapita) %>% 
  as.vector() %>% 
  unlist()

# conduct t-test with GDPpercapita in two treatment group in Year 2014
t.test(PC_group1_2014,PC_group2_2014)



### DID Model ###
# without control variable
did_yrd <- lm(GDPPCRate~Treatment*Time,
              data=yrd_1)
summary(did_yrd)

did_yrd <- lm(GDPpercapita~Treatment*Time ,
              data=yrd_1)
summary(did_yrd)

###Parallel Trend###
## Coefficients and Confidence Interval ##
yrd_1$Year <- as.factor(yrd_1$Year)
par_test <- lm(GDPPCRate~Year*Treatment+ï»¿City+Year+IS+FAI+NOE+RSCG,
               data=yrd_1) #model with interaction between year and HSR as a variable
summary(par_test) #mode; summary

coef <- data.frame(coefficients(par_test)) #collect coeffcients
con_i <- as.data.frame(confint.lm(par_test,level = 0.95)) #retrieve confidence interval
coef_coni <- cbind(coef,con_i) #bind coeffiicients and their confidence interval
coef_coni <- coef_coni[32:38,] # drop unnecessary data
row <- c(0,0,0) # add 0 as the value of the year of 2007
coef_coni <- rbind(row,coef_coni)
row.names(coef_coni) <- c("2007*HSR","2008*HSR","2009*HSR",
                          "2010*HSR","2011*HSR","2012*HSR",
                          "2013*HSR","2014*HSR") # alter the row names
colnames(coef_coni) <- c("coef","low","high") # alter the column names

# repeat
par_test <- lm(GDPpercapita~Year*Treatment+ï»¿City+Year+IS+FAI+NOE+RSCG,
               data=yrd_1)
summary(par_test)
coef <- data.frame(coefficients(par_test))
con_i <- as.data.frame(confint.lm(par_test,level = 0.95))
coef_coni_1 <- cbind(coef,con_i)
coef_coni_1 <- coef_coni_1[32:38,]
row <- c(0,0,0)
coef_coni_1 <- rbind(row,coef_coni_1)
row.names(coef_coni_1) <- c("2007*HSR","2008*HSR","2009*HSR",
                            "2010*HSR","2011*HSR","2012*HSR",
                            "2013*HSR","2014*HSR")
colnames(coef_coni_1) <- c("coef","low","high")

#nice coefficient and confidence interval plot
g<-ggplot(coef_coni,aes(x=row.names(coef_coni_1),y=coef,
                        ymin=low,ymax=high)) +
  geom_pointrange(color = "blue",size=1)+
  geom_hline(yintercept=0, color = "red",size=1)+
  theme_light()+
  labs(x="Growth Rate of the GDP Per Capita",y="coefficient value")+
  coord_cartesian(ylim = c(-15,15))+
  theme(axis.text.x = element_text(angle = 40,hjust = 1))

h<-ggplot(coef_coni_1,aes(x=row.names(coef_coni_1),y=coef,
                          ymin=low,ymax=high)) +
  geom_pointrange(color = "blue",size=1)+
  geom_hline(yintercept=0, color = "red",size=1)+
  theme_light()+
  labs(x="GDP Per Capita",y="coefficient value")+
  coord_cartesian(ylim = c(-23000,23000))+
  theme(axis.text.x = element_text(angle = 40,hjust = 1))
grid.arrange(g,h,ncol=2)

## visual inspection ##
GDPPC <- group_by(yrd_1,Treatment,Year) %>% 
  summarise(n=n(),
            mean_GDPPC = mean(GDPpercapita),
            mean_GDPPCRate = mean(GDPPCRate))

I <- ggplot(data=GDPPC,aes(x=Year,y=mean_GDPPC,color = Treatment))+
  geom_point(size=1.5)+
  geom_line(aes(group=Treatment),size=1) +
  geom_vline(xintercept = 2010.5,linetype = "dashed")+
  labs(x = "Year", y = "Mean GDP Per Capita") +
  scale_y_continuous(labels = dollar_format(prefix="£¤"))+
  theme_light()+
  scale_fill_discrete(name="Group",labels=c("Control","Treatment"))+
  scale_color_brewer(palette="Set1")

J <- ggplot(data=GDPPC,aes(x=Year,y=mean_GDPPCRate,color = Treatment))+
  geom_point(size=1.5)+
  geom_line(aes(group=Treatment),size=1) +
  geom_vline(xintercept = 2010.5,linetype = "dashed")+
  theme_light()+
  scale_fill_discrete(name="Group",labels=c("Control","Treatment"))+
  labs(x = "Year", y = "Mean GDP Per Capita Increase Rate") +
  scale_color_brewer(palette="Set1")

grid.arrange(I,J,ncol=2)



#####Robustness#####
#Drop Ningbo
rob_1 <- filter(yrd_1,!ï»¿City == "Ningbo")
did_yrd <- lm(GDPPCRate~Treatment*Time,
              data=rob_1)
summary(did_yrd)

did_yrd <- lm(GDPpercapita~Treatment*Time,
              data=rob_1)
summary(did_yrd)

#Introduce Control Variables

did_yrd <- lm(GDPPCRate~Treatment*Time+ ï»¿City+IS+FAI+NOE+RSCG,
              data=yrd_1)
summary(did_yrd)

did_yrd <- lm(GDPpercapita~Treatment*Time+ ï»¿City+FAI+NOE+RSCG+IS,
              data=yrd_1)
summary(did_yrd)







