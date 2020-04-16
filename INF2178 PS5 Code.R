library(tidyverse)
library(haven)
library(here)
library(gridExtra)
library(rdrobust)
library(psych)
library(Hmisc)


df_0 <- read_dta("data_for_analysis.dta")

df <- mutate(df_0,probation = ifelse(dist_from_cut<0,1,0))


df <- df[abs(df$dist_from_cut)<1.2,]# limit the sample to students within 1 grade points of their academic probation cutoff
df <- select(df,GPA_year1,GPA_year2,probation_year1,dist_from_cut,
             year2_dist_from_cut,sex,mtongue,age_at_entry,left_school)

# Data Description
Hmisc::describe(as.factor(df$left_school))
Hmisc::describe(as.factor(df[df$sex=="M",]$left_school))
Hmisc::describe(as.factor(df[df$sex=="F",]$left_school))
Hmisc::describe(as.factor(df[df$mtongue=="English",]$left_school))
Hmisc::describe(as.factor(df[df$mtongue!="English",]$left_school))

Hmisc::describe(as.factor(df$probation_year1))
Hmisc::describe(as.factor(df[df$sex=="M",]$probation_year1))
Hmisc::describe(as.factor(df[df$sex=="F",]$probation_year1))
Hmisc::describe(as.factor(df[df$mtongue=="English",]$probation_year1))
Hmisc::describe(as.factor(df[df$mtongue!="English",]$probation_year1))

describe(df$gpaimprove)
describe(df[df$sex=="M",]$gpaimprove)
describe(df[df$sex=="F",]$gpaimprove)
describe(df[df$mtongue=="English",]$gpaimprove)
describe(df[df$mtongue!="English",]$gpaimprove)

describe(df$dist_from_cut)
describe(df[df$sex=="M",]$dist_from_cut)
describe(df[df$sex=="F",]$dist_from_cut)
describe(df[df$mtongue=="English",]$dist_from_cut)
describe(df[df$mtongue!="English",]$dist_from_cut)

# Ansewer the first Research Question:
# Does academic probation have a causal effect on students' 
# decision to drop out and their subsequent performance improvement? 

df_1 <- df 
df_1$bin = cut_width(df_1$dist_from_cut,width=0.1,closed="left",boundary=0)
df_2 <- df_1 %>% group_by(bin)%>%summarise(Num=n(),Distance_from_Cutoff = mean(dist_from_cut,na.rm=T),
                                           Subsequent_GDP_Minus_Cutoff = mean(year2_dist_from_cut,na.rm = T),
                                           GPA_Improvement = mean(GPA_year2-GPA_year1,na.rm=T),
                                           left_school = mean(left_school,na.rm=T),
                                           probation = mean(probation_year1,na.rm = T))
# Check for male
df_M <- df_1 %>% filter(sex == "M") %>% 
  group_by(bin)%>%summarise(Num=n(),Distance_from_Cutoff = mean(dist_from_cut,na.rm=T),
                                           Subsequent_GDP_Minus_Cutoff = mean(year2_dist_from_cut,na.rm = T),
                                           GPA_Improvement = mean(GPA_year2-GPA_year1,na.rm=T),
                            left_school = mean(left_school,na.rm=T))
# Check for female
df_F <- df_1 %>% filter(sex == "F") %>% 
  group_by(bin)%>%summarise(Num=n(),Distance_from_Cutoff = mean(dist_from_cut,na.rm=T),
                            Subsequent_GDP_Minus_Cutoff = mean(year2_dist_from_cut,na.rm = T),
                            GPA_Improvement = mean(GPA_year2-GPA_year1,na.rm=T),
                            left_school = mean(left_school,na.rm=T))
# Check for English
df_En <- df_1 %>% filter(mtongue == "English") %>% 
  group_by(bin)%>%summarise(Num=n(),Distance_from_Cutoff = mean(dist_from_cut,na.rm=T),
                            Subsequent_GDP_Minus_Cutoff = mean(year2_dist_from_cut,na.rm = T),
                            GPA_Improvement = mean(GPA_year2-GPA_year1,na.rm=T),
                            left_school = mean(left_school,na.rm=T))
# Check for non-English
df_Ot <- df_1 %>% filter(mtongue != "English") %>% 
  group_by(bin)%>%summarise(Num=n(),Distance_from_Cutoff = mean(dist_from_cut,na.rm=T),
                            Subsequent_GDP_Minus_Cutoff = mean(year2_dist_from_cut,na.rm = T),
                            GPA_Improvement = mean(GPA_year2-GPA_year1,na.rm=T),
                            left_school = mean(left_school,na.rm=T))


### Plot the Figures for GPA_improvement for different levels 
# Overall analysis
GPA_improve <- df_2 %>% 
  ggplot(aes(x = Distance_from_Cutoff,
             y = GPA_Improvement)) +
  geom_point(alpha = 0.3,size=3) +
  geom_smooth(data = df_2 %>% filter(Distance_from_Cutoff < 0), 
              method=NULL,
              color = "black",
              se = F) +
  geom_smooth(data = df_2 %>% filter(Distance_from_Cutoff > 0), 
              method=NULL,
              color = "black",
              se = F) +
  theme_minimal() +
  labs(x = "Distance from Cut-off (1st year)",
       y = "GPA Improvement",
       title = "GPA Improvement")+
  geom_vline(xintercept = 0,linetype="dashed")

# For male
GPA_improve_M <- df_M %>% 
  ggplot(aes(x = Distance_from_Cutoff,
             y = GPA_Improvement)) +
  geom_point(alpha = 0.3,size=3) +
  geom_smooth(data = df_M %>% filter(Distance_from_Cutoff < 0), 
              method=NULL,
              color = "black",
              se = F) +
  geom_smooth(data = df_M %>% filter(Distance_from_Cutoff > 0), 
              method=NULL,
              color = "black",
              se = F) +
  theme_minimal() +
  labs(x = "Distance from Cut-off (1st year)",
       y = "GPA Improvement",
       title = "Male")+
  geom_vline(xintercept = 0,linetype="dashed")

# For female
GPA_improve_F <- df_F %>% 
  ggplot(aes(x = Distance_from_Cutoff,
             y = GPA_Improvement)) +
  geom_point(alpha = 0.3,size=3) +
  geom_smooth(data = df_F %>% filter(Distance_from_Cutoff < 0), 
              method=NULL,
              color = "black",
              se = F) +
  geom_smooth(data = df_F %>% filter(Distance_from_Cutoff > 0), 
              method=NULL,
              color = "black",
              se = F) +
  theme_minimal() +
  labs(x = "Distance from Cut-off (1st year)",
       y = "GPA Improvement",
       title = "Female")+
  geom_vline(xintercept = 0,linetype="dashed")

# For English
GPA_improve_En <- df_En %>% 
  ggplot(aes(x = Distance_from_Cutoff,
             y = GPA_Improvement)) +
  geom_point(alpha = 0.3,size=3) +
  geom_smooth(data = df_En %>% filter(Distance_from_Cutoff < 0), 
              method=NULL,
              color = "black",
              se = F) +
  geom_smooth(data = df_En %>% filter(Distance_from_Cutoff > 0), 
              method=NULL,
              color = "black",
              se = F) +
  theme_minimal() +
  labs(x = "Distance from Cut-off (1st year)",
       y = "GPA Improvement",
       title = "English")+
  geom_vline(xintercept = 0,linetype="dashed")

# For non-English
GPA_improve_Ot <- df_Ot %>% 
  ggplot(aes(x = Distance_from_Cutoff,
             y = GPA_Improvement)) +
  geom_point(alpha = 0.3,size=3) +
  geom_smooth(data = df_Ot %>% filter(Distance_from_Cutoff < 0), 
              method=NULL,
              color = "black",
              se = F) +
  geom_smooth(data = df_Ot %>% filter(Distance_from_Cutoff > 0), 
              method=NULL,
              color = "black",
              se = F) +
  theme_minimal() +
  labs(x = "Distance from Cut-off (1st year)",
       y = "GPA Improvement",
       title = "Other Language")+
  geom_vline(xintercept = 0,linetype="dashed")

# Same as before, for df_2
drop <- df_2 %>% 
  ggplot(aes(x = Distance_from_Cutoff,
             y = left_school)) +
  geom_point(alpha = 0.3,size=3) +
  geom_smooth(data = df_2 %>% filter(Distance_from_Cutoff < 0), 
              method=NULL,
              color = "black",
              se = F) +
  geom_smooth(data = df_2 %>% filter(Distance_from_Cutoff > 0), 
              method=NULL,
              color = "black",
              se = F) +
  theme_minimal() +
  labs(x = "Distance from Cut-off (1st year)",
       y = "Left school voluntarily",
       title = "Dropping Out")+
  geom_vline(xintercept = 0,linetype="dashed")

# For male
drop_M <- df_M %>% 
  ggplot(aes(x = Distance_from_Cutoff,
             y = left_school)) +
  geom_point(alpha = 0.3,size=3) +
  geom_smooth(data = df_M %>% filter(Distance_from_Cutoff < 0), 
              method=NULL,
              color = "black",
              se = F) +
  geom_smooth(data = df_M %>% filter(Distance_from_Cutoff > 0), 
              method=NULL,
              color = "black",
              se = F) +
  theme_minimal() +
  labs(x = "Distance from Cut-off (1st year)",
       y = "Left school voluntarily",
       title = "Male")+
  geom_vline(xintercept = 0,linetype="dashed")

# For femal
drop_F <- df_F %>% 
  ggplot(aes(x = Distance_from_Cutoff,
             y = left_school)) +
  geom_point(alpha = 0.3,size=3) +
  geom_smooth(data = df_F %>% filter(Distance_from_Cutoff < 0), 
              method=NULL,
              color = "black",
              se = F) +
  geom_smooth(data = df_F %>% filter(Distance_from_Cutoff > 0), 
              method=NULL,
              color = "black",
              se = F) +
  theme_minimal() +
  labs(x = "Distance from Cut-off (1st year)",
       y = "Left school voluntarily",
       title = "Female")+
  geom_vline(xintercept = 0,linetype="dashed")

# For English
drop_En <- df_En %>% 
  ggplot(aes(x = Distance_from_Cutoff,
             y = left_school)) +
  geom_point(alpha = 0.3,size=3) +
  geom_smooth(data = df_En %>% filter(Distance_from_Cutoff < 0), 
              method=NULL,
              color = "black",
              se = F) +
  geom_smooth(data = df_En %>% filter(Distance_from_Cutoff > 0), 
              method=NULL,
              color = "black",
              se = F) +
  theme_minimal() +
  labs(x = "Distance from Cut-off (1st year)",
       y = "Left school voluntarily",
       title = "English")+
  geom_vline(xintercept = 0,linetype="dashed")

# For non-English
drop_Ot <- df_Ot %>% 
  ggplot(aes(x = Distance_from_Cutoff,
             y = left_school)) +
  geom_point(alpha = 0.3,size=3) +
  geom_smooth(data = df_Ot %>% filter(Distance_from_Cutoff < 0), 
              method=NULL,
              color = "black",
              se = F) +
  geom_smooth(data = df_Ot %>% filter(Distance_from_Cutoff > 0), 
              method=NULL,
              color = "black",
              se = F) +
  theme_minimal() +
  labs(x = "Distance from Cut-off (1st year)",
       y = "Left school voluntarily",
       title = "Other Language")+
  geom_vline(xintercept = 0,linetype="dashed")


grid.arrange(drop,GPA_improve,ncol=2)
grid.arrange(drop_M,drop_F,drop_En,drop_Ot,ncol=2)
grid.arrange(GPA_improve_M,GPA_improve_F,GPA_improve_En,GPA_improve_Ot,ncol=2)

probation <- df_2 %>% 
  ggplot(aes(x = Distance_from_Cutoff,
             y = probation)) +
  geom_point(alpha = 0.3,size=3) +
  geom_smooth(data = df_2 %>% filter(Distance_from_Cutoff < 0), 
              method=NULL,
              color = "black",
              se = F) +
  geom_smooth(data = df_2 %>% filter(Distance_from_Cutoff > 0), 
              method=NULL,
              color = "black",
              se = F) +
  theme_minimal() +
  labs(x = "Distance from Cut-off (1st year)",
       y = "Probation Status")+
  geom_vline(xintercept = 0,linetype="dashed")


#### Tests of the Validity of the RD Approach
discon_test_1 <- df_2 %>% 
  ggplot(aes(x = Distance_from_Cutoff,
             y = Num)) +
  geom_point(alpha = 0.3,size=3) +
  geom_smooth(data = df_2 %>% filter(Distance_from_Cutoff < 0), 
              method="lm",
              color = "black",
              se = F) +
  geom_smooth(data = df_2 %>% filter(Distance_from_Cutoff > 0), 
              method="lm",
              color = "black",
              se=F) +
  theme_minimal() +
  labs(x = "Distance from Cut-off (1st year)",
       y = "Frequency Count")+
  geom_vline(xintercept = 0,linetype="dashed")


test_m1 <- glm(factor(df$sex)~dist_from_cut*probation_year1,
           data=df,family = binomial)
summary(test_m1)

test_m2 <- glm(factor(df$mtongue)~dist_from_cut*probation_year1,
               data=df,family = binomial)
summary(test_m2)


##GPA_improvement - All
lm_2 <- lm((GPA_year2-GPA_year1)~dist_from_cut +probation_year1,
           data=df)
summary(lm_2)

##GPA_improvement - Gender
lm_2_M <- lm((GPA_year2-GPA_year1)~dist_from_cut+probation_year1,
             data=df[df$sex==1,])
summary(lm_2_M)

lm_2_F <- lm((GPA_year2-GPA_year1)~dist_from_cut+probation_year1,
             data=df[df$sex==0,])
summary(lm_2_F)

##GPA_improvement - Native_language
lm_2_E <- lm((GPA_year2-GPA_year1)~dist_from_cut+probation_year1,
             data=df[df$mtongue=="English",])
summary(lm_2_E)

lm_2_O <- lm((GPA_year2-GPA_year1)~dist_from_cut+probation_year1,
             data=df[df$mtongue!="English",])
summary(lm_2_O)

##Drop - All
lm_3 <- lm(left_school~dist_from_cut+probation_year1,
           data=df)
summary(lm_3)



##Drop - Gender
lm_3_M <- lm(left_school~dist_from_cut+probation_year1,
             data=df[df$sex==1,])
summary(lm_3_M)

lm_3_F <- lm(left_school~dist_from_cut+probation_year1,
             data=df[df$sex==0,])
summary(lm_3_F)


##Drop - Native_language
lm_3_E <- lm(left_school~dist_from_cut+probation_year1,
             data=df[df$mtongue=="English",])
summary(lm_3_E)

lm_3_O <- lm(left_school~dist_from_cut+probation_year1,
             data=df[df$mtongue!="English",])
summary(lm_3_O)


rdrobust(y = df$GPA_year2-df$GPA_year1, x = df$dist_from_cut, c = 0, all = TRUE) %>% summary()





