#Authors: Elizabeth Kolln
#Last edited by: Elizabeth Kolln 05/06/2025

####Installing packages
#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("readr")
#install.packages("tidyr")
#install.packages("lme4")
#install.packages("sjPlot")
#install.packages("plyr")
#install.packages('writexl')
#install.packages("wesanderson")
#install.packages("cowplot")
#install.packages("psych")

####Calling packages
library(tidyverse)
library(dplyr)
library(readr)
library(tidyr)
library(lme4)
library(sjPlot)
library(plyr)
library(writexl)
library(wesanderson)
library(cowplot)
library(psych)

####Make sure working directory is correct
setwd("~/Desktop/wellesley/thesis/final_data/ALL_DATA")

###importing the TOM data###
TOM_data <- read_csv("ALL_TOM.csv")

View(TOM_data)

#removing unncessary information
TOM_cleaned <- select(TOM_data,
                      ID,
                      DDS2_OD, DDS2_TQ,
                      DDS7_OD, DDS7_TQ,
                      DDS8_OD, DDS8_TQ,
                      DBOG_OB, DBOG_TQ,
                      DBS2_OB, DBS2_TQ,
                      DBS4_OB, DBS4_TQ,
                      KAOG_MC, KAOG_TQ, KAOG_MQ,
                      KAS5_MC, KAS5_TQ, KAS5_MQ,
                      KAS7_MC, KAS7_TQ, KAS7_MQ,
                      FBS1_TQ, FBS1_MQ,
                      FBS3_TQ, FBS3_MQ,
                      FBS4_TQ, FBS4_MQ,
                      RAES1_TFQ, RAES1_TLQ,
                      RAES4_TFQ, RAES4_TLQ,
                      RAES9_TFQ, RAES9_TLQ)

TOM_cleaned <- TOM_cleaned[-c(1:2) , ]

View(TOM_cleaned)

##Scoring TOM, based on Jaime's code from github
#scoring DD
TOM_cleaned$DD <- ifelse(TOM_cleaned$DDS2_OD != TOM_cleaned$DDS2_TQ | 
                           TOM_cleaned$DDS7_OD != TOM_cleaned$DDS7_TQ | 
                           TOM_cleaned$DDS8_OD != TOM_cleaned$DDS8_TQ,
                         1, 0)
TOM_cleaned$DD <- ifelse(is.na(TOM_cleaned$DD), 0, 1)

#scoring DB
TOM_cleaned$DB <- ifelse(TOM_cleaned$DBOG_OB != TOM_cleaned$DBOG_TQ | 
                           TOM_cleaned$DBS2_OB != TOM_cleaned$DBS2_TQ | 
                           TOM_cleaned$DBS4_OB != TOM_cleaned$DBS4_TQ, 
                         1, 0)
TOM_cleaned$DB <- ifelse(is.na(TOM_cleaned$DB), 0, 1)

#scoring KA
TOM_cleaned$KA <- ifelse(TOM_cleaned$KAOG_TQ == 2 & TOM_cleaned$KAOG_MQ == 2 |
                           TOM_cleaned$KAS5_TQ == 2 & TOM_cleaned$KAS5_MQ == 2 |
                           TOM_cleaned$KAS7_TQ == 2 & TOM_cleaned$KAS7_MQ == 2,
                         1, 0)
TOM_cleaned$KA <- ifelse(is.na(TOM_cleaned$KA), 0, 1)

#scoring FB
TOM_cleaned$FB <- ifelse(TOM_cleaned$FBS1_TQ == 1 & TOM_cleaned$FBS1_MQ == 2 |
                           TOM_cleaned$FBS3_TQ == 1 & TOM_cleaned$FBS3_MQ == 2 |
                           TOM_cleaned$FBS4_TQ == 1 & TOM_cleaned$FBS4_MQ == 2,
                         1, 0)
TOM_cleaned$FB <- ifelse(is.na(TOM_cleaned$FB), 0, 1)

#scoring RAE
TOM_cleaned$RAE <- ifelse(TOM_cleaned$RAES1_TFQ < TOM_cleaned$RAES1_TLQ |
                            TOM_cleaned$RAES4_TFQ < TOM_cleaned$RAES4_TLQ |
                            TOM_cleaned$RAES9_TFQ < TOM_cleaned$RAES9_TLQ, 
                          1, 0)
TOM_cleaned$RAE <- ifelse(is.na(TOM_cleaned$RAE), 0, 1)

##final data frame of all TOM data
TOM_final <- select(TOM_cleaned, 
                    ID,
                    TOM_DD = DD,
                    TOM_DB = DB,
                    TOM_KA = KA,
                    TOM_FB = FB,
                    TOM_RAE = RAE)

#creating a TOM sum variable
TOM_final$TOM_sum <- (TOM_final$TOM_DD + 
                        TOM_final$TOM_DB + 
                        TOM_final$TOM_KA + 
                        TOM_final$TOM_FB + 
                        TOM_final$TOM_RAE)

View(TOM_final)

###importing the SLA data###
SLA_data <- read.csv("ALL_SLA.csv")

View(SLA_data)

#removing unnecessary information
SLA_cleaned <- select(SLA_data,
                      ID,
                      IDS1 = Baby..Hallway,
                      ADS1 = Teacher..Hallway,
                      PDS1 = Peer..Hallway,
                      FDS1 = Foreign..Hallway,
                      IDS2 = Baby..Classroom,
                      ADS2 = Teacher..Classroom,
                      PDS2 = Peer..Classroom,
                      FDS2 = Foreign..Classroom,
                      IDS3 = Baby..Playground,
                      ADS3 = Teacher..Playground,
                      PDS3 = Peer..Playground,
                      FDS3 = Foreign..Playground)

SLA_cleaned <- SLA_cleaned[-c(1:2) , ]

#creating a SLA_Sum variable for correlation
SLA_cleaned$SLA_Sum <- (as.integer(SLA_cleaned$IDS1) + 
                          as.integer(SLA_cleaned$ADS1) + 
                          as.integer(SLA_cleaned$PDS1) + 
                          as.integer(SLA_cleaned$FDS1) + 
                          
                          as.integer(SLA_cleaned$IDS2) + 
                          as.integer(SLA_cleaned$ADS2) + 
                          as.integer(SLA_cleaned$PDS2) + 
                          as.integer(SLA_cleaned$FDS2) +
                          
                          as.integer(SLA_cleaned$IDS3) + 
                          as.integer(SLA_cleaned$ADS3) + 
                          as.integer(SLA_cleaned$PDS3) + 
                          as.integer(SLA_cleaned$FDS3))

View(SLA_cleaned)

#reformat data from long to wide
SLA_final <- SLA_cleaned %>% 
  pivot_longer(-c(ID, SLA_Sum), 
               names_to = "SLA_Type",
               values_to = "SLA_Score")

#separating SLA_type from SLA_setting
SLA_final$SLA_Setting <- gsub("[a-zA-Z]", "", SLA_final$SLA_Type)
SLA_final$SLA_Type <- gsub("[0-9]", "", SLA_final$SLA_Type)

#convert to numeric to avoid issues with the model later
SLA_final$SLA_Score <- as.numeric(SLA_final$SLA_Score)
SLA_final$SLA_Setting <- as.numeric(SLA_final$SLA_Setting)

View(SLA_final)

###importing the EF and PVT data###
EF_PVT_data <- read.csv("ALL_EF_PVT.csv")

View(EF_PVT_data)

#fixing incorrect ID
EF_PVT_data[EF_PVT_data$PID==1025, "PID"] <- 1043
EF_PVT_data[EF_PVT_data$PID=="1025-", "PID"] <- 1025

#eliminating unnecessary information
EF_PVT_data_cleaned <- select(EF_PVT_data, 
                              ID = PID,
                              PVT = Theta,
                              EF = ComputedScore)

#making sure it still includes participants who did not complete EF
EF_PVT_data_cleaned <- EF_PVT_data_cleaned %>%
  mutate(EF = ifelse(is.na(PVT) & is.na(EF), "null", EF))

View(EF_PVT_data_cleaned)

#combining information
EF_PVT_data_cleaned <- EF_PVT_data_cleaned %>%
  group_by(ID) %>%
  summarize_all(na.omit)

#resetting the participants who did not complete EF to NA
EF_PVT_data_cleaned <- EF_PVT_data_cleaned %>%
  mutate(EF = ifelse(EF == "null", NA, EF))

View(EF_PVT_data_cleaned)

##Adding in demographics
#adding ages to IDs
ID_ages <- read.csv("ALL_ages.csv")

##Merging data grames
#merging the data
data_all <- merge(EF_PVT_data_cleaned, SLA_final, by = "ID", all.x = TRUE)
data_all <- merge(data_all, TOM_final, by = "ID", all.x = TRUE)
data_all <- merge(data_all, ID_ages, by = "ID", all.x = TRUE)

data_all$EF <- as.numeric(data_all$EF)
data_all$PVT <- as.numeric(data_all$PVT)

View(data_all)

str(data_all)

#removing the one participant who did not complete SLA
#NOTE: n = 70, 1 excluded so 69, 24 3yr, 22 4yr, 23 5yr
data_all <- subset(data_all, ID != 7006)

#merge for graphing
df_graph1 <- select(SLA_cleaned,
                    ID,
                    SLA_Sum)

df_graph1$Infant <- (as.integer(SLA_cleaned$IDS1)
                     + as.integer(SLA_cleaned$IDS2)
                     + as.integer(SLA_cleaned$IDS3))

df_graph1$Adult <- (as.integer(SLA_cleaned$ADS1)
                    + as.integer(SLA_cleaned$ADS2)
                    + as.integer(SLA_cleaned$ADS3))

df_graph1$Peer <- (as.integer(SLA_cleaned$PDS1)
                   + as.integer(SLA_cleaned$PDS2)
                   + as.integer(SLA_cleaned$PDS3))

df_graph1$Foreign <- (as.integer(SLA_cleaned$FDS1)
                      + as.integer(SLA_cleaned$FDS2)
                      + as.integer(SLA_cleaned$FDS3))

df_graph1 <- merge(EF_PVT_data_cleaned, df_graph1, by = "ID")
df_graph1 <- merge(df_graph1, TOM_final, by = "ID")
df_graph1 <- merge(df_graph1, ID_ages, by = "ID")

#removing the one participant who did not complete SLA
df_graph1 <- subset(df_graph1, ID != 7006)

View(df_graph1)

#age visualizations
ggplot(df_graph1, aes(x=Age_Mo)) +
  geom_bar(fill = "#ea5458")

ggplot(df_graph1, aes(x=Age_Yr)) +
  geom_bar(fill = "#ea5458") +
  ylab("Count") +
  xlab("Age in Years")

#turning into an excel for show my data
write_xlsx(df_graph1, "df_graph1.xlsx")

####SLA visualizations only

#Making the data frame
df_graph_SLA1 <- select(df_graph1,
                       ID,
                       Infant,
                       Adult,
                       Peer,
                       Foreign,
                       Age_Mo,
                       Age_Yr)

View(df_graph_SLA1)

df_graph_SLA2 <- pivot_longer(df_graph_SLA1, c(Infant, Adult, Peer, Foreign), 
               names_to = "Type",
               values_to = "Score")

View(df_graph_SLA2)

##visualize
#passing each register based on age
df_graph_SLA2$Type <- factor(df_graph_SLA2$Type, levels = c("Foreign", "Infant", "Peer", "Adult"))

ggplot(df_graph_SLA2, aes(x=Age_Mo, y=Score, color=Type)) +
  geom_point(size=1) +
  geom_jitter() +
  geom_smooth(method = lm) +
  geom_hline(yintercept = 1.5, linetype = "dashed") +
  ylab("Score (out of 3)") +
  xlab("Age in months") +
  labs(color = "Register") +
  theme(text=element_text(size=14,  family="Arial"))

#Wagner-like visualizations
#Box plot, NOTE: Issues of IQRs being funky because not enough data
df_graph_SLA2$Age_Yr <- as.factor(df_graph_SLA2$Age_Yr)
df_graph_SLA2$Proportion_correct <- df_graph_SLA2$Score/3
view(df_graph_SLA2)

ggplot(df_graph_SLA2, aes(x = Type, y = Proportion_correct, color = Age_Yr)) +
  geom_boxplot() +
  geom_jitter(position = position_jitterdodge(jitter.width = 0, jitter.height = 0.1, dodge.width = 0.75)) +
  ylim(0, 1) +
  geom_hline(yintercept = 0.50, linetype = "dashed") +
  scale_color_manual(values = c("#764ccf", "#4cb0cf", "#cf4c8d")) +
  ylab("Proportion of correct responses") +
  xlab("Register") +
  labs(color = "Age (years)")

#WORK IN PROGRESS bar plot to show differences in age groups by register
ggplot(df_graph_SLA2, aes(x = Type, y = Proportion_correct, fill = Age_Yr)) +
  geom_col(position = "dodge") +
  ylim(0, 1) +
  geom_hline(yintercept = 0.50, linetype = "dashed") +
  scale_fill_manual(values = c("#764ccf", "#4cb0cf", "#cf4c8d")) +
  ylab("Proportion of correct responses") +
  xlab("Register") +
  labs(color = "Age (years)")

#SLA foil comparison bar graph
#Bringing in the information about foils which I manually put together in a spreadsheet
SLA_foils <- read_csv("Foil_information_SLA.csv")
view(SLA_foils)

#remove participants as needed
SLA_cleaned <- subset(SLA_cleaned, ID!= 7006)

SLA_foils$Score <- NA
view(SLA_foils)

#Dropping in the total number of children who got it correct for each SLA type
SLA_foils$Score = ifelse((SLA_foils$SLA_Type == "IDS" & SLA_foils$SLA_Setting == 1), 
                         sum(as.integer(SLA_cleaned$IDS1)), 
                         SLA_foils$Score)
SLA_foils$Score = ifelse((SLA_foils$SLA_Type == "IDS" & SLA_foils$SLA_Setting == 2), 
                         sum(as.integer(SLA_cleaned$IDS2)), 
                         SLA_foils$Score)
SLA_foils$Score = ifelse((SLA_foils$SLA_Type == "IDS" & SLA_foils$SLA_Setting == 3), 
                         sum(as.integer(SLA_cleaned$IDS3)), 
                         SLA_foils$Score)

SLA_foils$Score = ifelse((SLA_foils$SLA_Type == "ADS" & SLA_foils$SLA_Setting == 1), 
                         sum(as.integer(SLA_cleaned$ADS1)), 
                         SLA_foils$Score)
SLA_foils$Score = ifelse((SLA_foils$SLA_Type == "ADS" & SLA_foils$SLA_Setting == 2), 
                         sum(as.integer(SLA_cleaned$ADS2)), 
                         SLA_foils$Score)
SLA_foils$Score = ifelse((SLA_foils$SLA_Type == "ADS" & SLA_foils$SLA_Setting == 3), 
                         sum(as.integer(SLA_cleaned$ADS3)), 
                         SLA_foils$Score)

SLA_foils$Score = ifelse((SLA_foils$SLA_Type == "PDS" & SLA_foils$SLA_Setting == 1), 
                         sum(as.integer(SLA_cleaned$PDS1)), 
                         SLA_foils$Score)
SLA_foils$Score = ifelse((SLA_foils$SLA_Type == "PDS" & SLA_foils$SLA_Setting == 2), 
                         sum(as.integer(SLA_cleaned$PDS2)), 
                         SLA_foils$Score)
SLA_foils$Score = ifelse((SLA_foils$SLA_Type == "PDS" & SLA_foils$SLA_Setting == 3), 
                         sum(as.integer(SLA_cleaned$PDS3)), 
                         SLA_foils$Score)

SLA_foils$Score = ifelse((SLA_foils$SLA_Type == "FDS" & SLA_foils$SLA_Setting == 1), 
                         sum(as.integer(SLA_cleaned$FDS1)), 
                         SLA_foils$Score)
SLA_foils$Score = ifelse((SLA_foils$SLA_Type == "FDS" & SLA_foils$SLA_Setting == 2), 
                         sum(as.integer(SLA_cleaned$FDS2)), 
                         SLA_foils$Score)
SLA_foils$Score = ifelse((SLA_foils$SLA_Type == "FDS" & SLA_foils$SLA_Setting == 3), 
                         sum(as.integer(SLA_cleaned$FDS3)), 
                         SLA_foils$Score)

view(SLA_foils)

#Proportion who got it correct is the number who got it correct divided by n
SLA_foils$Score_Proportion = SLA_foils$Score/69
view(SLA_foils)

#renaming labels
SLA_foils[SLA_foils$SLA_Type=="IDS", "SLA_Type"] <- "Infant"
SLA_foils[SLA_foils$SLA_Type=="ADS", "SLA_Type"] <- "Adult"
SLA_foils[SLA_foils$SLA_Type=="FDS", "SLA_Type"] <- "Foreign"
SLA_foils[SLA_foils$SLA_Type=="PDS", "SLA_Type"] <- "Peer"

SLA_foils[SLA_foils$SLA_Foil=="IDS", "SLA_Foil"] <- "Infant"
SLA_foils[SLA_foils$SLA_Foil=="ADS", "SLA_Foil"] <- "Adult"
SLA_foils[SLA_foils$SLA_Foil=="FDS", "SLA_Foil"] <- "Foreign"
SLA_foils[SLA_foils$SLA_Foil=="PDS", "SLA_Foil"] <- "Peer"

view(SLA_foils)

#Foil plot for all ages merged together
ggplot(SLA_foils, aes(x = SLA_Type, y = Score_Proportion, fill = SLA_Foil)) +
  geom_col(position = "dodge") +
  ylim(0, 1) +
  scale_fill_manual(values = wes_palette("AsteroidCity1")) +
  labs(title = "Foil Comparison (All Ages)")

#Foil comparison by age group
#Making each data set
SLA_Score_Age <- merge(SLA_cleaned, ID_ages, by = "ID")
view(SLA_Score_Age)

SLA_Score_3 <- subset(SLA_Score_Age, Age_Yr == 3)
view(SLA_Score_3)

SLA_Score_4 <- subset(SLA_Score_Age, Age_Yr == 4)
view(SLA_Score_4)

SLA_Score_5 <- subset(SLA_Score_Age, Age_Yr == "5 & 6")
view(SLA_Score_5)

#Just 3-year-olds data set
#Making data frame
SLA_foils3 <- read_csv("Foil_information_SLA.csv")
SLA_foils3$Score <- NA
view(SLA_foils3)

#Dropping in the total number of 3yr children who got it correct for each SLA type
SLA_foils3$Score = ifelse((SLA_foils3$SLA_Type == "IDS" & SLA_foils3$SLA_Setting == 1), 
                         sum(as.integer(SLA_Score_3$IDS1)), 
                         SLA_foils3$Score)
SLA_foils3$Score = ifelse((SLA_foils3$SLA_Type == "IDS" & SLA_foils3$SLA_Setting == 2), 
                         sum(as.integer(SLA_Score_3$IDS2)), 
                         SLA_foils3$Score)
SLA_foils3$Score = ifelse((SLA_foils3$SLA_Type == "IDS" & SLA_foils3$SLA_Setting == 3), 
                         sum(as.integer(SLA_Score_3$IDS3)), 
                         SLA_foils3$Score)

SLA_foils3$Score = ifelse((SLA_foils3$SLA_Type == "ADS" & SLA_foils3$SLA_Setting == 1), 
                         sum(as.integer(SLA_Score_3$ADS1)), 
                         SLA_foils3$Score)
SLA_foils3$Score = ifelse((SLA_foils3$SLA_Type == "ADS" & SLA_foils3$SLA_Setting == 2), 
                         sum(as.integer(SLA_Score_3$ADS2)), 
                         SLA_foils3$Score)
SLA_foils3$Score = ifelse((SLA_foils3$SLA_Type == "ADS" & SLA_foils3$SLA_Setting == 3), 
                         sum(as.integer(SLA_Score_3$ADS3)), 
                         SLA_foils3$Score)

SLA_foils3$Score = ifelse((SLA_foils3$SLA_Type == "PDS" & SLA_foils3$SLA_Setting == 1), 
                         sum(as.integer(SLA_Score_3$PDS1)), 
                         SLA_foils3$Score)
SLA_foils3$Score = ifelse((SLA_foils3$SLA_Type == "PDS" & SLA_foils3$SLA_Setting == 2), 
                         sum(as.integer(SLA_Score_3$PDS2)), 
                         SLA_foils3$Score)
SLA_foils3$Score = ifelse((SLA_foils3$SLA_Type == "PDS" & SLA_foils3$SLA_Setting == 3), 
                         sum(as.integer(SLA_Score_3$PDS3)), 
                         SLA_foils3$Score)

SLA_foils3$Score = ifelse((SLA_foils3$SLA_Type == "FDS" & SLA_foils3$SLA_Setting == 1), 
                         sum(as.integer(SLA_Score_3$FDS1)), 
                         SLA_foils3$Score)
SLA_foils3$Score = ifelse((SLA_foils3$SLA_Type == "FDS" & SLA_foils3$SLA_Setting == 2), 
                         sum(as.integer(SLA_Score_3$FDS2)), 
                         SLA_foils3$Score)
SLA_foils3$Score = ifelse((SLA_foils3$SLA_Type == "FDS" & SLA_foils3$SLA_Setting == 3), 
                         sum(as.integer(SLA_Score_3$FDS3)), 
                         SLA_foils3$Score)

view(SLA_foils3)

#Proportion who got it correct is the number who got it correct divided by 3yr n
SLA_foils3$Score_Proportion = SLA_foils3$Score/24
view(SLA_foils3)

#renaming labels
SLA_foils3[SLA_foils3$SLA_Type=="IDS", "SLA_Type"] <- "Infant"
SLA_foils3[SLA_foils3$SLA_Type=="ADS", "SLA_Type"] <- "Adult"
SLA_foils3[SLA_foils3$SLA_Type=="FDS", "SLA_Type"] <- "Foreign"
SLA_foils3[SLA_foils3$SLA_Type=="PDS", "SLA_Type"] <- "Peer"

SLA_foils3[SLA_foils3$SLA_Foil=="IDS", "SLA_Foil"] <- "Infant"
SLA_foils3[SLA_foils3$SLA_Foil=="ADS", "SLA_Foil"] <- "Adult"
SLA_foils3[SLA_foils3$SLA_Foil=="FDS", "SLA_Foil"] <- "Foreign"
SLA_foils3[SLA_foils3$SLA_Foil=="PDS", "SLA_Foil"] <- "Peer"

view(SLA_foils3)

#Just 4-year-olds data set
#Making data frame
SLA_foils4 <- read_csv("Foil_information_SLA.csv")
SLA_foils4$Score <- NA
view(SLA_foils4)

#Dropping in the total number of 4yr children who got it correct for each SLA type
SLA_foils4$Score = ifelse((SLA_foils4$SLA_Type == "IDS" & SLA_foils4$SLA_Setting == 1), 
                          sum(as.integer(SLA_Score_4$IDS1)), 
                          SLA_foils4$Score)
SLA_foils4$Score = ifelse((SLA_foils4$SLA_Type == "IDS" & SLA_foils4$SLA_Setting == 2), 
                          sum(as.integer(SLA_Score_4$IDS2)), 
                          SLA_foils4$Score)
SLA_foils4$Score = ifelse((SLA_foils4$SLA_Type == "IDS" & SLA_foils4$SLA_Setting == 3), 
                          sum(as.integer(SLA_Score_4$IDS3)), 
                          SLA_foils4$Score)

SLA_foils4$Score = ifelse((SLA_foils4$SLA_Type == "ADS" & SLA_foils4$SLA_Setting == 1), 
                          sum(as.integer(SLA_Score_4$ADS1)), 
                          SLA_foils4$Score)
SLA_foils4$Score = ifelse((SLA_foils4$SLA_Type == "ADS" & SLA_foils4$SLA_Setting == 2), 
                          sum(as.integer(SLA_Score_4$ADS2)), 
                          SLA_foils4$Score)
SLA_foils4$Score = ifelse((SLA_foils4$SLA_Type == "ADS" & SLA_foils4$SLA_Setting == 3), 
                          sum(as.integer(SLA_Score_4$ADS3)), 
                          SLA_foils4$Score)

SLA_foils4$Score = ifelse((SLA_foils4$SLA_Type == "PDS" & SLA_foils4$SLA_Setting == 1), 
                          sum(as.integer(SLA_Score_4$PDS1)), 
                          SLA_foils4$Score)
SLA_foils4$Score = ifelse((SLA_foils4$SLA_Type == "PDS" & SLA_foils4$SLA_Setting == 2), 
                          sum(as.integer(SLA_Score_4$PDS2)), 
                          SLA_foils4$Score)
SLA_foils4$Score = ifelse((SLA_foils4$SLA_Type == "PDS" & SLA_foils4$SLA_Setting == 3), 
                          sum(as.integer(SLA_Score_4$PDS3)), 
                          SLA_foils4$Score)

SLA_foils4$Score = ifelse((SLA_foils4$SLA_Type == "FDS" & SLA_foils4$SLA_Setting == 1), 
                          sum(as.integer(SLA_Score_4$FDS1)), 
                          SLA_foils4$Score)
SLA_foils4$Score = ifelse((SLA_foils4$SLA_Type == "FDS" & SLA_foils4$SLA_Setting == 2), 
                          sum(as.integer(SLA_Score_4$FDS2)), 
                          SLA_foils4$Score)
SLA_foils4$Score = ifelse((SLA_foils4$SLA_Type == "FDS" & SLA_foils4$SLA_Setting == 3), 
                          sum(as.integer(SLA_Score_4$FDS3)), 
                          SLA_foils4$Score)

view(SLA_foils4)

#Proportion who got it correct is the number who got it correct divided by n
SLA_foils4$Score_Proportion = SLA_foils4$Score/22
view(SLA_foils4)

#renaming labels
SLA_foils4[SLA_foils4$SLA_Type=="IDS", "SLA_Type"] <- "Infant"
SLA_foils4[SLA_foils4$SLA_Type=="ADS", "SLA_Type"] <- "Adult"
SLA_foils4[SLA_foils4$SLA_Type=="FDS", "SLA_Type"] <- "Foreign"
SLA_foils4[SLA_foils4$SLA_Type=="PDS", "SLA_Type"] <- "Peer"

SLA_foils4[SLA_foils4$SLA_Foil=="IDS", "SLA_Foil"] <- "Infant"
SLA_foils4[SLA_foils4$SLA_Foil=="ADS", "SLA_Foil"] <- "Adult"
SLA_foils4[SLA_foils4$SLA_Foil=="FDS", "SLA_Foil"] <- "Foreign"
SLA_foils4[SLA_foils4$SLA_Foil=="PDS", "SLA_Foil"] <- "Peer"

view(SLA_foils4)

#Just 5-year-olds data set
#Making data frame
SLA_foils5 <- read_csv("Foil_information_SLA.csv")
SLA_foils5$Score <- NA
view(SLA_foils5)

#Dropping in the total number of 5yr children who got it correct for each SLA type
SLA_foils5$Score = ifelse((SLA_foils5$SLA_Type == "IDS" & SLA_foils5$SLA_Setting == 1), 
                          sum(as.integer(SLA_Score_5$IDS1)), 
                          SLA_foils5$Score)
SLA_foils5$Score = ifelse((SLA_foils5$SLA_Type == "IDS" & SLA_foils5$SLA_Setting == 2), 
                          sum(as.integer(SLA_Score_5$IDS2)), 
                          SLA_foils5$Score)
SLA_foils5$Score = ifelse((SLA_foils5$SLA_Type == "IDS" & SLA_foils5$SLA_Setting == 3), 
                          sum(as.integer(SLA_Score_5$IDS3)), 
                          SLA_foils5$Score)

SLA_foils5$Score = ifelse((SLA_foils5$SLA_Type == "ADS" & SLA_foils5$SLA_Setting == 1), 
                          sum(as.integer(SLA_Score_5$ADS1)), 
                          SLA_foils5$Score)
SLA_foils5$Score = ifelse((SLA_foils5$SLA_Type == "ADS" & SLA_foils5$SLA_Setting == 2), 
                          sum(as.integer(SLA_Score_5$ADS2)), 
                          SLA_foils5$Score)
SLA_foils5$Score = ifelse((SLA_foils5$SLA_Type == "ADS" & SLA_foils5$SLA_Setting == 3), 
                          sum(as.integer(SLA_Score_5$ADS3)), 
                          SLA_foils5$Score)

SLA_foils5$Score = ifelse((SLA_foils5$SLA_Type == "PDS" & SLA_foils5$SLA_Setting == 1), 
                          sum(as.integer(SLA_Score_5$PDS1)), 
                          SLA_foils5$Score)
SLA_foils5$Score = ifelse((SLA_foils5$SLA_Type == "PDS" & SLA_foils5$SLA_Setting == 2), 
                          sum(as.integer(SLA_Score_5$PDS2)), 
                          SLA_foils5$Score)
SLA_foils5$Score = ifelse((SLA_foils5$SLA_Type == "PDS" & SLA_foils5$SLA_Setting == 3), 
                          sum(as.integer(SLA_Score_5$PDS3)), 
                          SLA_foils5$Score)

SLA_foils5$Score = ifelse((SLA_foils5$SLA_Type == "FDS" & SLA_foils5$SLA_Setting == 1), 
                          sum(as.integer(SLA_Score_5$FDS1)), 
                          SLA_foils5$Score)
SLA_foils5$Score = ifelse((SLA_foils5$SLA_Type == "FDS" & SLA_foils5$SLA_Setting == 2), 
                          sum(as.integer(SLA_Score_5$FDS2)), 
                          SLA_foils5$Score)
SLA_foils5$Score = ifelse((SLA_foils5$SLA_Type == "FDS" & SLA_foils5$SLA_Setting == 3), 
                          sum(as.integer(SLA_Score_5$FDS3)), 
                          SLA_foils5$Score)

view(SLA_foils5)

#Proportion who got it correct is the number who got it correct divided by 5yr n
SLA_foils5$Score_Proportion = SLA_foils5$Score/23
view(SLA_foils5)

#renaming labels
SLA_foils5[SLA_foils5$SLA_Type=="IDS", "SLA_Type"] <- "Infant"
SLA_foils5[SLA_foils5$SLA_Type=="ADS", "SLA_Type"] <- "Adult"
SLA_foils5[SLA_foils5$SLA_Type=="FDS", "SLA_Type"] <- "Foreign"
SLA_foils5[SLA_foils5$SLA_Type=="PDS", "SLA_Type"] <- "Peer"

SLA_foils5[SLA_foils5$SLA_Foil=="IDS", "SLA_Foil"] <- "Infant"
SLA_foils5[SLA_foils5$SLA_Foil=="ADS", "SLA_Foil"] <- "Adult"
SLA_foils5[SLA_foils5$SLA_Foil=="FDS", "SLA_Foil"] <- "Foreign"
SLA_foils5[SLA_foils5$SLA_Foil=="PDS", "SLA_Foil"] <- "Peer"

view(SLA_foils5)

#SLA foil comparison graphs by age group
SLA_foils$SLA_Type <- factor(SLA_foils$SLA_Type, levels = c("Foreign", "Infant", "Peer", "Adult"))

FoilALL <- ggplot(SLA_foils, aes(x = SLA_Type, y = Score_Proportion, fill = SLA_Foil)) +
  geom_col(position = "dodge") +
  ylim(0, 1) +
  scale_fill_manual(values = c("#ea5458", "#6ba203", "#16b3b7", "#b95eff"), breaks = c("Foreign", "Infant", "Peer", "Adult")) +
  labs(title = "Foil Comparison (All Ages)", fill = "Foil Register") +
  xlab("Target Register") +
  ylab("Proportion Correct") +
  theme(text=element_text(size=12,  family="Arial"))

FoilALL

SLA_foils3$SLA_Type <- factor(SLA_foils3$SLA_Type, levels = c("Foreign", "Infant", "Peer", "Adult"))

Foil3 <- ggplot(SLA_foils3, aes(x = SLA_Type, y = Score_Proportion, fill = SLA_Foil)) +
  geom_col(position = "dodge", show.legend = FALSE) +
  ylim(0, 1) +
  scale_fill_manual(values = c("#ea5458", "#6ba203", "#16b3b7", "#b95eff")) +
  labs(title = "Foil Comparison (3-year-olds)") +
  xlab("Target Register") +
  ylab("Proportion Correct") +
  theme(text=element_text(size=12,  family="Arial"))

SLA_foils4$SLA_Type <- factor(SLA_foils4$SLA_Type, levels = c("Foreign", "Infant", "Peer", "Adult"))

Foil4 <- ggplot(SLA_foils4, aes(x = SLA_Type, y = Score_Proportion, fill = SLA_Foil)) +
  geom_col(position = "dodge", show.legend = FALSE) +
  ylim(0, 1) +
  scale_fill_manual(values = c("#ea5458", "#6ba203", "#16b3b7", "#b95eff")) +
  labs(title = "Foil Comparison (4-year-olds)") +
  xlab("Target Register") +
  ylab("Proportion Correct") +
  theme(text=element_text(size=12,  family="Arial"))

SLA_foils5$SLA_Type <- factor(SLA_foils5$SLA_Type, levels = c("Foreign", "Infant", "Peer", "Adult"))

Foil5 <- ggplot(SLA_foils5, aes(x = SLA_Type, y = Score_Proportion, fill = SLA_Foil)) +
  geom_col(position = "dodge", show.legend = FALSE) +
  ylim(0, 1) +
  scale_fill_manual(values = c("#ea5458", "#6ba203", "#16b3b7", "#b95eff")) +
  labs(title = "Foil Comparison (5- & 6-year-olds)") +
  xlab("Target Register") +
  ylab("Proportion Correct") +
  theme(text=element_text(size=12,  family="Arial"))

foils_bottom_row <- plot_grid(Foil3, Foil4, Foil5,
                              ncol = 3,
                              axis = "tblr", align = "h", 
                              rel_widths = c(1, 1, 1), rel_heights = c(.5, .5, .5),
                              labels = c("B", "C", "D"))
foils_bottom_row

all_foils_graph <- plot_grid(FoilALL, foils_bottom_row,
                             ncol = 1,
                             labels = c("A", " "))
all_foils_graph

#Model analyses
#SLA model, maybe add random slope (1+SLA_Type|ID)
data_all$Age_MoScaled <- scale(data_all$Age_Mo, center = TRUE, scale = TRUE)
view(data_all)

modelSLAOnly <- glmer(SLA_Score ~ SLA_Type * Age_MoScaled 
                + (1|ID) + (1+SLA_Type|ID), 
                family = "binomial",
                data = data_all,
                control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

tab_model(modelSLAOnly)

#Model Comparison
model1.1 <- glmer(SLA_Score ~ Age_Mo
                    + (1|ID), 
                    family = "binomial",
                    data = data_all,
                    control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

tab_model(model1.1)

model1.2 <- glmer(SLA_Score ~ Age_Mo + TOM_sum
                    + (1|ID), 
                    family = "binomial",
                    data = data_all,
                    control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

tab_model(model1.2)

model1.3 <- glmer(SLA_Score ~ Age_Mo + TOM_sum + PVT
                    + (1|ID), 
                    family = "binomial",
                    data = data_all,
                    control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

tab_model(model1.3)

model1.4 <- glmer(SLA_Score ~ Age_Mo + TOM_sum + PVT + EF
                + (1|ID), 
                family = "binomial",
                data = data_all,
                control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

tab_model(model1.4)

#model comparison, checking this out before including model 1.4
anova(model1.1, model1.2)
anova(model1.1, model1.3)
anova(model1.2, model1.3)

#it throws a fit because of the missing EF values, new data frame that excludes those
data_excludeNA <- na.omit(data_all)
view(data_excludeNA)

model2.1 <- glmer(SLA_Score ~ Age_Mo
                  + (1|ID), 
                  family = "binomial",
                  data = data_excludeNA,
                  control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

model2.2 <- glmer(SLA_Score ~ Age_Mo + TOM_sum
                  + (1|ID), 
                  family = "binomial",
                  data = data_excludeNA,
                  control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

model2.3 <- glmer(SLA_Score ~ Age_Mo + TOM_sum + PVT
                  + (1|ID), 
                  family = "binomial",
                  data = data_excludeNA,
                  control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

model2.4 <- glmer(SLA_Score ~ Age_Mo + TOM_sum + PVT + EF
                  + (1|ID), 
                  family = "binomial",
                  data = data_excludeNA,
                  control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

anova(model2.1, model2.2)
anova(model2.1, model2.3)
anova(model2.1, model2.4)

anova(model2.2, model2.3)
anova(model2.2, model2.4)

anova(model2.3, model2.4)

#Trying abandoning things to see if that improves the model
model3.1 <- glmer(SLA_Score ~ Age_Mo + PVT
                  + (1|ID),
                  family = "binomial",
                  data = data_excludeNA,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))

anova(model2.1, model3.1)

model3.2 <- glmer(SLA_Score ~ Age_Mo + EF
                  + (1|ID), 
                  family = "binomial",
                  data = data_excludeNA,
                  control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

anova(model2.1, model3.2)

model3.3 <- glmer(SLA_Score ~ Age_Mo + PVT + EF
                  + (1|ID), 
                  family = "binomial",
                  data = data_excludeNA,
                  control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

anova(model2.1, model3.3)

#Interitem correlations
df_graph1 <- subset(df_graph1, ID!= 7006)

data_correlations <- select(df_graph1, 
                            PVT, EF, SLA_Sum, TOM_sum, Age_Mo)

data_correlations$PVT <- as.numeric(data_correlations$PVT)
data_correlations$EF <- as.numeric(data_correlations$EF)
data_correlations$SLA_Sum <- as.numeric(data_correlations$SLA_Sum)
data_correlations$TOM_sum <- as.numeric(data_correlations$TOM_sum)
data_correlations$Age_Mo <- as.numeric(data_correlations$Age_Mo)

cor(data_correlations, use = "complete.obs", method = "pearson")
view(data_correlations)

tab_corr(data_correlations, show.p = TRUE, p.numeric = TRUE)

#Descriptives
data_descriptives <- select(df_graph1, 
                            PVT, EF, IDS, ADS, PDS, FDS, SLA_Sum, TOM_sum, Age_Yr, Age_Mo)

data_descriptives$PVT <- as.numeric(data_descriptives$PVT)
data_descriptives$EF <- as.numeric(data_descriptives$EF)
data_descriptives$SLA_Sum <- as.numeric(data_descriptives$SLA_Sum)
data_descriptives$TOM_sum <- as.numeric(data_descriptives$TOM_sum)
data_descriptives$Age_Mo <- as.numeric(data_descriptives$Age_Mo)
data_descriptives$IDS <- as.numeric(data_descriptives$IDS)
data_descriptives$ADS <- as.numeric(data_descriptives$ADS)
data_descriptives$FDS <- as.numeric(data_descriptives$FDS)
data_descriptives$PDS <- as.numeric(data_descriptives$PDS)

view(data_descriptives)


describeBy(data_descriptives, group = "Age_Yr", mat = TRUE)
describe(data_descriptives)

#TOM contingency tables
#Diverse Desires
DD_Pass <- select(df_graph1, ID)

DD_Pass$IDS <- ifelse(df_graph1$TOM_DD == 1, 
                  df_graph1$IDS, 
                  NA)
DD_Pass$ADS <- ifelse(df_graph1$TOM_DD == 1, 
                      df_graph1$ADS, 
                      NA)
DD_Pass$PDS <- ifelse(df_graph1$TOM_DD == 1, 
                      df_graph1$PDS, 
                      NA)
DD_Pass$FDS <- ifelse(df_graph1$TOM_DD == 1, 
                      df_graph1$FDS, 
                      NA)
describe(DD_Pass)

DD_Fail <- select(df_graph1, ID)

DD_Fail$IDS <- ifelse(df_graph1$TOM_DD == 0, 
                      df_graph1$IDS, 
                      NA)
DD_Fail$ADS <- ifelse(df_graph1$TOM_DD == 0, 
                      df_graph1$ADS, 
                      NA)
DD_Fail$PDS <- ifelse(df_graph1$TOM_DD == 0, 
                      df_graph1$PDS, 
                      NA)
DD_Fail$FDS <- ifelse(df_graph1$TOM_DD == 0, 
                      df_graph1$FDS, 
                      NA)
describe(DD_Fail)

#Diverse Beliefs
DB_Pass <- select(df_graph1, ID)

DB_Pass$IDS <- ifelse(df_graph1$TOM_DB == 1, 
                      df_graph1$IDS, 
                      NA)
DB_Pass$ADS <- ifelse(df_graph1$TOM_DB == 1, 
                      df_graph1$ADS, 
                      NA)
DB_Pass$PDS <- ifelse(df_graph1$TOM_DB == 1, 
                      df_graph1$PDS, 
                      NA)
DB_Pass$FDS <- ifelse(df_graph1$TOM_DB == 1, 
                      df_graph1$FDS, 
                      NA)
describe(DB_Pass)

DB_Fail <- select(df_graph1, ID)

DB_Fail$IDS <- ifelse(df_graph1$TOM_DB == 0, 
                      df_graph1$IDS, 
                      NA)
DB_Fail$ADS <- ifelse(df_graph1$TOM_DB == 0, 
                      df_graph1$ADS, 
                      NA)
DB_Fail$PDS <- ifelse(df_graph1$TOM_DB == 0, 
                      df_graph1$PDS, 
                      NA)
DB_Fail$FDS <- ifelse(df_graph1$TOM_DB == 0, 
                      df_graph1$FDS, 
                      NA)
describe(DB_Fail)

#Knowledge Access
KA_Pass <- select(df_graph1, ID)

KA_Pass$IDS <- ifelse(df_graph1$TOM_KA == 1, 
                      df_graph1$IDS, 
                      NA)
KA_Pass$ADS <- ifelse(df_graph1$TOM_KA == 1, 
                      df_graph1$ADS, 
                      NA)
KA_Pass$PDS <- ifelse(df_graph1$TOM_KA == 1, 
                      df_graph1$PDS, 
                      NA)
KA_Pass$FDS <- ifelse(df_graph1$TOM_KA == 1, 
                      df_graph1$FDS, 
                      NA)
describe(KA_Pass)

KA_Fail <- select(df_graph1, ID)

KA_Fail$IDS <- ifelse(df_graph1$TOM_KA == 0, 
                      df_graph1$IDS, 
                      NA)
KA_Fail$ADS <- ifelse(df_graph1$TOM_KA == 0, 
                      df_graph1$ADS, 
                      NA)
KA_Fail$PDS <- ifelse(df_graph1$TOM_KA == 0, 
                      df_graph1$PDS, 
                      NA)
KA_Fail$FDS <- ifelse(df_graph1$TOM_KA == 0, 
                      df_graph1$FDS, 
                      NA)
describe(KA_Fail)

#False Belief
FB_Pass <- select(df_graph1, ID)

FB_Pass$IDS <- ifelse(df_graph1$TOM_FB == 1, 
                      df_graph1$IDS, 
                      NA)
FB_Pass$ADS <- ifelse(df_graph1$TOM_FB == 1, 
                      df_graph1$ADS, 
                      NA)
FB_Pass$PDS <- ifelse(df_graph1$TOM_FB == 1, 
                      df_graph1$PDS, 
                      NA)
FB_Pass$FDS <- ifelse(df_graph1$TOM_FB == 1, 
                      df_graph1$FDS, 
                      NA)
describe(FB_Pass)

FB_Fail <- select(df_graph1, ID)

FB_Fail$IDS <- ifelse(df_graph1$TOM_FB == 0, 
                      df_graph1$IDS, 
                      NA)
FB_Fail$ADS <- ifelse(df_graph1$TOM_FB == 0, 
                      df_graph1$ADS, 
                      NA)
FB_Fail$PDS <- ifelse(df_graph1$TOM_FB == 0, 
                      df_graph1$PDS, 
                      NA)
FB_Fail$FDS <- ifelse(df_graph1$TOM_FB == 0, 
                      df_graph1$FDS, 
                      NA)
describe(FB_Fail)

#Real-Apparent Emotion
RAE_Pass <- select(df_graph1, ID)

RAE_Pass$IDS <- ifelse(df_graph1$TOM_RAE == 1, 
                      df_graph1$IDS, 
                      NA)
RAE_Pass$ADS <- ifelse(df_graph1$TOM_RAE == 1, 
                      df_graph1$ADS, 
                      NA)
RAE_Pass$PDS <- ifelse(df_graph1$TOM_RAE == 1, 
                      df_graph1$PDS, 
                      NA)
RAE_Pass$FDS <- ifelse(df_graph1$TOM_RAE == 1, 
                      df_graph1$FDS, 
                      NA)
describe(RAE_Pass)

RAE_Fail <- select(df_graph1, ID)

RAE_Fail$IDS <- ifelse(df_graph1$TOM_RAE == 0, 
                      df_graph1$IDS, 
                      NA)
RAE_Fail$ADS <- ifelse(df_graph1$TOM_RAE == 0, 
                      df_graph1$ADS, 
                      NA)
RAE_Fail$PDS <- ifelse(df_graph1$TOM_RAE == 0, 
                      df_graph1$PDS, 
                      NA)
RAE_Fail$FDS <- ifelse(df_graph1$TOM_RAE == 0, 
                      df_graph1$FDS, 
                      NA)
describe(RAE_Fail)

#Data without multilinguals
demographicsdata <- read.csv("ALL_Demographics.csv")
view(demographicsdata)

#Forming data frame for correlations without multilinguals
data_correlations_NoMulti1 <- merge(demographicsdata, df_graph1, by = "ID", all.x = TRUE)
view(data_correlations_NoMulti1)

data_correlations_NoMulti2 <- subset(data_correlations_NoMulti1, (Multilingual == 0 | is.na(Multilingual)))
data_correlations_NoMulti2 <- subset(data_correlations_NoMulti2, ID != 7006)
view(data_correlations_NoMulti2)

data_correlations_NoMulti3 <-select(data_correlations_NoMulti2, 
                                     PVT, EF, SLA_Sum, TOM_sum, Age_Mo)
view(data_correlations_NoMulti3)

#Redoing models without multilinguals
data_excludeMulti <- merge(EF_PVT_data_cleaned, SLA_final, by = "ID")
data_excludeMulti <- merge(data_excludeMulti, TOM_final, by = "ID", all.x = TRUE)
data_excludeMulti <- merge(data_excludeMulti, ID_ages, by = "ID", all.x = TRUE)
data_excludeMulti <- merge(data_excludeMulti, demographicsdata, by = "ID", all.x = TRUE)

data_excludeMulti$EF <- as.numeric(data_excludeMulti$EF)
data_excludeMulti$PVT <- as.numeric(data_excludeMulti$PVT)

data_excludeMulti <- subset(data_excludeMulti, ID != 7006) #exclude child who didn't finish SLA

view(data_excludeMulti)

data_excludeMultiFinal <- subset(data_excludeMulti, (Multilingual == 0 | is.na(Multilingual)))

view(data_excludeMultiFinal)

modelAgeNM <- glmer(SLA_Score ~ Age_Mo
                  + (1|ID), 
                  family = "binomial",
                  data = data_excludeMultiFinal,
                  control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

tab_model(modelAgeNM)

modelTOMNM <- glmer(SLA_Score ~ Age_Mo + TOM_sum
                  + (1|ID), 
                  family = "binomial",
                  data = data_excludeMultiFinal,
                  control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

tab_model(modelTOMNM)

modelPVTNM <- glmer(SLA_Score ~ Age_Mo + TOM_sum + PVT
                  + (1|ID), 
                  family = "binomial",
                  data = data_excludeMultiFinal,
                  control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

tab_model(modelPVTNM)

modelEFNM <- glmer(SLA_Score ~ Age_Mo + TOM_sum + PVT + EF
                  + (1|ID), 
                  family = "binomial",
                  data = data_excludeMultiFinal,
                  control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

tab_model(modelEFNM)

#Forming data frame for model comparisons without multilinguals
data_excludeNAMulti <- merge(demographicsdata, data_excludeNA, by = "ID", all.x = TRUE)

data_excludeNAMulti <- subset(data_excludeNAMulti, ID != 7006) #exclude child who didn't finish SLA
data_excludeNAMulti <- subset(data_excludeNAMulti, ID != 1028) #exclude children who didn't finish EF
data_excludeNAMulti <- subset(data_excludeNAMulti, ID != 7010)
data_excludeNAMulti <- subset(data_excludeNAMulti, ID != 1043)

view(data_excludeNAMulti)

data_excludeNAMultiFinal <- subset(data_excludeNAMulti, (Multilingual == 0 | is.na(Multilingual)))

view(data_excludeNAMultiFinal)

#Redoing model comparisons without multilinguals
model4.1 <- glmer(SLA_Score ~ Age_Mo
                  + (1|ID), 
                  family = "binomial",
                  data = data_excludeNAMultiFinal,
                  control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

model4.2 <- glmer(SLA_Score ~ Age_Mo + TOM_sum
                  + (1|ID), 
                  family = "binomial",
                  data = data_excludeNAMultiFinal,
                  control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

model4.3 <- glmer(SLA_Score ~ Age_Mo + TOM_sum + PVT
                  + (1|ID), 
                  family = "binomial",
                  data = data_excludeNAMultiFinal,
                  control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

model4.4 <- glmer(SLA_Score ~ Age_Mo + TOM_sum + PVT + EF
                  + (1|ID), 
                  family = "binomial",
                  data = data_excludeNAMultiFinal,
                  control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

anova(model4.1, model4.2)
anova(model4.1, model4.3)
anova(model4.1, model4.4)

#Abandoning parts of the models without multilinguals
model5.1 <- glmer(SLA_Score ~ Age_Mo + PVT
                  + (1|ID),
                  family = "binomial",
                  data = data_excludeNAMultiFinal,
                  control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun=2e5)))

anova(model4.1, model5.1)

model5.2 <- glmer(SLA_Score ~ Age_Mo + EF
                  + (1|ID), 
                  family = "binomial",
                  data = data_excludeNAMultiFinal,
                  control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

anova(model4.1, model5.2)

model5.3 <- glmer(SLA_Score ~ Age_Mo + PVT + EF
                  + (1|ID), 
                  family = "binomial",
                  data = data_excludeNAMultiFinal,
                  control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

anova(model4.1, model5.3)

#TOM false-belief exploratory analysis
data_all$TOM_FB <- as.factor(data_all$TOM_FB)
data_all$TOM_FB <- relevel(data_all$TOM_FB, ref = "0") #reference level is failing

modelFB <- glmer(SLA_Score ~ Age_Mo + TOM_FB
                  + (1|ID), 
                  family = "binomial",
                  data = data_all,
                  control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

tab_model(modelFB)

anova(model1.2, modelFB) #compare with best model in case the AIC is lower

#TOM false-belief exploratory analysis
data_all$TOM_RAE <- as.factor(data_all$TOM_RAE)
data_all$TOM_RAE <- relevel(data_all$TOM_RAE, ref = "0") #reference level is failing

modelRAE <- glmer(SLA_Score ~ Age_Mo + TOM_RAE
                 + (1|ID), 
                 family = "binomial",
                 data = data_all,
                 control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

tab_model(modelRAE)

anova(model1.2, modelRAE) #compare with best model in case the AIC is lower

