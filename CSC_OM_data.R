#Authors: Elizabeth Kolln
#Last edited by: Elizabeth Kolln 01/02/2025

#install.packages("tidyverse")
#install.packages("dplyr")
#install.packages("readr")
#install.packages("tidyr")
#install.packages("lme4")
#install.packages("sjPlot")

library(tidyverse)
library(dplyr)
library(readr)
library(tidyr)
library(lme4)
library(sjPlot)

setwd("~/Desktop/wellesley/thesis/final_data")

###importing the TOM data###
TOM_data <- read_csv("CSC_OM_TOM.csv")

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

#final data frame
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
SLA_data <- read.csv("CSC_OM_SLA.csv")

View(SLA_data)

#removing unnecessary information
SLA_cleaned <- select(SLA_data,
                      ID,
                      IDS1 = Baby..Hallway,
                      ADS1 = Teacher..Hallway,
                      PDS1 = Peer,
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
EF_PVT_data <- read.csv("CSC_OM_EF_PVT.csv")

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

#adding ages to IDs
ID_ages <- read.csv("CSC_OM_Ages.csv")

#merging the data
data_all <- merge(EF_PVT_data_cleaned, SLA_final, by = "ID", all.x = TRUE)
data_all <- merge(data_all, TOM_final, by = "ID", all.x = TRUE)
data_all <- merge(data_all, ID_ages, by = "ID", all.x = TRUE)

View(data_all)

str(data_all)

#building the model, maybe add random slope (1+SLA_Type|ID)
model1 <- glmer(SLA_Score ~ SLA_Type * Age_Mo 
                + (1|ID), 
                family = "binomial",
                data = data_all,
                control = glmerControl(optimizer="bobyqa",optCtrl=list(maxfun=2e5)))

tab_model(model1)




