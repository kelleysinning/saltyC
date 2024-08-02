# Updated >1 mm biomass data for Kelley Sinning Salty Carbon Data
# Adpated from 

rm(list=ls())  # clears workspace                     

#load important packages##
library(ggplot2)
library(gridExtra)
library(viridis)
library(ggthemes)
library(dplyr)
library(tidyverse)
library(RColorBrewer)
library(rcartocolor)
library(dplyr)
library(purrr)

# Setting the working directory to "SUMMARY SHEETS" where I store my biomass
setwd("~/Library/CloudStorage/GoogleDrive-ksinning@vt.edu/My Drive/Data/saltyC/SUMMARY SHEETS")

# Ok, bring in the csv summary sheets for each site and month
# September monthly
EASsept=read.csv("EAS_Sept.2023_SUMMARY.csv")
FRYsept=read.csv("FRY_Sept.2023_SUMMARY.csv")
RICsept=read.csv("RIC_Sept.2023_SUMMARY.csv")
# October quarterly
CROoct=read.csv("CRO_Oct.2023_SUMMARY.csv")
EASoct=read.csv("EAS_Oct.2023_SUMMARY.csv")
HCNoct=read.csv("HCN_Oct.2023_SUMMARY.csv")
FRYoct=read.csv("FRY_Oct.2023_SUMMARY.csv")
HURoct=read.csv("HUR_Oct.2023_SUMMARY.csv")
RUToct=read.csv("RUT_Oct.2023_SUMMARY.csv")
LLWoct=read.csv("LLW_Oct.2023_SUMMARY.csv")
LLCoct=read.csv("LLC_Oct.2023_SUMMARY.csv")
RICoct=read.csv("RIC_Oct.2023_SUMMARY.csv")
# November monthly
EASnov=read.csv("EAS_Nov.2023_SUMMARY.csv")
FRYnov=read.csv("FRY_Nov.2023_SUMMARY.csv")
RICnov=read.csv("RIC_Nov.2023_SUMMARY.csv")
# December monthly
EASdec=read.csv("EAS_Dec.2023_SUMMARY.csv")
FRYdec=read.csv("FRY_Dec.2023_SUMMARY.csv")
RICdec=read.csv("RIC_Dec.2023_SUMMARY.csv")
# January monthly
EASjan=read.csv("EAS_Jan.2024_SUMMARY.csv")
FRYjan=read.csv("FRY_Jan.2024_SUMMARY.csv")
RICjan=read.csv("RIC_Jan.2024_SUMMARY.csv")
# February quarterly
CROfeb=read.csv("CRO_Feb.2024_SUMMARY.csv")
EASfeb=read.csv("EAS_Feb.2024_SUMMARY.csv")
HCNfeb=read.csv("HCN_Feb.2024_SUMMARY.csv")
FRYfeb=read.csv("FRY_Feb.2024_SUMMARY.csv")
HURfeb=read.csv("HUR_Feb.2024_SUMMARY.csv")
RUTfeb=read.csv("RUT_Feb.2024_SUMMARY.csv")
LLWfeb=read.csv("LLW_Feb.2024_SUMMARY.csv")
LLCfeb=read.csv("LLC_Feb.2024_SUMMARY.csv")
RICfeb=read.csv("RIC_Feb.2024_SUMMARY.csv")
# March monthly
EASmarch=read.csv("EAS_March.2024_SUMMARY.csv")
FRYmarch=read.csv("FRY_March.2024_SUMMARY.csv")
RICmarch=read.csv("RIC_March.2024_SUMMARY.csv")
# April monthly
EASapril=read.csv("EAS_April.2024_SUMMARY.csv")
FRYapril=read.csv("FRY_April.2024_SUMMARY.csv")
RICapril=read.csv("RIC_April.2024_SUMMARY.csv")
# May quarterly
CROmay=read.csv("CRO_May.2024_SUMMARY.csv")
EASmay=read.csv("EAS_May.2024_SUMMARY.csv")
HCNmay=read.csv("HCN_May.2024_SUMMARY.csv")
FRYmay=read.csv("FRY_May.2024_SUMMARY.csv")
HURmay=read.csv("HUR_May.2024_SUMMARY.csv")
RUTmay=read.csv("RUT_May.2024_SUMMARY.csv")
LLWmay=read.csv("LLW_May.2024_SUMMARY.csv")
LLCmay=read.csv("LLC_May.2024_SUMMARY.csv")
RICmay=read.csv("RIC_May.2024_SUMMARY.csv")
# June monthly
EASjune=read.csv("EAS_June.2024_SUMMARY.csv")
FRYjune=read.csv("FRY_June.2024_SUMMARY.csv")
RICjune=read.csv("RIC_June.2024_SUMMARY.csv")
# July monthly
EASjuly=read.csv("EAS_July.2024_SUMMARY.csv")
FRYjuly=read.csv("FRY_July.2024_SUMMARY.csv")
RICjuly=read.csv("RIC_July.2024_SUMMARY.csv")




# Adding a column with site name bc once we merge we won't know
# September monthly
EASsept$Site <- c("EAS")
FRYsept$Site <- c("FRY")
RICsept$Site <- c("RIC")
# October quarterly
CROoct$Site <- c("CRO")
EASoct$Site <- c("EAS")
HCNoct$Site <- c("HCN")
FRYoct$Site <- c("FRY")
HURoct$Site <- c("HUR")
RUToct$Site <- c("RUT")
LLWoct$Site <- c("LLW")
LLCoct$Site <- c("LLC")
RICoct$Site <- c("RIC")
# November monthly
EASnov$Site <- c("EAS")
FRYnov$Site <- c("FRY")
RICnov$Site <- c("RIC")
# December monthly
EASdec$Site <- c("EAS")
FRYdec$Site <- c("FRY")
RICdec$Site <- c("RIC")
# January monthly
EASjan$Site <- c("EAS")
FRYjan$Site <- c("FRY")
RICjan$Site <- c("RIC")
# February quarterly
CROfeb$Site <- c("CRO")
EASfeb$Site <- c("EAS")
HCNfeb$Site <- c("HCN")
FRYfeb$Site <- c("FRY")
HURfeb$Site <- c("HUR")
RUTfeb$Site <- c("RUT")
LLWfeb$Site <- c("LLW")
LLCfeb$Site <- c("LLC")
RICfeb$Site <- c("RIC")
# March monthly
EASmarch$Site <- c("EAS")
FRYmarch$Site <- c("FRY")
RICmarch$Site <- c("RIC")
# April monthly
EASapril$Site <- c("EAS")
FRYapril$Site <- c("FRY")
RICapril$Site <- c("RIC")
# May quarterly
CROmay$Site <- c("CRO")
EASmay$Site <- c("EAS")
HCNmay$Site <- c("HCN")
FRYmay$Site <- c("FRY")
HURmay$Site <- c("HUR")
RUTmay$Site <- c("RUT")
LLWmay$Site <- c("LLW")
LLCmay$Site <- c("LLC")
RICmay$Site <- c("RIC")
# June monthly
EASjune$Site <- c("EAS")
FRYjune$Site <- c("FRY")
RICjune$Site <- c("RIC")
# July monthly
EASjuly$Site <- c("EAS")
FRYjuly$Site <- c("FRY")
RICjuly$Site <- c("RIC")

# Adding a column with sample month so the dates aren't weird
# September monthly
EASsept$Sample.Month <- c("September")
FRYsept$Sample.Month <- c("September")
RICsept$Sample.Month <- c("September")
# October quarterly
CROoct$Sample.Month <- c("October")
EASoct$Sample.Month <- c("October")
HCNoct$Sample.Month <- c("October")
FRYoct$Sample.Month <- c("October")
HURoct$Sample.Month <- c("October")
RUToct$Sample.Month <- c("October")
LLWoct$Sample.Month <- c("October")
LLCoct$Sample.Month <- c("October")
RICoct$Sample.Month <- c("October")
# November monthly
EASnov$Sample.Month <- c("November")
FRYnov$Sample.Month <- c("November")
RICnov$Sample.Month <- c("November")
# December monthly
EASdec$Sample.Month <- c("December")
FRYdec$Sample.Month <- c("December")
RICdec$Sample.Month <- c("December")
# January monthly
EASjan$Sample.Month <- c("January")
FRYjan$Sample.Month <- c("January")
RICjan$Sample.Month <- c("January")
# February quarterly
CROfeb$Sample.Month <- c("February")
EASfeb$Sample.Month <- c("February")
HCNfeb$Sample.Month <- c("February")
FRYfeb$Sample.Month <- c("February")
HURfeb$Sample.Month <- c("February")
RUTfeb$Sample.Month <- c("February")
LLWfeb$Sample.Month <- c("February")
LLCfeb$Sample.Month <- c("February")
RICfeb$Sample.Month <- c("February")
# March monthly
EASmarch$Sample.Month <- c("March")
FRYmarch$Sample.Month <- c("March")
RICmarch$Sample.Month <- c("March")
# April monthly
EASapril$Sample.Month <- c("April")
FRYapril$Sample.Month <- c("April")
RICapril$Sample.Month <- c("April")
# May quarterly
CROmay$Sample.Month <- c("May")
EASmay$Sample.Month <- c("May")
HCNmay$Sample.Month <- c("May")
FRYmay$Sample.Month <- c("May")
HURmay$Sample.Month <- c("May")
RUTmay$Sample.Month <- c("May")
LLWmay$Sample.Month <- c("May")
LLCmay$Sample.Month <- c("May")
RICmay$Sample.Month <- c("May")
# June monthly
EASjune$Sample.Month <- c("June")
FRYjune$Sample.Month <- c("June")
RICjune$Sample.Month <- c("June")
# July monthly
EASjuly$Sample.Month <- c("July")
FRYjuly$Sample.Month <- c("July")
RICjuly$Sample.Month <- c("July")


# Adding a column with SC level bc once we merge we won't know
# September monthly
EASsept$SC.Level <- c("25")
FRYsept$SC.Level <- c("402")
RICsept$SC.Level <- c("1457")
# October quarterly
CROoct$SC.Level <- c("72")
EASoct$SC.Level <- c("25")
HCNoct$SC.Level <- c("78")
FRYoct$SC.Level <- c("402")
HURoct$SC.Level <- c("387")
RUToct$SC.Level <- c("594")
LLWoct$SC.Level <- c("1119")
LLCoct$SC.Level <- c("1242")
RICoct$SC.Level <- c("1457")
# November monthly
EASnov$SC.Level <- c("25")
FRYnov$SC.Level <- c("402")
RICnov$SC.Level <- c("1457")
# December monthly
EASdec$SC.Level <- c("25")
FRYdec$SC.Level <- c("402")
RICdec$SC.Level <- c("1457")
# January monthly
EASjan$SC.Level <- c("25")
FRYjan$SC.Level <- c("402")
RICjan$SC.Level <- c("1457")
# February quarterly
CROfeb$SC.Level <- c("72")
EASfeb$SC.Level <- c("25")
HCNfeb$SC.Level <- c("78")
FRYfeb$SC.Level <- c("402")
HURfeb$SC.Level <- c("387")
RUTfeb$SC.Level <- c("594")
LLWfeb$SC.Level <- c("1119")
LLCfeb$SC.Level <- c("1242")
RICfeb$SC.Level <- c("1457")
# March monthly
EASmarch$SC.Level <- c("25")
FRYmarch$SC.Level <- c("402")
RICmarch$SC.Level <- c("1457")
# April monthly
EASapril$SC.Level <- c("25")
FRYapril$SC.Level <- c("402")
RICapril$SC.Level <- c("1457")
# May quarterly
CROmay$SC.Level <- c("72")
EASmay$SC.Level <- c("25")
HCNmay$SC.Level <- c("78")
FRYmay$SC.Level <- c("402")
HURmay$SC.Level <- c("387")
RUTmay$SC.Level <- c("594")
LLWmay$SC.Level <- c("1119")
LLCmay$SC.Level <- c("1242")
RICmay$SC.Level <- c("1457")
# June monthly
EASjune$SC.Level <- c("25")
FRYjune$SC.Level <- c("402")
RICjune$SC.Level <- c("1457")
# July monthly
EASjuly$SC.Level <- c("25")
FRYjuly$SC.Level <- c("402")
RICjuly$SC.Level <- c("1457")



# Adding a column with SC Category bc once we merge we won't know
# September monthly
EASsept$SC.Category <- c("REF")
FRYsept$SC.Category <- c("MID")
RICsept$SC.Category <- c("HIGH")
# October quarterly
CROoct$SC.Category <- c("REF")
EASoct$SC.Category <- c("REF")
HCNoct$SC.Category <- c("REF")
FRYoct$SC.Category <- c("MID")
HURoct$SC.Category <- c("MID")
RUToct$SC.Category <- c("MID")
LLWoct$SC.Category <- c("HIGH")
LLCoct$SC.Category <- c("HIGH")
RICoct$SC.Category <- c("HIGH")
# November monthly
EASnov$SC.Category <- c("REF")
FRYnov$SC.Category <- c("MID")
RICnov$SC.Category <- c("HIGH")
# December monthly
EASdec$SC.Category <- c("REF")
FRYdec$SC.Category <- c("MID")
RICdec$SC.Category <- c("HIGH")
# January monthly
EASjan$SC.Category <- c("REF")
FRYjan$SC.Category <- c("MID")
RICjan$SC.Category <- c("HIGH")
# February quarterly
CROfeb$SC.Category <- c("REF")
EASfeb$SC.Category <- c("REF")
HCNfeb$SC.Category <- c("REF")
FRYfeb$SC.Category <- c("MID")
HURfeb$SC.Category <- c("MID")
RUTfeb$SC.Category <- c("MID")
LLWfeb$SC.Category <- c("HIGH")
LLCfeb$SC.Category <- c("HIGH")
RICfeb$SC.Category <- c("HIGH")
# March monthly
EASmarch$SC.Category <- c("REF")
FRYmarch$SC.Category <- c("MID")
RICmarch$SC.Category <- c("HIGH")
# April monthly
EASapril$SC.Category <- c("REF")
FRYapril$SC.Category <- c("MID")
RICapril$SC.Category <- c("HIGH")
# May quarterly
CROmay$SC.Category <- c("REF")
EASmay$SC.Category <- c("REF")
HCNmay$SC.Category <- c("REF")
FRYmay$SC.Category <- c("MID")
HURmay$SC.Category <- c("MID")
RUTmay$SC.Category <- c("MID")
LLWmay$SC.Category <- c("HIGH")
LLCmay$SC.Category <- c("HIGH")
RICmay$SC.Category <- c("HIGH")
# June monthly
EASjune$SC.Category <- c("REF")
FRYjune$SC.Category <- c("MID")
RICjune$SC.Category <- c("HIGH")
# July monthly
EASjuly$SC.Category <- c("REF")
FRYjuly$SC.Category <- c("MID")
RICjuly$SC.Category <- c("HIGH")


# Let's re-arrange the columns so these new additions are at the front
# September monthly
EASsept<- EASsept %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
FRYsept<- FRYsept %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
RICsept<- RICsept %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
# October quarterly
CROoct <- CROoct %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
EASoct <- EASoct %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
HCNoct <- HCNoct %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
FRYoct <- FRYoct %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
HURoct <- HURoct %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
RUToct <- RUToct %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
RICoct <- RICoct %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
LLCoct <- LLCoct %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
LLWoct <- LLWoct %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
# November monthly
EASnov<- EASnov %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
FRYnov<- FRYnov %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
RICnov<- RICnov %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
# December monthly
EASdec<- EASdec %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
FRYdec<- FRYdec %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
RICdec<- RICdec %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
# January monthly
EASjan<- EASjan %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
FRYjan<- FRYjan %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
RICjan<- RICjan %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
# February quarterly
CROfeb<- CROfeb %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
EASfeb<- EASfeb %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
HCNfeb<- HCNfeb %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
FRYfeb<- FRYfeb %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
HURfeb<- HURfeb %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
RUTfeb<- RUTfeb %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
LLWfeb<- LLWfeb %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
LLCfeb<- LLCfeb %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
RICfeb<- RICfeb %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
# March monthly
EASmarch<- EASmarch %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
FRYmarch<- FRYmarch %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
RICmarch<- RICmarch %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
# April monthly
EASapril<- EASapril %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
FRYapril<- FRYapril %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
RICapril<- RICapril %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
# May quarterly
CROmay<- CROmay %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
EASmay<- EASmay %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
HCNmay<- HCNmay %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
FRYmay<- FRYmay %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
HURmay<- HURmay %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
RUTmay<- RUTmay %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
LLWmay<- LLWmay %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
LLCmay<- LLCmay %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
RICmay<- RICmay %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
# June monthly
EASjune<- EASjune %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
FRYjune<- FRYjune %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
RICjune<- RICjune %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
# July monthly
EASjuly<- EASjuly %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
FRYjuly<- FRYjuly %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())
RICjuly<- RICjuly %>% 
  select(c("Site","SC.Category","SC.Level","Sample.Date","Fraction",
           "Replicate","Order","Family","Genus"), everything())



# Exclude all the abundance and biomass, only totals and descriptors
columns_to_keep <- c("Sample.Month", "Sample.Date", "Site", "SC.Level", "SC.Category", "Fraction", "Replicate", "Order", "Family", "Genus","Abundance","Density","Biomass")  # Columns to keep

# September monthly
EASsept.totals <- select(EASsept, all_of(columns_to_keep))
FRYsept.totals <- select(FRYsept, all_of(columns_to_keep))
RICsept.totals <- select(RICsept, all_of(columns_to_keep))
# October quarterly
CROoct.totals <- select(CROoct, all_of(columns_to_keep))
EASoct.totals <- select(EASoct, all_of(columns_to_keep))
HCNoct.totals <- select(HCNoct, all_of(columns_to_keep))
FRYoct.totals <- select(FRYoct, all_of(columns_to_keep))
HURoct.totals <- select(HURoct, all_of(columns_to_keep))
RUToct.totals <- select(RUToct, all_of(columns_to_keep))
LLWoct.totals <- select(LLWoct, all_of(columns_to_keep))
LLCoct.totals <- select(LLCoct, all_of(columns_to_keep))
RICoct.totals <- select(RICoct, all_of(columns_to_keep))
# November monthly
EASnov.totals <- select(EASnov, all_of(columns_to_keep))
FRYnov.totals <- select(FRYnov, all_of(columns_to_keep))
RICnov.totals <- select(RICnov, all_of(columns_to_keep))
# December monthly
EASdec.totals <- select(EASdec, all_of(columns_to_keep))
FRYdec.totals <- select(FRYdec, all_of(columns_to_keep))
RICdec.totals <- select(RICdec, all_of(columns_to_keep))
# January monthly
EASjan.totals <- select(EASjan, all_of(columns_to_keep))
FRYjan.totals <- select(FRYjan, all_of(columns_to_keep))
RICjan.totals <- select(RICjan, all_of(columns_to_keep))
# February quarterly
CROfeb.totals <- select(CROfeb, all_of(columns_to_keep))
EASfeb.totals <- select(EASfeb, all_of(columns_to_keep))
HCNfeb.totals <- select(HCNfeb, all_of(columns_to_keep))
FRYfeb.totals <- select(FRYfeb, all_of(columns_to_keep))
HURfeb.totals <- select(HURfeb, all_of(columns_to_keep))
RUTfeb.totals <- select(RUTfeb, all_of(columns_to_keep))
LLWfeb.totals <- select(LLWfeb, all_of(columns_to_keep))
LLCfeb.totals <- select(LLCfeb, all_of(columns_to_keep))
RICfeb.totals <- select(RICfeb, all_of(columns_to_keep))
# March monthly
EASmarch.totals <- select(EASmarch, all_of(columns_to_keep))
FRYmarch.totals <- select(FRYmarch, all_of(columns_to_keep))
RICmarch.totals <- select(RICmarch, all_of(columns_to_keep))
# April monthly
EASapril.totals <- select(EASapril, all_of(columns_to_keep))
FRYapril.totals <- select(FRYapril, all_of(columns_to_keep))
RICapril.totals <- select(RICapril, all_of(columns_to_keep))
# May quarterly
CROmay.totals <- select(CROmay, all_of(columns_to_keep))
EASmay.totals <- select(EASmay, all_of(columns_to_keep))
HCNmay.totals <- select(HCNmay, all_of(columns_to_keep))
FRYmay.totals <- select(FRYmay, all_of(columns_to_keep))
HURmay.totals <- select(HURmay, all_of(columns_to_keep))
RUTmay.totals <- select(RUTmay, all_of(columns_to_keep))
LLWmay.totals <- select(LLWmay, all_of(columns_to_keep))
LLCmay.totals <- select(LLCmay, all_of(columns_to_keep))
RICmay.totals <- select(RICmay, all_of(columns_to_keep))
# June monthly
EASjune.totals <- select(EASjune, all_of(columns_to_keep))
FRYjune.totals <- select(FRYjune, all_of(columns_to_keep))
RICjune.totals <- select(RICjune, all_of(columns_to_keep))
# July monthly
EASjuly.totals <- select(EASjuly, all_of(columns_to_keep))
FRYjuly.totals <- select(FRYjuly, all_of(columns_to_keep))
RICjuly.totals <- select(RICjuly, all_of(columns_to_keep))



# Merge data frames based on trimmed columns (the totals and descriptors)
list_of_greater_totals <- list(EASsept.totals, FRYsept.totals,RICsept.totals,
                           CROoct.totals, EASoct.totals,HCNoct.totals, FRYoct.totals,
                           HURoct.totals, RUToct.totals, LLWoct.totals, LLCoct.totals,
                           RICoct.totals, EASnov.totals, FRYnov.totals,RICnov.totals,
                           EASdec.totals, FRYdec.totals,RICdec.totals,EASjan.totals, 
                           FRYjan.totals,RICjan.totals,CROfeb.totals, EASfeb.totals,
                           HCNfeb.totals, FRYfeb.totals, HURfeb.totals, RUTfeb.totals, 
                           LLWfeb.totals, LLCfeb.totals, RICfeb.totals,EASmarch.totals, 
                           FRYmarch.totals,RICmarch.totals,EASapril.totals, FRYapril.totals,
                           RICapril.totals,CROmay.totals, EASmay.totals,HCNmay.totals, 
                           FRYmay.totals, HURmay.totals, RUTmay.totals, LLWmay.totals, LLCmay.totals,
                           RICmay.totals,EASjune.totals, FRYjune.totals,RICjune.totals, 
                           EASjuly.totals, FRYjuly.totals,RICjuly.totals) 

greaterbiomass <- do.call(rbind, list_of_greater_totals) #THIS WORKS


# Rarranging order of columns
greaterbiomass <- select(greaterbiomass,Site,SC.Category,SC.Level,Sample.Month,
                        Sample.Date,Fraction,Replicate,Order,Family,Genus,Abundance,
                        Density,Biomass)


# Cleaning up data sheet

greaterbiomass <- greaterbiomass %>%
  mutate(Genus = ifelse(Genus == "Stylogomphurus", "Stylogomphus", Genus))
greaterbiomass <- greaterbiomass %>%
  mutate(Genus = ifelse(Genus == "Hydratophylax", "Hydatophylax", Genus))
greaterbiomass <- greaterbiomass %>%
  mutate(Genus = ifelse(Genus == "Oulimnius(L)", "Oulimnius", Genus))
greaterbiomass <- greaterbiomass %>%
  mutate(Genus = ifelse(Genus == "Optioservus(L)", "Optioservus", Genus))
greaterbiomass <- greaterbiomass %>%
  mutate(Genus = ifelse(Genus == "Stenelmis (L)", "Stenelmis", Genus))
greaterbiomass <- greaterbiomass %>%
  mutate(Genus = ifelse(Genus == "Paraleptophlebiidae", "Paraleptophlebia", Genus))
greaterbiomass <- greaterbiomass %>%
  mutate(Fraction = ifelse(Fraction == ">1", "> 1", Fraction))
greaterbiomass <- greaterbiomass %>%
  mutate(Biomass = ifelse(Genus == "Acroneuria", Biomass / 10, Biomass)) # realized i had
# decimal in wrong place and needed to move one spot to the left
greaterbiomass <- greaterbiomass %>%
  mutate(Biomass = ifelse(Genus == "Paraleptophlebia", Biomass / 10, Biomass))
greaterbiomass <- greaterbiomass %>%
  mutate(Biomass = ifelse(Genus == "Probezzia", Biomass / 10, Biomass))

# Finally, let's add a new column to correct biomass by area
greaterbiomass <- greaterbiomass %>% mutate(Biomass.Area.Corrected = Biomass*Density)

#------------------------------------------------------
# Adding FFGs...this will become relevant later

# Note: Some genus will be family, this is so that we can analyze the biomass still 
# with the genus column, may go in later and say ex. all baetiae are baetis
# Note: No adult or pupa included
# Note: For family, best FFG guess made despite broad generalization


greaterbiomass$FFG[greaterbiomass$Genus=="Acerpenna"] ="Collector-Gatherer"
greaterbiomass$FFG[greaterbiomass$Genus=="Acentrella"] = "Collector-Gatherer"
greaterbiomass$FFG[greaterbiomass$Genus=="Acroneuria"]="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Allocapnia"]="Shredder"
greaterbiomass$FFG[greaterbiomass$Genus=="Allognasta"]="Collector-Gatherer"
greaterbiomass$FFG[greaterbiomass$Genus=="Alloperla"]="Collector-Gatherer"
greaterbiomass$FFG[greaterbiomass$Genus=="Ameletus"]="Collector-Gatherer"
greaterbiomass$FFG[greaterbiomass$Genus=="Amphinemura"]="Shredder"
greaterbiomass$FFG[greaterbiomass$Genus=="Antocha"]="Collector-Gatherer"
greaterbiomass$FFG[greaterbiomass$Genus=="Atherix"]="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Attenella"]="Collector-Gatherer"
greaterbiomass$FFG[greaterbiomass$Genus=="Baetidae"]="Collector-Gatherer"
greaterbiomass$FFG[greaterbiomass$Genus=="Baetis"]="Collector-Gatherer"
greaterbiomass$FFG[greaterbiomass$Genus=="Baetisca"]="Collector-Gatherer"
greaterbiomass$FFG[greaterbiomass$Genus=="Boyeria"]="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Calopteryx"]=="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Capniidae"]="Shredder"
greaterbiomass$FFG[greaterbiomass$Genus=="Ceratopogonidae"]="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Cernotina"]="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Chauloides"]="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Chelifera"]="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Chimarra"]="Collector-Filterer"
greaterbiomass$FFG[greaterbiomass$Genus=="Cheumatopsyche"]="Collector-Filterer"
greaterbiomass$FFG[greaterbiomass$Genus=="Chironomidae"]="Collector-Gatherer"
greaterbiomass$FFG[greaterbiomass$Genus=="Chironomini"]="Collector-Gatherer"
greaterbiomass$FFG[greaterbiomass$Genus=="Circulionidae"]="Scraper"
greaterbiomass$FFG[greaterbiomass$Genus=="Collembola"]="Collector-Gatherer"
greaterbiomass$FFG[greaterbiomass$Genus=="Cyrnellus"]="Collector-Filterer"
greaterbiomass$FFG[greaterbiomass$Genus=="Dicranota"]="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Diplectrona"]="Collector-Filterer"
greaterbiomass$FFG[greaterbiomass$Genus=="Discocerina"]="Collector-Gatherer"
greaterbiomass$FFG[greaterbiomass$Genus=="Dixa"]="Collector-Gatherer"
greaterbiomass$FFG[greaterbiomass$Genus=="Dixella"]="Collector-Gatherer"
greaterbiomass$FFG[greaterbiomass$Genus=="Dolophilodes"]="Collector-Filterer"
greaterbiomass$FFG[greaterbiomass$Genus=="Ectopria"]="Scraper"
greaterbiomass$FFG[greaterbiomass$Genus=="Eloeophila"]="Shredder"
greaterbiomass$FFG[greaterbiomass$Genus=="Epeorus"]="Scraper"
greaterbiomass$FFG[greaterbiomass$Genus=="Eriopterini"]="Collector-Gatherer"
greaterbiomass$FFG[greaterbiomass$Genus=="Ephemera"]="Collector-Gatherer"
greaterbiomass$FFG[greaterbiomass$Genus=="Ephemerellidae"]="Collector-Gatherer"
greaterbiomass$FFG[greaterbiomass$Genus=="Eurylophella"]="Collector-Gatherer"
greaterbiomass$FFG[greaterbiomass$Genus=="Gerris"]="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Glossosoma"]="Scraper"
greaterbiomass$FFG[greaterbiomass$Genus=="Goera"]="Scraper"
greaterbiomass$FFG[greaterbiomass$Genus=="Gomphus"]="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Gomphurus"]="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Gyrinus"]="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Helichus"]="Scraper"
greaterbiomass$FFG[greaterbiomass$Genus=="Hemiptera"]="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Heptageniidae"]="Scraper"
greaterbiomass$FFG[greaterbiomass$Genus=="Hetaerina"]="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Hexatoma"]="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Hydrachnia"]="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Hydatophylax"]="Shredder"
greaterbiomass$FFG[greaterbiomass$Genus=="Hydropsyche"]="Collector-Filterer"
greaterbiomass$FFG[greaterbiomass$Genus=="Isonychia"]="Collector-Filterer"
greaterbiomass$FFG[greaterbiomass$Genus=="Isoperla"]="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Langessa"]="Scraper"
greaterbiomass$FFG[greaterbiomass$Genus=="Lanthus"]="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Leptophlebiidae"]="Collector-Gatherer"
greaterbiomass$FFG[greaterbiomass$Genus=="Lepidostoma"]= "Shredder"
greaterbiomass$FFG[greaterbiomass$Genus=="Leuctra"]="Shredder"
greaterbiomass$FFG[greaterbiomass$Genus=="Leuctridae"]="Shredder"
greaterbiomass$FFG[greaterbiomass$Genus=="Limnephilidae"]="Shredder"
greaterbiomass$FFG[greaterbiomass$Genus=="Limnophila"]= "Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Limoniidae"]= "Shredder"
greaterbiomass$FFG[greaterbiomass$Genus=="Lypodiversa"]="Collector-Filterer"
greaterbiomass$FFG[greaterbiomass$Genus=="Micrasema"]="Shredder"
greaterbiomass$FFG[greaterbiomass$Genus=="Microvelia"]="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Molophilus"]="Shredder"
greaterbiomass$FFG[greaterbiomass$Genus=="Neocleon"]="Collector-Gatherer"
greaterbiomass$FFG[greaterbiomass$Genus=="Neophylax"]="Scraper"
greaterbiomass$FFG[greaterbiomass$Genus=="Neoplasta"]="Collector-Gatherer"
greaterbiomass$FFG[greaterbiomass$Genus=="Nigronia"]="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Oligochaeta"]="Collector-Gatherer"
greaterbiomass$FFG[greaterbiomass$Genus=="Optioservus"]="Scraper"
greaterbiomass$FFG[greaterbiomass$Genus=="Oreogeton"]="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Orthocladine"]="Collector-Gatherer"
greaterbiomass$FFG[greaterbiomass$Genus=="Oulimnius"]="Scraper"
greaterbiomass$FFG[greaterbiomass$Genus=="Paracapnia"]="Shredder"
greaterbiomass$FFG[greaterbiomass$Genus=="Paraleptophlebiidae"]="Collector-Gatherer"
greaterbiomass$FFG[greaterbiomass$Genus=="Polycentropodidae"]="Collector-Filterer"
greaterbiomass$FFG[greaterbiomass$Genus=="Polycentropus"]="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Probezzia"]="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Prodaticus"]="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Prosimulium"]="Collector-Filterer"
greaterbiomass$FFG[greaterbiomass$Genus=="Psephenus"]="Scraper"
greaterbiomass$FFG[greaterbiomass$Genus=="Pseudolimnophila"]="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Psychodini"]="Collector-Gatherer"
greaterbiomass$FFG[greaterbiomass$Genus=="Pteronarcys"]="Shredder"
greaterbiomass$FFG[greaterbiomass$Genus=="Pycnopsyche"]="Shredder"
greaterbiomass$FFG[greaterbiomass$Genus=="Remenus"]="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Rhagovelia"]="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Rhyacophila"]="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Prostoia"]="Shredder"
greaterbiomass$FFG[greaterbiomass$Genus=="Sialis"]="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Simulium"]="Collector-Filterer"
greaterbiomass$FFG[greaterbiomass$Genus=="Stratiomyidae"]="Collector-Gatherer"
greaterbiomass$FFG[greaterbiomass$Genus=="Stylogomphus"]="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Tallaperla"]="Shredder"
greaterbiomass$FFG[greaterbiomass$Genus=="Stenelmis"]="Scraper"
greaterbiomass$FFG[greaterbiomass$Genus=="Stenonema"]="Scraper" 
greaterbiomass$FFG[greaterbiomass$Genus=="Taeniopteryx"]="Shredder"
greaterbiomass$FFG[greaterbiomass$Genus=="Tanypodinae"]="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Tanytarsini"]="Collector-Gatherer"
greaterbiomass$FFG[greaterbiomass$Genus=="Tipula"]="Shredder"
greaterbiomass$FFG[greaterbiomass$Genus=="Triacanthagyna"]="Predator"
greaterbiomass$FFG[greaterbiomass$Genus=="Wormaldia"]="Collector-Filterer"
greaterbiomass$FFG[greaterbiomass$Genus=="Zoraena"]="Predator"



# Now, adding synchronisty. Poor = asynchronous, Well = synchronous, some family assumptions
# made based on genus

greaterbiomass$SYNC[greaterbiomass$Genus=="Acerpenna"] ="Asynchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Acentrella"] = "Asynchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Acroneuria"]="Asynchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Allocapnia"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Allognasta"]=""
greaterbiomass$SYNC[greaterbiomass$Genus=="Alloperla"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Ameletus"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Amphinemura"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Antocha"]="Asynchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Atherix"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Attenella"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Baetidae"]="Asynchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Baetis"]="Asynchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Baetisca"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Boyeria"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Calopteryx"]=="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Capniidae"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Ceratopogonidae"]=""
greaterbiomass$SYNC[greaterbiomass$Genus=="Cernotina"]="NA"
greaterbiomass$SYNC[greaterbiomass$Genus=="Chauloides"]="NA"
greaterbiomass$SYNC[greaterbiomass$Genus=="Chelifera"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Chimarra"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Cheumatopsyche"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Chironomidae"]=""
greaterbiomass$SYNC[greaterbiomass$Genus=="Chironomini"]="NA"
greaterbiomass$SYNC[greaterbiomass$Genus=="Circulionidae"]=""
greaterbiomass$SYNC[greaterbiomass$Genus=="Collembola"]=""
greaterbiomass$SYNC[greaterbiomass$Genus=="Cyrnellus"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Dicranota"]="Asynchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Diplectrona"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Discocerina"]="NA"
greaterbiomass$SYNC[greaterbiomass$Genus=="Dixa"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Dixella"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Dolophilodes"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Ectopria"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Eloeophila"]="NA"
greaterbiomass$SYNC[greaterbiomass$Genus=="Epeorus"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Eriopterini"]="Asynchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Ephemera"]="Asynchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Ephemerellidae"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Eurylophella"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Gerris"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Glossosoma"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Goera"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Gomphus"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Gyrinus"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Helichus"]="Asynchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Hemiptera"]=""
greaterbiomass$SYNC[greaterbiomass$Genus=="Heptageniidae"]=""
greaterbiomass$SYNC[greaterbiomass$Genus=="Hetaerina"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Hexatoma"]="Asynchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Hydrachnia"]=""
greaterbiomass$SYNC[greaterbiomass$Genus=="Hydatophylax"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Hydropsyche"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Isonychia"]="Asynchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Isoperla"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Langessa"]=""
greaterbiomass$SYNC[greaterbiomass$Genus=="Lanthus"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Leptophlebiidae"]=""
greaterbiomass$SYNC[greaterbiomass$Genus=="Lepidostoma"]= "Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Leuctra"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Leuctridae"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Limnephilidae"]="Asynchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Limnophila"]= "Asynchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Limoniidae"]= ""
greaterbiomass$SYNC[greaterbiomass$Genus=="Lypodiversa"]=""
greaterbiomass$SYNC[greaterbiomass$Genus=="Micrasema"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Microvelia"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Molophilus"]="Asynchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Neocleon"]=""
greaterbiomass$SYNC[greaterbiomass$Genus=="Neophylax"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Neoplasta"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Nigronia"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Oligochaeta"]=""
greaterbiomass$SYNC[greaterbiomass$Genus=="Optioservus"]="Asynchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Oreogeton"]="Asynchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Orthocladine"]="Asynchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Oulimnius"]="Asynchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Paracapnia"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Paraleptophlebiidae"]="Asynchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Polycentropodidae"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Polycentropus"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Probezzia"]="Asynchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Prodaticus"]=""
greaterbiomass$SYNC[greaterbiomass$Genus=="Prosimulium"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Psephenus"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Pseudolimnophila"]="Asynchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Psychodini"]=""
greaterbiomass$SYNC[greaterbiomass$Genus=="Pteronarcys"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Pycnopsyche"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Remenus"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Rhagovelia"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Rhyacophila"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Prostoia"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Sialis"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Simulium"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Stratiomyidae"]=""
greaterbiomass$SYNC[greaterbiomass$Genus=="Stylogomphus"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Tallaperla"]="Asynchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Stenelmis"]="Asynchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Stenonema"]="Asynchronous" 
greaterbiomass$SYNC[greaterbiomass$Genus=="Taeniopteryx"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Tanypodinae"]=""
greaterbiomass$SYNC[greaterbiomass$Genus=="Tanytarsini"]=""
greaterbiomass$SYNC[greaterbiomass$Genus=="Tipula"]="Asynchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Triacanthagyna"]=""
greaterbiomass$SYNC[greaterbiomass$Genus=="Wormaldia"]="Synchronous"
greaterbiomass$SYNC[greaterbiomass$Genus=="Zoraena"]=""


#taking out data points for things with no assigned FFGs (adults, pupa, terrestrial)
greaterbiomass <- greaterbiomass[!is.na(greaterbiomass$FFG), ]

# Taking out zeroes in biomass too
greaterbiomass <- greaterbiomass[greaterbiomass$Biomass.Area.Corrected != 0,]


# Summarizing means of each FFG in each replicate for each stream
biomassmeantable = greaterbiomass %>% 
  group_by(Sample.Month, SC.Category,SC.Level,Site,Replicate,FFG, SYNC ) %>% 
  summarise(mean.biomass=mean(Biomass.Area.Corrected,na.rm=FALSE))

biomassmeantable 

# Let's QAQC
greaterbiomass %>%
  filter(Site == "HUR", FFG == "Collector-Filterer", Replicate == "1", Sample.Date == "10/20/23") %>% 
  summarise(biomassmean = mean(Biomass.Area.Corrected)) # This confirms that biomassmeantable
#is averaging FFGs per site per replicate correctly

# Now, averaging the replicates from each stream
meansites = biomassmeantable %>% 
  group_by(Sample.Month,SC.Category,SC.Level,Site,FFG, SYNC ) %>% 
  summarise(mean.biomass=mean(mean.biomass,na.rm=FALSE))

meansites # Essentially, the means of the means. I QAQCed this manually and checked that
# it is averaging the replicates

# Make a new df with just quarterly info
meansites.quarterly <- meansites %>%
  filter(Sample.Month %in% c("October", "February", "May")) # This filters just quarterly data from meansites


greaterbiomass.quarterly <- greaterbiomass %>% 
  filter(Sample.Month %in% c("October", "February", "May")) #this will be for NMDS, includes everything

# GG PLOT MANIA BEGINS---------------------------

# First, let's order them correctly
meansites$Site <- factor(meansites$Site, levels = c("EAS", "CRO","HCN","FRY","HUR","RUT","LLC","LLW","RIC"))
meansites$SC.Category <- factor(meansites$SC.Category, levels = c("REF","MID","HIGH"))
meansites$SC.Level <- factor(meansites$SC.Level, levels = c("25","72","78","387","402","594","1119","1242","1457"))
meansites$FFG <- factor(meansites$FFG, levels = c("Scraper","Shredder","Predator","Collector-Gatherer","Collector-Filterer"))
meansites$Sample.Month <- factor(meansites$Sample.Month, levels = c("September","October","November","December","January", "February", "March", "April", "May", "June", "July"))

meansites.quarterly$Site <- factor(meansites.quarterly$Site, levels = c("EAS", "CRO","HCN","FRY","HUR","RUT","LLC","LLW","RIC"))
meansites.quarterly$SC.Category <- factor(meansites.quarterly$SC.Category, levels = c("REF","MID","HIGH"))
meansites.quarterly$SC.Level <- factor(meansites.quarterly$SC.Level, levels = c("25","72","78","387","402","594","1119","1242","1457"))
meansites.quarterly$FFG <- factor(meansites.quarterly$FFG, levels = c("Scraper","Shredder","Predator","Collector-Gatherer","Collector-Filterer"))
meansites.quarterly$Sample.Month <- factor(meansites.quarterly$Sample.Month, levels = c("September","October","November","December","January", "February", "March", "April", "May", "June", "July"))

biomassmeantable$Site <- factor(biomassmeantable$Site, levels = c("EAS", "CRO","HCN","FRY","HUR","RUT","LLC","LLW","RIC"))
biomassmeantable$SC.Category <- factor(biomassmeantable$SC.Category, levels = c("REF","MID","HIGH"))
biomassmeantable$SC.Level <- factor(biomassmeantable$SC.Level, levels = c("25","72","78","387","402","594","1119","1242","1457"))

# Also, for color scheme
install.packages("rcartocolor")# Colorblind color schemes
library(rcartocolor)
display_carto_all()

ffg_colors <- c("Scraper" = "#008080", 
                "Shredder" = "#CA562C", 
                "Predator" = "#F6EDBD", 
                "Collector-Gatherer" = "#DE8A5A", 
                "Collector-Filterer" = "#70A494")  


# FFG boxplot with median data, need to use biomassmeantable to get boxplot, since meansites
# only have one value per FFG
FFGgplot=ggplot(data=biomassmeantable,aes(x=SC.Level,y=mean.biomass, color=FFG))+
  geom_boxplot() +
  labs(x = "Stream", y = "Biomass (g/m2)") +
  theme_minimal() +
  scale_color_carto_d(name = "FFG", palette = "Vivid", n = length(unique(biomassmeantable$FFG))) 

FFGgplot # These colors are pretty


# Panels by FFG across SC gradient
FFGgplot1 <- ggplot(data = meansites, aes(x = SC.Level, y = log(mean.biomass))) +
  facet_wrap(~FFG, ncol = 5, nrow = 5) +  # This creates multiple "panels" for site
  geom_boxplot(aes(fill = FFG)) +  # Use fill aesthetic for boxplot
  geom_point() +
  ylab("log(Biomass (g/m2))") +
  xlab("") +
  scale_fill_manual(values = ffg_colors, name = "FFG") +  # Specify colors for FFGs
  theme_bw() +
  theme(
    axis.title = element_text(size = 23),
    axis.text = element_text(size = 15),
    panel.grid = element_blank(), 
    axis.line = element_line(),
    axis.text.x = element_text(angle = 90, hjust = 1, face = "italic"))

FFGgplot1  # subtle downward scraper trend

FFGgplot1.5 <- ggplot(data = meansites.quarterly, aes(x = SC.Level, y = log(mean.biomass))) +
  facet_wrap(~FFG, ncol = 5, nrow = 5) +  # This creates multiple "panels" for site
  geom_boxplot(aes(fill = FFG)) +  # Use fill aesthetic for boxplot
  geom_point() +
  ylab("log(Biomass (g/m2))") +
  xlab("") +
  scale_fill_manual(values = ffg_colors, name = "FFG") +  # Specify colors for FFGs
  theme_bw() +
  theme(
    axis.title = element_text(size = 23),
    axis.text = element_text(size = 15),
    panel.grid = element_blank(), 
    axis.line = element_line(),
    axis.text.x = element_text(angle = 90, hjust = 1, face = "italic"))

FFGgplot1.5   # The dots are ex. every scraper log biomass for 25

#  Let's make panels based on SC category
FFGgplot2 <- ggplot(data = meansites, aes(x = FFG, y = (log(mean.biomass)))) +
  facet_wrap(~SC.Category, ncol = 3, nrow = 3) +  # Facet by FFG
  geom_boxplot() +
  geom_point(aes(color = FFG), size = 2) +
  ylab(expression(log(Biomass(g/m^2)))) +  
  xlab("") +
  scale_color_carto_d(name = "SC.Category", palette = "Earth", n = length(unique(meansites$SC.Category))) + 
  theme_bw() +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15),
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle = 90, hjust = 1, face = "italic"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 20),
    legend.background = element_blank(),
    legend.key = element_rect(fill = "white", color = "white")
  )

print(FFGgplot2) # Doesn't really look like anything is happening here, maybe categorical
# isn't the best way to display

# FFGs across month
FFGgplot3 <- ggplot(data = meansites, aes(x = Sample.Month, y = (log(mean.biomass)))) +
  facet_wrap(~FFG, ncol = 5, nrow = 5) +  
  geom_boxplot(fill = "white") +  
  geom_point(aes(color = FFG), size = 2) +  
  ylab(expression(log(Biomass(g/m^2)))) +  
  xlab("") +
  scale_color_manual(values = ffg_colors, name = "FFG") +  
  theme_bw() +
  theme(
    axis.title = element_text(size = 23),
    axis.text = element_text(size = 15),
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle = 90, hjust = 1, face = "italic"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 20),
    legend.background = element_blank(),
    legend.key = element_rect(fill = "white", color = "white")
  )
print(FFGgplot3) # A hump in scrapers and collector-gatherers in spring,
# collector-filterers and predators remain fairly consistent across time,
# shredders increasing throughout the year. Of course this figure doesn't show the SC

FFGgplot3.5 <- ggplot(data = meansites.quarterly, aes(x = Sample.Month, y = (log(mean.biomass)))) +
  facet_wrap(~FFG, ncol = 5, nrow = 5) +  
  geom_boxplot(fill = "white") +  
  geom_point(aes(color = FFG), size = 2) +  
  ylab(expression(log(Biomass(g/m^2)))) +  
  xlab("") +
  scale_color_manual(values = ffg_colors, name = "FFG") +  
  theme_bw() +
  theme(
    axis.title = element_text(size = 23),
    axis.text = element_text(size = 15),
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle = 90, hjust = 1, face = "italic"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 20),
    legend.background = element_blank(),
    legend.key = element_rect(fill = "white", color = "white")
  )
print(FFGgplot3.5)

# Months as panels, FFGs on x, another way of sharing previous graph
FFGgplot4 <- ggplot(data = meansites, aes(x = FFG, y = (log(mean.biomass)))) +
  facet_wrap(~Sample.Month, ncol = 10, nrow = 10) +  # Facet by FFG
  geom_boxplot(fill = "white") +  # Set fill color for boxplot to white
  geom_point(aes(color = FFG), size = 2) +  # Assign color to points based on FFG
  ylab(expression(log(Biomass(g/m^2)))) +  
  xlab("") +
  scale_color_manual(values = ffg_colors, name = "FFG") +  # Assign specific colors to points
  theme_bw() +
  theme(
    axis.title = element_text(size = 23),
    axis.text = element_text(size = 15),
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle = 90, hjust = 1, face = "italic"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 20),
    legend.background = element_blank(),
    legend.key = element_rect(fill = "white", color = "white")
  )
print(FFGgplot4) 

# There is a lot here but it shows decently FFGs across time and SC gradient
my_colors = carto_pal(10, "Geyser") 

FFGgplot5<- ggplot(data = meansites, aes(x = Sample.Month, y = log(mean.biomass), fill = SC.Level)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bar graph with dodge position
  ylab(expression(log(Biomass~(g/m^2)))) +  # Y-axis label
  xlab("Sample Month") +
  scale_fill_manual(values = my_colors, name = "SC.Level") +  # Assign specific colors to FFG
  theme_bw() +
  theme(
    axis.title = element_text(size = 23),
    axis.text = element_text(size = 15),
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle = 90, hjust = 1, face = "italic"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 20),
    legend.background = element_blank(),
    legend.key = element_rect(fill = "white", color = "white")
  ) +
  facet_wrap(~ FFG, ncol = 2)  # Facet by SC.Level with 2 columns

print(FFGgplot5) #this is pretty wild

# Let's do it again but with SC category
my_colors = carto_pal(3, "Geyser") 

FFGgplot6<- ggplot(data = meansites, aes(x = Sample.Month, y = log(mean.biomass), fill = SC.Category)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bar graph with dodge position
  ylab(expression(log(Biomass~(g/m^2)))) +  # Y-axis label
  xlab("Sample Month") +
  scale_fill_manual(values = my_colors, name = "SC.Level") +  # Assign specific colors to FFG
  theme_bw() +
  theme(
    axis.title = element_text(size = 23),
    axis.text = element_text(size = 15),
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle = 90, hjust = 1, face = "italic"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 20),
    legend.background = element_blank(),
    legend.key = element_rect(fill = "white", color = "white")
  ) +
  facet_wrap(~ FFG, ncol = 2)  # Facet by SC.Level with 2 columns

print(FFGgplot6) # This is actually not toooo hard to follow

FFGgplot6.5<- ggplot(data = meansites.quarterly, aes(x = Sample.Month, y = log(mean.biomass), fill = SC.Category)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bar graph with dodge position
  ylab(expression(log(Biomass~(g/m^2)))) +  # Y-axis label
  xlab("Sample Month") +
  scale_fill_manual(values = my_colors, name = "SC.Level") +  # Assign specific colors to FFG
  theme_bw() +
  theme(
    axis.title = element_text(size = 23),
    axis.text = element_text(size = 15),
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle = 90, hjust = 1, face = "italic"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 20),
    legend.background = element_blank(),
    legend.key = element_rect(fill = "white", color = "white")
  ) +
  facet_wrap(~ FFG, ncol = 2)  # Facet by SC.Level with 2 columns

print(FFGgplot6.5) # Easier to follow with quarterly data

# Same thing but months are colored this time, can actually see patterns along the SC gradient
my_colors = carto_pal(11, "Geyser") 

FFGgplot7<- ggplot(data = meansites, aes(x = SC.Level, y = log(mean.biomass), fill = Sample.Month)) +
  geom_bar(stat = "identity", position = "dodge") +  # Bar graph with dodge position
  ylab(expression(log(Biomass~(g/m^2)))) +  # Y-axis label
  xlab("SC Level") +
  scale_fill_manual(values = my_colors, name = "Sample.Month") +  # Assign specific colors to FFG
  theme_bw() +
  theme(
    axis.title = element_text(size = 23),
    axis.text = element_text(size = 15),
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle = 90, hjust = 1, face = "italic"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 20),
    legend.background = element_blank(),
    legend.key = element_rect(fill = "white", color = "white")
  ) +
  facet_wrap(~ FFG, ncol = 2)  

print(FFGgplot7) 

# boxplots to show that biomass isn't significantly different
bioboxplot=ggplot(data=meansites,aes(x=SC.Level,y=(log(mean.biomass))))+ 
  geom_boxplot()+
  geom_point(aes(color=SC.Category),size=2)+
  ylab(expression(Biomass(g/m^2)))+
  xlab("")+
  scale_colour_manual(values = c("REF" = "#70A494", "MID" = "#DE8A5A", "HIGH" = "#CA562C")) +
  theme_bw()+
  theme(axis.title=element_text(size=23),
        axis.text=element_text(size=15),
        panel.grid = element_blank(), 
        axis.line=element_line(),
        axis.text.x = element_text(angle = 90, hjust = 1,face="italic"),
        legend.position="top",
        legend.title = element_blank(),
        legend.text = element_text(size=20),
        legend.background = element_blank(),
        legend.key=element_rect(fill="white",color="white"))


bioboxplot # Log to see better

bioboxplotquarterly=ggplot(data=meansites.quarterly,aes(x=SC.Level,y=(log(mean.biomass))))+ 
  geom_boxplot()+
  geom_point(aes(color=SC.Category),size=2)+
  ylab(expression(Biomass(g/m^2)))+
  xlab("")+
  scale_colour_manual(values = c("REF" = "#70A494", "MID" = "#DE8A5A", "HIGH" = "#CA562C")) +
  theme_bw()+
  theme(axis.title=element_text(size=23),
        axis.text=element_text(size=15),
        panel.grid = element_blank(), 
        axis.line=element_line(),
        axis.text.x = element_text(angle = 90, hjust = 1,face="italic"),
        legend.position="top",
        legend.title = element_blank(),
        legend.text = element_text(size=20),
        legend.background = element_blank(),
        legend.key=element_rect(fill="white",color="white"))


bioboxplotquarterly # Log to see better

#PROPORTIONS PARTY!------------------------------------

meansites$SC.Category <- factor(meansites$SC.Category, levels = c("REF","MID","HIGH"))
meansites$SC.Level <- factor(meansites$SC.Level, levels = c("25","72","78","387","402","594","1119","1242","1457"))
meansites$FFG <- factor(meansites$FFG, levels = c("Scraper","Shredder","Predator","Collector-Gatherer","Collector-Filterer"))
meansites$Sample.Month <- factor(meansites$Sample.Month, levels = c("September","October","November","December","January", "February", "March", "April", "May", "June", "July"))

meansites.quarterly$Site <- factor(meansites.quarterly$Site, levels = c("EAS", "CRO","HCN","FRY","HUR","RUT","LLC","LLW","RIC"))
meansites.quarterly$SC.Category <- factor(meansites.quarterly$SC.Category, levels = c("REF","MID","HIGH"))
meansites.quarterly$SC.Level <- factor(meansites.quarterly$SC.Level, levels = c("25","72","78","387","402","594","1119","1242","1457"))
meansites.quarterly$FFG <- factor(meansites.quarterly$FFG, levels = c("Scraper","Shredder","Predator","Collector-Gatherer","Collector-Filterer"))
meansites.quarterly$Sample.Month <- factor(meansites.quarterly$Sample.Month, levels = c("September","October","November","December","January", "February", "March", "April", "May", "June", "July"))

install.packages("rcartocolor")# Colorblind color schemes
library(rcartocolor)
display_carto_all()

# Define a custom palette with desired colors

my_colors = carto_pal(7, "Geyser") # to get hexcodes for geyser to assign FFGS to, for 
#consistency in graphs I'll use for SFS
my_colors
ffg_colors <- c("Scraper" = "#008080", 
                "Shredder" = "#CA562C", 
                "Predator" = "#F6EDBD", 
                "Collector-Gatherer" = "#DE8A5A", 
                "Collector-Filterer" = "#70A494") 

sync_colors <- c("Asynchronous" = "#CA562C","Synchronous" = "#70A494") 



# Now to do actual proportions, scaling everything to 100% biomass for their respective categories
# Let's do it for sc cat, calculate total biomass for each SC cat
total_biomass_cat <- meansites %>%
  group_by(SC.Category) %>%
  summarise(total_biomass = sum(mean.biomass)) # summing the mean biomass for each sc cat

# Calculate proportions of total biomass for each FFG for each site
df_proportions_cat <- meansites %>%
  left_join(total_biomass_cat, by = "SC.Category") %>%
  group_by(SC.Category, FFG, SYNC) %>%
  summarise(Proportion = sum(mean.biomass) / first(total_biomass)) # Summing mean biomass
#for each FFG for each SC category and dividing it by summed mean biomass for each sc cat


# Plot with specific colors assigned to each FFG using hexadecimal codes
propgg_cat = ggplot(df_proportions_cat, aes(x = SC.Category, y = Proportion, fill = FFG)) +
  geom_bar(stat = "identity") +
  labs(x = "Specific Conductivity Category", y = "Proportion of Total Biomass", fill = "FFGs") +
  scale_fill_manual(values = ffg_colors, name = "FFG") +  # Assign specific colors
  theme_minimal()

propgg_cat # oh my, shredders are poppign off




# Now with quarterly
total_biomass_cat_quarterly <- meansites.quarterly %>%
  group_by(SC.Category) %>%
  summarise(total_biomass = sum(mean.biomass)) # summing the mean biomass for each sc cat

# Calculate proportions of total biomass for each FFG for each site
df_proportions_cat_quarterly <- meansites.quarterly %>%
  left_join(total_biomass_cat_quarterly, by = "SC.Category") %>%
  group_by(SC.Category, FFG, SYNC) %>%
  summarise(Proportion = sum(mean.biomass) / first(total_biomass)) # Summing mean biomass
#for each FFG for each SC category and dividing it by summed mean biomass for each sc cat


# Plot with specific colors assigned to each FFG using hexadecimal codes
propgg_cat_quarterly = ggplot(df_proportions_cat_quarterly, aes(x = SC.Category, y = Proportion, fill = FFG)) +
  geom_bar(stat = "identity") +
  labs(x = "Specific Conductivity Category", y = "Proportion of Total Biomass", fill = "FFGs") +
  scale_fill_manual(values = ffg_colors, name = "FFG") +  # Assign specific colors
  theme_minimal()

propgg_cat_quarterly # oh my, shredders are poppign off




# Now let's do it for SC level, calculate total biomass for each SC Level
total_biomass_sites <- meansites %>%
  group_by(SC.Level) %>%
  summarise(total_biomass = sum(mean.biomass)) # Summing the mean biomass for each sc level,
#replicates have already been averaged

# Calculate proportions of total biomass for each FFG for each site
df_proportions_sites <- meansites %>%
  left_join(total_biomass_sites, by = "SC.Level") %>%
  group_by(SC.Level, FFG, SYNC) %>%
  summarise(Proportion = sum(mean.biomass) / first(total_biomass)) # Summing mean biomass
#for each FFG for each SC category and dividing it by summed mean biomass for each sc cat

propgg_site <- ggplot(df_proportions_sites, aes(x = SC.Level, y = Proportion, fill = FFG)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "SC Level", y = "Proportion of Total Biomass", fill = "Functional Group") +
  scale_fill_manual(values = ffg_colors, name = "FFG") +  # Assign specific colors
  theme_bw() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        panel.grid = element_blank(), 
        axis.line = element_line(),
        axis.text.x = element_text(angle = 90, hjust = 1, face = "italic"),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 13),
        legend.background = element_blank(),
        legend.key = element_rect(fill = "white", color = "white"))

propgg_site# Can really see the scraper decline and shredder increas across the 
# gradient in this one



# Now, quarterly sites only

# Now let's do it for SC level, calculate total biomass for each SC Level
total_biomass_sites_quarterly <- meansites.quarterly %>%
  group_by(SC.Level) %>%
  summarise(total_biomass = sum(mean.biomass)) # Summing the mean biomass for each sc level,
#replicates have already been averaged

# Calculate proportions of total biomass for each FFG for each site
df_proportions_sites_quarterly <- meansites.quarterly %>%
  left_join(total_biomass_sites_quarterly, by = "SC.Level") %>%
  group_by(SC.Level, FFG, SYNC) %>%
  summarise(Proportion = sum(mean.biomass) / first(total_biomass)) # Summing mean biomass
#for each FFG for each SC category and dividing it by summed mean biomass for each sc cat

propgg_site_quarterly <- ggplot(df_proportions_sites_quarterly, aes(x = SC.Level, y = Proportion, fill = FFG)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "SC Level", y = "Proportion of Total Biomass", fill = "Functional Group") +
  scale_fill_manual(values = ffg_colors, name = "FFG") +  # Assign specific colors
  theme_bw() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        panel.grid = element_blank(), 
        axis.line = element_line(),
        axis.text.x = element_text(angle = 90, hjust = 1, face = "italic"),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 13),
        legend.background = element_blank(),
        legend.key = element_rect(fill = "white", color = "white"))

propgg_site_quarterly# Can really see the scraper decline and shredder increas across the 
# gradient in this one

# Who are the players in this pattern? Have to go back to raw data to see genus
filtered <- greaterbiomass.quarterly %>%
  filter(FFG == "Shredder" & SC.Level == "25")

print(filtered)

top_5_genera <- filtered %>%
  group_by(Genus) %>%
  summarise(total_biomass = sum(Biomass.Area.Corrected)) %>%
  arrange(desc(total_biomass)) %>%
  slice_head(n = 5)

print(top_5_genera)


# COMBINED TRAITS WHOO--------------------------------

# Filter the data to include only relevant groups
combo_proportions <- meansites %>%
  filter((FFG == "Scraper" & SYNC == "Synchronous") |
           (FFG == "Shredder" & SYNC == "Asynchronous"))

# Calculate total biomass at each time point
total_biomass <- combo_proportions %>%
  group_by(Sample.Month) %>%
  summarise(TotalBiomass = sum(mean.biomass))

# Calculate the biomass contributions of each group
group_biomass <- combo_proportions %>%
  group_by(Sample.Month, FFG, SYNC) %>%
  summarise(GroupBiomass = sum(mean.biomass))

# Merge total biomass with group biomass
df_merged <- merge(group_biomass, total_biomass, by = "Sample.Month")

# Calculate the proportion of biomass for each group
df_merged <- df_merged %>%
  mutate(Proportion = GroupBiomass / TotalBiomass)

# Graph proportions
ggplot(df_merged, aes(x = Sample.Month, y = Proportion, fill = interaction(FFG, SYNC))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "SC Category",
       y = "Proportion of Biomass",
       color = "Group") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()+
  theme(axis.title=element_text(size=15),
        axis.text=element_text(size=15),
        panel.grid = element_blank(), 
        axis.line=element_line(),
        axis.text.x = element_text(angle = 90, hjust = 1,face="italic"),
        legend.position="right",
        legend.title = element_blank(),
        legend.text = element_text(size=13),
        legend.background = element_blank(),
        legend.key=element_rect(fill="white",color="white")) #WOAH

# Making it continuous
ggplot(df_merged, aes(x = Sample.Month, y = Proportion, color = interaction(FFG, SYNC), group = interaction(FFG, SYNC))) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +  
  labs(x = "SC Category",
       y = "Proportion of Biomass",
       color = "Group") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        panel.grid = element_blank(), 
        axis.line = element_line(),
        axis.text.x = element_text(angle = 90, hjust = 1, face = "italic"),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 13),
        legend.background = element_blank(),
        legend.key = element_rect(fill = "white", color = "white"))






# Same but with quarterly data 

# Who are the players?
filtered <- greaterbiomass.quarterly %>%
  filter(FFG == "Shredder" & SYNC == "Asynchronous" & Sample.Month == "May")
           
print(filtered)

top_5_genera <- filtered %>%
  group_by(Genus) %>%
  summarise(total_biomass = sum(Biomass.Area.Corrected)) %>%
  arrange(desc(total_biomass)) %>%
  slice_head(n = 5)

print(top_5_genera)


# Filter the data to include only relevant groups
combo_proportions_quarterly <- meansites.quarterly %>%
  filter((FFG == "Scraper" & SYNC == "Synchronous") |
           (FFG == "Shredder" & SYNC == "Asynchronous"))

# Calculate total biomass at each time point
total_biomass_quarterly <- combo_proportions_quarterly %>%
  group_by(Sample.Month) %>%
  summarise(TotalBiomass = sum(mean.biomass))

# Calculate the biomass contributions of each group
group_biomass_quarterly <- combo_proportions_quarterly %>%
  group_by(Sample.Month, FFG, SYNC) %>%
  summarise(GroupBiomass = sum(mean.biomass))

# Merge total biomass with group biomass
df_merged_quarterly <- merge(group_biomass_quarterly, total_biomass_quarterly, by = "Sample.Month")

# Calculate the proportion of biomass for each group
df_merged_quarterly <- df_merged_quarterly %>%
  mutate(Proportion = GroupBiomass / TotalBiomass)

# Graph proportions
ggplot(df_merged_quarterly, aes(x = Sample.Month, y = Proportion, fill = interaction(FFG, SYNC))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "SC Category",
       y = "Proportion of Biomass",
       color = "Group") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()+
  theme(axis.title=element_text(size=15),
        axis.text=element_text(size=15),
        panel.grid = element_blank(), 
        axis.line=element_line(),
        axis.text.x = element_text(angle = 90, hjust = 1,face="italic"),
        legend.position="right",
        legend.title = element_blank(),
        legend.text = element_text(size=13),
        legend.background = element_blank(),
        legend.key=element_rect(fill="white",color="white")) #WOAH

# Making it continuous
ggplot(df_merged_quarterly, aes(x = Sample.Month, y = Proportion, color = interaction(FFG, SYNC), group = interaction(FFG, SYNC))) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +  
  labs(x = "SC Category",
       y = "Percent Biomass",
       color = "Group") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        panel.grid = element_blank(), 
        axis.line = element_line(),
        axis.text.x = element_text(angle = 90, hjust = 1, face = "italic"),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 13),
        legend.background = element_blank(),
        legend.key = element_rect(fill = "white", color = "white"))





# Different quarterly trait combos
combo_proportions_quarterly <- meansites.quarterly %>%
  filter((FFG == "Scraper" & SYNC == "Asynchronous") |
           (FFG == "Shredder" & SYNC == "Synchronous"))

# Calculate total biomass at each time point
total_biomass_quarterly <- combo_proportions_quarterly %>%
  group_by(Sample.Month) %>%
  summarise(TotalBiomass = sum(mean.biomass))

# Calculate the biomass contributions of each group
group_biomass_quarterly <- combo_proportions_quarterly %>%
  group_by(Sample.Month, FFG, SYNC) %>%
  summarise(GroupBiomass = sum(mean.biomass))

# Merge total biomass with group biomass
df_merged_quarterly <- merge(group_biomass_quarterly, total_biomass_quarterly, by = "Sample.Month")

# Calculate the proportion of biomass for each group
df_merged_quarterly <- df_merged_quarterly %>%
  mutate(Proportion = GroupBiomass / TotalBiomass)

# Graph proportions
ggplot(df_merged_quarterly, aes(x = Sample.Month, y = Proportion, fill = interaction(FFG, SYNC))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "SC Category",
       y = "Proportion of Biomass",
       color = "Group") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()+
  theme(axis.title=element_text(size=15),
        axis.text=element_text(size=15),
        panel.grid = element_blank(), 
        axis.line=element_line(),
        axis.text.x = element_text(angle = 90, hjust = 1,face="italic"),
        legend.position="right",
        legend.title = element_blank(),
        legend.text = element_text(size=13),
        legend.background = element_blank(),
        legend.key=element_rect(fill="white",color="white")) #WOAH

# Making it continuous
ggplot(df_merged_quarterly, aes(x = Sample.Month, y = Proportion, color = interaction(FFG, SYNC), group = interaction(FFG, SYNC))) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +  
  labs(x = "SC Category",
       y = "Percent Biomass",
       color = "Group") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        panel.grid = element_blank(), 
        axis.line = element_line(),
        axis.text.x = element_text(angle = 90, hjust = 1, face = "italic"),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 13),
        legend.background = element_blank(),
        legend.key = element_rect(fill = "white", color = "white"))


# Who are the players?
filtered <- greaterbiomass.quarterly %>%
  filter(FFG == "Scraper" & SYNC == "Asynchronous" & Sample.Month == "May")

print(filtered)

top_5_genera <- filtered %>%
  group_by(Genus) %>%
  summarise(total_biomass = sum(Biomass.Area.Corrected)) %>%
  arrange(desc(total_biomass)) %>%
  slice_head(n = 5)

print(top_5_genera)





# Again, but across and SC gradient
# Filter the data to include only relevant groups
combo_proportions <- meansites %>%
  filter((FFG == "Scraper" & SYNC == "Synchronous") |
           (FFG == "Shredder" & SYNC == "Asynchronous"))

# Calculate total biomass at each time point
total_biomass <- combo_proportions %>%
  group_by(SC.Level) %>%
  summarise(TotalBiomass = sum(mean.biomass))

# Calculate the biomass contributions of each group
group_biomass <- combo_proportions %>%
  group_by(SC.Level, FFG, SYNC) %>%
  summarise(GroupBiomass = sum(mean.biomass))

# Merge total biomass with group biomass
df_merged <- merge(group_biomass, total_biomass, by = "SC.Level")

# Calculate the proportion of biomass for each group
df_merged <- df_merged %>%
  mutate(Proportion = GroupBiomass / TotalBiomass)

# Graph proportions
ggplot(df_merged, aes(x = SC.Level, y = Proportion, fill = interaction(FFG, SYNC))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "SC Category",
       y = "Proportion of Biomass",
       color = "Group") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()+
  theme(axis.title=element_text(size=15),
        axis.text=element_text(size=15),
        panel.grid = element_blank(), 
        axis.line=element_line(),
        axis.text.x = element_text(angle = 90, hjust = 1,face="italic"),
        legend.position="right",
        legend.title = element_blank(),
        legend.text = element_text(size=13),
        legend.background = element_blank(),
        legend.key=element_rect(fill="white",color="white")) #WOAH

# Continuous
ggplot(df_merged, aes(x = SC.Level, y = Proportion, color = interaction(FFG, SYNC), group = interaction(FFG, SYNC))) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +  
  labs(x = "SC Category",
       y = "Proportion of Biomass",
       color = "Group") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        panel.grid = element_blank(), 
        axis.line = element_line(),
        axis.text.x = element_text(angle = 90, hjust = 1, face = "italic"),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 13),
        legend.background = element_blank(),
        legend.key = element_rect(fill = "white", color = "white"))
# Shredders/Acynchronous are defintiely having a subsidy


# Same thing but with quarterly
combo_proportions_quarterly <- meansites.quarterly %>%
  filter((FFG == "Scraper" & SYNC == "Synchronous") |
           (FFG == "Shredder" & SYNC == "Asynchronous"))

# Calculate total biomass at each time point
total_biomass_quarterly <- combo_proportions_quarterly %>%
  group_by(SC.Level) %>%
  summarise(TotalBiomass = sum(mean.biomass))

# Calculate the biomass contributions of each group
group_biomass_quarterly <- combo_proportions_quarterly%>%
  group_by(SC.Level, FFG, SYNC) %>%
  summarise(GroupBiomass = sum(mean.biomass))

# Merge total biomass with group biomass
df_merged_quarterly <- merge(group_biomass_quarterly, total_biomass_quarterly, by = "SC.Level")

# Calculate the proportion of biomass for each group
df_merged_quarterly <- df_merged_quarterly %>%
  mutate(Proportion = GroupBiomass / TotalBiomass)

# Graph proportions
ggplot(df_merged_quarterly, aes(x = SC.Level, y = Proportion, fill = interaction(FFG, SYNC))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "SC Category",
       y = "Proportion of Biomass",
       color = "Group") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()+
  theme(axis.title=element_text(size=15),
        axis.text=element_text(size=15),
        panel.grid = element_blank(), 
        axis.line=element_line(),
        axis.text.x = element_text(angle = 90, hjust = 1,face="italic"),
        legend.position="right",
        legend.title = element_blank(),
        legend.text = element_text(size=13),
        legend.background = element_blank(),
        legend.key=element_rect(fill="white",color="white")) #WOAH

# Continuous
ggplot(df_merged_quarterly, aes(x = SC.Level, y = Proportion, color = interaction(FFG, SYNC), group = interaction(FFG, SYNC))) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +  
  labs(x = "SC Category",
       y = "Proportion of Biomass",
       color = "Group") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        panel.grid = element_blank(), 
        axis.line = element_line(),
        axis.text.x = element_text(angle = 90, hjust = 1, face = "italic"),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 13),
        legend.background = element_blank(),
        legend.key = element_rect(fill = "white", color = "white"))





# More quarterly only and different trait combos, this is awesome
combo_proportions_quarterly <- meansites.quarterly %>%
  filter((FFG == "Scraper" & SYNC == "Asynchronous") |
           (FFG == "Shredder" & SYNC == "Synchronous"))

# Calculate total biomass at each time point
total_biomass_quarterly <- combo_proportions_quarterly %>%
  group_by(SC.Level) %>%
  summarise(TotalBiomass = sum(mean.biomass))

# Calculate the biomass contributions of each group
group_biomass_quarterly <- combo_proportions_quarterly%>%
  group_by(SC.Level, FFG, SYNC) %>%
  summarise(GroupBiomass = sum(mean.biomass))

# Merge total biomass with group biomass
df_merged_quarterly <- merge(group_biomass_quarterly, total_biomass_quarterly, by = "SC.Level")

# Calculate the proportion of biomass for each group
df_merged_quarterly <- df_merged_quarterly %>%
  mutate(Proportion = GroupBiomass / TotalBiomass)

# Graph proportions
ggplot(df_merged_quarterly, aes(x = SC.Level, y = Proportion, fill = interaction(FFG, SYNC))) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "SC Category",
       y = "Proportion of Biomass",
       color = "Group") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw()+
  theme(axis.title=element_text(size=15),
        axis.text=element_text(size=15),
        panel.grid = element_blank(), 
        axis.line=element_line(),
        axis.text.x = element_text(angle = 90, hjust = 1,face="italic"),
        legend.position="right",
        legend.title = element_blank(),
        legend.text = element_text(size=13),
        legend.background = element_blank(),
        legend.key=element_rect(fill="white",color="white")) #WOAH

# Continuous
ggplot(df_merged_quarterly, aes(x = SC.Level, y = Proportion, color = interaction(FFG, SYNC), group = interaction(FFG, SYNC))) +
  geom_line(size = 1.2) +
  geom_point(size = 3) +  
  labs(x = "SC Category",
       y = "Percent Biomass",
       color = "Group") +
  scale_y_continuous(labels = scales::percent) +
  theme_bw() +
  theme(axis.title = element_text(size = 15),
        axis.text = element_text(size = 15),
        panel.grid = element_blank(), 
        axis.line = element_line(),
        axis.text.x = element_text(angle = 90, hjust = 1, face = "italic"),
        legend.position = "right",
        legend.title = element_blank(),
        legend.text = element_text(size = 13),
        legend.background = element_blank(),
        legend.key = element_rect(fill = "white", color = "white"))

# Who are the players?
filtered <- greaterbiomass.quarterly %>%
  filter(FFG == "Shredder" & SYNC == "Synchronous" & SC.Level == "1457")

print(filtered)

top_5_genera <- filtered %>%
  group_by(Genus) %>%
  summarise(total_biomass = sum(Biomass.Area.Corrected)) %>%
  arrange(desc(total_biomass)) %>%
  slice_head(n = 5)

print(top_5_genera)







# Want to see continuous decline of scrapers in comparison to shredders
filtered_data_ffg <- df_proportions_sites %>%
  filter(FFG %in% c("Scraper", "Shredder"))

# Plot the filtered data
filtered_data_ffg$SC.Level <- as.numeric(as.character(filtered_data_ffg$SC.Level))

propgg_site_scatter <- ggplot(filtered_data_ffg, aes(x = SC.Level, y = Proportion, color = FFG)) +
  geom_point( size = 3) +  # Set alpha to control transparency
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Specific Conductivity", y = "Proportion of Total Biomass", color = "Functional Group") +
  scale_color_manual(values = c("Scraper" = "#008080", "Shredder" = "#CA562C")) +  
  theme_bw() +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15),
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle = 90, hjust = 1, face = "italic"),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 13),
    legend.background = element_blank(),
    legend.key = element_rect(fill = "white", color = "white")
  ) +
  scale_x_continuous(expand = c(0.05, 0.05)) +  # Adjust the expand parameter to add extra space
  coord_cartesian(ylim = c(0, max(filtered_data_ffg$Proportion) * 1.1))  # Adjust y-axis limits

propgg_site_scatter


# Want to see continuous decline of synchronous in comparison to async
filtered_data_sync <- df_proportions_sites %>%
  filter(SYNC %in% c("Synchronous", "Asynchronous"))

# Plot the filtered data
filtered_data_sync$SC.Level <- as.numeric(as.character(filtered_data_sync$SC.Level))

propgg_site_scatter <- ggplot(filtered_data_sync, aes(x = SC.Level, y = Proportion, color = SYNC)) +
  geom_point( size = 3) +  # Set alpha to control transparency
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Specific Conductivity", y = "Proportion of Total Biomass", color = "SYNC") +
  scale_color_manual(values = c("Synchronous" = "#008080", "Asynchronous" = "#CA562C")) +  
  theme_bw() +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15),
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle = 90, hjust = 1, face = "italic"),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 13),
    legend.background = element_blank(),
    legend.key = element_rect(fill = "white", color = "white")
  ) +
  scale_x_continuous(expand = c(0.05, 0.05)) +  # Adjust the expand parameter to add extra space
  coord_cartesian(ylim = c(0, max(filtered_data_sync$Proportion) * 1.1))  # Adjust y-axis limits

propgg_site_scatter





# Again with quarterly

# Want to see continuous decline of scrapers in comparison to shredders
filtered_data_ffg_quarterly <- df_proportions_sites_quarterly %>%
  filter(FFG %in% c("Scraper", "Shredder"))

# Plot the filtered data
filtered_data_ffg_quarterly$SC.Level <- as.numeric(as.character(filtered_data_ffg_quarterly$SC.Level))

propgg_site_scatter <- ggplot(filtered_data_ffg_quarterly, aes(x = SC.Level, y = Proportion, color = FFG)) +
  geom_point( size = 3) +  # Set alpha to control transparency
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Specific Conductivity", y = "Proportion of Total Biomass", color = "Functional Group") +
  scale_color_manual(values = c("Scraper" = "#008080", "Shredder" = "#CA562C")) +  
  theme_bw() +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15),
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle = 90, hjust = 1, face = "italic"),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 13),
    legend.background = element_blank(),
    legend.key = element_rect(fill = "white", color = "white")
  ) +
  scale_x_continuous(expand = c(0.05, 0.05)) +  # Adjust the expand parameter to add extra space
  coord_cartesian(ylim = c(0, max(filtered_data_ffg_quarterly$Proportion) * 1.1))  # Adjust y-axis limits

propgg_site_scatter


# Want to see continuous decline of synchronous in comparison to async
filtered_data_sync_quarterly <- df_proportions_sites_quarterly %>%
  filter(SYNC %in% c("Synchronous", "Asynchronous"))

# Plot the filtered data
filtered_data_sync_quarterly$SC.Level <- as.numeric(as.character(filtered_data_sync_quarterly$SC.Level))

propgg_site_scatter <- ggplot(filtered_data_sync_quarterly, aes(x = SC.Level, y = Proportion, color = SYNC)) +
  geom_point( size = 3) +  # Set alpha to control transparency
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Specific Conductivity", y = "Proportion of Total Biomass", color = "SYNC") +
  scale_color_manual(values = c("Synchronous" = "#008080", "Asynchronous" = "#CA562C")) +  
  theme_bw() +
  theme(
    axis.title = element_text(size = 15),
    axis.text = element_text(size = 15),
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle = 90, hjust = 1, face = "italic"),
    legend.position = "right",
    legend.title = element_blank(),
    legend.text = element_text(size = 13),
    legend.background = element_blank(),
    legend.key = element_rect(fill = "white", color = "white")
  ) +
  scale_x_continuous(expand = c(0.05, 0.05)) +  # Adjust the expand parameter to add extra space
  coord_cartesian(ylim = c(0, max(filtered_data_sync_quarterly$Proportion) * 1.1))  # Adjust y-axis limits

propgg_site_scatter



# PERCENT PARTY -----------------------------------

meansites$SC.Category <- factor(meansites$SC.Category, levels = c("REF","MID","HIGH"))
meansites$FFG <- factor(meansites$FFG, levels = c("Scraper","Shredder","Predator","Collector-Gatherer","Collector-Filterer"))
meansites$SC.Level <- factor(meansites$SC.Level, levels = c("25","72","78","387","402","594","1119","1242","1457"))

greaterbiomass.quarterly$SC.Level <- factor(greaterbiomass.quarterly$SC.Level, levels = c("25","72","78","387","402","594","1119","1242","1457"))


my_colors = carto_pal(7, "Geyser") # to get hexcodes for geyser to assign FFGS to, for 
#consistency in graphs I'll use for SFS
my_colors
ffg_colors <- c("Scraper" = "#008080", 
                "Shredder" = "#CA562C", 
                "Predator" = "#F6EDBD", 
                "Collector-Gatherer" = "#DE8A5A", 
                "Collector-Filterer" = "#70A494")  



df_proportions_table <- biomassmeantable %>%
  left_join(total_biomass_sites, by = "SC.Level") %>%
  group_by(SC.Level, FFG, Replicate) %>% #getting proportion by FFG and Rep per site
  summarise(Proportion = sum(mean.biomass) / first(total_biomass)) 

df_filtered <- df_proportions_table %>%
  filter(FFG %in% c("Scraper", "Shredder")) #just using scrapers and shredders

# Calculate percentage biomass for each FFG
df_filtered <- df_filtered %>%
  mutate(percentage.biomass = Proportion* 100) #getting percent biomass

# Plot box plots
percent <- ggplot(df_filtered, aes(x = SC.Level, y = percentage.biomass, fill = FFG)) +
  geom_boxplot() +  # Adjust color, fill, transparency, and width of boxplots
  labs(x = "Salinity", y = "Percentage Biomass", fill = "FFGs") +
  scale_fill_manual(values = ffg_colors, name = "FFG") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle = 90, hjust = 1, face = "italic"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.background = element_blank(),
    legend.key = element_rect(fill = "white", color = "white")
  ) +
  ylim(0, 100)  # Limit y-axis to 0-100

percent

# Quarterly just for fun
total_biomass_sites_quarterly <- greaterbiomass.quarterly %>%
  group_by(SC.Level) %>%
  summarise(total_biomass = sum(Biomass.Area.Corrected))

df_proportions_table <- greaterbiomass.quarterly %>%
  left_join(total_biomass_sites_quarterly, by = "SC.Level") %>%
  group_by(SC.Level, FFG, Replicate) %>% #getting proportion by FFG and Rep per site
  summarise(Proportion = sum(Biomass.Area.Corrected) / first(total_biomass)) 

df_filtered <- df_proportions_table %>%
  filter(FFG %in% c("Scraper", "Shredder")) #just using scrapers and shredders

# Calculate percentage biomass for each FFG
df_filtered <- df_filtered %>%
  mutate(percentage.biomass = Proportion* 100) #getting percent biomass

# Plot box plots
percent <- ggplot(df_filtered, aes(x = SC.Level, y = percentage.biomass, fill = FFG)) +
  geom_boxplot() +  # Adjust color, fill, transparency, and width of boxplots
  labs(x = "Salinity", y = "Percentage Biomass", fill = "FFGs") +
  scale_fill_manual(values = ffg_colors, name = "FFG") +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 14),
    axis.text = element_text(size = 12),
    panel.grid = element_blank(),
    axis.line = element_line(),
    axis.text.x = element_text(angle = 90, hjust = 1, face = "italic"),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    legend.background = element_blank(),
    legend.key = element_rect(fill = "white", color = "white")
  ) +
  ylim(0, 100)  # Limit y-axis to 0-100

percent


#NMDS attempts---------------------------

# Loading the appropariate packages
library(vegan) # vegan to calculate distance matrices
library(ggplot2) # ggplot for plotting
library(tidyverse) 
library(dplyr)
library(rcartocolor)

greaterbiomass.nmds.all$Site <- factor(greaterbiomass.nmds.all$Site, levels = c("EAS","CRO","HCN","FRY","HUR","RUT","RIC","LLW","LLC"))

# Running the NMDS for all taxa
aggregated.greaterone<- aggregate(Biomass.Area.Corrected ~ Site + SC.Level + SC.Category + 
                                 + Genus, data = greaterbiomass, FUN = median, na.rm = TRUE)

greaterbiomass.nmds.all = aggregated.greaterone %>% 
  pivot_wider(
    names_from = Genus, 
    values_from = Biomass.Area.Corrected,
  ) 

#  Rename the ID part of the matrix; take out the columns for streams, SC, season, genus
greaterbiomass.nmds.ID<- greaterbiomass.nmds.all[,-c(1:3)]


greaterbiomass.nmds.ID[is.na(greaterbiomass.nmds.ID)] <- 0


#  metaMDS integrates functions from several packages to perform NMDS.....
#  ....including'vegdist' from the vegan package

greaterbiomass.nmds.final <- metaMDS(greaterbiomass.nmds.ID, distance='bray', k=2, trymax=20, autotransform=FALSE, pc=FALSE, plot=FALSE)

# Gives average stress
greaterbiomass.nmds.final$stress

# Gives weights that different species hold in the axis
greaterbiomass.nmds.final$species

# Basic plot of all of the points
plot(greaterbiomass.nmds.final, display=c('sites', 'species'), choices=c(1,2), type='p')

nmds_species <- scores(greaterbiomass.nmds.final, display = "species")

nmds_sites <- scores(greaterbiomass.nmds.final, display = "sites")

# Scores
site.scores <- as.data.frame(scores(nmds_sites)) #Using the scores function from vegan to extract the site scores and convert to a data.frame
site.scores$site <- rownames(site.scores)# Add site column to dataframe
head(site.scores)

species.scores <- as.data.frame(scores(nmds_species)) 
species.scores$species <- rownames(species.scores)  
head(species.scores)


#Filtering based on top abundance
Biomass_Sums <- colSums(greaterbiomass.nmds.ID)
Biomass_Sums # Sums of all taxa

Biomass_CutOff <- 100# Don't want taxa with an abundance less than 150

TOP <- greaterbiomass.nmds.ID %>%
  select(where( ~ sum(.) >= Biomass_CutOff)) # Taxa with abundances greater than 150


# NMDS for top taxa only
TOPScores <-metaMDS(TOP, distance='bray', k=2, trymax=20, autotransform=FALSE, pc=FALSE, plot=FALSE)
TOPScores$stress

# TOP Scores

TOPGenera <- scores(TOPScores, display="species") # Scores for top species
TOPGenera <- as.data.frame(TOPGenera)
TOPGenera$species <- rownames(TOPGenera)  # create a column of species, from the rownames of TOPGenera

TOPsites <- scores(TOPScores, display="sites")
TOPsites <- as.data.frame(TOPsites)
rownames(TOPsites) <- c("LLW", "LLC", "RIC", "FRY",
                           "EAS", "HCN", "HUR", "RUT", "CRO") # Change sites from numbers to categorical
TOPsites$site <- rownames(TOPsites) 

# Filter data for specific colors
ref <- subset(TOPsites, site %in% c("CRO", "EAS", "HCN"))
mid <- subset(TOPsites, site %in% c("HUR", "FRY", "RUT"))
high <- subset(TOPsites, site %in% c("RIC", "LLC", "LLW"))

# Create polygons around points of specific colors
TOPPlot <- ggplot() +    
  geom_polygon(data = ref, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#70A494", alpha = 0.3) +
  geom_polygon(data = mid, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#EDBB8A", alpha = 0.3) +
  geom_polygon(data = high, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#CA562C", alpha = 0.3) +
  geom_text(data = TOPGenera, aes(x = NMDS1, y = NMDS2, label = species), alpha = 0.5, vjust = 0.5, color = "grey") +   
  geom_point(data = TOPsites, aes(x = NMDS1, y = NMDS2, color = site), size = 3) + 
  geom_text(data = TOPsites, aes(x = NMDS1, y = NMDS2, label = site), size = 4, vjust = 0.5) +
  scale_colour_manual(values = c("CRO" = "#70A494", "EAS" = "#70A494", "HCN" = "#70A494",
                                 "HUR" = "#EDBB8A", "FRY" = "#EDBB8A", "RUT" = "#EDBB8A", 
                                 "RIC" = "#CA562C", "LLC" = "#CA562C", "LLW" = "#CA562C")) +
  coord_equal() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(name = "NMDS1", limits = c(-1, 1)) +
  scale_y_continuous(name = "NMDS2", limits = c(-1, 1)) 

# Display the plot
print(TOPPlot)


#Just scraper and shredder NMDS--------------------------------

# Loading the appropariate packages
library(vegan) # vegan to calculate distance matrices
library(ggplot2) # ggplot for plotting
library(tidyverse) 
library(dplyr)

aggregated.df.ffg.filter <- subset(greaterbiomass, FFG %in% c("Scraper", "Shredder"))

aggregated.df.ffg.filter <- aggregate(Biomass.Area.Corrected ~ Site + SC.Level + SC.Category + 
                                        + Genus, data = aggregated.df.ffg.filter, FUN = median, na.rm = TRUE)


biomass.nmds.ffg.filter <- aggregated.df.ffg.filter %>% 
  pivot_wider(
    names_from = Genus, 
    values_from = Biomass.Area.Corrected,
  )


#  Rename the ID part of the matrix; take out the columns for streams, SC, season, genus
greaterone.nmds.ffg.filter <- biomass.nmds.ffg.filter[,-c(1:3)]


greaterone.nmds.ffg.filter[is.na(greaterone.nmds.ffg.filter)] <- 0

# Running the NMDS for all taxa

#  metaMDS integrates functions from several packages to perform NMDS.....
#  ....including'vegdist' from the vegan package

greaterone.nmds.ffg <- metaMDS(greaterone.nmds.ffg.filter, distance='bray', k=2, trymax=20, autotransform=FALSE, pc=FALSE, plot=FALSE)

# Gives average stress
greaterone.nmds.ffg$stress

# Gives weights that different species hold in the axis
greaterone.nmds.ffg$species


# Basic plot of all of the points
plot(greaterone.nmds.ffg, display=c('sites', 'species'), choices=c(1,2), type='p')

nmds_species <- scores(greaterone.nmds.ffg, display = "species")

nmds_sites <- scores(greaterone.nmds.ffg, display = "sites")

# Scores
site.scores <- as.data.frame(scores(nmds_sites)) #Using the scores function from vegan to extract the site scores and convert to a data.frame
site.scores$site <- rownames(site.scores)# Add site column to dataframe
head(site.scores)

species.scores <- as.data.frame(scores(nmds_species)) 
species.scores$species <- rownames(species.scores)  
head(species.scores)


#Filtering based on top abundance
Biomass_Sums <- colSums(greaterone.nmds.ffg.filter)
Biomass_Sums # Sums of all taxa

Biomass_CutOff <- 100# Don't want taxa with an abundance less than 150

TOP <- greaterone.nmds.ffg.filter %>%
  select(where( ~ sum(.) >= Biomass_CutOff)) # Taxa with abundances greater than 150


# NMDS for top taxa only
TOPScores <-metaMDS(TOP, distance='bray', k=2, trymax=20, autotransform=FALSE, pc=FALSE, plot=FALSE)
TOPScores$stress

# TOP Scores

TOPGenera <- scores(TOPScores, display="species") # Scores for top species
TOPGenera <- as.data.frame(TOPGenera)
TOPGenera$species <- rownames(TOPGenera)  # create a column of species, from the rownames of TOPGenera

TOPsites <- scores(TOPScores, display="sites")
TOPsites <- as.data.frame(TOPsites)
rownames(TOPsites) <- c("RIC", "FRY", "EAS", "LLW",
                        "LLC", "HUR", "RUT", "HCN", "CRO") # Change sites from numbers to categorical
TOPsites$site <- rownames(TOPsites) 

# Filter data for specific colors
ref <- subset(TOPsites, site %in% c("CRO", "EAS", "HCN"))
mid <- subset(TOPsites, site %in% c("HUR", "FRY", "RUT"))
high <- subset(TOPsites, site %in% c("RIC", "LLC", "LLW"))

# Create polygons around points of specific colors
TOPPlott <- ggplot() +    
  geom_polygon(data = ref, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#70A494", alpha = 0.3) +
  geom_polygon(data = mid, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#EDBB8A", alpha = 0.3) +
  geom_polygon(data = high, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#CA562C", alpha = 0.3) +
  geom_text(data = TOPGenera, aes(x = NMDS1, y = NMDS2, label = species), alpha = 0.5, vjust = 0.5, color = "grey") +   
  geom_point(data = TOPsites, aes(x = NMDS1, y = NMDS2, color = site), size = 3) + 
  geom_text(data = TOPsites, aes(x = NMDS1, y = NMDS2, label = site), size = 4, vjust = 1.5) +
  scale_colour_manual(values = c("CRO" = "#70A494", "EAS" = "#70A494", "HCN" = "#70A494",
                                 "HUR" = "#EDBB8A", "FRY" = "#EDBB8A", "RUT" = "#EDBB8A", 
                                 "RIC" = "#CA562C", "LLC" = "#CA562C", "LLW" = "#CA562C")) +
  coord_equal() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(name = "NMDS1", limits = c(-3,3)) +
  scale_y_continuous(name = "NMDS2", limits = c(-3, 3)) 

# Display the plot
print(TOPPlott)


# Trait combo NMDS--------------------------------

# Loading the appropariate packages
library(vegan) # vegan to calculate distance matrices
library(ggplot2) # ggplot for plotting
library(tidyverse) 
library(dplyr)

aggregated.df.combo.filter <- subset(greaterbiomass, 
                                   (FFG == "Scraper" & SYNC == "Synchronous") | 
                                     (FFG == "Shredder" & SYNC == "Asynchronous"))

aggregated.df.combo.filter <- aggregate(Biomass.Area.Corrected ~ Site + SC.Level + SC.Category + 
                                        + Genus, data = aggregated.df.combo.filter, FUN = median, na.rm = TRUE)


biomass.nmds.combo.filter <- aggregated.df.combo.filter %>% 
  pivot_wider(
    names_from = Genus, 
    values_from = Biomass.Area.Corrected,
  )


#  Rename the ID part of the matrix; take out the columns for streams, SC, season, genus
greaterone.nmds.combo.filter <- biomass.nmds.combo.filter[,-c(1:4)]


greaterone.nmds.combo.filter[is.na(greaterone.nmds.combo.filter)] <- 0

# Running the NMDS for all taxa

#  metaMDS integrates functions from several packages to perform NMDS.....
#  ....including'vegdist' from the vegan package

greaterone.nmds.combo <- metaMDS(greaterone.nmds.combo.filter, distance='bray', k=2, trymax=20, autotransform=FALSE, pc=FALSE, plot=FALSE)

# Gives average stress
greaterone.nmds.combo$stress

# Gives weights that different species hold in the axis
greaterone.nmds.combo$species


# Basic plot of all of the points
plot(greaterone.nmds.combo, display=c('sites', 'species'), choices=c(1,2), type='p')

nmds_species <- scores(greaterone.nmds.combo, display = "species")



nmds_sites <- scores(greaterone.nmds.combo, display = "sites")

# Scores
site.scores <- as.data.frame(scores(nmds_sites)) #Using the scores function from vegan to extract the site scores and convert to a data.frame
site.scores$site <- rownames(site.scores)# Add site column to dataframe
head(site.scores)

species.scores <- as.data.frame(scores(nmds_species)) 
species.scores$species <- rownames(species.scores)  
head(species.scores)


#Filtering based on top abundance
Biomass_Sums <- colSums(greaterone.nmds.combo.filter)
Biomass_Sums # Sums of all taxa

Biomass_CutOff <- 100# Don't want taxa with an abundance less than 150



TOP <- greaterone.nmds.combo.filter %>%
  select(where( ~ sum(.) >= Biomass_CutOff)) # Taxa with abundances greater than 150

# NMDS for top taxa only
TOPScores <-metaMDS(TOP, distance='bray', k=2, trymax=20, autotransform=FALSE, pc=FALSE, plot=FALSE)
TOPScores$stress

# TOP Scores

TOPGenera <- scores(TOPScores, display="species") # Scores for top species
TOPGenera <- as.data.frame(TOPGenera)
TOPGenera$species <- rownames(TOPGenera)  # create a column of species, from the rownames of TOPGenera

TOPsites <- scores(TOPScores, display="sites")
TOPsites <- as.data.frame(TOPsites)
rownames(TOPsites) <- c("LLW", "RIC", "HUR", "FRY",
                        "RUT", "EAS", "CRO", "HCN", "LLC") # Change sites from numbers to categorical
TOPsites$site <- rownames(TOPsites) 

# Filter data for specific colors
ref <- subset(TOPsites, site %in% c("CRO", "EAS", "HCN"))
mid <- subset(TOPsites, site %in% c("HUR", "FRY", "RUT"))
high <- subset(TOPsites, site %in% c("RIC", "LLC", "LLW"))

# Create polygons around points of specific colors
TOPPlott <- ggplot() +    
  geom_polygon(data = ref, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#70A494", alpha = 0.3) +
  geom_polygon(data = mid, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#EDBB8A", alpha = 0.3) +
  geom_polygon(data = high, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#CA562C", alpha = 0.3) +
  geom_text(data = TOPGenera, aes(x = NMDS1, y = NMDS2, label = species), alpha = 0.5, vjust = 0.5, color = "grey") +   
  geom_point(data = TOPsites, aes(x = NMDS1, y = NMDS2, color = site), size = 3) + 
  geom_text(data = TOPsites, aes(x = NMDS1, y = NMDS2, label = site), size = 4, vjust = 1.5) +
  scale_colour_manual(values = c("CRO" = "#70A494", "EAS" = "#70A494", "HCN" = "#70A494",
                                 "HUR" = "#EDBB8A", "FRY" = "#EDBB8A", "RUT" = "#EDBB8A", 
                                 "RIC" = "#CA562C", "LLC" = "#CA562C", "LLW" = "#CA562C")) +
  coord_equal() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(name = "NMDS1", limits = c(-2,2)) +
  scale_y_continuous(name = "NMDS2", limits = c(-2, 2)) 

# Display the plot
print(TOPPlott)

# Quarterly comparisons------------------

# Loading the appropariate packages
library(vegan) # vegan to calculate distance matrices
library(ggplot2) # ggplot for plotting
library(tidyverse) 
library(dplyr)
library(rcartocolor)


# Convert Sample.Month and Site to factors with specified levels
greaterbiomass.quarterly$Sample.Month <- factor(greaterbiomass.quarterly$Sample.Month, levels = c("October", "February", "May"))
greaterbiomass.quarterly$Site <- factor(greaterbiomass.quarterly$Site, levels = c("EAS", "CRO", "HCN", "HUR", "FRY", "RUT", "RIC", "LLW", "LLC"))

# Aggregate the data
aggregated.greaterone.quarterly <- aggregate(Biomass.Area.Corrected ~ Site + SC.Level + SC.Category + Sample.Month + Genus, 
                                             data = greaterbiomass.quarterly, 
                                             FUN = median, 
                                             na.rm = TRUE)

# Convert Sample.Month and Site to factors with specified levels again to ensure they are ordered correctly in the aggregated data
aggregated.greaterone.quarterly$Sample.Month <- factor(aggregated.greaterone.quarterly$Sample.Month, levels = c("October", "February", "May"))
aggregated.greaterone.quarterly$Site <- factor(aggregated.greaterone.quarterly$Site, levels = c("EAS", "CRO", "HCN", "HUR", "FRY", "RUT", "RIC", "LLW", "LLC"))

# Pivot the data to wider format
library(tidyr)
greaterbiomass.nmds.quarterly <- aggregated.greaterone.quarterly %>%
  pivot_wider(
    names_from = Genus, 
    values_from = Biomass.Area.Corrected,
  )

# Sort the dataframe by Site and Sample.Month
greaterbiomass.nmds.quarterly <- greaterbiomass.nmds.quarterly[order(greaterbiomass.nmds.quarterly$Site, greaterbiomass.nmds.quarterly$Sample.Month), ]


#  Rename the ID part of the matrix; take out the columns for streams, SC, season, genus
greaterbiomass.nmds.ID.quarterly<- greaterbiomass.nmds.quarterly[,-c(1:4)]


greaterbiomass.nmds.ID.quarterly[is.na(greaterbiomass.nmds.ID.quarterly)] <- 0



#  metaMDS integrates functions from several packages to perform NMDS.....
#  ....including'vegdist' from the vegan package

greaterbiomass.nmds.quarterly.final <- metaMDS(greaterbiomass.nmds.ID.quarterly, distance='bray', k=2, trymax=20, autotransform=FALSE, pc=FALSE, plot=FALSE)

# Gives average stress
greaterbiomass.nmds.quarterly.final$stress

# Gives weights that different species hold in the axis
greaterbiomass.nmds.quarterly.final$species

# Basic plot of all of the points
plot(greaterbiomass.nmds.quarterly.final, display=c('sites', 'species'), choices=c(1,2), type='p')

nmds_species <- scores(greaterbiomass.nmds.quarterly.final, display = "species")

nmds_sites <- scores(greaterbiomass.nmds.quarterly.final, display = "sites")

# Scores
site.scores <- as.data.frame(scores(nmds_sites)) #Using the scores function from vegan to extract the site scores and convert to a data.frame
site.scores$site <- rownames(site.scores)# Add site column to dataframe
head(site.scores)

species.scores <- as.data.frame(scores(nmds_species)) 
species.scores$species <- rownames(species.scores)  
head(species.scores)


#Filtering based on top abundance
Biomass_Sums <- colSums(greaterbiomass.nmds.ID.quarterly)
Biomass_Sums # Sums of all taxa

Biomass_CutOff <- 100# Don't want taxa with an abundance less than 150

TOP <- greaterbiomass.nmds.ID.quarterly %>%
  select(where( ~ sum(.) >= Biomass_CutOff)) # Taxa with abundances greater than 150


# NMDS for top taxa only
TOPScores <-metaMDS(TOP, distance='bray', k=2, trymax=20, autotransform=FALSE, pc=FALSE, plot=FALSE)
TOPScores$stress

# TOP Scores

TOPGenera <- scores(TOPScores, display="species") # Scores for top species
TOPGenera <- as.data.frame(TOPGenera)
TOPGenera$species <- rownames(TOPGenera)  # create a column of species, from the rownames of TOPGenera

TOPsites <- scores(TOPScores, display="sites")
TOPsites <- as.data.frame(TOPsites)
rownames(TOPsites) <- c("EAS.OCT", "EAS.FEB", "EAS.MAY", "CRO.OCT","CRO.FEB", "CRO.MAY", 
                        "HCN.OCT", "HCN.FEB", "HCN.MAY","HUR.OCT", "HUR.FEB", "HUR.MAY", 
                        "FRY.OCT","FRY.FEB", "FRY.MAY", "RUT.OCT", "RUT.FEB", "RUT.MAY",
                        "RIC.OCT", "RIC.FEB", "RIC.MAY","LLW.OCT", "LLW.FEB","LLW.MAY", 
                        "LLC.OCT", "LLC.FEB", "LLC.MAY") # Change sites from numbers to categorical
TOPsites$site <- rownames(TOPsites) 

# Filter data for specific colors
ref <- subset(TOPsites, site %in% c("EAS.OCT", "EAS.FEB", "EAS.MAY", "CRO.OCT","CRO.FEB", "CRO.MAY", 
                        "HCN.OCT", "HCN.FEB", "HCN.MAY"))
mid <- subset(TOPsites, site %in% c("HUR.OCT", "HUR.FEB", "HUR.MAY", 
                                    "FRY.OCT","FRY.FEB", "FRY.MAY", "RUT.OCT", "RUT.FEB", "RUT.MAY"))
high <- subset(TOPsites, site %in% c("RIC.OCT", "RIC.FEB", "RIC.MAY","LLW.OCT", "LLW.FEB","LLW.MAY", 
                                     "LLC.OCT", "LLC.FEB", "LLC.MAY"))


# Create polygons around October sites

# Filter some data first for the ellipses
ref.oct <- subset(TOPsites, site %in% c("EAS.OCT", "CRO.OCT", "HCN.OCT"))
mid.oct <- subset(TOPsites, site %in% c("HUR.OCT", "FRY.OCT", "RUT.OCT"))
high.oct <- subset(TOPsites, site %in% c("RIC.OCT","LLW.OCT", "LLC.OCT"))

TOPsites_oct <- TOPsites %>% 
  filter(site %in% c("EAS.OCT", "CRO.OCT", "HCN.OCT", "HUR.OCT", "FRY.OCT", "RUT.OCT", "RIC.OCT", "LLW.OCT", "LLC.OCT"))

install.packages("ggforce")
library(ggforce)


 TOPPlot <- ggplot() +    
  geom_mark_ellipse(data = ref.oct, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#70A494", alpha = 0.3) +
  geom_mark_ellipse(data = mid.oct, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#EDBB8A", alpha = 0.3) +
  geom_mark_ellipse(data = high.oct, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#CA562C", alpha = 0.3) +
  geom_text(data = TOPGenera, aes(x = NMDS1, y = NMDS2, label = species), alpha = 0.0, vjust = 0.5, color = "white") +   
  geom_point(data = TOPsites_oct, aes(x = NMDS1, y = NMDS2, color = site), size = 3) + 
  geom_text(data = TOPsites_oct, aes(x = NMDS1, y = NMDS2, label = site), size = 2, vjust = 0.5) +
  scale_colour_manual(values = c(  "EAS.OCT" = "#70A494", "EAS.FEB" = "#70A49400", "EAS.MAY" = "#70A49400",
                                   "CRO.OCT" = "#70A494", "CRO.FEB" = "#70A49400", "CRO.MAY" = "#70A49400", 
                                   "HCN.OCT" = "#70A494", "HCN.FEB" = "#70A49400", "HCN.MAY" = "#70A49400",
                                   "HUR.OCT" = "#EDBB8A", "HUR.FEB" = "#EDBB8A00", "HUR.MAY" = "#EDBB8A00",
                                   "FRY.OCT" = "#EDBB8A", "FRY.FEB" = "#EDBB8A00", "FRY.MAY" = "#EDBB8A00", 
                                   "RUT.OCT" = "#EDBB8A", "RUT.FEB" = "#EDBB8A00", "RUT.MAY" = "#EDBB8A00",
                                   "RIC.OCT" = "#CA562C", "RIC.FEB" = "#CA562C00", "RIC.MAY" = "#CA562C00",
                                   "LLW.OCT" = "#CA562C", "LLW.FEB" = "#CA562C00", "LLW.MAY" = "#CA562C00", 
                                   "LLC.OCT" = "#CA562C", "LLC.FEB" = "#CA562C00", "LLC.MAY" = "#CA562C00")) +
  coord_equal() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(name = "NMDS1", limits = c(-.15, .15)) +
  scale_y_continuous(name = "NMDS2", limits = c(-.15, .15)) 

# Display the plot
print(TOPPlot)



# Create polygons around February sites

# Filter some data first for the ellipses
ref.feb <- subset(TOPsites, site %in% c("EAS.FEB", "CRO.FEB", "HCN.FEB"))
mid.feb <- subset(TOPsites, site %in% c("HUR.FEB", "FRY.FEB", "RUT.FEB"))
high.feb <- subset(TOPsites, site %in% c("RIC.FEB","LLW.FEB", "LLC.FEB"))

TOPsites_feb <- TOPsites %>% 
  filter(site %in% c("EAS.FEB", "CRO.FEB", "HCN.FEB", "HUR.FEB", "FRY.FEB", "RUT.FEB", "RIC.FEB", "LLW.FEB", "LLC.FEB"))

install.packages("ggforce")
library(ggforce)


TOPPlot <- ggplot() +    
  geom_mark_ellipse(data = ref.feb, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#70A494", alpha = 0.3) +
  geom_mark_ellipse(data = mid.feb, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#EDBB8A", alpha = 0.3) +
  geom_mark_ellipse(data = high.feb, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#CA562C", alpha = 0.3) +
  geom_text(data = TOPGenera, aes(x = NMDS1, y = NMDS2, label = species), alpha = 0.0, vjust = 0.5, color = "white") +   
  geom_point(data = TOPsites_feb, aes(x = NMDS1, y = NMDS2, color = site), size = 3) + 
  geom_text(data = TOPsites_feb, aes(x = NMDS1, y = NMDS2, label = site), size = 2, vjust = 0.5) +
  scale_colour_manual(values = c(  "EAS.OCT" = "#70A49400", "EAS.FEB" = "#70A494", "EAS.MAY" = "#70A49400",
                                   "CRO.OCT" = "#70A49400", "CRO.FEB" = "#70A494", "CRO.MAY" = "#70A49400", 
                                   "HCN.OCT" = "#70A49400", "HCN.FEB" = "#70A494", "HCN.MAY" = "#70A49400",
                                   "HUR.OCT" = "#EDBB8A00", "HUR.FEB" = "#EDBB8A", "HUR.MAY" = "#70A49400",
                                   "FRY.OCT" = "#EDBB8A00", "FRY.FEB" = "#EDBB8A", "FRY.MAY" = "#EDBB8A00", 
                                   "RUT.OCT" = "#EDBB8A00", "RUT.FEB" = "#EDBB8A", "RUT.MAY" = "#EDBB8A00",
                                   "RIC.OCT" = "#CA562C00", "RIC.FEB" = "#CA562C", "RIC.MAY" = "#CA562C00",
                                   "LLW.OCT" = "#CA562C00", "LLW.FEB" = "#CA562C", "LLW.MAY" = "#CA562C00", 
                                   "LLC.OCT" = "#CA562C00", "LLC.FEB" = "#CA562C", "LLC.MAY" = "#CA562C00")) +
  coord_equal() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(name = "NMDS1", limits = c(-.15, .15)) +
  scale_y_continuous(name = "NMDS2", limits = c(-.15, .15)) 

# Display the plot
print(TOPPlot)




# Create polygons around May sites

# Filter some data first for the ellipses
ref.may <- subset(TOPsites, site %in% c("EAS.MAY", "CRO.MAY", "HCN.MAY"))
mid.may <- subset(TOPsites, site %in% c("HUR.MAY", "FRY.MAY", "RUT.MAY"))
high.may <- subset(TOPsites, site %in% c("RIC.MAY","LLW.MAY", "LLC.MAY"))

TOPsites_may <- TOPsites %>% 
  filter(site %in% c("EAS.MAY", "CRO.MAY", "HCN.MAY", "HUR.MAY", "FRY.MAY", "RUT.MAY", "RIC.MAY", "LLW.MAY", "LLC.MAY"))

install.packages("ggforce")
library(ggforce)


TOPPlot <- ggplot() +    
  geom_mark_ellipse(data = ref.may, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#70A494", alpha = 0.3) +
  geom_mark_ellipse(data = mid.may, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#EDBB8A", alpha = 0.3) +
  geom_mark_ellipse(data = high.may, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#CA562C", alpha = 0.3) +
  geom_text(data = TOPGenera, aes(x = NMDS1, y = NMDS2, label = species), alpha = 0.0, vjust = 0.5, color = "white") +   
  geom_point(data = TOPsites_may, aes(x = NMDS1, y = NMDS2, color = site), size = 3) + 
  geom_text(data = TOPsites_may, aes(x = NMDS1, y = NMDS2, label = site), size = 2, vjust = 0.5) +
  scale_colour_manual(values = c(  "EAS.OCT" = "#70A49400", "EAS.FEB" = "#70A49400", "EAS.MAY" = "#70A494",
                                   "CRO.OCT" = "#70A49400", "CRO.FEB" = "#70A49400", "CRO.MAY" = "#70A494", 
                                   "HCN.OCT" = "#70A49400", "HCN.FEB" = "#70A49400", "HCN.MAY" = "#70A494",
                                   "HUR.OCT" = "#EDBB8A00", "HUR.FEB" = "#EDBB8A00", "HUR.MAY" = "#EDBB8A",
                                   "FRY.OCT" = "#EDBB8A00", "FRY.FEB" = "#EDBB8A00", "FRY.MAY" = "#EDBB8A", 
                                   "RUT.OCT" = "#EDBB8A00", "RUT.FEB" = "#EDBB8A00", "RUT.MAY" = "#EDBB8A",
                                   "RIC.OCT" = "#CA562C00", "RIC.FEB" = "#CA562C00", "RIC.MAY" = "#CA562C",
                                   "LLW.OCT" = "#CA562C00", "LLW.FEB" = "#CA562C00", "LLW.MAY" = "#CA562C", 
                                   "LLC.OCT" = "#CA562C00", "LLC.FEB" = "#CA562C00", "LLC.MAY" = "#CA562C")) +
  coord_equal() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(name = "NMDS1", limits = c(-.15, .15)) +
  scale_y_continuous(name = "NMDS2", limits = c(-.15, .15)) 

print(TOPPlot)



# Isolate REF sites for each quarterly

ref.oct <- subset(TOPsites, site %in% c("EAS.OCT", "CRO.OCT", "HCN.OCT"))
ref.feb <- subset(TOPsites, site %in% c("EAS.FEB", "CRO.FEB", "HCN.FEB"))
ref.may <- subset(TOPsites, site %in% c("EAS.MAY", "CRO.MAY", "HCN.MAY"))

TOPsites_ref <- TOPsites %>% 
  filter(site %in% c("EAS.OCT", "EAS.FEB", "EAS.MAY","CRO.OCT", "CRO.FEB", "CRO.MAY",
                     "HCN.OCT", "HCN.FEB", "HCN.MAY"))

REF.NMDS <- ggplot() +    
  geom_mark_ellipse(data = ref.oct, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#70A494", alpha = 0.3) +
  geom_mark_ellipse(data = ref.feb, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#EDBB8A", alpha = 0.3) +
  geom_mark_ellipse(data = ref.may, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#CA562C", alpha = 0.3) +
  geom_text(data = TOPGenera, aes(x = NMDS1, y = NMDS2, label = species), alpha = 0.0, vjust = 0.5, color = "white") +   
  geom_point(data = TOPsites_ref, aes(x = NMDS1, y = NMDS2, color = site), size = 3) + 
  geom_text(data = TOPsites_ref, aes(x = NMDS1, y = NMDS2, label = site), size = 2, vjust = 0.5) +
  scale_colour_manual(values = c(  "EAS.OCT" = "#70A494", "EAS.FEB" = "#EDBB8A", "EAS.MAY" = "#CA562C",
                                   "CRO.OCT" = "#70A494", "CRO.FEB" = "#EDBB8A", "CRO.MAY" = "#CA562C", 
                                   "HCN.OCT" = "#70A494", "HCN.FEB" = "#EDBB8A", "HCN.MAY" = "#CA562C",
                                   "HUR.OCT" = "#EDBB8A00", "HUR.FEB" = "#EDBB8A00", "HUR.MAY" = "#EDBB8A00",
                                   "FRY.OCT" = "#EDBB8A00", "FRY.FEB" = "#EDBB8A00", "FRY.MAY" = "#EDBB8A00", 
                                   "RUT.OCT" = "#EDBB8A00", "RUT.FEB" = "#EDBB8A00", "RUT.MAY" = "#EDBB8A00",
                                   "RIC.OCT" = "#CA562C00", "RIC.FEB" = "#CA562C00", "RIC.MAY" = "#CA562C00",
                                   "LLW.OCT" = "#CA562C00", "LLW.FEB" = "#CA562C00", "LLW.MAY" = "#CA562C00", 
                                   "LLC.OCT" = "#CA562C00", "LLC.FEB" = "#CA562C00", "LLC.MAY" = "#CA562C00")) +
  coord_equal() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(name = "NMDS1", limits = c(-.15, .15)) +
  scale_y_continuous(name = "NMDS2", limits = c(-.15, .15)) 

print(REF.NMDS)




# Isolate MID sites for each quarterly

mid.oct <- subset(TOPsites, site %in% c("HUR.OCT", "FRY.OCT", "RUT.OCT"))
mid.feb <- subset(TOPsites, site %in% c("HUR.FEB", "FRY.FEB", "RUT.FEB"))
mid.may <- subset(TOPsites, site %in% c("HUR.MAY", "FRY.MAY", "RUT.MAY"))

TOPsites_mid <- TOPsites %>% 
  filter(site %in% c("HUR.OCT", "FRY.OCT", "RUT.OCT","HUR.FEB", "FRY.FEB", "RUT.FEB","HUR.MAY", "FRY.MAY", "RUT.MAY"))

MID.NMDS <- ggplot() +    
  geom_mark_ellipse(data = mid.oct, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#70A494", alpha = 0.3) +
  geom_mark_ellipse(data = mid.feb, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#EDBB8A", alpha = 0.3) +
  geom_mark_ellipse(data = mid.may, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#CA562C", alpha = 0.3) +
  geom_text(data = TOPGenera, aes(x = NMDS1, y = NMDS2, label = species), alpha = 0.0, vjust = 0.5, color = "white") +   
  geom_point(data = TOPsites_mid, aes(x = NMDS1, y = NMDS2, color = site), size = 3) + 
  geom_text(data = TOPsites_mid, aes(x = NMDS1, y = NMDS2, label = site), size = 2, vjust = 0.5) +
  scale_colour_manual(values = c(  "EAS.OCT" = "#70A49400", "EAS.FEB" = "#70A49400", "EAS.MAY" = "#70A49400",
                                   "CRO.OCT" = "#70A49400", "CRO.FEB" = "#70A49400", "CRO.MAY" = "#70A49400", 
                                   "HCN.OCT" = "#70A49400", "HCN.FEB" = "#70A49400", "HCN.MAY" = "#70A49400",
                                   "HUR.OCT" = "#70A494", "HUR.FEB" = "#EDBB8A", "HUR.MAY" = "#CA562C",
                                   "FRY.OCT" = "#70A494", "FRY.FEB" = "#EDBB8A", "FRY.MAY" = "#CA562C", 
                                   "RUT.OCT" = "#70A494", "RUT.FEB" = "#EDBB8A", "RUT.MAY" = "#CA562C",
                                   "RIC.OCT" = "#CA562C00", "RIC.FEB" = "#CA562C00", "RIC.MAY" = "#CA562C00",
                                   "LLW.OCT" = "#CA562C00", "LLW.FEB" = "#CA562C00", "LLW.MAY" = "#CA562C00", 
                                   "LLC.OCT" = "#CA562C00", "LLC.FEB" = "#CA562C00", "LLC.MAY" = "#CA562C00")) +
  coord_equal() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(name = "NMDS1", limits = c(-.15, .15)) +
  scale_y_continuous(name = "NMDS2", limits = c(-.15, .15)) 

print(MID.NMDS)


# Isolate HIGH sites for each quarterly

high.oct <- subset(TOPsites, site %in% c("RIC.OCT", "LLW.OCT", "LLC.OCT"))
high.feb <- subset(TOPsites, site %in% c("RIC.FEB", "LLW.FEB", "LLC.FEB"))
high.may <- subset(TOPsites, site %in% c("RIC.MAY", "LLW.MAY", "LLC.MAY"))

TOPsites_high <- TOPsites %>% 
  filter(site %in% c("RIC.OCT", "LLW.OCT", "LLC.OCT", "RIC.FEB", "LLW.FEB", "LLC.FEB", "RIC.MAY", "LLW.MAY", "LLC.MAY"))

HIGH.NMDS <- ggplot() +    
  geom_mark_ellipse(data = high.oct, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#70A494", alpha = 0.3) +
  geom_mark_ellipse(data = high.feb, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#EDBB8A", alpha = 0.3) +
  geom_mark_ellipse(data = high.may, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#CA562C", alpha = 0.3) +
  geom_text(data = TOPGenera, aes(x = NMDS1, y = NMDS2, label = species), alpha = 0.0, vjust = 0.5, color = "white") +   
  geom_point(data = TOPsites_high, aes(x = NMDS1, y = NMDS2, color = site), size = 3) + 
  geom_text(data = TOPsites_high, aes(x = NMDS1, y = NMDS2, label = site), size = 2, vjust = 0.5) +
  scale_colour_manual(values = c(  "EAS.OCT" = "#70A49400", "EAS.FEB" = "#70A49400", "EAS.MAY" = "#70A49400",
                                   "CRO.OCT" = "#70A49400", "CRO.FEB" = "#70A49400", "CRO.MAY" = "#70A49400", 
                                   "HCN.OCT" = "#70A49400", "HCN.FEB" = "#70A49400", "HCN.MAY" = "#70A49400",
                                   "HUR.OCT" = "#EDBB8A00", "HUR.FEB" = "#EDBB8A00", "HUR.MAY" = "#EDBB8A00",
                                   "FRY.OCT" = "#EDBB8A00", "FRY.FEB" = "#EDBB8A00", "FRY.MAY" = "#EDBB8A00", 
                                   "RUT.OCT" = "#EDBB8A00", "RUT.FEB" = "#EDBB8A00", "RUT.MAY" = "#EDBB8A00",
                                   "RIC.OCT" = "#70A494", "RIC.FEB" = "#EDBB8A", "RIC.MAY" = "#CA562C",
                                   "LLW.OCT" = "#70A494", "LLW.FEB" = "#EDBB8A", "LLW.MAY" = "#CA562C", 
                                   "LLC.OCT" = "#70A494", "LLC.FEB" = "#EDBB8A", "LLC.MAY" = "#CA562C")) +
  coord_equal() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(name = "NMDS1", limits = c(-.15, .15)) +
  scale_y_continuous(name = "NMDS2", limits = c(-.15, .15)) 

print(HIGH.NMDS)




# STATS----------------

#Let's look at effects
pr <- function(m) printCoefmat(coef(summary(m)),
                               digits=3,signif.stars=FALSE) 

#Get the estimates per group, rather than the differences
pr(lm1 <- lm(mean.biomass~SC.Category,data=meansites))

pr(lm2 <- lm(mean.biomass~SC.Level,data=meansites))

pr(lm3<- lm(mean.biomass~FFG, data=meansites))


anova(lm1,lm2,lm3)

library(effects)

summary(allEffects(lm1))

summary(allEffects(lm2))

summary(allEffects(lm3))


#plot the effects 
plot(allEffects(lm1)) #High has highest biomass of the sites, RIC probably pulling it a bit

plot(allEffects(lm2))

plot(allEffects(lm3))


#Playing around with glm
library(DHARMa)
news(package='DHARMa')





