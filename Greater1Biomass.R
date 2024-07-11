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
EASjan=read.csv("EAS_Jan.2023_SUMMARY.csv")
FRYjan=read.csv("FRY_Jan.2023_SUMMARY.csv")
RICjan=read.csv("RIC_Jan.2023_SUMMARY.csv")
# February quarterly
CROfeb=read.csv("CRO_Feb.2023_SUMMARY.csv")
EASfeb=read.csv("EAS_Feb.2023_SUMMARY.csv")
HCNfeb=read.csv("HCN_Feb.2023_SUMMARY.csv")
FRYfeb=read.csv("FRY_Feb.2023_SUMMARY.csv")
HURfeb=read.csv("HUR_Feb.2023_SUMMARY.csv")
RUTfeb=read.csv("RUT_Feb.2023_SUMMARY.csv")
LLWfeb=read.csv("LLW_Feb.2023_SUMMARY.csv")
LLCfeb=read.csv("LLC_Feb.2023_SUMMARY.csv")
RICfeb=read.csv("RIC_Feb.2023_SUMMARY.csv")
# March monthly
EASmarch=read.csv("EAS_March.2023_SUMMARY.csv")
FRYmarch=read.csv("FRY_March.2023_SUMMARY.csv")
RICmarch=read.csv("RIC_March.2023_SUMMARY.csv")
# April monthly
EASapril=read.csv("EAS_April.2023_SUMMARY.csv")
FRYapril=read.csv("FRY_April.2023_SUMMARY.csv")
RICapril=read.csv("RIC_April.2023_SUMMARY.csv")
# May quarterly
CROmay=read.csv("CRO_May.2023_SUMMARY.csv")
EASmay=read.csv("EAS_May.2023_SUMMARY.csv")
HCNmay=read.csv("HCN_May.2023_SUMMARY.csv")
FRYmay=read.csv("FRY_May.2023_SUMMARY.csv")
HURmay=read.csv("HUR_May.2023_SUMMARY.csv")
RUTmay=read.csv("RUT_May.2023_SUMMARY.csv")
LLWmay=read.csv("LLW_May.2023_SUMMARY.csv")
LLCmay=read.csv("LLC_May.2023_SUMMARY.csv")
RICmay=read.csv("RIC_May.2023_SUMMARY.csv")
# June monthly
EASjune=read.csv("EAS_June.2023_SUMMARY.csv")
FRYjune=read.csv("FRY_June.2023_SUMMARY.csv")
RICjune=read.csv("RIC_June.2023_SUMMARY.csv")


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
RICoct$SC.Level <- c("1457"))
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
EASmarch<- EAsmarch %>% 
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



# Exclude all the abundance and biomass, only totals and descriptors
columns_to_keep <- c("Sample.Date", "Site", "SC.Level", "SC.Category", "Fraction", "Replicate", "Order", "Family", "Genus","Abundance","Density","Biomass")  # Columns to keep

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



