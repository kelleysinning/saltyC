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

#I made a "totals"doc already for stats, but wanted to challenge myself with making the code for
#it, can refer back to this to doubl check myself
biomassoctpre.made=read.csv("OCT23BIOMASS.csv")

head(biomassoctpre.made)

# Ok, bring in the csv summary sheets for each site 
CROoct=read.csv("CRO_Oct.2023_SUMMARY.csv")
EASoct=read.csv("EAS_Oct.2023_SUMMARY.csv")
HCNoct=read.csv("HCN_Oct.2023_SUMMARY.csv")
FRYoct=read.csv("FRY_Oct.2023_SUMMARY.csv")
HURoct=read.csv("HUR_Oct.2023_SUMMARY.csv")
RUToct=read.csv("RUT_Oct.2023_SUMMARY.csv")
LLWoct=read.csv("LLW_Oct.2023_SUMMARY.csv")
LLCoct=read.csv("LLC_Oct.2023_SUMMARY.csv")
RICoct=read.csv("RIC_Oct.2023_SUMMARY.csv")

# Adding a column with site name bc once we merge we won't know
CROoct$Site <- c("CRO")
EASoct$Site <- c("EAS")
HCNoct$Site <- c("HCN")
FRYoct$Site <- c("FRY")
HURoct$Site <- c("HUR")
RUToct$Site <- c("RUT")
LLWoct$Site <- c("LLW")
LLCoct$Site <- c("LLC")
RICoct$Site <- c("RIC")

# Adding a column with SC level bc once we merge we won't know
CROoct$SC.Level <- c("72")
EASoct$SC.Level <- c("25")
HCNoct$SC.Level <- c("78")
FRYoct$SC.Level <- c("402")
HURoct$SC.Level <- c("387")
RUToct$SC.Level <- c("594")
LLWoct$SC.Level <- c("1119")
LLCoct$SC.Level <- c("1242")
RICoct$SC.Level <- c("1457")

# Adding a column with SC Category bc once we merge we won't know
CROoct$SC.Category <- c("REF")
EASoct$SC.Category <- c("REF")
HCNoct$SC.Category <- c("REF")
FRYoct$SC.Category <- c("MID")
HURoct$SC.Category <- c("MID")
RUToct$SC.Category <- c("MID")
LLWoct$SC.Category <- c("HIGH")
LLCoct$SC.Category <- c("HIGH")
RICoct$SC.Category <- c("HIGH")


# Let's re-arrange the columns so these new additions are at the front
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

# Exclude all the abundance and biomass, only totals and descriptors
columns_to_keep <- c("Sample.Date", "Site", "SC.Level", "SC.Category", "Fraction", "Replicate", "Order", "Family", "Genus","Abundance","Density","Biomass")  # Columns to keep

CROoct.totals <- select(CROoct, all_of(columns_to_keep))
EASoct.totals <- select(EASoct, all_of(columns_to_keep))
HCNoct.totals <- select(HCNoct, all_of(columns_to_keep))
FRYoct.totals <- select(FRYoct, all_of(columns_to_keep))
HURoct.totals <- select(HURoct, all_of(columns_to_keep))
RUToct.totals <- select(RUToct, all_of(columns_to_keep))
LLWoct.totals <- select(LLWoct, all_of(columns_to_keep))
LLCoct.totals <- select(LLCoct, all_of(columns_to_keep))
RICoct.totals <- select(RICoct, all_of(columns_to_keep))


# Merge data frames based on trimmed columns (the totals and descriptors)

list_of_oct_totals <- list(CROoct.totals, EASoct.totals,HCNoct.totals, FRYoct.totals,
                           HURoct.totals, RUToct.totals, LLWoct.totals, LLCoct.totals,
                           RICoct.totals) 

biomassoct <- do.call(rbind, list_of_oct_totals) #THIS WORKS


# Rarranging order of columns
biomassoct <- select(biomassoct,Site,SC.Category,SC.Level,Sample.Date,Fraction,
                     Replicate,Order,Family,Genus,Abundance,Density,Biomass)


# Fixing up data sheets
# First, let's add a new column to correct biomass by area
biomassoct <- biomassoct %>% mutate(Biomass.Area.Corrected = Biomass*Density)

## Okay, now we gotta rearrange the data in another funky way 
# First, I want cut out the abundance columns (the count data), total abundance, density, and biomass,
# order and family, and FFG for each site. I want to do this so that I just have 
# site info and biomass (0s and #s) for each genera

# Find the index of the target column, have to do this for each df
target_column_indexCRO <- which(names(CROoct) == 'BIOMASS.STARTS.HERE')
target_column_indexEAS <- which(names(EASoct) == 'BIOMASS.STARTS.HERE')
target_column_indexHCN <- which(names(HCNoct) == 'BIOMASS.STARTS.HERE')
target_column_indexFRY <- which(names(FRYoct) == 'BIOMASS.STARTS.HERE')
target_column_indexHUR <- which(names(HURoct) == 'BIOMASS.STARTS.HERE')
target_column_indexRUT <- which(names(RUToct) == 'BIOMASS.STARTS.HERE')
target_column_indexRIC <- which(names(RICoct) == 'BIOMASS.STARTS.HERE')
target_column_indexLLC <- which(names(LLCoct) == 'BIOMASS.STARTS.HERE')
target_column_indexLLW <- which(names(LLWoct) == 'BIOMASS.STARTS.HERE')

# Subset the data frame to keep specified columns and columns after the target column
# Keeping Site, SC.Leve;, SC. Category, Sample Date, Fraction, Replicate, Genus, 
#and every thing for biomass

CRO_subset <- CROoct[, c(1:6, 9, (target_column_indexCRO + 1):ncol(CROoct))]
EAS_subset <- EASoct[, c(1:6, 9, (target_column_indexEAS + 1):ncol(EASoct))]
HCN_subset <- HCNoct[, c(1:6, 9, (target_column_indexHCN + 1):ncol(HCNoct))]
FRY_subset <- FRYoct[, c(1:6, 9, (target_column_indexFRY + 1):ncol(FRYoct))]
HUR_subset <- HURoct[, c(1:6, 9, (target_column_indexHUR + 1):ncol(HURoct))]
RUT_subset <- RUToct[, c(1:6, 9, (target_column_indexRUT + 1):ncol(RUToct))]
RIC_subset <- RICoct[, c(1:6, 9, (target_column_indexRIC + 1):ncol(RICoct))]
LLC_subset <- LLCoct[, c(1:6, 9, (target_column_indexLLC + 1):ncol(LLCoct))]
LLW_subset <- LLWoct[, c(1:6, 9, (target_column_indexLLW + 1):ncol(LLWoct))]

# Before I can merge all those guys to add in columns with zeros to make them the same
#number of columns, I need to cut out the summary totals at the end of everything.
#Yes, I could've probably done that above but would've been a bit complicated

columns_to_delete <- c("Abundance", "Density", "Biomass")  

# Subset the data frame by selecting only the columns that are not in 'columns_to_delete'
CRO_subset_final <- CRO_subset[, !names(CRO_subset) %in% columns_to_delete]
EAS_subset_final <- EAS_subset[, !names(EAS_subset) %in% columns_to_delete]
HCN_subset_final <- HCN_subset[, !names(HCN_subset) %in% columns_to_delete]
FRY_subset_final <- FRY_subset[, !names(FRY_subset) %in% columns_to_delete]
HUR_subset_final <- HUR_subset[, !names(HUR_subset) %in% columns_to_delete]
RUT_subset_final <- RUT_subset[, !names(RUT_subset) %in% columns_to_delete]
RIC_subset_final <- RIC_subset[, !names(RIC_subset) %in% columns_to_delete]
LLC_subset_final <- LLC_subset[, !names(LLC_subset) %in% columns_to_delete]
LLW_subset_final <- LLW_subset[, !names(LLW_subset) %in% columns_to_delete]



# Now, I was silly and have different numbers of columns for each df. In future months,
#I'll make sure they all have, say 50, columns for size class to avoid this step. 
#But for now, I had to add columns of zeroes to make them all equal. Our df with the most
#columns is LLW with 48, so I'll have to make sure the others have 48 and just fill them with 
#zeroes. This way I can merge them for nmds

num_new_columnsCRO <- 48-27 # In CRO I have 27 variables and LLW has 48, so I'll subtract to
                              #know what I need to add
num_new_columnsEAS <- 48-26
num_new_columnsHCN <- 48-27
num_new_columnsFRY <- 48-27
num_new_columnsHUR <- 48-40
num_new_columnsRUT <- 48-24
num_new_columnsRIC <- 48-32
num_new_columnsLLC <- 48-27


# Add new columns filled with zeroes
for (i in 1:num_new_columnsCRO) {
  CRO_subset_final[[paste0("New_Column", i)]] <- 0
}

for (i in 1:num_new_columnsEAS) {
  EAS_subset_final[[paste0("New_Column", i)]] <- 0
}

for (i in 1:num_new_columnsHCN) {
  HCN_subset_final[[paste0("New_Column", i)]] <- 0
}

for (i in 1:num_new_columnsFRY) {
  FRY_subset_final[[paste0("New_Column", i)]] <- 0
}

for (i in 1:num_new_columnsHUR) {
  HUR_subset_final[[paste0("New_Column", i)]] <- 0
}

for (i in 1:num_new_columnsRUT) {
  RUT_subset_final[[paste0("New_Column", i)]] <- 0
}

for (i in 1:num_new_columnsRIC) {
  RIC_subset_final[[paste0("New_Column", i)]] <- 0
}

for (i in 1:num_new_columnsLLC) {
  LLC_subset_final[[paste0("New_Column", i)]] <- 0
}


# Next, we have to rename all the columns to be the same size class so there aren't
# certain dfs with 12 "new_columns" but another with 4 and it's weird


# Generate new column names
new_column_namesCRO <- seq_len(ncol(CRO_subset_final) - 7)
new_column_namesEAS <- seq_len(ncol(EAS_subset_final) - 7)
new_column_namesHCN <- seq_len(ncol(HCN_subset_final) - 7)
new_column_namesFRY <- seq_len(ncol(FRY_subset_final) - 7)
new_column_namesHUR <- seq_len(ncol(HUR_subset_final) - 7)
new_column_namesRUT <- seq_len(ncol(RUT_subset_final) - 7)
new_column_namesRIC <- seq_len(ncol(RIC_subset_final) - 7)
new_column_namesLLC <- seq_len(ncol(LLC_subset_final) - 7)
new_column_namesLLW <- seq_len(ncol(LLW_subset_final) - 7)

# Rename the columns
names(CRO_subset_final)[8:ncol(CRO_subset_final)] <- new_column_namesCRO
names(EAS_subset_final)[8:ncol(EAS_subset_final)] <- new_column_namesEAS
names(HCN_subset_final)[8:ncol(HCN_subset_final)] <- new_column_namesHCN
names(FRY_subset_final)[8:ncol(FRY_subset_final)] <- new_column_namesFRY
names(HUR_subset_final)[8:ncol(HUR_subset_final)] <- new_column_namesHUR
names(RUT_subset_final)[8:ncol(RUT_subset_final)] <- new_column_namesRUT
names(RIC_subset_final)[8:ncol(RIC_subset_final)] <- new_column_namesRIC
names(LLC_subset_final)[8:ncol(LLC_subset_final)] <- new_column_namesLLC
names(LLW_subset_final)[8:ncol(LLW_subset_final)] <- new_column_namesLLW

## Amazing, now all of the subsetted data has the same amount of columns. Let's merge

list_of_oct <- list(CRO_subset_final, EAS_subset_final, HCN_subset_final, FRY_subset_final,
                           HUR_subset_final, RUT_subset_final, RIC_subset_final, LLC_subset_final,
                           LLW_subset_final) 

mergedoct <- do.call(rbind, list_of_oct)

# Reaslized I didn't need to do this lol, hopefully will use later on
# Saving these as csv
write.csv(mergedoct, "merged_oct_biomass.csv", row.names=FALSE)
write.csv(biomassoct, "BiomassOctTotals.csv", row.names = FALSE)
#-----------------------------------------

#Adding FFGs...this will become relevant later 

biomassoct$FFG[biomassoct$Genus=="Acerpenna"] ="Collector-Gatherer"
biomassoct$FFG[biomassoct$Genus=="Acroneuria"]="Predator"
biomassoct$FFG[biomassoct$Genus=="Allognasta"]="Collector-Gatherer"
biomassoct$FFG[biomassoct$Genus=="Alloperla"]="Collector-Gatherer"
biomassoct$FFG[biomassoct$Genus=="Antocha"]="Collector-Gatherer"
biomassoct$FFG[biomassoct$Genus=="Atherix"]="Predator"
biomassoct$FFG[biomassoct$Genus=="Baetis"]="Collector-Gatherer"
biomassoct$FFG[biomassoct$Genus=="Baetisca"]="Collector-Gatherer"
biomassoct$FFG[biomassoct$Genus=="Boyeria"]="Predator"
biomassoct$FFG[biomassoct$Genus=="Capniidae"]="Shredder"
biomassoct$FFG[biomassoct$Genus=="Ceratopogonidae"]="Predator"
biomassoct$FFG[biomassoct$Genus=="Cernotina"]="Predator"
biomassoct$FFG[biomassoct$Genus=="Chauloides"]="Predator"
biomassoct$FFG[biomassoct$Genus=="Chelifera"]="Predator"
biomassoct$FFG[biomassoct$Genus=="Cheumatopsyche"]="Collector-Filterer"
biomassoct$FFG[biomassoct$Genus=="Chironomidae"]="Collector-Gatherer"
biomassoct$FFG[biomassoct$Genus=="Chironomini"]="Collector-Gatherer"
biomassoct$FFG[biomassoct$Genus=="Circulionidae"]="Scraper"
biomassoct$FFG[biomassoct$Genus=="Collembola"]="Collector-Gatherer"
biomassoct$FFG[biomassoct$Genus=="Cyrnellus"]="Collector-Filterer"
biomassoct$FFG[biomassoct$Genus=="Dicranota"]="Predator"
biomassoct$FFG[biomassoct$Genus=="Diplectrona"]="Collector-Filterer"
biomassoct$FFG[biomassoct$Genus=="Discocerina"]="Collector-Gatherer"
biomassoct$FFG[biomassoct$Genus=="Dixa"]="Collector-Gatherer"
biomassoct$FFG[biomassoct$Genus=="Dixella"]="Collector-Gatherer"
biomassoct$FFG[biomassoct$Genus=="Dolophilodes"]="Collector-Filterer"
biomassoct$FFG[biomassoct$Genus=="Ectopria"]="Scraper"
biomassoct$FFG[biomassoct$Genus=="Ephemera"]="Collector-Gatherer"
biomassoct$FFG[biomassoct$Genus=="Ephemerellidae"]="Collector-Gatherer"
biomassoct$FFG[biomassoct$Genus=="Eurylophella"]="Collector-Gatherer"
biomassoct$FFG[biomassoct$Genus=="Glossosoma"]="Scraper"
biomassoct$FFG[biomassoct$Genus=="Goera"]="Scraper"
biomassoct$FFG[biomassoct$Genus=="Gomphus"]="Predator"
biomassoct$FFG[biomassoct$Genus=="Gomphurus"]="Predator"
biomassoct$FFG[biomassoct$Genus=="Gyrinus"]="Predator"
biomassoct$FFG[biomassoct$Genus=="Helichus"]="Scraper"
biomassoct$FFG[biomassoct$Genus=="Hemiptera"]="Predator"
biomassoct$FFG[biomassoct$Genus=="Heptageniidae"]="Scraper"
biomassoct$FFG[biomassoct$Genus=="Hetaerina"]="Predator"
biomassoct$FFG[biomassoct$Genus=="Hexatoma"]="Predator"
biomassoct$FFG[biomassoct$Genus=="Hydrachnia"]="Predator"
biomassoct$FFG[biomassoct$Genus=="Hydropsyche"]="Collector-Filterer"
biomassoct$FFG[biomassoct$Genus=="Isoperla"]="Predator"
biomassoct$FFG[biomassoct$Genus=="Langessa"]="Scraper"
biomassoct$FFG[biomassoct$Genus=="Leptophlebiidae"]="Collector-Gatherer"
biomassoct$FFG[biomassoct$Genus=="Leuctra"]="Shredder"
biomassoct$FFG[biomassoct$Genus=="Leuctridae"]="Shredder"
biomassoct$FFG[biomassoct$Genus=="Lypodiversa"]="Collector-Filterer"
biomassoct$FFG[biomassoct$Genus=="Microvelia"]="Predator"
biomassoct$FFG[biomassoct$Genus=="Molophilus"]="Shredder"
biomassoct$FFG[biomassoct$Genus=="Neoplasta"]="Collector-Gatherer"
biomassoct$FFG[biomassoct$Genus=="Oligochaeta"]="Collector-Gatherer"
biomassoct$FFG[biomassoct$Genus=="Optioservus(L)"]="Scraper"
biomassoct$FFG[biomassoct$Genus=="Oreogeton"]="Predator"
biomassoct$FFG[biomassoct$Genus=="Orthocladine"]="Collector-Gatherer"
biomassoct$FFG[biomassoct$Genus=="Oulimnius(L)"]="Scraper"
biomassoct$FFG[biomassoct$Genus=="Paracapnia"]="Shredder"
biomassoct$FFG[biomassoct$Genus=="Paraleptophlebiidae"]="Collector-Gatherer"
biomassoct$FFG[biomassoct$Genus=="Polycentropodidae"]="Collector-Filterer"
biomassoct$FFG[biomassoct$Genus=="Polycentropus"]="Predator"
biomassoct$FFG[biomassoct$Genus=="Probezzia"]="Predator"
biomassoct$FFG[biomassoct$Genus=="Psephenus"]="Scraper"
biomassoct$FFG[biomassoct$Genus=="Pseudolimnophila"]="Predator"
biomassoct$FFG[biomassoct$Genus=="Psychodini"]="Collector-Gatherer"
biomassoct$FFG[biomassoct$Genus=="Pteronarcys"]="Shredder"
biomassoct$FFG[biomassoct$Genus=="Paracapnia"]="Shredder"
biomassoct$FFG[biomassoct$Genus=="Rhagovelia"]="Predator"
biomassoct$FFG[biomassoct$Genus=="Rhyacophila"]="Predator"
biomassoct$FFG[biomassoct$Genus=="Prostoia"]="Shredder"
biomassoct$FFG[biomassoct$Genus=="Sialis"]="Predator"
biomassoct$FFG[biomassoct$Genus=="Simulium"]="Collector-Filterer"
biomassoct$FFG[biomassoct$Genus=="Stratiomyidae"]="Collector-Gatherer"
biomassoct$FFG[biomassoct$Genus=="Stylogomphus"]="Predator"
biomassoct$FFG[biomassoct$Genus=="Stylogomphurus"]="Predator"
biomassoct$FFG[biomassoct$Genus=="Stenelmis (L)"]="Scraper"
biomassoct$FFG[biomassoct$Genus=="Stenonema"]="Scraper" 
biomassoct$FFG[biomassoct$Genus=="Taeniopteryx"]="Shredder"
biomassoct$FFG[biomassoct$Genus=="Tanypodinae"]="Predator"
biomassoct$FFG[biomassoct$Genus=="Tanytarsini"]="Collector-Gatherer"
biomassoct$FFG[biomassoct$Genus=="Tipula"]="Shredder"
biomassoct$FFG[biomassoct$Genus=="Triacanthagyna"]="Predator"
biomassoct$FFG[biomassoct$Genus=="Wormaldia"]="Collector-Filterer"
biomassoct$FFG[biomassoct$Genus=="Zoraena"]="Predator"


#taking out data points for things with no assigned FFGs (adults, pupa, terrestrial)
biomassoct <- biomassoct[!is.na(biomassoct$FFG), ]

# Taking out zeroes in biomass too
biomassoct <- biomassoct[biomassoct$Biomass.Area.Corrected != 0,]

#-----Checking biomass for hiya for target taxa
filtered_data <- subset(biomassoct, Genus == "Leuctra" & Site == "RIC")

total_biomass <- sum(filtered_data$Density, na.rm = TRUE)

# Print the result
print(total_biomass)


filtered_data <- subset(meansites, SC.Category == "REF")

total_biomass <- sum(filtered_data$mean.biomass, na.rm = TRUE)

# Print the result
print(total_biomass)



#-----Summarizing means of each replicate for each stream, changing this to median
biomassmeantable = biomassoct %>% 
  group_by(SC.Category,SC.Level,Site,Replicate,FFG ) %>% 
  summarise(mean.biomass=median(Biomass.Area.Corrected,na.rm=FALSE))

biomassmeantable #it worked!!

# Let's QAQC
biomassoct %>%
  filter(Site == "LLW", Replicate == 2, FFG == "Collector-Filterer") %>%  # Group data by category
  summarise(biomasssum = median(Biomass.Area.Corrected)) # This confirms that biomassmeantable
#is averaging FFGs per site per replicate correctly

# Now, averaging the replicates from each stream
meansites = biomassmeantable %>% 
  group_by(SC.Category,SC.Level,Site,FFG ) %>% 
  summarise(mean.biomass=median(mean.biomass,na.rm=FALSE))

# This should be finding the median of mean.biomass by Site and FFG, so should be the
#median of the medians of each replicate

meansites #this also worked, i double checked. This is what we want to use for analysis
#bc averages of each site for each FFG. Do we want medians of medians of means of medians


#But...Let's QAQC with raw data again
biomassoct %>%
  filter(Site == "LLW", FFG == "Collector-Filterer") %>%  # Group data by category
  summarise(biomasssum = median(Biomass.Area.Corrected, na.rm = TRUE),) #this value
#is slightly off from meansites, but it is the correct mean based on biomassmeantable,
#which I think is more what we want


#------------------------------------------------
# We aren't going to used these trimmed data sets anymore for now, too transformative upon discussion 

Q1 <- quantile(biomassmeantable$mean.biomass, 0.25, na.rm=TRUE) #na.rm = true means
#ignoring missing values which is ok bc the NAs will be adults or pupas
Q3 <- quantile(biomassmeantable$mean.biomass, 0.75, na.rm=TRUE)

IQR <- Q3 - Q1

#Define upper and lower bounds for outliers
lower_bound <- Q1 - 1.5 * IQR
upper_bound <- Q3 + 1.5 * IQR

#Trim outliers on biomassmeantable (includes reps per site)
trimmed_data <- subset(biomassmeantable, mean.biomass >= lower_bound & mean.biomass <= upper_bound)


#And let's do this for our mean reps --> mean sites
Q1mean <- quantile(meansites$mean.biomass, 0.25, na.rm=TRUE)
Q3mean <- quantile(meansites$mean.biomass, 0.75, na.rm=TRUE)
IQRmean <- Q3mean - Q1mean

#Define upper and lower bounds for outliers
lower_boundmean <- Q1mean - 1.5 * IQRmean
upper_boundmean <- Q3mean + 1.5 * IQRmean

#Trim outliers for site averages
trimmed_means <- subset(meansites, mean.biomass >= lower_boundmean & mean.biomass <= upper_boundmean)

#to not plot NAs...
trimmed_data[trimmed_data == ""] <- NA

library(tidyverse)
trimmed_data = trimmed_data %>%
  drop_na(FFG)%>%
  drop_na(mean.biomass)

trimmed_means[trimmed_means == ""] <- NA

library(tidyverse)
trimmed_means = trimmed_means %>%
  drop_na(FFG)%>%
  drop_na(mean.biomass)

#---------------------------------
# GGplot mania 
# First, let's order them correctly
meansites$Site <- factor(meansites$Site, levels = c("EAS", "CRO","HCN","FRY","HUR","RUT","LLC","LLW","RIC"))
meansites$SC.Category <- factor(meansites$SC.Category, levels = c("REF","MID","HIGH"))
meansites$SC.Level <- factor(meansites$SC.Level, levels = c("25","72","78","387","402","594","1119","1242","1457"))

biomassmeantable$Site <- factor(biomassmeantable$Site, levels = c("EAS", "CRO","HCN","FRY","HUR","RUT","LLC","LLW","RIC"))
biomassmeantable$SC.Category <- factor(biomassmeantable$SC.Category, levels = c("REF","MID","HIGH"))
biomassmeantable$SC.Level <- factor(biomassmeantable$SC.Level, levels = c("25","72","78","387","402","594","1119","1242","1457"))

# Also, for color scheme
install.packages("rcartocolor")# Colorblind color schemes
library(rcartocolor)
display_carto_all()



# Create basic ggplot with meansites, initial look
meansitesbasic = ggplot(meansites, aes(x = Site, y = mean.biomass, color = FFG)) +
  geom_point() +
  labs(x = "Stream", y = "Biomass") +
  scale_color_carto_d(name = "FFG", palette = "Vivid", n = length(unique(meansites$FFG))) + 
  theme_minimal()

meansitesbasic

meansitesbasic = ggplot(meansites, aes(x = Site, y = mean.biomass, color= SC.Category)) +
  geom_boxplot() +
  labs(x = "Stream", y = "Biomass") +
  scale_color_carto_d(name = "SC.Category", palette = "Vivid", n = length(unique(meansites$SC.Category))) + 
  theme_minimal()

meansitesbasic # Boxplots are using points from each FFG for the whole site


# FFG boxplot with median data, need to use biomassmeantable to get boxplot, since meansites
# only have one value per FFG
FFGgplot=ggplot(data=biomassmeantable,aes(x=SC.Level,y=mean.biomass, color=FFG))+
  geom_boxplot() +
  labs(x = "Stream", y = "Biomass (g/m2)") +
  theme_minimal() +
  scale_color_carto_d(name = "FFG", palette = "Vivid", n = length(unique(biomassmeantable$FFG))) 

FFGgplot # These colors are pretty


##PANEL PARTY------------------------------------------------
meansites$Site <- factor(meansites$Site, levels = c("EAS", "CRO","HCN","FRY","HUR","RUT","LLC","LLW","RIC"))
meansites$SC.Category <- factor(meansites$SC.Category, levels = c("REF","MID","HIGH"))
meansites$SC.Level <- factor(meansites$SC.Level, levels = c("25","72","78","387","402","594","1119","1242","1457"))
meansites$FFG <- factor(meansites$FFG, levels = c("Scraper","Shredder","Predator","Collector-Gatherer","Collector-Filterer"))

biomassmeantable$Site <- factor(biomassmeantable$Site, levels = c("EAS", "CRO","HCN","FRY","HUR","RUT","LLC","LLW","RIC"))
biomassmeantable$SC.Category <- factor(biomassmeantable$SC.Category, levels = c("REF","MID","HIGH"))
biomassmeantable$SC.Level <- factor(biomassmeantable$SC.Level, levels = c("25","72","78","387","402","594","1119","1242","1457"))


my_colors = carto_pal(7, "Geyser") # to get hexcodes for geyser to assign FFGS to, for 
#consistency in graphs I'll use for SFS
my_colors
ffg_colors <- c("Scraper" = "#008080", 
                "Shredder" = "#CA562C", 
                "Predator" = "#F6EDBD", 
                "Collector-Gatherer" = "#DE8A5A", 
                "Collector-Filterer" = "#70A494")  

# Panels by FFG across SC gradient
FFGgplot1 <- ggplot(data = biomassmeantable, aes(x = SC.Level, y = log(mean.biomass))) +
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
FFGgplot1 

# Let's make panels based on SC category, can use meansites year since we're doing SC.Cat
# so there'll be 3 points per each FFG per each category
FFGgplot2=ggplot(data=meansites,aes(x=FFG,y=mean.biomass))+
  facet_wrap(~SC.Category,ncol=6,nrow=6)+ # This is creating multiple "panels" for site
  geom_boxplot()+
  geom_point(aes(color=SC.Category),size=2)+
  ylab("Biomass (g/m2)")+
  xlab("")+
  scale_color_carto_d(name = "FFG", palette = "Vivid", n = length(unique(meansites$FFG))) +
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

FFGgplot2 # Hard to see patterns rn 

# Same plot, but log transformed to see better
FFGgplot2.5 <- ggplot(data = meansites, aes(x = FFG, y = (log(mean.biomass)))) +
  facet_wrap(~SC.Category, ncol = 3, nrow = 3) +  # Facet by FFG
  geom_boxplot() +
  geom_point(aes(color = FFG), size = 2) +
  ylab(expression(log(Biomass(g/m^2)))) +  
  xlab("") +
  scale_color_carto_d(name = "SC.Category", palette = "Earth", n = length(unique(meansites$SC.Category))) + # Add color scale
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

print(FFGgplot2.5)

# Panels by FFG log transformed medians to better see patterns


FFGgplot3 <- ggplot(data = meansites, aes(x = SC.Category, y = (log(mean.biomass)))) +
  facet_wrap(~FFG, ncol = 5, nrow = 5) +  # Facet by FFG
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
print(FFGgplot3) # This is my favorite so far of the panels


# Panels by FFG, color by SC category

FFGgplot3.5 <- ggplot(data = biomassmeantable, aes(x = Site, y = log(mean.biomass), color = SC.Category)) +
  facet_wrap(~ FFG, ncol = 6, nrow = 6) +  # Facet by FFG
  geom_boxplot() +
  geom_point(size = 2) +
  ylab("Biomass (g/m2)") +
  xlab("") +
  scale_color_carto_d(name = "SC Category", palette = "Prism", n = length(unique(biomassmeantable$SC.Category))) +
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

FFGgplot3.5 # Eh, still hard to see things unless log transform

# Boxplots for total biomass, first have to get sum of each replicate for each stream
sitestotal = biomassoct %>% 
  group_by(SC.Category, Site, Replicate) %>% 
  summarise(sum.biomass=sum(Biomass.Area.Corrected,na.rm=FALSE))

medsitestotal = sitestotal %>% 
  group_by(SC.Category, Site) %>% 
  summarise(mean.biomass=median(sum.biomass,na.rm=FALSE))



FFGgplot4=ggplot(data=medsitestotal,aes(x=SC.Category,y=sum.biomass))+ # This is creating multiple "panels" for site
  geom_boxplot()+
  geom_point(aes(color=SC.Category),size=2)+
  ylab("Biomass (g/m^2)")+
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

FFGgplot4 # Log to see better

# Checking that it is plotting correctly 
filtered_data <- subset(biomassoct, Site == "RIC")

total_biomass <- sum(filtered_data$Biomass.Area.Corrected, na.rm = TRUE)

print(total_biomass)

#PROPORTIONS PARTY!------------------------------------

meansites$SC.Category <- factor(meansites$SC.Category, levels = c("REF","MID","HIGH"))
meansites$FFG <- factor(meansites$FFG, levels = c("Scraper","Shredder","Predator","Collector-Gatherer","Collector-Filterer"))
meansites$SC.Level <- factor(meansites$SC.Level, levels = c("25","72","78","387","402","594","1119","1242","1457"))

install.packages("rcartocolor")# Colorblind color schemes
library(rcartocolor)
display_carto_all()


# Proportions across SC Cat
FFGgplot4 <- ggplot(meansites, aes(x = SC.Category, y = mean.biomass, fill = FFG)) +
  geom_bar(stat = "identity") +
  labs(
    x = "SC Level (µS/cm)",
    y = "Biomass (g/m2)",
    fill = "FFG"
  ) +
  scale_fill_carto_d(name = "FFG", palette = "Geyser", n = length(unique(meansites$FFG))) +
  theme_minimal()
FFGgplot4

# Proportions across sites
FFGgplot5= ggplot(meansites, aes(x = SC.Level, y = mean.biomass, fill = FFG)) +
  geom_bar(stat = "identity") +
  labs(
    x = "SC Level (µS/cm)",
    y = "Biomass (g/m2)",
    fill = "FFG"
  ) +
  scale_fill_carto_d(name = "FFG", palette = "Geyser", n = length(unique(meansites$FFG))) +
  theme_minimal()

FFGgplot5 # Significantly more variation when averaged rather than summed, reps not super similar
# with small sample size


# Now to do actual proportions, scaling everything to 100% biomass for their respective categories

# Now let's do it for sc cat, calculate total biomass for each SC cat
total_biomass_cat <- meansites %>%
  group_by(SC.Category) %>%
  summarise(total_biomass = sum(mean.biomass)) # summing the median biomass for each sc cat

# Calculate proportions of total biomass for each FFG for each site
df_proportions_cat <- meansites %>%
  left_join(total_biomass_cat, by = "SC.Category") %>%
  group_by(SC.Category, FFG) %>%
  summarise(Proportion = sum(mean.biomass) / first(total_biomass)) # Summing median biomass
#for each FFG for each SC category and dividing it by summed median biomass for each sc cat
            

# Define a custom palette with desired colors

my_colors = carto_pal(7, "Geyser") # to get hexcodes for geyser to assign FFGS to, for 
#consistency in graphs I'll use for SFS
my_colors
ffg_colors <- c("Scraper" = "#008080", 
                "Shredder" = "#CA562C", 
                "Predator" = "#F6EDBD", 
                "Collector-Gatherer" = "#DE8A5A", 
                "Collector-Filterer" = "#70A494")  

# Plot with specific colors assigned to each FFG using hexadecimal codes
propgg_cat = ggplot(df_proportions_cat, aes(x = SC.Category, y = Proportion, fill = FFG)) +
  geom_bar(stat = "identity") +
  labs(x = "Specific Conductivity Category", y = "Proportion of Total Biomass", fill = "FFGs") +
  scale_fill_manual(values = ffg_colors, name = "FFG") +  # Assign specific colors
  theme_minimal()

propgg_cat # Beautiful!!


# Now let's do it for SC level, calculate total biomass for each SC Level
total_biomass_sites <- meansites %>%
  group_by(SC.Level) %>%
  summarise(total_biomass = sum(mean.biomass)) # Summing the median biomass for each sc level,
#replicates have already been averaged

# Calculate proportions of total biomass for each FFG for each site
df_proportions_sites <- meansites %>%
  left_join(total_biomass_sites, by = "SC.Level") %>%
  group_by(SC.Level, FFG) %>%
  summarise(Proportion = sum(mean.biomass) / first(total_biomass)) # Summing median biomass
#for each FFG for each SC category and dividing it by summed median biomass for each sc cat

propgg_site <- ggplot(df_proportions_sites, aes(x = SC.Level, y = Proportion, fill = FFG)) +
  geom_bar(stat = "identity", position = "stack") +
  labs(x = "Site", y = "Proportion of Total Biomass", fill = "Functional Group") +
  scale_fill_manual(values = ffg_colors, name = "FFG") +  # Assign specific colors
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
        legend.key=element_rect(fill="white",color="white"))

propgg_site # Nice!



# Double check some things with a scatter plot
total_biomass_sites <- biomassmeantable %>%
  group_by(SC.Level) %>%
  summarise(total_biomass = sum(mean.biomass)) # Summing the median biomass for each sc level,
#replicates have already been averaged

# Calculate proportions of total biomass for each FFG for each site
df_proportions_sites <- biomassmeantable %>%
  left_join(total_biomass_sites, by = "SC.Level") %>%
  group_by(SC.Level, FFG, Replicate) %>%
  summarise(Proportion = sum(mean.biomass) / first(total_biomass))


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


# Let's try % biomass-------------------------------------

meansites$SC.Category <- factor(meansites$SC.Category, levels = c("REF","MID","HIGH"))
meansites$FFG <- factor(meansites$FFG, levels = c("Scraper","Shredder","Predator","Collector-Gatherer","Collector-Filterer"))
meansites$SC.Level <- factor(meansites$SC.Level, levels = c("25","72","78","387","402","594","1119","1242","1457"))


my_colors = carto_pal(7, "Geyser") # to get hexcodes for geyser to assign FFGS to, for 
#consistency in graphs I'll use for SFS
my_colors
ffg_colors <- c("Scraper" = "#008080", 
                "Shredder" = "#CA562C", 
                "Predator" = "#F6EDBD", 
                "Collector-Gatherer" = "#DE8A5A", 
                "Collector-Filterer" = "#70A494")  

 

total_biomass_sites <- biomassmeantable %>% #summing total biomass for each sc.level, is there
    #a way to QAQC this with raw data? I worry it make differ from meansites
  group_by(SC.Level) %>%
  summarise(total_biomass = sum(mean.biomass)) 

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



# Only scrapers

# Plot boxplots for scrapers with a continuous x-axis and regression line
scrapers_data$SC.Level <- as.numeric(as.character(scrapers_data$SC.Level))

# Plot boxplots for scrapers with a continuous x-axis and regression line
percentscrap <- ggplot(scrapers_data, aes(group = as.numeric(SC.Level), x = as.numeric(SC.Level), y = percentage.biomass)) +
  geom_boxplot(fill = ffg_colors["Scraper"], width = 75.0) +  # Adjust color, fill, transparency, and width of boxplots
  geom_smooth(method = "lm", se = FALSE, color = "yellow") +  # Add regression line
  labs(x = "Salinity", y = "Percentage Biomass", fill = "FFGs") +
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

percentscrap #isn't adding regression line


#the math isn't mathing so let's look at this as a bar lot to see if it makes sense
stackedpercent = percentt <- ggplot(df_filtered, aes(x = SC.Level, y = percentage.biomass, fill = FFG)) +
  geom_bar(stat = "identity") +  # Create stacked bar plot
  labs(x = "Specific Conductivity", y = "% Biomass", fill = "FFGs") +
  scale_fill_manual(values = ffg_colors, name = "FFG")  +  # Assign specific colors
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

stackedpercent # I guess it lines up for the most part? since the stacked bars dont show variation
#-----------------------------------------
#ANOVA
install.packages("broom")

install.packages("kableExtra")
library(broom)
library(kableExtra)

meansites$SC.Category <- factor(meansites$SC.Category, levels = c("REF","MID","HIGH"))
meansites$FFG <- factor(meansites$FFG, levels = c("Scraper","Shredder","Collector-Gatherer","Predator","Collector-Filterer"))


#Biomass across SC category
anova_result <- aov(mean.biomass ~ SC.Category, data = meansites)
summary(anova_result) #biomass across SC.category isn't significant

tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

#Biomass across SC Level
anova_result2 <- aov(mean.biomass ~ SC.Level, data = meansites)
summary(anova_result2)

tidy_anova2 <- tidy(anova_result2)

install.packages("kableExtra")
library(kableExtra)

tidy_anova2 %>%
  knitr::kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)


tukey_result <- TukeyHSD(anova_result2)
print(tukey_result) #biomass across site isn't significant

#Biomass across FFG
anova_result3 <- aov(mean.biomass ~ FFG, data = meansites)
summary(anova_result3)

tidy_anova3 <- tidy(anova_result3)

install.packages("kableExtra")
library(kableExtra)

tidy_anova3 %>%
  knitr::kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

tukey_result <- TukeyHSD(anova_result3)
print(tukey_result)



# For proportions
# Is log(biomass) different across SC.Cat for each FFG? This goes along with FFGgplot3

# Edit FFGs as needed
scrap_prop <- meansites[meansites$FFG == "Scraper", ] # first, subset by FFG

anova_result4 <- aov(log(mean.biomass) ~ SC.Category, data = scrap_prop)
summary(anova_result4)

tidy_anova4 <- tidy(anova_result4)

install.packages("kableExtra")
library(kableExtra)

tidy_anova4 %>%
  knitr::kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

tukey_result <- TukeyHSD(anova_result4)
print(tukey_result)


#Biomass across FFG and SC.Category interactive
anova_result5 <- aov(mean.biomass ~ FFG*SC.Category, data = trimmed_data)
summary(anova_result5)
#KATE QUESTION: why is the p value for FFG and SC.Level different on this one than 
#the anova_result3 and anova_result?

tidy_anova5 <- tidy(anova_result5)

install.packages("kableExtra")
library(kableExtra)

tidy_anova5 %>%
  knitr::kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

tukey_result <- TukeyHSD(anova_result5)
print(tukey_result)

#Biomass across FFG and SC.Level additive, doesn't really work
anova_result6 <- aov(mean.biomass ~ FFG+SC.Level, data = trimmed_data)
summary(anova_result6)
#KATE QUESTION: why is the p value for FFG and SC.Level different on this one than 
#the anova_result3 and anova_result?

tidy_anova6 <- tidy(anova_result6)

install.packages("kableExtra")
library(kableExtra)

tidy_anova6 %>%
  knitr::kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

tukey_result <- TukeyHSD(anova_result6)
print(tukey_result)



#------------------------
#Let's look at effects
pr <- function(m) printCoefmat(coef(summary(m)),
                               digits=3,signif.stars=FALSE) 

#Get the estimates per group, rather than the differences
pr(lm1 <- lm(mean.biomass~SC.Category,data=trimmed_data))

pr(lm2 <- lm(mean.biomass~SC.Level,data=trimmed_means))

pr(lm3<- lm(mean.biomass~FFG, data=trimmed_data))


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

#---------------------------Gamma time
biomassmeantable$mean.biomass2 <- rgamma(length(biomassmeantable$mean.biomass),shape=1,rate=1)
g1 = glm(mean.biomass2~SC.Level,data=biomassmeantable,family = Gamma(link = inverse));
summary(g1)

simulationOutput <- simulateResiduals(fittedModel = g1, plot = T)
simulationOutput #Gamma distribution is good! Makes sense bc positive continuous

library(emmeans)
emmeans(g1, pairwise~SC.Level)
emmeans(g1, pairwise~SC.Level, type="response") #tells you the expected abundance
#at each stream and confidence limits for Y-variable
#strong significant differences between ref and mid-high salinity stream


#Just for fun, let's do poisson on my abundance
##Poisson GLM## 
g2 = glm(Abundance~Site,data=biomassoct, family=poisson);
summary(g2)

library(AER)
#performs a dispersion test
dispersiontest(g2) #Greater than one--> it's overdispersed


library(DHARMa)
news(package='DHARMa')

testDispersion(g2) #poisson model
#definitely too much dispersion, would want to do neg binomial if interested in abundance
-----------------------------------------

#AIC
library(AICcmodavg)
biomassmeantable$mean.biomass2 <- rgamma(length(biomassmeantable$mean.biomass),shape=1,rate=1)
g1 = glm(mean.biomass~SC.Level,data=biomassmeantable,family = Gamma(link = inverse));
g2 = glm(mean.biomass2~FFG,data=biomassmeantable,family = Gamma(link = inverse));
g3 = glm(mean.biomass2~SC.Level*FFG,data=biomassmeantable,family = Gamma(link = inverse));
g4 = glm(mean.biomass2~SC.Level+FFG,data=biomassmeantable,family = Gamma(link = inverse));


AIC(g1,g2,g3, g4)

#tabular
aictab(cand.set=list(g1,g2,g3,g4),modnames=c("Mean Biomass ~ SC Level", "Mean Biomass ~ FFG", "Mean Biomass ~ SC Level*FFG", "Mean Biomass ~ SC Level+FFG"))#AIC table
aictab(cand.set=list(g1,g2,g3,g4),modnames=c("Mean Biomass ~ SC Level", "Mean Biomass ~ FFG", "Mean Biomass ~ SC Level*FFG", "Mean Biomass ~ SC Level+FFG"), second.ord = F)#AIC table
#here all of the weight is for the first model
#lets look at a more nuanced version
b1 = glm(mean.biomass2~SC.Level,data=biomassmeantable,family = Gamma(link = inverse));
b2 = glm(mean.biomass2~FFG,data=biomassmeantable,family = Gamma(link = inverse));
b3 = glm(mean.biomass2~SC.Level*FFG,data=biomassmeantable,family = Gamma(link = inverse));
b4 = glm(mean.biomass2~SC.Level+FFG,data=biomassmeantable,family = Gamma(link = inverse));

#for AICc
n=nrow(biomassmeantable)#or whatever the length of your df is
tabA = AIC(b1,b2,b3,b4)
#it would be nice to have AICC for a dataset this small
tabA$k<-c(b1$rank,b2$rank,b3$rank,b4$rank)
tabA$aiccs<-tabA$AIC+((2*tabA$k*(tabA$k+1))/(n-tabA$k-1))
#now order from smallest to biggest
tabA=tabA[order(tabA$aiccs),]
#calculate delta AIC
tabA$dAIC = tabA$aiccs - min(tabA$aiccs)
#you use the next two lines to get weights
tabA$edel<-exp(-0.5*tabA$dAIC) 
tabA$wt<-tabA$edel/sum(tabA$edel)
tabA

#what issue do we have here?
b5 = glm(mean.biomass2~1,data=biomassmeantable,family = Gamma(link = inverse))

#now run this all again with b5!
tabA = AIC(b1,b2,b3,b4,b5)
#it would be nice to have AICC for a dataset this small
tabA$k<-c(b1$rank,b2$rank,b3$rank,b4$rank,b5$rank)
tabA$aiccs<-tabA$AIC+((2*tabA$k*(tabA$k+1))/(n-tabA$k-1))
#now order from smallest to biggest
tabA=tabA[order(tabA$aiccs),]
#calculate delta AIC
tabA$dAIC = tabA$aiccs - min(tabA$aiccs)
#you use the next two lines to get weights
tabA$edel<-exp(-0.5*tabA$dAIC) 
tabA$wt<-tabA$edel/sum(tabA$edel)

print(tabA)

# Extract AICc table as a data frame
aic_c <- as.data.frame(tabA)

# Modify row names
rownames(aic_c) <- c("Mean Biomass ~ FFG","Mean Biomass ~ SC Level+FFG", "Mean Biomass ~ SC Level*FFG", "Mean Biomass ~ 1","Mean Biomass ~ SC Level")

# Print the modified data frame
print(aic_c)

install.packages("kableExtra")
library(kableExtra)

aic_c %>%
  knitr::kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)


#-----------------------------

#playing with t-tests, these aren't working, saying all sites have the same p-value
# Why isn't this working?


# Function to perform t-test for scrap and shred biomass within a site
t_test_site <- function(data) {
  scrap_data <- data[data$FFG == "Scraper", "mean.biomass"]
  shred_data <- data[data$FFG == "Shredder", "mean.biomass"]
  t.test(scrap_data, shred_data)
}

# Apply t-test function for each site
t_test_results <- by(biomassmeantable, biomassmeantable$Site, t_test_site) #can't use trimmed means
#bc too little data points

# Print t-test results
t_test_results


# Convert t-test results to dataframe
t_test_df <- do.call(rbind, lapply(names(t_test_results), function(Site) {
  data.frame(
    Site = Site,
    p.value = t_test_results[[Site]]$p.value
  )
}))

# Plot t-test results
ggplot(t_test_df, aes(x = Site, y = p.value)) +
  geom_point(size = 3, color = "blue") +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +  # Add significance threshold line
  labs(x = "Site", y = "p-value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

#This works, nothin is significant, but some are closer than others



#Try again with poportion data
# Function to perform t-test for scrap and shred biomass within a site
t_test_cat <- function(data) {
  scrap_data <- data[data$FFG == "Scraper", "Proportion"]
  shred_data <- data[data$FFG == "Shredder", "Proportion"]
  t.test(scrap_data, shred_data)
}

# Apply t-test function for each site
t_test_resultss <- by(df_proportions, df_proportions$SC.Category, t_test_cat) #can't use trimmed means
#bc too little data points

# Print t-test results
t_test_resultss


# Convert t-test results to dataframe
t_test_df <- do.call(rbind, lapply(names(t_test_results), function(SC.Category) {
  data.frame(
    SC.Category = SC.Category,
    p.value = t_test_results[[SC.Category]]$p.value
  )
}))

# Plot t-test results
ggplot(t_test_df, aes(x = Site, y = p.value)) +
  geom_point(size = 3, color = "blue") +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "red") +  # Add significance threshold line
  labs(x = "Site", y = "p-value") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Rotate x-axis labels for better readability

#This works, nothin is significant, but some are closer than others




# Fit a linear regression model for scrapers across SC.Cat, do a gamma
scraper_data <- meansites[meansites$FFG == "Scraper", ]

lm_model1 <- lm(mean.biomass ~ SC.Category, data = scraper_data)

# Check the summary of the regression model
summary(lm_model1)

# Plot the data points
scrap <- ggplot(scraper_data, aes(x = SC.Category, y = mean.biomass)) +
  geom_point() +
  labs(x = "SC categories", y = "Biomass")

# Add the linear model line
scrap + geom_smooth(method = "lm", se = FALSE)

# Fit a linear regression model for shredders across SC.Cat
shredder_data <- meansites[meansites$FFG == "Shredder", ]

lm_model2 <- lm(mean.biomass ~ SC.Category, data = shredder_data)

# Check the summary of the regression model
summary(lm_model2)




#NMDS------------------------------
# Loading the appropariate packages
library(vegan) # vegan to calculate distance matrices
library(ggplot2) # ggplot for plotting
library(tidyverse) 
library(dplyr)

aggregated_df <- aggregate(Biomass.Area.Corrected ~ Site + SC.Level + SC.Category + 
                             Genus, data = biomassoct, FUN = median, na.rm = TRUE)


biomass.nmds = aggregated_df %>% 
  pivot_wider(
    names_from = Genus, 
    values_from = Biomass.Area.Corrected,
  ) #ahhh ok it worked



#  Rename the ID part of the matrix; take out the columns for streams, SC, and season
oct.nmds <- biomass.nmds[,-c(1:3)]


oct.nmds[is.na(oct.nmds)] <- 0

# Running the NMDS for all taxa

#  metaMDS integrates functions from several packages to perform NMDS.....
#  ....including'vegdist' from the vegan package

X <- metaMDS(oct.nmds, distance='bray', k=2, trymax=20, autotransform=FALSE, pc=FALSE, plot=FALSE)

# Gives average stress
X$stress

# Gives weights that different species hold in the axis
X$species

# Basic plot of all of the points
plot(X, display=c('sites', 'species'), choices=c(1,2), type='p')


# Scores
nmds_species <- scores(X, display = "species")
nmds_sites <- scores(X, display = "sites")


site.scores <- as.data.frame(scores(nmds_sites)) #Using the scores function from 
# vegan to extract the site scores and convert to a data.frame
# Add site column to dataframe
site.scores$site <- rownames(site.scores)


species.scores <- as.data.frame(scores(nmds_species))  #Using the scores function 
# from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)


# Plot sites using ggplot2
ggplot(site.scores, aes(x = NMDS1, y = NMDS2, label = site)) +
  geom_point() +
  geom_text(hjust = 1, nudge_x = 0.05) +  # Add labels with slight offset
  labs(x = "NMDS1", y = "NMDS2") +
  theme_minimal()



#Filtering based on top abundance

Biomass_Sums <- colSums(oct.nmds)
Biomass_Sums # Sums of all taxa

Biomass_CutOff <- .01# Don't want taxa with an abundance less than ___

TOP <- oct.nmds %>%
  select(where( ~ sum(.) >= Biomass_CutOff))

TOP # Taxa with abundances greater than ___
colSums(TOP)

# NMDS for top taxa only

TOPScores <-metaMDS(TOP, distance='bray', k=2, trymax=20, autotransform=FALSE, pc=FALSE, plot=FALSE)
TOPScores

TOPScores$stress

# Plotting species of top abundance in base plot

Y <- scores(TOPScores, display="species") # Scores for top species
Y # Scores for top taxa with species name

plot(Y, type="n", xlim=c(-2, 2), ylim=c(-1.5, 1.5))
text(Y[,1], Y[,2], rownames(Y), col="blue")
all(colnames(TOP) == rownames(Y))

Z <- scores(TOPScores, display="sites")
Z # Scores for top taxa with site lens


site.scores <- as.data.frame(scores(Z)) #Using the scores function from vegan 
# to extract the site scores and convert to a data.frame
rownames(site.scores) <- c("LLW", "RIC", "HUR", "FRY",
                           "RUT", "EAS", "CRO", "HCN", "LLC") # Change sites from numbers to categorical

site.scores$site <- factor(rownames(site.scores), levels = c("EAS", "CRO", "HCN", "FRY", "RUT", "HUR", "RIC", "LLW", "LLC"))

# Reorder rows based on the new factor
site.scores <- site.scores[order(site.scores$site), ]


species.scores <- as.data.frame(scores(TOPScores, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data


# Filter data for specific colors
ref <- subset(site.scores, site %in% c("CRO", "EAS", "HCN"))
mid <- subset(site.scores, site %in% c("HUR", "FRY", "RUT"))
high <- subset(site.scores, site %in% c("RIC", "LLC", "LLW"))

# Create polygons around points of specific colors
TOPPlott <- ggplot() +    
  geom_polygon(data = ref, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#70A494", alpha = 0.3) +
  geom_polygon(data = mid, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#EDBB8A", alpha = 0.3) +
  geom_polygon(data = high, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#CA562C", alpha = 0.3) +
  geom_text(data = species.scores, aes(x = NMDS1, y = NMDS2, label = species), alpha = 0.5, vjust = 1.5, color = "grey") +   
  geom_point(data = site.scores, aes(x = NMDS1, y = NMDS2, color = site), size = 3) + 
  geom_text(data = site.scores, aes(x = NMDS1, y = NMDS2, label = site), size = 4, vjust = 1.5) +
  scale_colour_manual(values = c("CRO" = "#70A494", "EAS" = "#70A494", "HCN" = "#70A494",
                                 "HUR" = "#EDBB8A", "FRY" = "#EDBB8A", "RUT" = "#EDBB8A", 
                                 "RIC" = "#CA562C", "LLC" = "#CA562C", "LLW" = "#CA562C")) +
  coord_equal() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(name = "NMDS1", limits = c(-.75, .75)) +
  scale_y_continuous(name = "NMDS2", limits = c(-.75, .75)) 

# Display the plot
print(TOPPlott)





#FFG NMDS attempts---------------------------
#Let's try nmds again

# Loading the appropariate packages
library(vegan) # vegan to calculate distance matrices
library(ggplot2) # ggplot for plotting
library(tidyverse) 
library(dplyr)
library(rcartocolor)

aggregated_df_ffg <- aggregate(Biomass.Area.Corrected ~ Site + SC.Level + SC.Category + 
                       + FFG, data = biomassoct, FUN = median, na.rm = TRUE)

shredder_biomass_sum <- biomassoct %>%
  filter(Site == "HCN", FFG == "Shredder") %>%
  summarize(Total_Biomass = sum(Biomass.Area.Corrected, na.rm = TRUE))


biomass.nmds.ffg = aggregated_df_ffg %>% 
  pivot_wider(
    names_from = FFG, 
    values_from = Biomass.Area.Corrected,
  ) #ahhh ok it worked




#  Rename the ID part of the matrix; take out the columns for streams, SC, season, genus
oct.nmds.ffg <- biomass.nmds.ffg[,-c(1:3)]


oct.nmds.ffg[is.na(oct.nmds.ffg)] <- 0

# Running the NMDS for all taxa

#  metaMDS integrates functions from several packages to perform NMDS.....
#  ....including'vegdist' from the vegan package

X <- metaMDS(oct.nmds.ffg, distance='bray', k=2, trymax=20, autotransform=FALSE, pc=FALSE, plot=FALSE)

# Gives average stress
X$stress

# Gives weights that different species hold in the axis
X$species

# Basic plot of all of the points
plot(X, display=c('sites', 'species'), choices=c(1,2), type='p')

nmds_species <- scores(X, display = "species")

nmds_sites <- scores(X, display = "sites")


site.scores <- as.data.frame(scores(nmds_sites)) #Using the scores function from vegan to extract the site scores and convert to a data.frame
# Add site column to dataframe
site.scores$site <- rownames(site.scores)
head(site.scores)

species.scores <- as.data.frame(scores(nmds_species))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)


# Plot NMDS using ggplot2
ggplot(site.scores, aes(x = NMDS1, y = NMDS2, label = site)) +
  geom_point() +
  geom_text(hjust = 1, nudge_x = 0.05) +  # Add labels with slight offset
  labs(x = "NMDS1", y = "NMDS2") +
  theme_minimal()



#Filtering based on top abundance

Biomass_Sums <- colSums(oct.nmds.ffg)
Biomass_Sums # Sums of all taxa

Biomass_CutOff <- .01# Don't want taxa with an abundance less than 150

TOP <- oct.nmds.ffg %>%
  select(where( ~ sum(.) >= Biomass_CutOff))

TOP # Taxa with abundances greater than 400
colSums(TOP)

# NMDS for top taxa only

TOPScores <-metaMDS(TOP, distance='bray', k=2, trymax=20, autotransform=FALSE, pc=FALSE, plot=FALSE)
TOPScores

TOPScores$stress

# Plotting species of top abundance in base plot

Y <- scores(TOPScores, display="species") # Scores for top species
Y # Scores for top taxa with species name

plot(Y, type="n", xlim=c(-2, 2), ylim=c(-1.5, 1.5))
text(Y[,1], Y[,2], rownames(Y), col="blue")
all(colnames(TOP) == rownames(Y))

Z <- scores(TOPScores, display="sites")
Z # Scores for top taxa with site lens


site.scores <- as.data.frame(scores(Z)) #Using the scores function from vegan to extract the site scores and convert to a data.frame
rownames(site.scores) <- c("LLW", "LLC", "RIC", "HUR",
                           "FRY", "RUT", "EAS", "CRO", "HCN") # Change sites from numbers to categorical

site.scores$site <- factor(rownames(site.scores), levels = c("EAS", "CRO", "HCN", "FRY", "RUT", "HUR", "RIC", "LLW", "LLC"))

# Reorder rows based on the new factor
site.scores <- site.scores[order(site.scores$site), ]


species.scores <- as.data.frame(scores(TOPScores, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data


# Filter data for specific colors
ref <- subset(site.scores, site %in% c("CRO", "EAS", "HCN"))
mid <- subset(site.scores, site %in% c("HUR", "FRY", "RUT"))
high <- subset(site.scores, site %in% c("RIC", "LLC", "LLW"))

# Create polygons around points of specific colors
TOPPlott <- ggplot() +    
  geom_polygon(data = ref, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#70A494", alpha = 0.3) +
  geom_polygon(data = mid, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#EDBB8A", alpha = 0.3) +
  geom_polygon(data = high, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#CA562C", alpha = 0.3) +
  geom_text(data = species.scores, aes(x = NMDS1, y = NMDS2, label = species), alpha = 0.5, vjust = 0.5, color = "grey") +   
  geom_point(data = site.scores, aes(x = NMDS1, y = NMDS2, color = site), size = 3) + 
  geom_text(data = site.scores, aes(x = NMDS1, y = NMDS2, label = site), size = 4, vjust = 0.5) +
  scale_colour_manual(values = c("CRO" = "#70A494", "EAS" = "#70A494", "HCN" = "#70A494",
                                 "HUR" = "#EDBB8A", "FRY" = "#EDBB8A", "RUT" = "#EDBB8A", 
                                 "RIC" = "#CA562C", "LLC" = "#CA562C", "LLW" = "#CA562C")) +
  coord_equal() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(name = "NMDS1", limits = c(-1.5, 1.5)) +
  scale_y_continuous(name = "NMDS2", limits = c(-1.5, 1.5)) 

# Display the plot
print(TOPPlott)


#Just scraper and shredder NMDS--------------------------------

# Loading the appropariate packages
library(vegan) # vegan to calculate distance matrices
library(ggplot2) # ggplot for plotting
library(tidyverse) 
library(dplyr)

aggregated_df_ffg_filter <- subset(biomassoct, FFG %in% c("Scraper", "Shredder"))

aggregated_df_ffg_filter <- aggregate(Biomass.Area.Corrected ~ Site + SC.Level + SC.Category + 
                                 + Genus, data = aggregated_df_ffg_filter, FUN = median, na.rm = TRUE)


biomass.nmds.ffg.filter <- aggregated_df_ffg_filter %>% 
  pivot_wider(
    names_from = Genus, 
    values_from = Biomass.Area.Corrected,
  )


#  Rename the ID part of the matrix; take out the columns for streams, SC, season, genus
oct.nmds.ffg.filter <- biomass.nmds.ffg.filter[,-c(1:3)]


oct.nmds.ffg.filter[is.na(oct.nmds.ffg.filter)] <- 0

# Running the NMDS for all taxa

#  metaMDS integrates functions from several packages to perform NMDS.....
#  ....including'vegdist' from the vegan package

X <- metaMDS(oct.nmds.ffg.filter, distance='bray', k=2, trymax=20, autotransform=FALSE, pc=FALSE, plot=FALSE)

# Gives average stress
X$stress

# Gives weights that different species hold in the axis
X$species

# Basic plot of all of the points
plot(X, display=c('sites', 'species'), choices=c(1,2), type='p')

nmds_species <- scores(X, display = "species")

nmds_sites <- scores(X, display = "sites")


site.scores <- as.data.frame(scores(nmds_sites)) #Using the scores function from vegan to extract the site scores and convert to a data.frame
# Add site column to dataframe
site.scores$site <- rownames(site.scores)
head(site.scores)

species.scores <- as.data.frame(scores(nmds_species))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)


# Plot NMDS using ggplot2
ggplot(site.scores, aes(x = NMDS1, y = NMDS2, label = site)) +
  geom_point() +
  geom_text(hjust = 1, nudge_x = 0.05) +  # Add labels with slight offset
  labs(x = "NMDS1", y = "NMDS2") +
  theme_minimal()



#Filtering based on top abundance

Biomass_Sums <- colSums(oct.nmds.ffg.filter)
Biomass_Sums # Sums of all taxa

Biomass_CutOff <- 0.01 # Don't want taxa with an abundance less than 150

TOP <- oct.nmds.ffg.filter %>%
  select(where( ~ sum(.) >= Biomass_CutOff))

TOP # Taxa with abundances greater than 400
colSums(TOP)

# NMDS for top taxa only

TOPScores <-metaMDS(TOP, distance='bray', k=2, trymax=20, autotransform=FALSE, pc=FALSE, plot=FALSE)
TOPScores

TOPScores$stress

# Plotting species of top abundance in base plot

Y <- scores(TOPScores, display="species") # Scores for top species
Y # Scores for top taxa with species name

plot(Y, type="n", xlim=c(-2, 2), ylim=c(-1.5, 1.5))
text(Y[,1], Y[,2], rownames(Y), col="blue")
all(colnames(TOP) == rownames(Y))

Z <- scores(TOPScores, display="sites")
Z # Scores for top taxa with site lens


site.scores <- as.data.frame(scores(Z)) #Using the scores function from vegan to extract the site scores and convert to a data.frame
rownames(site.scores) <- c("LLC", "RIC", "RUT", "EAS",
                           "HCN", "LLW", "HUR", "FRY", "CRO") # Change sites from numbers to categorical

site.scores$site <- factor(rownames(site.scores), levels = c("EAS", "CRO", "HCN", "FRY", "RUT", "HUR", "RIC", "LLW", "LLC"))

# Reorder rows based on the new factor
site.scores <- site.scores[order(site.scores$site), ]


species.scores <- as.data.frame(scores(TOPScores, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  #look at the data



# Filter data for specific colors
ref <- subset(site.scores, site %in% c("CRO", "EAS", "HCN"))
mid <- subset(site.scores, site %in% c("HUR", "FRY", "RUT"))
high <- subset(site.scores, site %in% c("RIC", "LLC", "LLW"))

# Create polygons around points of specific colors
TOPPlott <- ggplot() +    
  geom_polygon(data = ref, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#70A494", alpha = 0.3) +
  geom_polygon(data = mid, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#EDBB8A", alpha = 0.3) +
  geom_polygon(data = high, aes(x = NMDS1, y = NMDS2, group = "site"), fill = "#CA562C", alpha = 0.3) +
  geom_text(data = species.scores, aes(x = NMDS1, y = NMDS2, label = species), alpha = 0.5, vjust = 0.5, color = "grey") +   
  geom_point(data = site.scores, aes(x = NMDS1, y = NMDS2, color = site), size = 3) + 
  geom_text(data = site.scores, aes(x = NMDS1, y = NMDS2, label = site), size = 4, vjust = 1.5) +
  scale_colour_manual(values = c("CRO" = "#70A494", "EAS" = "#70A494", "HCN" = "#70A494",
                                 "HUR" = "#EDBB8A", "FRY" = "#EDBB8A", "RUT" = "#EDBB8A", 
                                 "RIC" = "#CA562C", "LLC" = "#CA562C", "LLW" = "#CA562C")) +
  coord_equal() +
  theme_bw() +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
  scale_x_continuous(name = "NMDS1", limits = c(-.75,.75)) +
  scale_y_continuous(name = "NMDS2", limits = c(-.75, .6)) 

# Display the plot
print(TOPPlott)

