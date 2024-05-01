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

#Ok, brinch in the csv summary sheets for each site 
CROoct=read.csv("CRO_Oct.2023_SUMMARY.csv")
EASoct=read.csv("EAS_Oct.2023_SUMMARY.csv")
HCNoct=read.csv("HCN_Oct.2023_SUMMARY.csv")
FRYoct=read.csv("FRY_Oct.2023_SUMMARY.csv")
HURoct=read.csv("HUR_Oct.2023_SUMMARY.csv")
RUToct=read.csv("RUT_Oct.2023_SUMMARY.csv")
LLWoct=read.csv("LLW_Oct.2023_SUMMARY.csv")
LLCoct=read.csv("LLC_Oct.2023_SUMMARY.csv")
RICoct=read.csv("RIC_Oct.2023_SUMMARY.csv")

#adding a column with site name bc once we merge we won't know
CROoct$Site <- c("CRO")
EASoct$Site <- c("EAS")
HCNoct$Site <- c("HCN")
FRYoct$Site <- c("FRY")
HURoct$Site <- c("HUR")
RUToct$Site <- c("RUT")
LLWoct$Site <- c("LLW")
LLCoct$Site <- c("LLC")
RICoct$Site <- c("RIC")

#adding a column with SC level bc once we merge we won't know
CROoct$SC.Level <- c("72")
EASoct$SC.Level <- c("25")
HCNoct$SC.Level <- c("78")
FRYoct$SC.Level <- c("402")
HURoct$SC.Level <- c("387")
RUToct$SC.Level <- c("594")
LLWoct$SC.Level <- c("1,119")
LLCoct$SC.Level <- c("1,242")
RICoct$SC.Level <- c("1,457")

#adding a column with SC Category bc once we merge we won't know
CROoct$SC.Category <- c("REF")
EASoct$SC.Category <- c("REF")
HCNoct$SC.Category <- c("REF")
FRYoct$SC.Category <- c("MID")
HURoct$SC.Category <- c("MID")
RUToct$SC.Category <- c("MID")
LLWoct$SC.Category <- c("HIGH")
LLCoct$SC.Category <- c("HIGH")
RICoct$SC.Category <- c("HIGH")


#Let's re-arrange the columns so these new additions are at the front
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

#rarranging order of columns
biomassoct <- select(biomassoct,Site,SC.Category,SC.Level,Sample.Date,Fraction,
                     Replicate,Order,Family,Genus,Abundance,Density,Biomass)


#Fixing up data sheets
#First, let's add a new column to correct biomass by area
biomassoct <- biomassoct %>% mutate(Biomass.Area.Corrected = Biomass*Density)

##Okay, now we gotta rearrange the data in another funky way for an NMDS
#First, I want cut out the abundance columns (the count data), total abundance, density, and biomass,
#order and family, and FFG for each site. I want to do this so that I just have 
#site info and biomass (0s and #s) for each genera

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

CRO_nmds_subset <- CROoct[, c(1:6, 9, (target_column_indexCRO + 1):ncol(CROoct))]
EAS_nmds_subset <- EASoct[, c(1:6, 9, (target_column_indexEAS + 1):ncol(EASoct))]
HCN_nmds_subset <- HCNoct[, c(1:6, 9, (target_column_indexHCN + 1):ncol(HCNoct))]
FRY_nmds_subset <- FRYoct[, c(1:6, 9, (target_column_indexFRY + 1):ncol(FRYoct))]
HUR_nmds_subset <- HURoct[, c(1:6, 9, (target_column_indexHUR + 1):ncol(HURoct))]
RUT_nmds_subset <- RUToct[, c(1:6, 9, (target_column_indexRUT + 1):ncol(RUToct))]
RIC_nmds_subset <- RICoct[, c(1:6, 9, (target_column_indexRIC + 1):ncol(RICoct))]
LLC_nmds_subset <- LLCoct[, c(1:6, 9, (target_column_indexLLC + 1):ncol(LLCoct))]
LLW_nmds_subset <- LLWoct[, c(1:6, 9, (target_column_indexLLW + 1):ncol(LLWoct))]

# Before I can merge all those guys to add in columns with zeros to make them the same
#number of columns, I need to cut out the summary totals at the end of everything.
#Yes, I could've probably done that above but would've been a bit complicated

columns_to_delete <- c("Abundance", "Density", "Biomass")  

# Subset the data frame by selecting only the columns that are not in 'columns_to_delete'
CRO_nmds_subset_final <- CRO_nmds_subset[, !names(CRO_nmds_subset) %in% columns_to_delete]
EAS_nmds_subset_final <- EAS_nmds_subset[, !names(EAS_nmds_subset) %in% columns_to_delete]
HCN_nmds_subset_final <- HCN_nmds_subset[, !names(HCN_nmds_subset) %in% columns_to_delete]
FRY_nmds_subset_final <- FRY_nmds_subset[, !names(FRY_nmds_subset) %in% columns_to_delete]
HUR_nmds_subset_final <- HUR_nmds_subset[, !names(HUR_nmds_subset) %in% columns_to_delete]
RUT_nmds_subset_final <- RUT_nmds_subset[, !names(RUT_nmds_subset) %in% columns_to_delete]
RIC_nmds_subset_final <- RIC_nmds_subset[, !names(RIC_nmds_subset) %in% columns_to_delete]
LLC_nmds_subset_final <- LLC_nmds_subset[, !names(LLC_nmds_subset) %in% columns_to_delete]
LLW_nmds_subset_final <- LLW_nmds_subset[, !names(LLW_nmds_subset) %in% columns_to_delete]



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
  CRO_nmds_subset_final[[paste0("New_Column", i)]] <- 0
}

for (i in 1:num_new_columnsEAS) {
  EAS_nmds_subset_final[[paste0("New_Column", i)]] <- 0
}

for (i in 1:num_new_columnsHCN) {
  HCN_nmds_subset_final[[paste0("New_Column", i)]] <- 0
}

for (i in 1:num_new_columnsFRY) {
  FRY_nmds_subset_final[[paste0("New_Column", i)]] <- 0
}

for (i in 1:num_new_columnsHUR) {
  HUR_nmds_subset_final[[paste0("New_Column", i)]] <- 0
}

for (i in 1:num_new_columnsRUT) {
  RUT_nmds_subset_final[[paste0("New_Column", i)]] <- 0
}

for (i in 1:num_new_columnsRIC) {
  RIC_nmds_subset_final[[paste0("New_Column", i)]] <- 0
}

for (i in 1:num_new_columnsLLC) {
  LLC_nmds_subset_final[[paste0("New_Column", i)]] <- 0
}


# Next, we have to rename all the columns to be the same size class so there aren't
# certain dfs with 12 "new_columns" but another with 4 and it's weird


# Generate new column names
new_column_namesCRO <- seq_len(ncol(CRO_nmds_subset_final) - 7)
new_column_namesEAS <- seq_len(ncol(EAS_nmds_subset_final) - 7)
new_column_namesHCN <- seq_len(ncol(HCN_nmds_subset_final) - 7)
new_column_namesFRY <- seq_len(ncol(FRY_nmds_subset_final) - 7)
new_column_namesHUR <- seq_len(ncol(HUR_nmds_subset_final) - 7)
new_column_namesRUT <- seq_len(ncol(RUT_nmds_subset_final) - 7)
new_column_namesRIC <- seq_len(ncol(RIC_nmds_subset_final) - 7)
new_column_namesLLC <- seq_len(ncol(LLC_nmds_subset_final) - 7)
new_column_namesLLW <- seq_len(ncol(LLW_nmds_subset_final) - 7)

# Rename the columns
names(CRO_nmds_subset_final)[8:ncol(CRO_nmds_subset_final)] <- new_column_namesCROO
names(EAS_nmds_subset_final)[8:ncol(EAS_nmds_subset_final)] <- new_column_namesEAS
names(HCN_nmds_subset_final)[8:ncol(HCN_nmds_subset_final)] <- new_column_namesHCN
names(FRY_nmds_subset_final)[8:ncol(FRY_nmds_subset_final)] <- new_column_namesFRY
names(HUR_nmds_subset_final)[8:ncol(HUR_nmds_subset_final)] <- new_column_namesHUR
names(RUT_nmds_subset_final)[8:ncol(RUT_nmds_subset_final)] <- new_column_namesRUT
names(RIC_nmds_subset_final)[8:ncol(RIC_nmds_subset_final)] <- new_column_namesRIC
names(LLC_nmds_subset_final)[8:ncol(LLC_nmds_subset_final)] <- new_column_namesLLC
names(LLW_nmds_subset_final)[8:ncol(LLW_nmds_subset_final)] <- new_column_namesLLW

## Amazing, now all of the subsetted data has the same amount of columns. Let's merge

list_of_oct_nmds <- list(CRO_nmds_subset_final, EAS_nmds_subset_final, HCN_nmds_subset_final, FRY_nmds_subset_final,
                           HUR_nmds_subset_final, RUT_nmds_subset_final, RIC_nmds_subset_final, LLC_nmds_subset_final,
                           LLW_nmds_subset_final) 

merged <- do.call(rbind, list_of_oct_nmds)

#Just realized IDK if an nmds requires all this... might be able to use to cumulative df

biomass.nmds = biomassoct %>% 
  pivot_wider(
    names_from = Genus, 
    values_from = Biomass.Area.Corrected,
    values_fn = sum
  ) #i dont know why this isn't working


write.csv(merged, "merged_oct_biomass.csv", row.names=FALSE)
write.csv(biomassoct, "BiomassOctTotals.csv", row.names = FALSE)
-----------------------------------------

#Adding FFGs...this will become relevant later 
  biomassoct$Genus=as.character(biomassoct$Genus)
biomassoct$FFG[biomassoct$Genus=="Acerpenna"]="Collector-Gatherer"
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
biomassoct$FFG[biomassoct$Genus=="Orthocladine"]="Scraper"
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
biomassoct$FFG[biomassoct$Genus=="Stenelmis (L)"]="Scraper"
biomassoct$FFG[biomassoct$Genus=="Stenonema"]="Scraper" 
biomassoct$FFG[biomassoct$Genus=="Taeniopteryx"]="Shredder"
biomassoct$FFG[biomassoct$Genus=="Tanypodinae"]="Predator"
biomassoct$FFG[biomassoct$Genus=="Tanytarsini"]="Collector-Gatherer"
biomassoct$FFG[biomassoct$Genus=="Tipula"]="Shredder"
biomassoct$FFG[biomassoct$Genus=="Triacanthagyna"]="Predator"
biomassoct$FFG[biomassoct$Genus=="Wormaldia"]="Collector-Filterer"
biomassoct$FFG[biomassoct$Genus=="Zoraena"]="Predator"

#Summarizing means of each replicate for each stream
biomassmeantable = biomassoct %>% 
  group_by(SC.Category,SC.Level,Site,Replicate,FFG) %>% 
  summarise(mean.biomass=mean(Biomass.Area.Corrected,na.rm=TRUE))

biomassmeantable #it worked!!

#Let's QAQC
biomassoct %>%
  filter(Site == "LLW", Replicate == 2, FFG == "Collector-Filterer") %>%  # Group data by category
  summarise(biomasssum = mean(Biomass.Area.Corrected)) #this confirms that biomassmeantabe
#is averaging FFGs per site per replicate correctly

#Now, averaging the replicates from each stream
meansites<- aggregate(mean.biomass ~ Site + SC.Category + SC.Level + FFG, data = biomassmeantable, FUN = mean)

meansites #this also worked, i double checked

#But...Let's QAQC with raw data again
biomassoct %>%
  filter(Site == "EAS", FFG == "Collector-Filterer") %>%  # Group data by category
  summarise(biomasssum = mean(Biomass.Area.Corrected)) #this confirms that meansites
#is averaging FFGs per site correctly

meansitegg = ggplot(data = meansites, aes(x = Site, y = mean.biomass, colour = SC.Category)) +
  geom_point(color="black") + #This is the average of replicates
  labs(x = "Stream", y = "Biomass") +
  theme_minimal()

meansitegg #This is the averages for all the replicates, with the replicates averaged
#that's why it is just one point per site

#Let's add in a boxplot with mean biomass per site to see how it compares
meansitegg2 = ggplot(data = meansites, aes(x = Site, y = mean.biomass, colour = SC.Category)) +
  geom_point(color="black") + #This is the average of replicates
  labs(x = "Stream", y = "Biomass") +
  theme_minimal()+
  geom_boxplot(data=biomassmeantable, aes(x=Site,y=mean.biomass, colour= SC.Category)) +
  #adding in the biomass per each site
  scale_color_manual(values = c("red","orange","forestgreen"))

meansitegg2 #Can't really see the plots bc of outliers



#------------------------------------------------
#Let's trim the outliers for total biomass
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

#Create ggplot with trimmed biomassmeantable data
trimmedbiotable = ggplot(trimmed_data, aes(x = Site, y = mean.biomass, colour = SC.Category)) +
  geom_boxplot() +
  labs(x = "Stream", y = "Biomass") +
  theme_minimal() +
  scale_color_manual(values = c("red","orange","forestgreen"))


trimmedbiotable #Not too shabby

#Create ggplot with trimmed meansites
trimmedmeansites = ggplot(trimmed_means, aes(x = Site, y = mean.biomass, color = FFG)) +
  geom_point(color="black") +
  labs(x = "Stream", y = "Biomass") +
  theme_minimal()

trimmedmeansites

#Okay, let's remake the overlap with the trimmed data
trimmedmeansites2 = ggplot(data = trimmed_means, aes(x = Site, y = mean.biomass, colour = SC)) +
  geom_point(color="black") + #This is the average of replicates
  labs(x = "Stream", y = "Biomass") +
  theme_minimal()+
  geom_boxplot(data=trimmed_data, aes(x=Site,y=mean.biomass, colour= SC.Category)) +
  #adding in the biomass per each site
  scale_color_manual(values = c("red","orange","forestgreen"))

trimmedmeansites2 #Interesting to see overlap

#let's do continuous
trimmed_means$SC.Level <- factor(trimmed_means$SC.Level, levels = c("25","72","78","387","402","594","1,119","1,242","1,457"))

octlm = ggplot(trimmed_means, aes(x = SC.Level, y = mean.biomass, colour = SC.Category)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  labs(x = "Specific Conductance", y = "Biomass") +
  theme_minimal() +
  scale_color_manual(values = c("red","orange","forestgreen"))


print(octlm) #hmmm it's not adding a line


#more fun boxplot code
#first, let's order them correctly
trimmed_data$Site <- factor(trimmed_data$Site, levels = c("EAS", "CRO","HCN","FRY","HUR","RUT","LLC","LLW","RIC"))
trimmed_data$SC.Category <- factor(trimmed_data$SC.Category, levels = c("REF","MID","HIGH"))
trimmed_data$SC.Level <- factor(trimmed_data$SC.Level, levels = c("25","72","78","387","402","594","1,119","1,242","1,457"))

siteplot=ggplot(data=trimmed_data,aes(x=Site,y=mean.biomass, color=SC.Category))+
  geom_boxplot() +
  labs(x = "Stream", y = "Biomass (g/m2)") +
  theme_minimal() +
  scale_color_manual(values = c("red","orange","forestgreen")) 
siteplot

levelplot=ggplot(data=trimmed_data,aes(x=SC.Level,y=mean.biomass, color=SC.Category))+
  geom_boxplot() +
  labs(x = "Stream", y = "Biomass (g/m2)") +
  theme_minimal() +
  scale_color_manual(values = c("red","orange","forestgreen")) 
levelplot


FFGgplot=ggplot(data=trimmed_data,aes(x=FFG,y=mean.biomass, na.rm=TRUE))+
  geom_boxplot()+
  geom_point(aes(color=SC.Category))+
  ylab(expression(mean.biomass))+ #can add log but it looks crazy
  xlab("Functional Feeding Groups") #change x axes
FFGgplot
#now the axes are better...

FFGgplot1=ggplot(data=trimmed_data,aes(x=FFG,y=mean.biomass))+
  facet_wrap(~Site,ncol=6,nrow=6)+ #this is creating multiple "panels" for site
  geom_boxplot()+
  geom_point(aes(color=Site),size=2)+
  ylab(expression(mean.biomass))+
  xlab("")+
  scale_colour_viridis(discrete = T)+
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
FFGgplot1#I kind love this one, great sign for my preliminary data too that target scraper
#ffgs are not there in mid and high SC sites

#Let's make panels based on SC category
FFGgplot2=ggplot(data=trimmed_data,aes(x=FFG,y=mean.biomass))+
  facet_wrap(~SC.Category,ncol=6,nrow=6)+ #this is creating multiple "panels" for site
  geom_boxplot()+
  geom_point(aes(color=SC.Category),size=2)+
  ylab(expression(Biomass))+
  xlab("")+
  scale_colour_viridis(discrete = T)+
  scale_color_manual(values = c("forestgreen","orange","red")) +
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
FFGgplot2 # a bit bleak on the scrapers still

#Let's make panels based on SC category
install.packages("rcartocolor")#Colorblind color schemes
display_carto_all(colorblind_friendly = TRUE)

colors <- carto_pal(n =5,"Geyser")

trimmed_data$FFG <- factor(trimmed_data$FFG, levels = c("Scraper","Shredder","Predator","Collector-Gatherer","Collector-Filterer"))
FFGgplot3=ggplot(data=trimmed_data,aes(x=SC.Category,y=mean.biomass))+
  facet_wrap(~FFG,ncol=6,nrow=6)+ #this is creating multiple "panels" for site
  geom_boxplot()+
  geom_point(aes(color=FFG),size=2)+
  ylab(expression(Biomass(g/m^2)))+
  xlab("")+
  scale_colour_viridis(discrete = T)+
  scale_color_manual(values = colors) +
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
FFGgplot3

#proportions!
library(RColorBrewer)
trimmed_data$SC.Category <- factor(trimmed_data$SC.Category, levels = c("REF","MID","HIGH"))
trimmed_data$FFG <- factor(trimmed_data$FFG, levels = c("Scraper","Shredder","Predator","Collector-Gatherer","Collector-Filterer"))
FFGgplot4= ggplot(trimmed_data, aes(x = SC.Category, y = mean.biomass, fill = FFG)) +
  geom_bar(stat = "identity") +
  labs(
    x = "SC Level (µS/cm)",
    y = "Biomass (g/m2)",
    fill = "FFG"
  ) +
  scale_fill_brewer(palette = "RdYlGn", type = "qual", direction = -1)+
  theme_minimal()
FFGgplot4

#proportions across sites
library(RColorBrewer)
trimmed_data$SC.Level <- factor(trimmed_data$SC.Level, levels = c("25","72","78","387","402","594","1,119","1,242","1,457"))
trimmed_data$FFG <- factor(trimmed_data$FFG, levels = c("Scraper","Shredder","Predator","Collector-Gatherer","Collector-Filterer"))
FFGgplot5= ggplot(trimmed_data, aes(x = SC.Level, y = mean.biomass, fill = FFG)) +
  geom_bar(stat = "identity") +
  labs(
    x = "SC Level (µS/cm)",
    y = "Biomass (g/m2)",
    fill = "FFG"
  ) +
  scale_fill_brewer(palette = "RdYlGn", type = "qual", direction = -1) +
  theme_minimal()
FFGgplot5

-----------------------------------------
#ANOVA
install.packages("broom")
install.packages("kableExtra")
library(broom)
library(kableExtra)

trimmed_data$SC.Category <- factor(trimmed_data$SC.Category, levels = c("REF","MID","HIGH"))
trimmed_data$FFG <- factor(trimmed_data$FFG, levels = c("Scraper","Shredder","Collector-Gatherer","Predator","Collector-Filterer"))


anova_result <- aov(mean.biomass ~ SC.Category, data = trimmed_data)
#Biomass across SC category
anova_result <- aov(mean.biomass ~ SC.Category, data = trimmed_data)
summary(anova_result) #biomass across SC.category isn't significant

tukey_result <- TukeyHSD(anova_result)
print(tukey_result)

#Biomass across SC Level
anova_result2 <- aov(mean.biomass ~ SC.Level, data = trimmed_data)
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
anova_result3 <- aov(mean.biomass ~ FFG, data = trimmed_data)
summary(anova_result3)

tidy_anova3 <- tidy(anova_result3)

install.packages("kableExtra")
library(kableExtra)

tidy_anova3 %>%
  knitr::kable() %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed"), full_width = F)

tukey_result <- TukeyHSD(anova_result3)
print(tukey_result)

#Biomass across FFG and SC.Level interactive
anova_result4 <- aov(mean.biomass ~ FFG*SC.Level, data = trimmed_data)
summary(anova_result4)
#KATE QUESTION: why is the p value for FFG and SC.Level different on this one than 
#the anova_result3 and anova_result?

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



#Let's look at effects
pr <- function(m) printCoefmat(coef(summary(m)),
                               digits=3,signif.stars=FALSE) 

#Get the estimates per group, rather than the differences
pr(lm1 <- lm(mean.biomass~SC.Category,data=trimmed_data))

pr(lm2 <- lm(mean.biomass~SC.Level,data=trimmed_data))

pr(lm3<- lm(mean.biomass~FFG, data=trimmed_data))

pr(lm4<- lm(mean.biomass~FFG*SC.Level, data=trimmed_data))

pr(lm5<- lm(mean.biomass~FFG*SC.Category, data=trimmed_data))

anova(lm1,lm2,lm3,lm4, lm5)

library(effects)

summary(allEffects(lm1))

summary(allEffects(lm2))

summary(allEffects(lm3))

summary(allEffects(lm4))

summary(allEffects(lm5))

#plot the effects 
plot(allEffects(lm1)) #High has highest biomass of the sites, RIC probably pulling it a bit

plot(allEffects(lm2))

plot(allEffects(lm3))

plot(allEffects(lm4))

plot(allEffects(lm5))
#Playing around with glm
library(DHARMa)
news(package='DHARMa')

#Gamma time
biomassmeantable$mean.biomass2 <- rgamma(length(biomassmeantable$mean.biomass),shape=1,rate=1)
g1 = glm(mean.biomass2~SC.Level,data=biomassmeantable,family = Gamma(link = inverse));
summary(g1)

simulationOutput <- simulateResiduals(fittedModel = g1, plot = T)
simulationOutput #Gamma distribution is good! Makes sense bc positive continuous

library(emmeans)
emmeans(g1, pairwise~Sc.Level)
emmeans(g1, pairwise~SC.Level, type="response") #tells you the expected abundance
#at each stream and confidence limits for Y-variable
#strong significant differences between ref and mid-high salinity stream


#Just for fun, let's do poisson on my abundance
##Poisson GLM## 
g2 = glm(Abundance....individuals.~Site,data=biomassoct, family=poisson);
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


