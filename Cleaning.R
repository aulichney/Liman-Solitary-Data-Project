#set working directory (where files are located) and load libraries
setwd("/Users/annieulichney/Desktop/SolitaryResearch")
library(readxl)    

#function to separate multiple sheets in xslx file
read_excel_allsheets <- function(filename, tibble = FALSE) {
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

#read in data, note that multiple sheets are contained in website data xlsx file so sheet separating function used to read them as individual dataframes
mysheets <- read_excel_allsheets("Website Data.xlsx")
data_2019 <- read.csv('2019\ Code\ and\ Data/clean_RH.csv')
data_2018_prelim <- data.frame(mysheets[1])
data_2016_prelim <- data.frame(mysheets[2])
data_2014_prelim <- data.frame(mysheets[3])



#---------------------2018 Data Cleaning------------------------------------ 

#create empty 2018 frame
data_2018 <- data.frame('ref_no' = c(1:51))


#Fill 2018 dataframe with corresponding variables from preliminary dataset
data_2018$jurisdiction <- data_2018_prelim$X2018.Table[3:53]

data_2018$total_custodial <- data_2018_prelim$X2018.Numbers.and.Percentages.of.Men.and.Women.in.Restrictive.Housing..RH..by.Jurisdiction..n.43..[3:53]


data_2018$total_rh <- data_2018_prelim$X2018....3[3:53]

data_2018$pct_rh <- data_2018_prelim$X2018....4[3:53]


#time in rh
data_2018$rh_intervals_15_30_days <- data_2018_prelim$X2018.Table.2..Numbers.of.Prisoners.in.Restrictive.Housing.by.Length.of.Time.and.by.Jurisdiction..n.36..[3:53]

data_2018$rh_intervals_31_90_days <- data_2018_prelim$X2018....6[3:53]

data_2018$rh_intervals_91_180_days <- data_2018_prelim$X2018....7[3:53]

data_2018$rh_intervals_181_365_days <- data_2018_prelim$X2018....8[3:53]

data_2018$rh_intervals_1_3_years <- data_2018_prelim$X2018....9[3:53]

data_2018$rh_intervals_3_6_years <- data_2018_prelim$X2018....10[3:53]

data_2018$rh_intervals_over_6_years <- data_2018_prelim$X2018....11[3:53]


#custodial/rh breakdowns
data_2018$total_custodial_male <- data_2018_prelim$X2018.Table.4..Number.and.Percentage.Total.Custodial.Population.of.Male.Custodial.Population.in.Restrictive.Housing..n.34..46[3:53]

data_2018$total_rh_male <- data_2018_prelim$X2018....14[3:53]

data_2018$total_custodial_female <- data_2018_prelim$X2018.Table.5..Number.and.Percentage.of.Female.Custodial.Population.in.Restrictive.Housing..n.32..48[3:53]

data_2018$total_rh_female <- data_2018_prelim$X2018....17[3:53]


#male demographics
data_2018$total_custodial_white_male <- data_2018_prelim$X2018.Table.7..Demographic.Composition.of.Total.Male.Custodial.Population.and.Male.Restrictive.Housing.Population..n...33...53...19[3:53]

data_2018$total_custodial_black_male <- data_2018_prelim$X2018....20[3:53]

data_2018$total_custodial_hispanic_or_latino_male <- data_2018_prelim$X2018....21[3:53]

data_2018$total_custodial_asian_male <- data_2018_prelim$X2018....22[3:53]

data_2018$total_custodial_hawaiian_or_pacific_islander_male <- data_2018_prelim$X2018....23[3:53]

data_2018$total_custodial_native_male <- data_2018_prelim$X2018....24[3:53]

data_2018$total_custodial_other_male <- data_2018_prelim$X2018....25[3:53]

data_2018$total_rh_white_male <- data_2018_prelim$X2018.Table.7..Demographic.Composition.of.Total.Male.Custodial.Population.and.Male.Restrictive.Housing.Population..n...33...53...27[3:53]

data_2018$total_rh_black_male <- data_2018_prelim$X2018....28[3:53]

data_2018$total_rh_hispanic_or_latino_male <- data_2018_prelim$X2018....29[3:53]

data_2018$total_rh_asian_male <- data_2018_prelim$X2018....30[3:53]

data_2018$total_rh_hawaiian_or_pacific_islander_male <- data_2018_prelim$X2018....31[3:53]

data_2018$total_rh_native_male <- data_2018_prelim$X2018....32[3:53]

data_2018$total_rh_other_male <- data_2018_prelim$X2018....33[3:53]


#female demographics
data_2018$total_custodial_white_female <- data_2018_prelim$X2018.Table.9..Demographic.Composition.of.Total.Female.Custodial.Population.and.Female.Restrictive.Housing.Population..n...32...55...49[3:53]

data_2018$total_custodial_black_female <- data_2018_prelim$X2018....50[3:53]

data_2018$total_custodial_hispanic_or_latino_female <- data_2018_prelim$X2018....51[3:53]

data_2018$total_custodial_asian_female <- data_2018_prelim$X2018....52[3:53]

data_2018$total_custodial_hawaiian_or_pacific_islander_female <- data_2018_prelim$X2018....53[3:53]

data_2018$total_custodial_native_female <- data_2018_prelim$X2018....54[3:53]

data_2018$total_custodial_other_female <- data_2018_prelim$X2018....55[3:53]

data_2018$total_rh_white_female <- data_2018_prelim$X2018.Table.9..Demographic.Composition.of.Total.Female.Custodial.Population.and.Female.Restrictive.Housing.Population..n...32...55...57[3:53]

data_2018$total_rh_black_female <- data_2018_prelim$X2018....58[3:53]

data_2018$total_rh_hispanic_or_latino_female <- data_2018_prelim$X2018....59[3:53]

data_2018$total_rh_asian_female <- data_2018_prelim$X2018....60[3:53]

data_2018$total_rh_hawaiian_or_pacific_islander_female <- data_2018_prelim$X2018....61[3:53]

data_2018$total_rh_native_female <- data_2018_prelim$X2018....62[3:53]

data_2018$total_rh_other_female <- data_2018_prelim$X2018....63[3:53]


#male age breakdown
data_2018$total_custodial_under_18_male <- data_2018_prelim$X2018.Table.11..Age.Cohorts.of.Male.Total.Custodial.Population.and.of.Male.Restrictive.Housing.Population..n.34..58...79[3:53]

data_2018$total_custodial_18_to_25_male <-data_2018_prelim$X2018....80[3:53]

data_2018$total_custodial_26_to_35_male <-data_2018_prelim$X2018....81[3:53]

data_2018$total_custodial_36_to_50_male <-data_2018_prelim$X2018....82[3:53]

data_2018$total_custodial_over_50_male <-data_2018_prelim$X2018....83[3:53]

data_2018$total_rh_under_18_male <- data_2018_prelim$X2018.Table.11..Age.Cohorts.of.Male.Total.Custodial.Population.and.of.Male.Restrictive.Housing.Population..n.34..58...85[3:53]

data_2018$total_rh_18_to_25_male <-data_2018_prelim$X2018....86[3:53]

data_2018$total_rh_26_to_35_male <-data_2018_prelim$X2018....87[3:53]

data_2018$total_rh_36_to_50_male <-data_2018_prelim$X2018....88[3:53]

data_2018$total_rh_over_50_male <-data_2018_prelim$X2018....89[3:53]


#female age breakdown
data_2018$total_custodial_under_18_female <- data_2018_prelim$X2018.Table.13..Age.Cohorts.of.Female.Total.Custodial.Population.and.of.Female.Restrictive.Housing.Population..n.32..59...101[3:53]

data_2018$total_custodial_18_to_25_female <-data_2018_prelim$X2018....102[3:53]

data_2018$total_custodial_26_to_35_female <-data_2018_prelim$X2018....103[3:53]

data_2018$total_custodial_36_to_50_female <-data_2018_prelim$X2018....104[3:53]

data_2018$total_custodial_over_50_female <-data_2018_prelim$X2018....105[3:53]

data_2018$total_rh_under_18_female <- data_2018_prelim$X2018.Table.13..Age.Cohorts.of.Female.Total.Custodial.Population.and.of.Female.Restrictive.Housing.Population..n.32..59...107[3:53]

data_2018$total_rh_18_to_25_female <-data_2018_prelim$X2018....108[3:53]

data_2018$total_rh_26_to_35_female <-data_2018_prelim$X2018....109[3:53]

data_2018$total_rh_36_to_50_female <-data_2018_prelim$X2018....110[3:53]

data_2018$total_rh_over_50_female <-data_2018_prelim$X2018....111[3:53]

#smi general

data_2018$smi_total_male <-data_2018_prelim$X2018....124[3:53]

data_2018$smi_rh_male <-data_2018_prelim$X2018....126[3:53]

data_2018$smi_total_female <-data_2018_prelim$X2018....129[3:53]

data_2018$smi_rh_female <-data_2018_prelim$X2018....131[3:53]

#smi by race male

data_2018$smi_white_male <-data_2018_prelim$X2018.Table.17..Male.Prisoners.with.Serious.Mental.Illness.by.Race.and.Ethnicity.in.the.Total.Custodial.Population.and.in.the.Restrictive.Housing.Population..n.31....133[3:53]

data_2018$smi_black_male <-data_2018_prelim$X2018....134[3:53]

data_2018$smi_hispanic_or_latino_male <-data_2018_prelim$X2018....135[3:53]

data_2018$smi_asian_male <-data_2018_prelim$X2018....136[3:53]

data_2018$smi_hawaiian_or_pacific_islander_male <-data_2018_prelim$X2018....137[3:53]

data_2018$smi_native_male <-data_2018_prelim$X2018....138[3:53]

data_2018$smi_other_male <-data_2018_prelim$X2018....139[3:53]

data_2018$smi_rh_white_male <-data_2018_prelim$X2018.Table.17..Male.Prisoners.with.Serious.Mental.Illness.by.Race.and.Ethnicity.in.the.Total.Custodial.Population.and.in.the.Restrictive.Housing.Population..n.31....141[3:53]

data_2018$smi_rh_black_male <-data_2018_prelim$X2018....142[3:53]

data_2018$smi_rh_hispanic_or_latino_male <-data_2018_prelim$X2018....142[3:53]

data_2018$smi_rh_asian_male <-data_2018_prelim$X2018....144[3:53]

data_2018$smi_rh_hawaiian_or_pacific_islander_male <-data_2018_prelim$X2018....145[3:53]

data_2018$smi_rh_native_male <-data_2018_prelim$X2018....146[3:53]

data_2018$smi_rh_other_male <-data_2018_prelim$X2018....147[3:53]

#smi by race female

data_2018$smi_white_female <-data_2018_prelim$X2018.Table.18..Female.Prisoners.with.Serious.Mental.Illness.by.Race.and.Ethnicity.in.the.Total.Custodial.Population.and.in.the.Restrictive.Housing.Population..n.28....149[3:53]

data_2018$smi_black_female <-data_2018_prelim$X2018....150[3:53]

data_2018$smi_hispanic_or_latino_female <-data_2018_prelim$X2018....151[3:53]

data_2018$smi_asian_female <-data_2018_prelim$X2018....152[3:53]

data_2018$smi_hawaiian_or_pacific_islander_female <-data_2018_prelim$X2018....153[3:53]

data_2018$smi_native_female <-data_2018_prelim$X2018....154[3:53]

data_2018$smi_other_female <-data_2018_prelim$X2018....155[3:53]

data_2018$smi_rh_white_female <-data_2018_prelim$X2018.Table.18..Female.Prisoners.with.Serious.Mental.Illness.by.Race.and.Ethnicity.in.the.Total.Custodial.Population.and.in.the.Restrictive.Housing.Population..n.28....157[3:53]

data_2018$smi_rh_black_female <-data_2018_prelim$X2018....158[3:53]

data_2018$smi_rh_hispanic_or_latino_female <-data_2018_prelim$X2018....159[3:53]

data_2018$smi_rh_asian_female <-data_2018_prelim$X2018....160[3:53]

data_2018$smi_rh_hawaiian_or_pacific_islander_female <-data_2018_prelim$X2018....161[3:53]

data_2018$smi_rh_native_female <-data_2018_prelim$X2018....162[3:53]

data_2018$smi_rh_other_female <-data_2018_prelim$X2018....163[3:53]



#-------------------2016 Data Cleaning---------------------------#

#create empty 2016 frame
data_2016 <- data.frame('ref_no' = c(1:51))


#Fill 2016 dataframe with corresponding variables from preliminary dataset
data_2016$jurisdiction <- data_2016_prelim$X2016.Table[3:53]
data_2016$jurisdiction <- gsub('\\^.*', '', data_2016$jurisdiction) #remove superscripts

data_2016$total_custodial <- gsub('\\*.*', '', data_2016_prelim$X2016....3[3:53])


data_2016$total_rh <- data_2016_prelim$X2016....4[3:53]

data_2016$pct_rh <- data_2016_prelim$X2016....5[3:53]



#time in cell daily
data_2016$rh_intervals_daily_over_22_hours <- data_2016_prelim$X2016....7[3:53]

data_2016$rh_intervals_daily_20_21_hours <- data_2016_prelim$X2016....9[3:53]

data_2016$rh_intervals_daily_16_19_hours <- data_2016_prelim$X2016....11[3:53]

data_2016$rh_intervals_daily_16_24_hours <- data_2016_prelim$X2016....13[3:53]


#time in rh
data_2016$rh_intervals_15_30_days <- data_2016_prelim$X2016.Table.4...Numbers.of.Prisoners.in.Restricted.Housing.by.Length.of.Time.and.by.Jurisdiction..n...41.[3:53]

data_2016$rh_intervals_31_90_days <- data_2016_prelim$X2016....16[3:53]

data_2016$rh_intervals_91_180_days <- data_2016_prelim$X2016....17[3:53]

data_2016$rh_intervals_181_365_days <- data_2016_prelim$X2016....18[3:53]

data_2016$rh_intervals_1_3_years <- data_2016_prelim$X2016....19[3:53]

data_2016$rh_intervals_3_6_years <- data_2016_prelim$X2016....20[3:53]

data_2016$rh_intervals_over_6_years <- data_2016_prelim$X2016....21[3:53]


#custodial/rh breakdowns
data_2016$total_custodial_male <- data_2016_prelim$X2016.Table.5...Number.and.Percentage.of.Male.Custodial.Population.in.Restricted.Housing..n.43..201[3:53]

data_2016$total_rh_male <- data_2016_prelim$X2016....23[3:53]

data_2016$total_custodial_female <- data_2016_prelim$X2016....25[3:53]

data_2016$total_rh_female <- data_2016_prelim$X2016....26[3:53]


#male demographics
data_2016$total_custodial_white_male <- data_2016_prelim$X2016.Table.7...Demographic.Composition.of.Total.Male.Custodial.Population.and.of.Male.Restricted.Housing.Population..n...43....28[3:53]

data_2016$total_custodial_black_male <- data_2016_prelim$X2016....29[3:53]

data_2016$total_custodial_hispanic_or_latino_male <- data_2016_prelim$X2016....30[3:53]

data_2016$total_custodial_asian_male <- data_2016_prelim$X2016....31[3:53]

data_2016$total_custodial_other_male <- data_2016_prelim$X2016....32[3:53]

data_2016$total_rh_white_male <- data_2016_prelim$X2016.Table.7...Demographic.Composition.of.Total.Male.Custodial.Population.and.of.Male.Restricted.Housing.Population..n...43....34[3:53]

data_2016$total_rh_black_male <- data_2016_prelim$X2016....35[3:53]

data_2016$total_rh_hispanic_or_latino_male <- data_2016_prelim$X2016....36[3:53]

data_2016$total_rh_asian_male <- data_2016_prelim$X2016....37[3:53]

data_2016$total_rh_other_male <- data_2016_prelim$X2016....38[3:53]


#female demographics
data_2016$total_custodial_white_female <- data_2016_prelim$X2016.Table.9...Demographic.Composition.of.Total.Female.Custodial.Population.and.Female.Restricted.Housing.Population..n...40....50[3:53]

data_2016$total_custodial_black_female <- data_2016_prelim$X2016....51[3:53]

data_2016$total_custodial_hispanic_or_latino_female <- data_2016_prelim$X2016....52[3:53]

data_2016$total_custodial_asian_female <- data_2016_prelim$X2016....53[3:53]

data_2016$total_custodial_other_female <- data_2016_prelim$X2016....54[3:53]

data_2016$total_rh_white_female <- data_2016_prelim$X2016.Table.9...Demographic.Composition.of.Total.Female.Custodial.Population.and.Female.Restricted.Housing.Population..n...40....56[3:53]

data_2016$total_rh_black_female <- data_2016_prelim$X2016....57[3:53]

data_2016$total_rh_hispanic_or_latino_female <- data_2016_prelim$X2016....58[3:53]

data_2016$total_rh_asian_female <- data_2016_prelim$X2016....59[3:53]

data_2016$total_rh_other_female <- data_2016_prelim$X2016....60[3:53]


#male age breakdown
data_2016$total_custodial_under_18_male <- data_2016_prelim$X2016.Table.11...Age.Cohorts.of.Male.Total.Custodial.Population.and.of.Male.Restricted.Housing.Population..n...43....72[3:53]

data_2016$total_custodial_18_to_49_male <-data_2016_prelim$X2016....73[3:53]

data_2016$total_custodial_over_50_male <-data_2016_prelim$X2016....74[3:53]

data_2016$total_rh_under_18_male <- data_2016_prelim$X2016.Table.11...Age.Cohorts.of.Male.Total.Custodial.Population.and.of.Male.Restricted.Housing.Population..n...43....76[3:53]

data_2016$total_rh_18_to_49_male <-data_2016_prelim$X2016....77[3:53]

data_2016$total_rh_over_50_male <-data_2016_prelim$X2016....78[3:53]


#female age breakdown
data_2016$total_custodial_under_18_female <- data_2016_prelim$X2016.Table.13...Age.Cohorts.of.Female.Total.Custodial.Population.and.of.Female.Restricted.Housing.Population..n...40....86[3:53]

data_2016$total_custodial_18_to_49_female <-data_2016_prelim$X2016....87[3:53]

data_2016$total_custodial_over_50_female <-data_2016_prelim$X2016....88[3:53]

data_2016$total_rh_under_18_female <- data_2016_prelim$X2016.Table.13...Age.Cohorts.of.Female.Total.Custodial.Population.and.of.Female.Restricted.Housing.Population..n...40....90[3:53]

data_2016$total_rh_18_to_49_female <-data_2016_prelim$X2016....91[3:53]

data_2016$total_rh_over_50_female <-data_2016_prelim$X2016....92[3:53]

#smi general

data_2016$smi_total_male <-data_2016_prelim$X2016....101[3:53]

data_2016$smi_rh_male <-data_2016_prelim$X2016....103[3:53]

data_2016$smi_total_female <-data_2016_prelim$X2016....106[3:53]

data_2016$smi_rh_female <-data_2016_prelim$X2016....108[3:53]

#smi by race male

data_2016$smi_white_male <-data_2016_prelim$X2016.Table.17...Male.Prisoners.with.a.Serious.Mental.Health.Issue.by.Race.and.Ethnicity..n...33.[3:53]

data_2016$smi_black_male <-data_2016_prelim$X2016....111[3:53]

data_2016$smi_hispanic_or_latino_male <-data_2016_prelim$X2016....112[3:53]

data_2016$smi_asian_male <-data_2016_prelim$X2016....113[3:53]

data_2016$smi_other_male <-data_2016_prelim$X2016....114[3:53]


#smi by race female

data_2016$smi_white_female <-data_2016_prelim$X2016.Table.18...Female.Prisoners.with.a.Serious.Mental.Health.Issue.by.Race.and.Ethnicity..n...30.[3:53]

data_2016$smi_black_female <-data_2016_prelim$X2016....117[3:53]

data_2016$smi_hispanic_or_latino_female <-data_2016_prelim$X2016....118[3:53]

data_2016$smi_asian_female <-data_2016_prelim$X2016....119[3:53]

data_2016$smi_other_female <-data_2016_prelim$X2016....120[3:53]


#pregnancy
data_2016$total_pregnant <-data_2016_prelim$X2016....123[3:53]

data_2016$rh_pregnant <-data_2016_prelim$X2016....124[3:53]


#-------------------2014 Data Cleaning---------------------------#




#---------------------Write Files----------------------------------#

data_2018$total_custodial <- gsub('\\*', '', data_2018$total_custodial)
data_2018$total_custodial <- gsub(',', '', data_2018$total_custodial)


data_2018[colnames(data_2018)[3:length(colnames(data_2018))]] <- sapply(data_2018[colnames(data_2018)[3:length(colnames(data_2018))]], as.numeric)

data_2016$total_custodial <- gsub('\\*', '', data_2016$total_custodial)
data_2016$total_custodial <- gsub(',', '', data_2016$total_custodial)
data_2016$total_rh <- gsub(',', '', data_2016$total_rh)
data_2016$total_rh <- gsub('\\^.*', '', data_2016$total_rh)



data_2016[colnames(data_2016)[3:length(colnames(data_2016))]] <- sapply(data_2016[colnames(data_2016)[3:length(colnames(data_2016))]], as.numeric)

write.csv(data_2018,"clean_2018.csv", row.names = TRUE)
write.csv(data_2016,"clean_2016.csv", row.names = TRUE)
