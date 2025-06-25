# script for tidying mismatch data
# section added by Limoilou to update LRS data file (EXCEL) from M -------

# Only import necessary libraries for data wrangling
library(tidyverse)
library(readxl) 

# HERE VARIABLES OF INTEREST AND THEIR DEFINITION. VERY IMPORTANT TO NOTE AND REMEMBER. 

# yr.st - this is the year when the ewe was 2 years old

# longevity - age at death or age in 2017 if still alive (LAR - year might not be up to date here!)

# notes 
# 0 - natural death
# 1 - still alive
# 2 - removed
# 3 - monitoring incomplete, did not start at age 2

#2 (all numbers, no letter) - Reproductive success, here at age 2, repeated for each year of life;
# 0 - no lamb
# 1 - neonatal death
# 2 - summer death
# 3 - survived to weaning (late September), died in winter
# 4 - survived to weaning, then unknown
# 5 - survived to 1 year of age

# status 8: received contraceptive implants, 9: human cause of death to lamb (but apparently could also be listed as 8 according to Marco)


# s2 - Sex of lamb produced at age 2
# 0 - unknown
# 1 - male
# 2 - female

# get reproductive success data  --------------------------------------------------
getwd()


lrs <- read_excel("data/raw/lrs.rm 2.xlsx") # updated

# here we rename every single column. the goal is to reshape the df from a wide format to a long format.

# lrs is a 354 by 56 df. 
# lrs2 is a 6018 by 7 df

# create a vector of column indices
cols <- seq(5, 55, 3) # will iterate over the column indices in steps of 3

# create a list of data frames
l_list <- lapply(cols, function(i) {
  lrs[, c(1:4, i:(i+2))] %>% setNames(c("ID", "yr_st", "longev", "notes", "code_sr", "lamb_sex", "lamb_id"))
})

# concatenate the data frames into a single long format data frame
lrs2 <- do.call(rbind, l_list)

# get the number of rows in l2
n <- nrow(l_list[[1]])

# recreate variable "age" by creating a sequence starting at 2 yo
lrs2 <- data.frame(lrs2,ewe_age=rep(2:18,each=n)) # you will be missing older females (n=1 of 19)
lrs2 <- lrs2[,c(1:4,8,5:7)]
lrs3 <- lrs2
lrs3$lamb_id <- as.character(lrs3$lamb_id)

# recreate lamb sex variable 
l_sex <- NA
l_sex[lrs3$lamb_sex==1] <- "M"; l_sex[lrs3$lamb_sex==2] <- "F"; l_sex[lrs3$lamb_sex==0] <- "I"
lrs3$lamb_sex <- l_sex

# recreate a year variable from the year when a ewe is 2yo and from the age. 
lrs3$yr <- lrs3$yr_st-2+lrs3$ewe_age#; lrs3[is.na(lrs3$lamb.id),"yr"] <- NA

# did the lamb survived to year t+1?
lamb_surv_t1 <- NA; lamb_surv_t1[lrs3$code_sr==5] <- T;lamb_surv_t1[!lrs3$code_sr==5] <-F
lamb_weaned <- NA; lamb_weaned[lrs3$code_sr>=3] <- T; lamb_weaned[lrs3$code_sr<3] <- F
reproduced <- NA; reproduced[lrs3$code_sr>0] <- T; reproduced[lrs3$code_sr==0] <- F
lrs3$lamb_surv_t1 <- as.numeric(lamb_surv_t1)
lrs3$lamb_weaned <- as.numeric(lamb_weaned)
lrs3$reproduced <- as.numeric(reproduced)

lrs3 <- lrs3[!is.na(lrs3$code_sr),]
lrs3 <- lrs3[order(lrs3$yr),]

rm(list = c('l_list',"l10", "l11", "l12", "l13", "l14", "l15", "l16", "l17", "l18", 
            "l2", "l3", "l4", "l5", "l6", "l7", "l8", "l9", "lamb_surv_t1", 
            "lamb_weaned", "lrs", "lrs2",  "l.sex", "n", "reproduced"))

lrs.f<- lrs3 # keep this one in case 

# updated to 2021
#get age at first reproduction
lrs.f <- lrs.f[!is.na(lrs.f$reproduced),]

tmp <- lrs.f %>% 
  filter(reproduced == 1) %>% 
  group_by(ID) %>% 
  mutate(first_repro = min(ewe_age)) %>% # added the prim column directly here
  mutate(prim=case_when(
    ewe_age==first_repro~T, 
    ewe_age!=first_repro~F
    ))

# OTHER OPTION with ifelse (from Explo script)
tmp$prim2<-ifelse(tmp$ewe_age==tmp$first_repro,T,F)

#get year of first reproduction
tmp$yr_prim <- tmp$yr_st-2+tmp$first_repro

# cross check with Marco's information 
# The first ewe with adequate data is 14E, row 58. She was primiparous at age 3.

# ok - but we might have more - all 10E to 10 L are not so bad in terms of information

# merge back with all females in lrs.f (since tmp is filtered on reproduced ==1)

lrs.f <- full_join(lrs.f, tmp[, c("ID", "yr", "yr_st", "first_repro", "prim")]) # 2056 n 

# verify number of rows
n <- nrow(lrs.f) # 2056 rows

# create variable pregnant at first capture 
# then comparing their adj. mass on Jn 5 between pregnant and non pregnant 
allewes <- read_excel("data/raw/Allewes 2.xlsx")  # there are multiple entries for a same female-year since this is per date 

# view(allewes)
allewes$Notes <- as.character(allewes$Notes)
allewes$pregnant <-
  case_when(
    allewes$Notes == 'pregnant' ~ 1,
    allewes$Notes == 'pregnant?' ~ 1,
    allewes$Notes == 'pregnant ??' ~ 1,
    TRUE ~ 0
  ) %>% as.factor()# tout le jeu de donnees, toutes les dates 

# Ensure date is in a date format
# Define a function to convert Julian day to standard date
convert_julian_to_date <- function(Yr, date, start_month = 5, start_day = 24) {
  # Create the start date for the given year
  start_date <- as.Date(paste(Yr, start_month, start_day, sep = "-"))
  # Add the Julian day to the start date
  actual_date <- start_date + days(date)  # Subtract 1 since Julian day 1 is the start date itself
  return(actual_date)
}

# Apply the conversion function
allewes <- allewes %>%
  mutate(dateJJ = convert_julian_to_date(Yr, date)) %>% rename(yr=Yr)

# finds if the first capture of the year is pregnant
first_capture <- allewes %>%
  group_by(ID,yr) %>% # important to be by yr too only selects one date overall 
  arrange(dateJJ) %>%
  dplyr::slice(1) %>% # ensure the first is right  - normally will keep one row per year/id/age
  ungroup() %>% 
  rename(first_pregnant=pregnant, 
         ewe_age = Age) %>% 
  select(ID, yr, ewe_age, first_pregnant) %>% 
  distinct() # this should give one value per year or age - wether this year she was first captured pregnant 

# Display the result
print(first_capture)

# merge with df
lrs.f <- left_join(lrs.f, first_capture) # good that merges with id, yr, and ewe-age
# les NA c'est femelle qui sont proches de la mort



# add spring & fall mass & lamb weaning mass -------
# adjwt <-read_excel("data/raw/adjWT_1973_2022.xlsx",col_names = T) # same - take the mine folder 

# add the updated version of the ms 
adjwt <-read.csv("data/raw/adjWT_1973_2022.csv", sep=";")

adjwt.f <- adjwt %>%
  filter(sex == "female", age > 1) %>%
  mutate(adjwt = as.numeric(adjwt)) %>%
  ungroup()

mass <- adjwt.f %>%
  filter(JJ %in% c(12, 114) & method=='mixed') %>% # excludes lamb wt adj. 22 and pop estimates. 
  mutate(mass_type = ifelse(JJ == 12, "mass_spring_f", "mass_fall_f")) %>%
  select(yr, ID, adjwt, mass_type)%>% distinct()  # 3670


# IMPORTANT - ONLY KEEP ADJUSTED MASS BASED ON MIXED MODELS 
# Otherwise will have no variation and the pop mean

lrs_f_mass <- full_join(mass, lrs.f) # you have 0 and yr old back here - and still females that did not reproduce at yr t

duplicates <- lrs_f_mass %>%
  group_by(yr, ID, yr_st, longev, notes, ewe_age, code_sr,
           lamb_sex, lamb_id, lamb_surv_t1, lamb_weaned,
           reproduced, first_repro, prim, mass_type, adjwt) %>%
  summarise(n = n(), .groups = "drop") %>%
  filter(n > 1L) # now 0 

rm(duplicates)

# Pivot to have separate columns for mass_spring_f and mass_fall_f
lrs_f_mass_pivot <- lrs_f_mass %>%
  pivot_wider(names_from = mass_type, values_from = adjwt) %>% arrange(yr, ID) # n=2061

# Now lrs_f_mass_pivoted contains separate columns for mass_spring_f and mass_fall_f

# add lamb mass 1387 going to 928 when only taking mixed method
adjwtlamb <- adjwt %>%
  filter(age == 0 & method=='mixed') %>% #  Attention if you keep pop estimate you might only have 1 point to estimate age.
  ungroup()


# Check if there are any missing values in the columns
summary(adjwtlamb)

weanMass <- adjwtlamb %>%
  filter(JJ == 114) %>%
  select(yr, lamb_id = ID, wean_mass = adjwt, lamb_sex_full = sex) # n=710 lambs NOw 464


lrs_f_mass_df <- lrs_f_mass_pivot %>%
  left_join(weanMass, by = c("yr", "lamb_id"))# n=1371


t <- lrs_f_mass_pivot %>%
  full_join(weanMass, by = c("yr", "lamb_id"))# n=1371



# when merging some lambs dont have mums or lamb_id were not correctly associated in prior lrs !! error! 
# lrs_f_mass_df <- merge(lrs_f_mass_pivot,
#                      weanMass,
#                      by.x=c("yr", "lamb_id"), 
#                      by.y = c("yr", "lamb_id"), 
#                      all.x=T) # only keep those with a real mom



# Reshape the data from wide to long format to graph (in other script)
# df_long <- lrs_f_mass_df %>%
#   select(ID, yr,first_pregnant, mass_spring_f, mass_fall_f) %>%
#   pivot_longer(cols = c(mass_spring_f, mass_fall_f),
#                names_to = "season",
#                values_to = "mass") %>%
#   mutate(date = case_when(
#     season == "mass_spring_f" ~ "June 5",
#     season == "mass_fall_f" ~ "September 15"
#   ))
# 
# # Convert date to factor with the correct order
# df_long$date <- factor(df_long$date, levels = c("June 5", "September 15"))
# df_long$unique_ID <- paste0(df_long$ID, '-', df_long$yr)


# add time lags for mass and code_sr --------------------------------------
lrs_f_mass_df$yr <- as.numeric(as.character(lrs_f_mass_df$yr))

#ajouter colonne de l'année précédente 
lrs_f_mass_df$previous_yr<- lrs_f_mass_df$yr-1

# ajouter succès reproducteur de la mère, sex de l'agneau et masse de la mère de l'automne précédent
lrs_f_mass_df <- merge(lrs_f_mass_df, 
                       lrs_f_mass_df[, c("ID", "yr", "code_sr", "mass_fall_f", "lamb_sex")], 
                       by.x= c("ID", "previous_yr"), 
                       by.y=c("ID", "yr"), 
                       all.x=TRUE) # add lines? removed the all.y=T if don't want lambs without mom.

colnames(lrs_f_mass_df)

lrs_f_mass_df <- lrs_f_mass_df %>% 
  dplyr::rename(mass_fall_f=mass_fall_f.x, 
                mass_fall_f_tm1=mass_fall_f.y, 
                code_sr=code_sr.x, 
                mom_prs=code_sr.y, 
                lamb_sex=lamb_sex.x, 
                lamb_sex_tm1=lamb_sex.y) %>% arrange(ID, yr) # 2061 rows

# verify lamb id, lamb sex, wean mass 
t.lamb=tibble(yr=lrs_f_mass_df$yr,fem.id=lrs_f_mass_df$ID, lb.id=lrs_f_mass_df$lamb_id, lb.sex=lrs_f_mass_df$lamb_sex, lb.sex.f=lrs_f_mass_df$lamb_sex_full, lb.mass=lrs_f_mass_df$wean_mass, sr=lrs_f_mass_df$code_sr)

which.na=t.lamb %>% filter(sr>2, is.na(lb.mass), !is.na(lb.id)) # this is partly because the method to estimate mass is more conservative
# from the beginning to 2021 (incl - the mass do not include 2022), 472 lambs of known ID and that survived at least to weaning don't have mass. 
# end verification



# add future reproductive success (frs) --------------------------------------

# Make a copy of the original data frame
lrs_f_mass_df2 <- lrs_f_mass_df

# Convert yr column to numeric
lrs_f_mass_df2$yr <- as.numeric(as.character(lrs_f_mass_df2$yr))

# Add a next year column
lrs_f_mass_df2$next_yr <- lrs_f_mass_df2$yr + 1

# Merge with the future reproductive success data
lrs_f_mass_df2 <- merge(lrs_f_mass_df2, lrs_f_mass_df2[, c("ID", "yr", "code_sr")], 
                      all.x = TRUE, by.x = c("ID", "next_yr"), by.y = c("ID", "yr"))

# Rename frs column and replace NA values with 0
lrs_f_mass_df2 <- lrs_f_mass_df2 %>% rename(frs = code_sr.y, code_sr = code_sr.x)
# lrs_f_mass_df2$frs[is.na(lrs_f_mass_df2$frs)] <- 0 #NOT A GOOD IDEA !!! might not have survived, which is different from did not reproduced

# Map frs values to new categories
lrs_f_mass_df2$frs <- case_when(lrs_f_mass_df2$frs == 0 ~ "noLamb",
                                lrs_f_mass_df2$frs == 1 ~ "neoDeath",
                                lrs_f_mass_df2$frs == 2 ~ "notWeaned",
                                lrs_f_mass_df2$frs %in% c(3, 4, 5) ~ "weaned",
                                lrs_f_mass_df2$frs %in% c(8, 9) ~ "human")

# Add repro_tp1 column
lrs_f_mass_df2$repro_tp1 <- case_when(lrs_f_mass_df2$frs %in% c("neoDeath", "notWeaned", "weaned") ~ "yes",
                                      lrs_f_mass_df2$frs == "noLamb" ~ "no")

# Add weaned_tp1 column (LAr added this to simplify)
lrs_f_mass_df2$wean_tp1 <- case_when(lrs_f_mass_df2$frs %in% c("neoDeath", "notWeaned") ~ "no",
                                     lrs_f_mass_df2$frs == "weaned" ~ "yes",
                                      lrs_f_mass_df2$frs == "noLamb" ~ NA)
                                     

# Convert repro_tp1 column to factor
lrs_f_mass_df2$repro_tp1 <- as.factor(lrs_f_mass_df2$repro_tp1)
lrs_f_mass_df2$wean_tp1 <- as.factor(lrs_f_mass_df2$wean_tp1)


#fix frs in 2020
# mismatch_df4[394, "frs"] <- "notWeaned"
# mismatch_df4[413, "frs"] <- "notWeaned" #worked! # ATTN this position depends on sorting
# lrs_f_mass_df2[lrs_f_mass_df2$yr=='2020', "frs"] <- "notWeaned"

# verify if lamb sex fits 
t.sex=tibble(mom.id=lrs_f_mass_df$ID, lb.id=lrs_f_mass_df$lamb_id, yr=lrs_f_mass_df$yr, l.sex1=lrs_f_mass_df$lamb_sex,
             l.sex4_tm1=lrs_f_mass_df$lamb_sex_tm1) %>% arrange(mom.id,yr)
# end verification

#clean 
rm(adjwt, adjwt.f, adjwtlamb, l_list, lrs_f_mass, lrs.f, lrs3, mass, mjune, msep, tmp, weanMass, lrs_f_mass_unique, lrs_f_mass_pivot, duplicates)

# ok here, before removing useless years, you want to make sure that females have their correct info from one year to the next (ex. mass, lamb sex, etc)
# view(lrs_f_mass_df) 

#all good n=2061 

# update : 2119







# import latest birthdates -----------------------------------------

#myBirthdates <- read_csv("data/raw/tidybdates_noneonatal.csv") # Once the setwd is re-done, don't need the full path here
myBirthdates <- read.csv("data/raw/ramMtnBirthdates_2025-05-08.csv")

# Add a next year column
myBirthdates$year_birth <- as.numeric(myBirthdates$year)
myBirthdates$next_yr <- myBirthdates$year_birth + 1

# create birthdate_tp1 here so that keep all data
myBirthdates <- merge(myBirthdates, 
                      myBirthdates[, c("mother_id", "year_birth", "birthdate")], 
                      all.x=TRUE,
                      by.x= c("mother_id", "next_yr"), 
                      by.y=c("mother_id", "year_birth")) %>% # rename duplicates 
  dplyr::rename(birthdate_tp1=birthdate.y, 
                birthdate=birthdate.x)

# adjust variable class
myBirthdates$birthdate_tp1 <- as.numeric(as.character(myBirthdates$birthdate_tp1))# parturition date, t+1


# merging parturition, mass and primiparity data  --------------------------
mismatch_df <- merge(myBirthdates, # 536 observations
                     lrs_f_mass_df2, 
                     by.x = c('year_birth','mother_id'), 
                     by.y=c('yr','ID'), 
                     all.y=TRUE) # to keep rows with reproduced ==1 even if no birthdate n=2060 update = 2119
# cleaning 
rm(mass, adjwt, all.data, lrs.f, lrs3, mjune, msep, previous_bdates, table, myBirthdates)

# select only what's needed 
colnames(mismatch_df) 

mismatch_df <- mismatch_df[, c("year_birth","mother_id","lamb_id.x","birthdate",'first_pregnant',
                               "mass_spring_f","mass_fall_f", "mass_fall_f_tm1",
                               "ewe_age","code_sr", "mom_prs", 
                               "lamb_surv_t1", "lamb_weaned","reproduced", 
                               "first_repro" , "prim",
                               "wean_mass","lamb_sex","lamb_sex_tm1", 'birthdate_tp1',"frs" ,"repro_tp1","wean_tp1")]

colnames(mismatch_df) <-  c("yr","mother_id","lamb_id","birthdate",'first_pregnant',
                            "mass_spring","mass_fall","mass_fall_tm1", # removed f
                            "ewe_age", "code_sr","mom_prs",
                            "lamb_surv_t1","lamb_weaned","reproduced",
                            "first_repro","prim",
                            "wean_mass", "lamb_sex","lamb_sex_tm1", 'birthdate_tp1',"frs","repro_tp1","wean_tp1")
# add median and timing 
mismatch_df <- mismatch_df%>% 
  group_by(yr) %>% 
  mutate(median_bdate= median(birthdate, na.rm=T), 
         timing_bdate = birthdate-median_bdate) %>% arrange(yr)

# add mass gain variable
mismatch_df$mass_spring<-as.numeric(as.character(as.factor(mismatch_df$mass_spring)))
mismatch_df$mass_fall<-as.numeric(as.character(as.factor(mismatch_df$mass_fall)))
mismatch_df$mass_gain <- mismatch_df$mass_fall-mismatch_df$mass_spring

# remove duplicate of U11 (lamb id is UNK)
mismatch_df <- mismatch_df %>% distinct(mother_id, yr, lamb_id, .keep_all = TRUE) # update - 2119 obs

# re-order
mismatch_df <- mismatch_df[, c("yr", "mother_id",'first_pregnant', "ewe_age", "code_sr", "mom_prs","first_repro", "prim", "lamb_id", "lamb_sex_tm1", "lamb_sex", "birthdate", "median_bdate", "timing_bdate", "mass_spring", "mass_fall", "mass_gain", "mass_fall_tm1","wean_mass", "lamb_surv_t1","lamb_weaned","reproduced", 'birthdate_tp1', "frs","repro_tp1","wean_tp1")] 



# create green-up and mismatch variables ------------------------------------------------

# get the pheno and merge by year - available up to 2020
pheno <- read_csv("data/raw/peaks_ram20201030.csv") 
greenup <- dplyr::select(pheno, c(year, evi_log_up_jul))
colnames(greenup) <- c("yr", "greenup_date") 

mismatch_df <- full_join(mismatch_df, greenup[,c("yr", "greenup_date")])

# calculate mismatch metric
mismatch_df$greenup_mismatch <- mismatch_df$birthdate - mismatch_df$greenup_date
# view(mismatch_df) 

# #remove 2021 because missing so much data (adjusted mass, greenup, etc.)
# mismatch_df2 <- subset(mismatch_df, yr != "2021") # n=2040

# create year lag
mismatch_df$next_yr <- mismatch_df$yr + 1

# create greenup_date_tp1 
mismatch_df2 <- merge(mismatch_df, 
                      mismatch_df[, c("mother_id", "yr", "greenup_date")], 
                      all.x=TRUE,
                      by.x= c("mother_id", "next_yr"), 
                      by.y=c("mother_id", "yr")) %>% 
  dplyr::rename(greenup_date_tp1=greenup_date.y, 
                greenup_date=greenup_date.x)

# recalculate mismatch at t+1
mismatch_df2$mismatch_tp1 <- mismatch_df2$birthdate_tp1 - mismatch_df2$greenup_date_tp1

# adjust variable class
mismatch_df2$birthdate_tp1 <- as.numeric(as.character(mismatch_df2$birthdate_tp1))# parturition date, t+1
mismatch_df2$greenup_date_tp1 <- as.numeric(as.character(mismatch_df2$greenup_date_tp1))# green-up date, t+1
mismatch_df2$mismatch_tp1 <- as.numeric(as.character(mismatch_df2$mismatch_tp1)) # mismatch, t+1


# add predation, prs, pregnant, create mismatch_df3 -------------------------------------------

#add pred variable (years from Poirier et al. 2019, but also from UnscldMismatch since none confirmed after 2017)
mismatch_df2$pred <- case_when(mismatch_df2$yr==2000 ~ 1, mismatch_df2$yr==2001 ~ 1, mismatch_df2$yr==2002 ~ 1, mismatch_df2$yr==2013 ~ 1, TRUE ~0)
# view(mismatchall)

# add clearer (to me)/more concise prs variable
mismatch_df2$prs <-
  case_when(
    mismatch_df2$prim == 'TRUE' ~ 'noLamb',
    mismatch_df2$mom_prs == '0' ~ 'noLamb',
    mismatch_df2$mom_prs == '1' ~ 'neoDeath',
    mismatch_df2$mom_prs == '2' ~ 'notWeaned',
    mismatch_df2$mom_prs == '3' ~ 'weaned',
    mismatch_df2$mom_prs == '4' ~ 'weaned',
    mismatch_df2$mom_prs == '5' ~ 'weaned',
    mismatch_df2$mom_prs == '8' ~ 'human',
    mismatch_df2$mom_prs == '9' ~ 'human'
  ) 


# create age class variable (prim=0, prime-age=1, senescent=2) 
# mismatch_df2$age_class <- case_when(mismatch_df2$prim=='TRUE'~'prim', mismatch_df2$ewe_age>7~'senescent', TRUE ~ 'adult' )

# revisions - calculating senescence based on mass, not fertility
mismatch_df2$age_class <-
  case_when(
    mismatch_df2$prim == 'TRUE' ~ 'prim',
    mismatch_df2$ewe_age > 11 ~ 'senescent',
    TRUE ~ 'adult'
  )

# sensu Berube: 
# Ewes attain >90% of their adult mass by age 4, with small increases until age 7
# Considering all ewes that survived to ≥2 yr of age, mass began declining at 11 yr

# remove duplicates again
mismatch_df3 <- mismatch_df2 %>% rename(ID=mother_id) %>% distinct(ID, yr, lamb_id, .keep_all = TRUE) #problem solved # n=431 nOW 2046 rows with all NAs kept - after revisions - 2036
# update: 2112 observations

# Print the structure of the data frame
str(mismatch_df3)
which.na <- mismatch_df3[is.na(mismatch_df3$mass_gain),]# 201 (now 237)

# remove abnormal mass gain
# mismatch_df4 <- mismatch_df3 %>% filter(mass_gain<35) %>% ungroup() # this also removes 202 rows with NA at mass gain - OK given mass gain is important

# Convert columns to appropriate data types
mismatch_df4 <- mismatch_df3 %>%
  mutate(
    across(c(ID, yr, frs, prs, code_sr, repro_tp1, lamb_sex, lamb_sex_tm1, age_class, pred, first_pregnant, lamb_id, prim, reproduced, lamb_surv_t1, lamb_weaned, wean_tp1), as.factor),
    birthdate = as.numeric(as.character(birthdate)),
    median_bdate = as.numeric(as.character(median_bdate)),
    greenup_date = as.numeric(as.character(greenup_date)),
    greenup_mismatch = as.numeric(as.character(greenup_mismatch)),
    ewe_age = as.integer(ewe_age),
    wean_mass = as.numeric(wean_mass),
    mass_fall = as.numeric(mass_fall),
    mass_spring = as.numeric(mass_spring),
    mass_fall_tm1 = as.numeric(mass_fall_tm1),
    mass_gain = as.numeric(mass_gain)
  )


# creating dataframes and storing sd and means for each set of analyses  ---------------------------
# 
# # Select only numeric variables to extract mean and SD - without NA!
# num_vars <- sapply(mismatch_df4, is.numeric)
# 
# # Calculate means and standard deviations
# means <- colMeans(mismatch_df4[, num_vars], na.rm = TRUE)
# sds <- apply(mismatch_df4[, num_vars], 2, sd, na.rm = TRUE)
# 
# # store means and standard deviations into a data frame
# summary_df <- data.frame(variable = names(means),
#                          mean = means,
#                          sd = sds)

# cheat for response variables 
mismatch_df4$birthdate_tp1 <- factor(as.character(mismatch_df4$birthdate_tp1))# parturition date, t+1
mismatch_df4$greenup_date_tp1 <- factor(as.character(mismatch_df4$greenup_date_tp1))# green-up date, t+1
mismatch_df4$mismatch_tp1 <- factor(as.character(mismatch_df4$mismatch_tp1)) # mismatch, t+1
mismatch_df4$mass_gain <- factor(as.character(mismatch_df4$mass_gain)) #


#make overall data file - unscaled
df_unscaled <- mismatch_df4 %>%
  select(ID, yr, ewe_age, age_class, prim, birthdate, median_bdate, birthdate_tp1,
         greenup_date, mass_fall_tm1, mass_fall, mass_spring, mass_gain,
         greenup_mismatch, mismatch_tp1, wean_mass, code_sr, prs, lamb_sex_tm1,
         repro_tp1, wean_tp1, frs, pred, first_pregnant, reproduced, lamb_weaned, lamb_surv_t1) %>%
  arrange(ID, yr) %>%
  as_tibble() %>% 
  filter(as.numeric(as.character(yr)) >=1999) # start at year 2000 because pheno is only available at that time # 524


# Select only numeric variables to extract mean and SD - without NA!
num_vars <- sapply(df_unscaled, is.numeric)

# Calculate means and standard deviations
means <- colMeans(df_unscaled[, num_vars], na.rm = TRUE)
sds <- apply(df_unscaled[, num_vars], 2, sd, na.rm = TRUE)

# store means and standard deviations into a data frame
summary_df <- data.frame(variable = names(means),
                         mean = means,
                         sd = sds)



# Select only numeric columns to scale
num_cols <- sapply(df_unscaled, is.numeric)
df_num <- df_unscaled[, num_cols]

# Scale each numeric column using lapply
df_scaled_unfiltered <- as.data.frame(lapply(df_num, scale))

# assign col names '_z' 
colnames(df_scaled_unfiltered) <- paste0(colnames(df_scaled_unfiltered), "_z")

# Combine scaled variables with non-numeric variables
df_scaled_unfiltered <- cbind(df_unscaled[, !num_cols], df_scaled_unfiltered)

# re-adjust variable class
df_scaled_unfiltered$birthdate_tp1 <- as.numeric(as.character(df_scaled_unfiltered$birthdate_tp1))# parturition date, t+1
df_scaled_unfiltered$mismatch_tp1 <- as.numeric(as.character(df_scaled_unfiltered$mismatch_tp1)) # mismatch, t+1
df_scaled_unfiltered$mass_gain <- as.numeric(as.character(df_scaled_unfiltered$mass_gain)) # mismatch, t+1

# re tidy
df_scaled_unfiltered <- df_scaled_unfiltered %>% 
  select(ID,yr, ewe_age_z,age_class,prim,mass_gain,repro_tp1, frs, wean_tp1,birthdate_tp1, mismatch_tp1, 
         pred, first_pregnant, birthdate_z, median_bdate_z,greenup_date_z,
         mass_fall_tm1_z,mass_fall_z, mass_spring_z,
         greenup_mismatch_z, code_sr, wean_mass_z, 
         prs, lamb_sex_tm1,
         reproduced, lamb_weaned, lamb_surv_t1) %>% droplevels() %>% 
  arrange(ID,yr)

# clean up 
rm(allewes, df_num, greenup, lrs_f_mass_df, mismatch_df, mismatch_df2, mismatch_df3, pheno, t.gu,t.sex, cols,file,i,l_sex,n,num_cols,num_vars,t, which.na, t.lamb, duplicates, df_scaled) # keep df4 in case missing vars

# this is the overall dataset, scaled, full
nlevels(df_scaled_unfiltered$ID) # 117 (or 124 including 1999) now 141 after revisions but that still includes lots of na
df_scaled_unfiltered$ID <- droplevels(df_scaled_unfiltered$ID)



# create mass gain df  ---------------------------------------------
# subset of females that reproduced (formely mismatchrepro), without outlier
# mass_gain_df_unf <- df_scaled[df_scaled$prs %in% c("weaned", "notWeaned"), ] %>% droplevels()
# mass_gain_df_unf <- mass_gain_df_unf[mass_gain_df_unf$lamb_sex_tm1 %in% c("M", "F"),] %>% droplevels()

# create subsets of females that reproduced (formely mismatchrepro), full df, scaled
mass_gain_df_full <- df_scaled_unfiltered %>%
  filter(df_scaled_unfiltered$reproduced==1) %>% droplevels() # 354

# create subsets of females that reproduced (formely mismatchrepro), without outlier, scaled, keep NA for now
# keep both non-NA and NA values in the greenup_mismatch column while keeping the other filtering and ungrouping steps intact.
mass_gain_df_filtered <- mass_gain_df_full %>%
  filter(greenup_mismatch_z < 3.3 | is.na(greenup_mismatch_z)) %>% # or scaled is ~3.3
  ungroup() %>%
  droplevels() # n=350

mass_gain_df_filtered$ID <- droplevels(mass_gain_df_filtered$ID)
mass_gain_df_full$ID <- droplevels(mass_gain_df_full$ID)
nlevels(mass_gain_df_full$ID) #87 (or 94 including 1999)
nlevels(mass_gain_df_filtered$ID) #87

# subset, no outliers, unscaled (main figure4) USELESS?
# mass_gain_df_unsc <- mismatch_unsc_df_filtered_noNA[mismatch_unsc_df_filtered_noNA$reproduced==1, ] %>% droplevels()
# # subset, keeping outliers, unscaled (inset figure4)
# mass_gain_df_unsc_unf <- mismatch_unsc_df[mismatch_unsc_df$reproduced==1, ] %>% droplevels()

# create repro_tp1 df -----------------------------------------------------
# create the subsets for the fitness consequences - females that reproduced (formely mismatchrepro)
# tmp.repro_df <- df_scaled_filtered[df_scaled_filtered$prs %in% c("weaned", "notWeaned"), ] %>% droplevels()
# tmp.repro_df <- tmp.repro_df[tmp.repro_df$lamb_sex_tm1 %in% c("M", "F"),] %>% droplevels()# n=148


# for this df we need females that reproduced at time t AND that survived the following year t+1 (no matter if they reproduced or not at time t+1)
# thus needs consecutive years for females 

# Convert yr column to numeric
df_scaled_unfiltered$yr <- as.numeric(as.character(df_scaled_unfiltered$yr))

# check females that don't have 2 consecutive years
filtered_data <- df_scaled_unfiltered %>%
  arrange(ID, yr) %>%
  group_by(ID) %>%
  mutate(prev_year = lag(yr), next_year = lead(yr)) %>%
  filter((yr - prev_year != 1 & !is.na(prev_year)) | (next_year - yr != 1 & !is.na(next_year))) %>%
  ungroup() %>%
  select(-prev_year, -next_year)

# these females have gaps in their year and it's not because they were outliers (removed) -they simply were not seen
# ID       yr ewe_age_z age_class prim  birth…¹ media…² green…³ mass_…⁴ mass_…⁵
# <fct> <dbl>     <dbl> <fct>     <fct>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
#   1 I6     2007  -0.597   adult     FALSE  -0.633  -1.04   0.499    0.170  0.0117
# 2 I6     2009  -0.00955 adult     FALSE  -0.633  -1.64  -0.0300   1.32   1.17  
# 3 U11    2017  -0.891   prim      TRUE   -0.292  -0.969 -0.471   -1.29  -0.859 
# 4 U11    2017  -0.891   prim      TRUE   -0.292  -0.969 -0.471   -1.29  -0.859 

repro_df_full <- df_scaled_unfiltered %>%
  group_by(ID) %>%
  mutate(survived_next_year = lead(ID) == ID & lead(yr) == yr + 1| lead(yr) == yr + 2) %>% # handles gap year. 
  filter(survived_next_year | !duplicated(ID)) %>%
  filter(reproduced == 1) %>%
  ungroup() %>%
  select(-survived_next_year) %>%
  droplevels() # n=282
# the last occurrence seen is thus deleted given we don't know if the female survived 

# Remove outliers
repro_df_filtered <- repro_df_full %>%
  filter(greenup_mismatch_z < 3.3 | is.na(greenup_mismatch_z)) %>% # or scaled is ~3.3
  ungroup() %>%
  droplevels() # n=279

nlevels(repro_df_full$ID) #76 (or 82 including 1999)
nlevels(repro_df_filtered$ID) #76

# create subset for analyses of frs, parturition tp1, weaning mass, mismatch tp1 df -------------------
# this subset must be frs ≠ noLamb # females that reproduced in 2 two consecutive years (and survived the next? )
test_df_repro <- repro_df_full %>%
  group_by(ID) %>%
  left_join(transmute(repro_df_full,ID=ID,yr=yr-1,rtp1=reproduced)) %>% # if yr is yr +1 then you have info on frs at yr+2
  group_by(ID) %>%
  filter(!frs%in% c('noLamb','human'),
         reproduced==1, 
         rtp1==1) %>% arrange(ID) %>% #  28T still there, only 1 ro
  select(-prs, -lamb_sex_tm1) %>% droplevels() %>% 
  arrange(ID,yr) # 213

# Keep those that reproduced in two consecutive years 
# consecutive does not mean 2 rows = rather means that there information on the next year both in survival and reproduction in the data frame to fill FRS. It is written on the last of the two occurrences. 
consec_df_full <- test_df_repro # n=213
consec_df_filtered <- consec_df_full  %>% 
  filter(greenup_mismatch_z < 3.3 | is.na(greenup_mismatch_z)) %>% # or scaled is ~3.3
  ungroup() %>%
  droplevels() # n=211

nlevels(consec_df_full$ID) #58 (including 1999)
nlevels(consec_df_filtered$ID) #57

# Remove late parturition date - outliers ---------------------------------
# filtering done on birthdate ≥200 which are extremely late in absolute value. 

# exclude outliers for ONCE in all analyses 
df_unscaled_filtered <- df_unscaled %>% filter(birthdate<200 | is.na(birthdate))
df_scaled_filtered <- df_scaled_unfiltered %>% filter(birthdate_z<3.1| is.na(birthdate_z)) # scaled value of bd
mass_gain_df_filtered <- mass_gain_df_full %>% filter(birthdate_z<3.1| is.na(birthdate_z))
repro_df_filtered <- repro_df_full %>% filter(birthdate_z<3.1| is.na(birthdate_z)) 
consec_df_filtered <- consec_df_full %>% 
  filter(birthdate_z<3.1| is.na(birthdate_z)) # NA in response variable deleted before analyses


# revisions for Ecosphere -------------------------------------------------

# Function to filter dataset by year
filter_start_year <- function(df) {
  filtered_df <- df %>%
    mutate(yr = as.numeric(as.character(yr))) %>%
    filter(yr >= 2000 & yr<2021)
  
  return(filtered_df)
}

# Filtering each dataset
df_unscaled_filtered <- filter_start_year(df_unscaled_filtered) # outliers ?
df_scaled_filtered <- filter_start_year(df_scaled_filtered)

mass_gain_df_filtered <- filter_start_year(mass_gain_df_filtered)
repro_df_filtered <- filter_start_year(repro_df_filtered)
consec_df_filtered <- filter_start_year(consec_df_filtered)



## clean up -----------------------------------------------------------

# Replace "my_df" with the name of your data frame
keep_obj <- c("mismatchdf4", "df_unscaled", "df_unscaled_filtered" ,"df_scaled_filtered","df_scaled_unfiltered","summary_df", "mass_gain_df_filtered", "repro_df_filtered", "consec_df_filtered")

# Get the names of all objects in the current environment
all_objs <- ls()

# Remove all objects except for "keep_obj"
rm(list = setdiff(all_objs, keep_obj))





# save both scaled and unscaled rdata, filtered and unfiltered for outliers
# save(list=ls(), file="data/mine/mismatch_df_2025-05-08.RData") 
# previous mismatchall files moved to 'old' every time something significant changes, or renamed with date & left in 'mine'




# final tidy up for publication -------------------------------------------------
load('data/mine/mismatch_df_2025-05-08.RData')

# change IDs for publication of database - to do at the very last 
df_unscaled_filtered$ID <- as.numeric(as.factor(df_unscaled_filtered$ID)) #worked!

df_unscaled_filtered <- df_unscaled_filtered %>% 
  select(-c(prs, lamb_sex_tm1,reproduced,lamb_weaned,lamb_surv_t1)) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 3)))

# write.table(df_unscaled_filtered,file="cache/ECS25-0363_df_unscaled_filtered.csv",sep=",",dec = ".", row.names = F)

# change IDs for publication of database - to do at the very last 
mass_gain_df_filtered$ID <- as.numeric(as.factor(mass_gain_df_filtered$ID)) #worked!

mass_gain_df_filtered <- mass_gain_df_filtered %>% 
  select(-c(prs, lamb_sex_tm1,reproduced,lamb_weaned,lamb_surv_t1)) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 3)))

# write.table(mass_gain_df_filtered,file="cache/ECS25-0363_mass_gain_df_filtered.csv",sep=",",dec = ".", row.names = F)


consec_df_filtered$ID <- as.numeric(as.factor(consec_df_filtered$ID)) #worked!

consec_df_filtered <- consec_df_filtered %>% 
  select(-c(reproduced,lamb_weaned,lamb_surv_t1)) %>% 
  mutate(across(where(is.numeric), ~ round(.x, 3)))

# write.table(consec_df_filtered,file="cache/ECS25-0363_consec_df_filtered.csv",sep=",",dec = ".", row.names = F)
