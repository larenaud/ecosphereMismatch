# this script is used for... to complete 

# load the required packages
pkgs <- c("mgcv", "lme4", "vroom", "tidyverse", "forcats", "tidyr", 'AICcmodavg', 'bbmle', "gamm4")
## install.packages(pkgs, Ncpus = 4)
vapply(pkgs, library, logical(1), character.only = TRUE, logical.return = TRUE,
       quietly = TRUE)


## install.packages("remotes")
## remotes::install_github('gavinsimpson/gratia')
library('gratia')

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

# DOUBLE CHECK STATUS 8 AND 09. WHAT THEY MEAN. DO WE EXCLUDE THEM. 


# s2 - Sex of lamb produced at age 2
# 0 - unknown
# 1 - male
# 2 - female




# Load the data as an R object --------------------------------------------
# load("data/mine/mismatch_df_2025-05-08.RData")

df_unscaled_filtered <- read.csv("data/ECS25-0363_breedingDat.csv")
mass_gain_df_filtered <-  read.csv("data/ECS25-0363_massGainDat.csv")
consec_df_filtered <- read.csv("data/ECS25-0363_breedingDat.csv")

# Different datasets are: 
# df_unscaled # entire dataframe, not scaled, not filtered for any outliers, to get range of different variables and overall sample sizes. 
# df_scaled_unfiltered # entire dataframe, numerical variables scaled, not filtered for any outliers
# mass_gain_df_full # data frame for analyses of mass gain. Includes females that reproduced at time t.Numerical variables scaled. NA still present in explanatory variables. No outliers removed.
# # mass_gain_df_filtered # same as mass_gain_df_full. Outliers ARE removed.
# repro_df_full # data frame for analyses of reproduction at time t+1. Includes females that reproduced AND that survived the following year (thus excludes last time seen since survival to next year is unknown). Numerical variables scaled. NA still present in explanatory variables. No outliers removed.
# # repro_df_filtered # Same as repro_df_full. Outliers ARE removed.
# consec_df_full # dataframe for analyses of consequences at time t+1. Females that produced a lamb in 2 consecutive years are selected. No outliers removed. 
# # consec_df_filtered # same as consec_df_full. Outliers are removed. 

# Function to extract information
extract_info <- function(df) {
  num_observations <- nrow(df)
  num_unique_id <- length(unique(df$ID))
  
  return(list(num_observations = num_observations, num_unique_id = num_unique_id))
}


# Applying the function to each dataset
extract_info(df_unscaled) # 524, 117 : revised: 581, 129
extract_info(df_scaled_unfiltered)# 524, 117
extract_info(df_scaled_filtered)# 520, 117: revised 534, 119

extract_info(mass_gain_df_filtered)# 350, 87, revised 359, 89
extract_info(repro_df_filtered) #273,71, rev: 281, 72
extract_info(consec_df_filtered) # 193,47, revised: 200, 47

# Get sample sizes and descriptive statistics for Table S1 WITHOUT OUTLIER --------------------------------------------------------

df_unscaled_filtered$ID <- droplevels(df_unscaled_filtered$ID )
df_unscaled_filtered$lamb_weaned <- droplevels(df_unscaled_filtered$lamb_weaned )


# MAKE SURE THE OUTLIERS ARE REMOVED 

birth_summary <- df_unscaled_filtered %>%
  mutate(yr = as.numeric(as.character(yr))) %>%
  # filter(yr < 2020) %>% # to fit ms
  group_by(ID) %>%
  summarise(
    num_birthdates = n_distinct(birthdate)
  ) %>% 
  summarize(
    total_females=n_distinct(ID),
    total_birthdates=sum(num_birthdates, na.rm=T),
    average_birthdates=mean(num_birthdates),
    .groups='drop' # removes grouping by female ID and return the summary
  )
 

# that excludes 4 late birthdates UP TO 2020
# total_females total_birthdates average_birthdates
# <int>            <int>              <dbl>
#   1           117              358               3.06
# 119              363               3.05
wean_summary <- df_unscaled_filtered %>%
  filter(lamb_weaned==1) %>% 
  summarise(
    missing_weaning_mass = sum(is.na(wean_mass)), # 34
   n_weaned=n()) # 245
  
# missing_weaning_mass n_weaned
# <int>    <int>
#   1                   33      241
# 54      245
wean_table <- df_unscaled_filtered %>%
  filter(!is.na(birthdate)) %>% 
  mutate(missing_wean_mass = is.na(wean_mass)) %>%
  select(ID, yr,lamb_weaned, code_sr,missing_wean_mass) %>% 
  filter(missing_wean_mass==T) %>% 
  distinct() # there were duplicates! 

table(wean_table$code_sr) # weird code_sr == 9. Biologically irrelevant
# 0  1  2  3  4  5  8  9 
# 0  0 26  9  0 15  0  1 

# new 
# 0  1  2  3  4  5  8  9 
# 0  4 30 17  0 24  0  1 

# Get the value of greenup_date, earliest birthdate, latest birthdate, and median parturition date

# Convert Julian day to dates and handle leap years
tmp.tableS1 <- df_unscaled_filtered %>%
  mutate(
    greenup_date = ymd(paste0(yr, "-01-01")) + days(greenup_date - 1),
    birthdate = ymd(paste0(yr, "-01-01")) + days(birthdate - 1)
  ) %>%
  group_by(yr) %>%
  summarise(
    greenup_date = min(greenup_date, na.rm=T),
    earliest_birthdate = min(birthdate, na.rm=T),
    latest_birthdate = max(birthdate, na.rm=T),
    median_parturition_date = median(birthdate, na.rm=T),
    greenup_mis_range = paste(range(greenup_mismatch, na.rm=T), collapse = ", ")
  ) %>%
  mutate(
    greenup_date = format(greenup_date, "%b-%d"),
    earliest_birthdate = format(earliest_birthdate, "%b-%d"),
    latest_birthdate = format(latest_birthdate, "%b-%d"),
    median_parturition_date = format(median_parturition_date, "%b-%d")
  ) %>%
  select(
    yr,
    greenup_date,
    earliest_birthdate,
    latest_birthdate,
    median_parturition_date,
    greenup_mis_range
  )

tmp.tableS1


# Calculate the number of unique lamb_id with negative, null, and positive greenup_mismatch values

# here a negative mismatch means born early as mismatch metric is calculated as follows:  mismatch_df$greenup_mismatch <- mismatch_df$birthdate - mismatch_df$greenup_date

num_negative <- df_unscaled_filtered %>% group_by(yr) %>%
  filter(greenup_mismatch < 0) %>%
  summarise(born_early = n_distinct(ID))
#   yr born_early
# <dbl>      <int>
# 1  2000          9
# 2  2001          6
# 3  2002          4
# 4  2007         13
# 5  2008         12
# 6  2009         11
# 7  2010         17
# 8  2011         11
# 9  2012          4
# 10  2013          2
# 11  2017          2
num_null <- df_unscaled_filtered %>% group_by(yr) %>%
  filter(greenup_mismatch == 0) %>%
  summarise(born_matched = n_distinct(ID))


# yr born_matched
# <dbl>        <int>
#   1  2007            1
# 2  2010            1
# 3  2012            2
# 4  2014            1
# 5  2017            1

num_positive <- df_unscaled_filtered %>% group_by(yr) %>%
  filter(greenup_mismatch > 0) %>%
  summarise(born_late = n_distinct(ID)) %>% print(n=25)

# A tibble: 21 × 2
# yr born_late
# <dbl>     <int>
# 1  2000         6
# 2  2001         9
# 3  2002         3
# 4  2003         9
# 5  2004        10
# 6  2005        10
# 7  2006        10
# 8  2007         2
# 9  2008         1
# 10  2009         2
# 11  2010         1
# 12  2011         3
# 13  2012        10
# 14  2013        10
# 15  2014         3
# 16  2015        15
# 17  2016        16
# 18  2017        21
# 19  2018        23
# 20  2019         7
# 21  2020         2


# Merge the data frames by year
merged_data <- merge(num_negative, num_null, by = "yr", all = TRUE)
merged_data <-
  merge(merged_data, num_positive, by = "yr", all = TRUE)

# Fill missing values with 0
merged_data[is.na(merged_data)] <- 0



# get trends in late females over yr
ggplot(merged_data, aes(x=yr, y=born_late))+geom_point() +geom_line() + cowplot::theme_cowplot() +
  theme(
    panel.grid.major.y = element_line(size = 0.4, color = "grey80", linetype = "dashed"),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 13),
    legend.position = 'top',
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 11),
    strip.text = element_text(size = 12),
    # plot.margin = margin(1, 1, 1, 1, "cm"),
    plot.background = element_rect(fill = 'white', color = NA)
  ) +
  ylab('Number of late births')+xlab('Year')
ggplot(merged_data, aes(x=yr, y=born_early))+geom_point() +geom_line()
ggplot(merged_data, aes(x=yr, y=born_matched))+geom_point() +geom_line()

ggsave('output/graph/final/exLateBirths.png',dpi=300)

# Merge with the mis_ranges table
tmp.tableS1 <-
  merge(tmp.tableS1, merged_data, by = "yr", all.x = TRUE)
tmp.tableS1 <- tmp.tableS1 %>%
  mutate(timing = paste0(born_early, ', ', born_matched, ', ', born_late)) %>% select(-born_early,-born_matched,-born_late)

# slight differences with actual table S1 of the manuscript but hard to tell where they come from given no code provided to reproduce it (I created this chunk just NOW)

# export table S1 for Crozier et al., in csv#
# write.csv2(tmp.tableS1, file='output/data/revisions_2024-05-24_tableS1_sample_size.csv')


# relax definition of 'matched'


num_negative <- df_unscaled_filtered %>% group_by(yr) %>%
  filter(greenup_mismatch < -2) %>%
  summarise(born_early = n_distinct(ID))
# yr born_early
# <dbl>      <int>
#   1  2000          9
# 2  2001          6
# 3  2002          4
# 4  2007         13
# 5  2008         13
# 6  2009         10
# 7  2010         13
# 8  2011          8
# 9  2012          2
# 10  2013          1

num_null <- df_unscaled_filtered %>% group_by(yr) %>%
  filter(greenup_mismatch >=-2&greenup_mismatch<=2) %>%
  summarise(born_matched = n_distinct(ID))

# yr born_matched
# <dbl>        <int>
#   1  2001            2
# 2  2005            1
# 3  2007            1
# 4  2009            2
# 5  2010            5
# 6  2011            3
# 7  2012            5
# 8  2013            1
# 9  2014            1
# 10  2017            9
# 11  2019            1

num_positive <- df_unscaled_filtered %>% group_by(yr) %>%
  filter(greenup_mismatch > 2) %>%
  summarise(born_late = n_distinct(ID)) %>% print(n=25)
# yr born_late
# <dbl>     <int>
#   1  2000         6
# 2  2001        10
# 3  2002         3
# 4  2003         9
# 5  2004        10
# 6  2005         9
# 7  2006        10
# 8  2007         2
# 9  2008         1
# 10  2009         2

# Merge the data frames by year
merged_data <- merge(num_negative, num_null, by = "yr", all = TRUE)
merged_data <-
  merge(merged_data, num_positive, by = "yr", all = TRUE)

# Fill missing values with 0
merged_data[is.na(merged_data)] <- 0










# clean up a bit
rm(tmp.tableS1, num_negative, num_null, num_positive, merged_data, wean_table, info_df_unscaled, birth_summary, wean_summary, summary_df)




# Figure S1 - parturition date over time ----------------------------------

# replaced df_Unscaled by df_unscaled_filtered (for outliers)

# Convert yr column to numeric
df_unscaled_filtered$yr <- as.numeric(as.character(df_unscaled_filtered$yr))

# make sure ID levels are right
df_unscaled_filtered$ID <- droplevels(df_unscaled_filtered$ID) # 117 individual females 

# arrange mass gain (now a factor?)
df_unscaled_filtered$mass_gain <- as.numeric(as.character(df_unscaled_filtered$mass_gain))
df_unscaled_filtered$mismatch_tp1 <- as.numeric(as.character(df_unscaled_filtered$mismatch_tp1))

# Subset data to first observation for each year
annual_df <- df_unscaled_filtered %>%
  filter(!is.na(yr), !is.na(greenup_date), !is.na(median_bdate)) %>%
  group_by(yr) %>%
  #  filter(yr < 2020) %>%
  dplyr::slice(1) %>%
  select(yr, greenup_date, median_bdate)

# gams instead of a linear model both for median and individual bdate
# median_bdate_change <- lm(median_bdate ~ yr, data = annual_df)
m1_bd_gam <- gam(median_bdate~s(yr), data=unique(annual_df[, c("yr", "median_bdate")]))  
m2_bd_gam <- gam(median_bdate~s(yr),gamma = 1.4, data=unique(annual_df[, c("yr", "median_bdate")]))  
m3_bd_gam <- gam(median_bdate~s(yr, k=9),gamma = 1.4, data=unique(annual_df[, c("yr", "median_bdate")])) 
m4_bd_gam <- gam(median_bdate~s(yr, k=8),gamma = 1.4, data=unique(annual_df[, c("yr", "median_bdate")]))
m5_bd_gam <- gam(median_bdate~s(yr, k=7),gamma = 1.4, data=unique(annual_df[, c("yr", "median_bdate")]))
m6_bd_gam <- gam(median_bdate~s(yr, k=6),gamma = 1.4, data=unique(annual_df[, c("yr", "median_bdate")]))


gam.check(m1_bd_gam) # p = 0.98 k = 9, edf = 6.8 Smoothing parameter selection converged after 10 iterations.
gam.check(m2_bd_gam) # p = 0.85 k = 9, edf = 6 Smoothing parameter selection converged after 8 iterations.
gam.check(m3_bd_gam) # p = 0.93 k = 6, edf = 5.63 - Smoothing parameter selection converged after 13 iterations.
gam.check(m4_bd_gam) 
gam.check(m5_bd_gam) 
gam.check(m6_bd_gam) 

delta_aic_t <- AIC(m1_bd_gam, m2_bd_gam, m3_bd_gam, m4_bd_gam, m5_bd_gam, m6_bd_gam) %>% 
  arrange(AIC) %>%
  mutate(delta_AIC = AIC - first(AIC)) %>% # calulate delta
  arrange(delta_AIC) %>% # arrange by dAIC
  round(2)
# Print the result
print(delta_aic_t)

# write.csv(as.data.frame(delta_aic_t), file='output/data/revisions/TS3_aic_median_birthdate_yr.csv')

#             df    AIC delta_AIC
# m5_bd_gam 7.63 119.59      0.00
# m4_bd_gam 7.92 120.97      1.38
# m1_bd_gam 8.82 121.04      1.45
# m3_bd_gam 7.94 122.54      2.94
# m2_bd_gam 8.01 122.82      3.22
# m6_bd_gam 6.76 127.29      7.70

summary(m1_bd_gam) # a bit of shrinkage here - looks good p=0.94
summary(m2_bd_gam) # same as m1
summary(m3_bd_gam) # not much shrinkage but residuals not randomly distributed (small p value)
summary(m5_bd_gam) # not much shrinkage  edf Ref.df    F  p-value    
# s(yr) 5.629  5.943 12.8 4.83e-05 ***

gam.check(m5_bd_gam) # p=0.92, k=6, edf= 5.63


# Temporal trends greenup date and mismatch--------------------------------------------

# green-up 
mod.gu.l <- lm(greenup_date~yr,data=unique(annual_df[, c("yr", "greenup_date")]) )
# mod.gu.gam <- gam(greenup_date~s(yr),gamma = 1.4, data=unique(annual_df[, c("yr", "greenup_date")]))  
# mod.gu.gam2 <- gam(greenup_date~s(yr),data=unique(annual_df[, c("yr", "greenup_date")]))

round(summary(mod.gu.l)$coeff,2)
#             Estimate Std. Error t value Pr(>|t|)
# (Intercept)  2394.12     602.01    3.98        0
# yr             -1.12       0.30   -3.73        0
round(confint(mod.gu.l), 2)
#                 2.5 %  97.5 %
#   (Intercept) 1134.09 3654.15
# yr            -1.75   -0.49




# mismatch
# there shouldn't be any NA in the response variable 
df_gu <- df_unscaled_filtered %>% 
  select(ID, yr, greenup_mismatch) %>% # this is now 275 rows
  filter(!is.na(greenup_mismatch)) %>% 
  distinct()

# filtering removes 4 emales with mismatch of 120, 119, 76, 71 - way too late
df_gu$ID <- droplevels(df_gu$ID) # 83 levels

df_gu %>%
  na.omit() %>%
  count(ID) %>%
  count(n, name = "n_female") # only 1 female has 8 measurements. 12 have 1, 6 have 2
# using the mcgv::gamm to ease AIC comparison
# n n_female
# 1     1       24
# 2     2       24
# 3     3        8
# 4     4        5
# 5     5        4
# 6     6        4
# 7     7        7
# 8     8        2
# 9     9        3
# 10    11        2

# fitting gams with different functions and random structures 
m1_gam <- gam(greenup_mismatch ~ s(yr)  + # i need the spline here 
                s(ID, bs = 're') +
                s(ID, yr, bs = 're'), # equivalent to a random slope 
              data = df_gu, method = 'REML')
# drops yr specific effect of ID 
m2_gam <- gam(greenup_mismatch ~ s(yr)  + # i need the spline here  - 10 is default K
                s(ID, bs = 're'),
              data = df_gu, method = 'REML')# try less complex functions
m3_gam <- gam(greenup_mismatch ~ s(yr, k=9)  + 
                s(ID, bs = 're'),
              data = df_gu, method = 'REML', 
              re.test = FALSE)
m4_gam <- gam(greenup_mismatch ~ s(yr, k=8)  +
                s(ID, bs = 're'),
              data = df_gu, method = 'REML', 
              re.test = FALSE)
m5_gam <- gam(greenup_mismatch ~ s(yr, k=7)  + 
                s(ID, bs = 're'),
              data = df_gu, method = 'REML', 
              re.test = FALSE)
m6_gam <- gam(greenup_mismatch ~ s(yr, k=6)  +
                s(ID, bs = 're'),
              data = df_gu, method = 'REML', 
              re.test = FALSE)
m7_gam <- gam(greenup_mismatch ~ s(yr, k=5)  + 
                s(ID, bs = 're'),
              data = df_gu, method = 'REML', 
              re.test = FALSE)
m8_gam <- gam(greenup_mismatch ~ s(yr, k=4)  + 
                s(ID, bs = 're'),
              data = df_gu, method = 'REML', 
              re.test = FALSE)

# did it converge? yes, all of them, with varying # of iterations.
gam.check(m1_gam)
gam.check(m2_gam)
gam.check(m3_gam)
gam.check(m4_gam)
gam.check(m5_gam)
gam.check(m6_gam)
gam.check(m7_gam)
gam.check(m8_gam) # takes 11 it

# model comparisons 
aict <- AIC(m1_gam, m2_gam, m3_gam, m4_gam, m5_gam, m6_gam, m7_gam, m8_gam) %>% arrange(AIC)


# Calculate delta AIC
aict <- aict %>%
  mutate(delta_AIC = AIC - first(AIC))

# Print the result
round(print(aict))


# write.csv(as.data.frame(aict), file='output/data/revisions/TS2_aic_mismatch_yr.csv')



# AIC clearly favours the simpler model as the fits of the two models are essentially the same.
# while AIC are similar, we discriminate on the number of functions and parameters in the model. 
# m5 seems the most parcimonious in these terms.



# model checks 
draw(m2_gam, parametric = FALSE) # not perfect but RE seems to follow normal distn

summary(m2_gam)
# Formula:
#   greenup_mismatch ~ s(yr) + s(ID, bs = "re")
# 
# Parametric coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)    7.292      0.719   10.14   <2e-16 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Approximate significance of smooth terms:
#   edf Ref.df     F p-value    
# s(yr)  6.978574  8.059 20.92  <2e-16 ***
#   s(ID) 0.006102 83.000  0.00   0.498    
# ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

# save(list=ls(), file='cache/figData_Trends.RData')


# quick vis
# new_data <- tidyr::expand(df_gu,ID,
#                           yr = unique(yr))
# 
# m5_pred <- bind_cols(new_data,
#                      as.data.frame(predict(m5_gam, newdata = new_data,
#                                            se.fit = TRUE)))
# 
# ggplot(m5_pred, aes(x = yr, y = fit, group = ID,
#                     colour = ID)) +
#   geom_line(alpha=0.5)
# ggplot(m5_pred, aes(x = yr, y = fit, group = ID,
#                     colour = ID)) +
#   geom_line() +
#   geom_point(data = df_unscaled_filtered, aes(y = greenup_mismatch)) +
#   facet_wrap(~ ID) 

# https://campus.datacamp.com/courses/nonlinear-modeling-with-generalized-additive-models-gams-in-r/interpreting-and-visualizing-gams?ex=8#:~:text=This%20shows%20a%20statistical%20test,statistic%2C%20and%20p%2Dvalue.

# Each line reports the test results for one smooth. It shows the k value or number of basis functions, the effective degrees of freedom, a test statistic, and p-value. Here, small p-values indicate that residuals are not randomly distributed. 

# all seem to suggest that residuals not randomly distribued - but isn't it somewhat normal given the repeated values per year?

# assess changes over time linearly after 2010 
df_subset <- df_gu %>% filter(yr>=2008)
m1_lmer <- lmer(greenup_mismatch~as.numeric(yr) + (1|ID), data = df_subset)




# I get this warning suggesting that lmer is no good
# boundary (singular) fit: see help('isSingular')

summary(m1_lmer)$coeff
#                 Estimate  Std. Error   t value
# (Intercept) -5803.364098 525.5816237 -11.04179
# yr              2.886086   0.2609955  11.05799

confint(m1_lmer) # bad spline fit. 
# 2.5 %       97.5 %
#   .sig01          0.000000     4.393075
# .sigma         11.015242    13.574016
# (Intercept) -6833.235109 -4773.493087
# yr              2.374668     3.397503

VarCorr(m1_lmer)
# Groups   Name        Std.Dev.
# ID       (Intercept)  0.000  
# Residual             12.262  




# Test for collinearity among potential covariates ------------------------
df_unscaled_filtered_noNA <- na.omit(df_unscaled_filtered) # ça enlève toutes les lignes avec des NA
hist(unlist(df_unscaled_filtered_noNA))# 
#pairs(df_unscaled[, c(3:27)], main="Bivariate Plots of the phenology Data" ) # detect colinearity ; peut pas mettre yr et gdd

library(ggcorrplot)

# Select only numeric columns
cor_df <- dplyr::select_if(df_unscaled_filtered_noNA, is.numeric)

# Compute the correlation matrix
mcor <- round(cor(cor_df, method='pearson'), 2)

# Use ggcorrplot to visualize the correlation matrix
ggcorrplot(mcor, 
           method = "square", 
           type = "upper", 
           outline.color = "darkgray",
           colors = c("blue", "white", "orangered"), 
           lab = TRUE, 
           lab_size = 3,
           tl.cex = 12, 
          # title = "Correlation Matrix",
           ggtheme = ggplot2::theme_minimal())

# ggsave('output/graph/revisions/revisions_corrplot_numeric.png', width=10,height = 8, dpi=300)
# ggsave('output/graph/revisions/revisions_corrplot_numeric.pdf', width=10,height = 8, dpi=300)


mcor_df <- as.data.frame(as.table(mcor))
colnames(mcor_df) <- c("Var1", "Var2", "Correlation")

# filter for high correlations
high_corr_pairs <- mcor_df %>%
  filter(abs(Correlation) > 0.6 & Var1 != Var2) %>% 
  rowwise() %>% # ensure unique pairs
  mutate(pair = paste(sort(c(Var1, Var2)), collapse = "-")) %>%
  distinct(pair, .keep_all = TRUE) %>%
  select(Var1, Var2, Correlation)

print(high_corr_pairs)
# Var1             Var2          Correlation
# <fct>            <fct>               <dbl>
# 1 wean_mass        birthdate           -0.62
# 2 greenup_mismatch greenup_date        -0.68
# 3 mass_fall        mass_fall_tm1        0.84
# 4 mass_spring      mass_fall_tm1        0.78
# 5 mass_spring      mass_fall            0.64




# Testing the effect of mismatch on mass gain without the outlier ----------------------------

# Effect of mismatch on female mass gain - all observations were tested but decided to remove for revisiosn
mass_gain_df_filtered <- mass_gain_df_filtered %>%
  select(ID, yr, wean_mass_z, mass_spring_z, greenup_mismatch_z, pred, age_class, first_pregnant, mass_gain,birthdate_z) %>%
  filter(if_all(everything(), ~ !is.na(.))) %>%
  droplevels()

# verification
# remove IDs from R memory:)
nlevels(mass_gain_df_filtered$ID) # 72

mass_gain_df_filtered$yr <- as.factor(mass_gain_df_filtered$yr) # 72
mass_gain_df_filtered$yr <- as.factor(mass_gain_df_filtered$yr) # 72

nrow(mass_gain_df_filtered)# 201
# end verification

# simple visual look at scatterplot suggest either poly or linear fit. Mixed. 
# testing best shape - do it visually 


# # cleaning up 
# # Replace "my_df" with the name of your data frame
# keep_obj <- c("df_scaled", "df_scaled_filtered", "mass_gain_df_filtered","mass_gain_df_full", "repro_df_full","repro_df_filtered", "summary_df","consec_df_filtered", "consec_df_full")
# 
# # Get the names of all objects in the current environment
# all_objs <- ls()
# 
# # Remove all objects except for "keep_obj"
# rm(list = setdiff(all_objs, keep_obj))


# likelihood ratio test for models of mass gain on filtered data

# Create a dummy factor with a single level
mass_gain_df_filtered$dummy <- factor(1)

# Fit the mixed effects model
m0r <- lmer(mass_gain ~ greenup_mismatch_z + (1 | dummy), 
            data = mass_gain_df_filtered, 
            na.action = na.exclude, 
            REML = TRUE, 
            control = lmerControl(
              optimizer = 'Nelder_Mead', 
              check.nlev.gtr.1 = "ignore")) # singular fit problem. I get it's normal. 

# Print the model summary
summary(m0r)

m1r<-lmer(mass_gain~greenup_mismatch_z + (1|ID), data = mass_gain_df_filtered, na.action=na.exclude, REML = T)
m2r<-lmer(mass_gain~greenup_mismatch_z + (1|yr), data = mass_gain_df_filtered, na.action=na.exclude, REML = T)
m3r<-lmer(mass_gain~greenup_mismatch_z + (1|ID)  + (1|yr), data = mass_gain_df_filtered, na.action=na.exclude, REML = T)

anova(m0r, m1r) #p of 0
anova(m0r, m2r) #p of 0
anova(m0r, m3r) #3 
anova(m1r, m3r) #3 
anova(m2r, m3r) # 0.1015 # adding yr and ID but not really significant

# Select only numeric columns
cor_df <- dplyr::select_if(mass_gain_df_filtered, is.numeric)

# Compute the correlation matrix
mcor <- round(cor(cor_df, method='pearson'), 2)
mcor_df <- as.data.frame(as.table(mcor))
colnames(mcor_df) <- c("Var1", "Var2", "Correlation")

# filter for high correlations
high_corr_pairs <- mcor_df %>%
  filter(abs(Correlation) > 0.5 & Var1 != Var2) %>% 
  rowwise() %>% # ensure unique pairs
  mutate(pair = paste(sort(c(Var1, Var2)), collapse = "-")) %>%
  distinct(pair, .keep_all = TRUE) %>%
  select(Var1, Var2, Correlation)

print(high_corr_pairs) 
# 1 birthdate_z wean_mass_z              -0.63
# 2 birthdate_z greenup_mismatch_z        0.68



# model selection
m.list <- list()

# fit the models # slightly different added a new #5 but same result
m.list$m1 <-
  lmer(
    mass_gain ~ 1 + (1 |ID) + (1 |yr),
    data = mass_gain_df_filtered,
    na.action = na.exclude,
    REML = F
  )
m.list$m2  <-
  lmer(
    mass_gain ~ mass_spring_z + (1 |ID) + (1 | yr),
    data = mass_gain_df_filtered,
    na.action = na.exclude,
    REML = F
  )
m.list$m3 <-
  lmer(
    mass_gain ~ mass_spring_z + first_pregnant + (1 |ID) + (1 | yr),
    data = mass_gain_df_filtered,
    na.action = na.exclude,
    REML = F
  )
m.list$m4  <-
  lmer(
    mass_gain ~ poly(greenup_mismatch_z, 2) + first_pregnant + mass_spring_z + (1|ID) + (1|yr),
    data = mass_gain_df_filtered,
    na.action = na.exclude,
    REML = F
  )
m.list$m5  <-
  lmer(
    mass_gain ~ greenup_mismatch_z +first_pregnant + mass_spring_z + (1|ID) + (1|yr),
    data = mass_gain_df_filtered,
    na.action = na.exclude,
    REML = F
  )
# m.list$m6  <-
#   lmer(
#     mass_gain ~ greenup_mismatch_z + I(greenup_mismatch_z^2)+ first_pregnant + mass_spring_z +# should be identical to poly. 
#       (1 |ID) + (1 |yr),
#     data = mass_gain_df_filtered,
#     na.action = na.exclude,
#     REML = F
#   )
m.list$m6 <-
  lmer(
    mass_gain ~ mass_spring_z + first_pregnant + pred + age_class + (1|ID) + (1|yr),
    data = mass_gain_df_filtered,
    na.action = na.exclude,
    REML = F
  )
m.list$m7  <-
  lmer(
    mass_gain ~ poly(greenup_mismatch_z, 2) + mass_spring_z  + (1|ID) + (1|yr),
    data = mass_gain_df_filtered,
    na.action = na.exclude,
    REML = F
  )
# m.list$m8  <-
#   lmer(
#     mass_gain ~ poly(greenup_mismatch_z, 2) + mass_spring_z:greenup_mismatch_z + (1|ID) + (1|yr),
#     data = mass_gain_df_filtered,
#     na.action = na.exclude,
#     REML = F
#   )
m.list$m8 <-
  lmer(
    mass_gain ~ mass_spring_z + first_pregnant + pred + age_class + wean_mass_z + (1|ID) + (1|yr),
    data = mass_gain_df_filtered,
    na.action = na.exclude,
    REML = F
  )
m.list$m9 <-
  lmer(
    mass_gain ~ poly(greenup_mismatch_z, 2) * mass_spring_z + (1|ID) + (1|yr),
    data = mass_gain_df_filtered,
    na.action = na.exclude,
    REML = F
  )
m.list$m10 <-
  lmer(
    mass_gain ~ poly(greenup_mismatch_z, 2) * mass_spring_z + first_pregnant + wean_mass_z  + pred + age_class + (1|ID) + (1|yr), # this one has an interaction
    data = mass_gain_df_filtered,
    na.action = na.exclude,
    REML = F
  )
m.list$m11 <-
  lmer(
    mass_gain ~ poly(greenup_mismatch_z, 2) +  mass_spring_z + first_pregnant + wean_mass_z + (1 |ID) + (1 | yr),
    #lactation
    data = mass_gain_df_filtered,
    na.action = na.exclude,
    REML = F
  )
m.list$m12 <-
  lmer(
    mass_gain ~ poly(greenup_mismatch_z, 2) + mass_spring_z + first_pregnant + wean_mass_z + pred + (1 |ID) +
      (1 |yr),
    data = mass_gain_df_filtered,
    na.action = na.exclude,
    REML = F
  )
m.list$m13  <-
  lmer(
    mass_gain ~ poly(greenup_mismatch_z, 2) + mass_spring_z + first_pregnant + wean_mass_z + pred + age_class + # full model without interaction
      (1 |ID) + (1 |yr),
    data = mass_gain_df_filtered,
    na.action = na.exclude,
    REML = F
  )
m.list$m14  <-
  lmer(
    mass_gain ~ poly(greenup_mismatch_z, 2) + mass_spring_z + first_pregnant + pred + age_class + # full model without interaction and wean mass
      (1 |ID) + (1 |yr),
    data = mass_gain_df_filtered,
    na.action = na.exclude,
    REML = F
  )
# m.list$m15  <-
#   lmer(
#     mass_gain ~ poly(greenup_mismatch_z, 2) + mass_spring_z + first_pregnant + wean_mass_z + pred +
#       (1 |ID) + (1 |yr),
#     data = mass_gain_df_filtered,
#     na.action = na.exclude,
#     REML = F
#   )
m.list$m15  <-
  lmer(
    mass_gain ~ poly(birthdate_z, 2) + mass_spring_z+ first_pregnant + pred + age_class + wean_mass_z +(1|ID) + (1|yr),
    data = mass_gain_df_filtered,
    na.action = na.exclude,
    REML = F
  )
m.list$m16 <-
  lmer(
    mass_gain ~ birthdate_z + mass_spring_z +  first_pregnant + pred + age_class + wean_mass_z +(1|ID) + (1|yr),
    data = mass_gain_df_filtered,
    na.action = na.exclude,
    REML = F
  )

# m.list$m17 <-
#   lmer(
#     mass_gain ~ greenup_mismatch_z*mass_spring_z +  first_pregnant + pred + age_class + wean_mass_z +(1|ID) + (1|yr),
#     data = mass_gain_df_filtered,
#     na.action = na.exclude,
#     REML = F
#   )


# compare best model with birthdate instead of mismatch
# m.list$m12   <- lmer(mass_gain ~ poly(birthdate_z, 2)+ pregnant + wean_mass_z + mass_spring_z + pred + age_class + (1|ID) + (1|yr), data=mass_gain_df_filtered, na.action=na.exclude, REML=F)

# generate AICc table using aictab()
aic_tab <- aictab(m.list) %>% mutate(across(where(is.numeric), ~ round(.x, 2)))
# gets a warning - good! m3 and m4 are indeed equivalent. 
# K    AICc Delta_AICc AICcWt Cum.Wt      LL
# m14 11  980.74       0.00   0.56   0.56 -478.67
# m13 12  982.58       1.84   0.22   0.79 -478.46
# m6   9  985.07       4.33   0.06   0.85 -483.06
# m8  10  985.52       4.78   0.05   0.91 -482.18
# m15 12  985.68       4.94   0.05   0.95 -480.01
# m10 14  986.79       6.05   0.03   0.98 -478.26
# m16 11  987.71       6.97   0.02   1.00 -482.15
# m12 10  993.61      12.87   0.00   1.00 -486.23
# m4   8  994.97      14.23   0.00   1.00 -489.11
# m11  9  995.31      14.57   0.00   1.00 -488.18
# m3   6  996.49      15.75   0.00   1.00 -492.03
# m5   7  997.32      16.58   0.00   1.00 -491.37
# m7   7 1023.97      43.23   0.00   1.00 -504.70
# m9   9 1027.94      47.20   0.00   1.00 -504.50
# m2   5 1061.08      80.34   0.00   1.00 -525.38
# m1   4 1080.76     100.02   0.00   1.00 -536.28

# find the row index of the best model (i.e., lowest AICc)
best_model <- which.min(aic_tab[, 3])

# Find the row index of the model with the minimum AIC value
best_model_row <- which.min(aic_tab[, 3])

# Print the modname of the model with the minimum AIC value
best_model_name <- aic_tab$Modnames[best_model_row]

# get the model object corresponding to the best model
best_model_object <- m.list[[best_model_name]]

# print summary of the best model, REML=FALSE
summary(best_model_object)

# update to REML = TRUE
best_model_reml_filt <- update(best_model_object, REML = TRUE) 

# print the summary of the best model
summary(best_model_reml_filt)

# create tidy df of fixed and random effects estimates 
model_results_subset <- broom.mixed::tidy(best_model_reml_filt, conf.int=TRUE, conf.method="profile")

# Extract R-squared
R_squared <- MuMIn::r.squaredGLMM(best_model_reml_filt)

# Store in mini df
random_df <- data.frame(effect = 'Model fit',
                        group=NA,
                        term= c("marg.R squared", 'cond.R squared'),
                        estimate = c(R_squared[,'R2m'],R_squared[,'R2c']),
                        std.error = NA,
                        statistic=NA,
                        conf.low = NA,
                        conf.high = NA)

# Merge data frames
model_results_subset <- rbind(model_results_subset, random_df); rownames(model_results_subset) <- NULL 

# Round numeric variables to 2 digits
model_results_subset[, 4:8] <- round(model_results_subset[, 4:8], 2)

# Print results for the best model
print(model_results_subset)
model_results_subset <- as.data.frame(model_results_subset)

# Compute GVIF values
gvif_values <- car::vif(best_model_object)

# Check for high GVIF values
# values greater than 10 as indicating high multicollinearity.
print(gvif_values) 
# GVIF Df GVIF^(1/(2*Df))
# poly(greenup_mismatch_z, 2) 1.727063  2        1.146376
# mass_spring_z               1.279298  1        1.131061
# first_pregnant              1.679720  1        1.296040
# pred                        1.059843  1        1.029487
# age_class                   1.253964  2        1.058208


# save useful tables for supp
# write.csv(as.data.frame(aic_tab), file='output/data/tableS4.aic.tab.final.csv')
# write.csv(model_results_subset, file='output/data/tableS5.parameters.final.csv')
# save(list=ls(), file='cache/massModels.RData')


# verify with females with only 2 + captures 

# t <- table(mass_gain_df_full$ID)
# selected_ids <- names(t[t >1])
# tmp.mass.gain <- mass_gain_df_full[mass_gain_df_full$ID %in% selected_ids, ] # 197
# tmp.mass.gain$ID <- droplevels(tmp.mass.gain$ID) # 52 levels

# tmp.mass.gain <- mass_gain_df_full %>%
#   group_by(ID) %>%
#   filter(n() > 1) %>% arrange(ID) %>% droplevels()# n=197, 52 levels
# 
# tmp.mass.gain$ID <- droplevels(tmp.mass.gain$ID)      
# t2 <- table(tmp.mass.gain$ID)
# names(t2[t2>1])
# 
# tmp.mass.gain <- mass_gain_df_filtered %>%
# filter(first_pregnant=='0')
# tmp.mass.gain$ID <- droplevels(tmp.mass.gain$ID)    # n=147, 57 femelles 
# 
# 
# # compare model selection with this subset of females with >2 captures
# m.list <- list()
# 
# # fit the models # slightly different added a new #5 but same result
# m.list$m1 <-
#   lmer(
#     mass_gain ~ 1 + (1 |ID) + (1 |yr),
#     data = tmp.mass.gain,
#     na.action = na.exclude,
#     REML = F
#   )
# m.list$m2  <-
#   lmer(
#     mass_gain ~ mass_spring_z + (1 |ID) + (1 | yr),
#     data = tmp.mass.gain,
#     na.action = na.exclude,
#     REML = F
#   )
# # m.list$m3 <-
# #   lmer(
# #     mass_gain ~ mass_spring_z + first_pregnant + (1 |ID) + (1 | yr),
# #     data = tmp.mass.gain,
# #     na.action = na.exclude,
# #     REML = F
# #   )
# m.list$m4  <-
#   lmer(
#     mass_gain ~ poly(greenup_mismatch_z, 2) + mass_spring_z + (1|ID) + (1|yr),
#     data = tmp.mass.gain,
#     na.action = na.exclude,
#     REML = F
#   )
# m.list$m5  <-
#   lmer(
#     mass_gain ~ greenup_mismatch_z+ mass_spring_z + (1|ID) + (1|yr),
#     data = tmp.mass.gain,
#     na.action = na.exclude,
#     REML = F
#   )
# # m.list$m6  <-
# #   lmer(
# #     mass_gain ~ greenup_mismatch_z + I(greenup_mismatch_z^2)+ first_pregnant + mass_spring_z +# should be identical to poly. 
# #       (1 |ID) + (1 |yr),
# #     data = tmp.mass.gain,
# #     na.action = na.exclude,
# #     REML = F
# # #   )
# # m.list$m7  <-
# #   lmer(
# #     mass_gain ~ poly(greenup_mismatch_z, 2) + mass_spring_z  + (1|ID) + (1|yr),
# #     data = tmp.mass.gain,
# #     na.action = na.exclude,
# #     REML = F
# #   )
# m.list$m8  <-
#   lmer(
#     mass_gain ~ poly(greenup_mismatch_z, 2) + mass_spring_z:greenup_mismatch_z + (1|ID) + (1|yr),
#     data = tmp.mass.gain,
#     na.action = na.exclude,
#     REML = F
#   )
# m.list$m9 <-
#   lmer(
#     mass_gain ~ poly(greenup_mismatch_z, 2) * mass_spring_z + (1|ID) + (1|yr),
#     data = tmp.mass.gain,
#     na.action = na.exclude,
#     REML = F
#   )
# m.list$m10 <-
#   lmer(
#     mass_gain ~ poly(greenup_mismatch_z, 2) * mass_spring_z + wean_mass_z  + pred + age_class + (1|ID) + (1|yr), # this one has an interaction
#     data = tmp.mass.gain,
#     na.action = na.exclude,
#     REML = F
#   )
# m.list$m11 <-
#   lmer(
#     mass_gain ~ poly(greenup_mismatch_z, 2) +  mass_spring_z + wean_mass_z + (1 |ID) + (1 | yr),
#     #lactation
#     data = tmp.mass.gain,
#     na.action = na.exclude,
#     REML = F
#   )
# m.list$m12 <-
#   lmer(
#     mass_gain ~ poly(greenup_mismatch_z, 2) + mass_spring_z + wean_mass_z + pred + (1 |ID) +
#       (1 |yr),
#     data = tmp.mass.gain,
#     na.action = na.exclude,
#     REML = F
#   )
# m.list$m13  <-
#   lmer(
#     mass_gain ~ poly(greenup_mismatch_z, 2) + mass_spring_z + wean_mass_z + pred + age_class + # full model without interaction
#       (1 |ID) + (1 |yr),
#     data = tmp.mass.gain,
#     na.action = na.exclude,
#     REML = F
#   )
# m.list$m14  <-
#   lmer(
#     mass_gain ~ poly(greenup_mismatch_z, 2) + mass_spring_z + pred + age_class + # full model without interaction and wean mass
#       (1 |ID) + (1 |yr),
#     data = tmp.mass.gain,
#     na.action = na.exclude,
#     REML = F
#   )
# m.list$m15  <-
#   lmer(
#     mass_gain ~ poly(greenup_mismatch_z, 2) + wean_mass_z + pred +
#       (1 |ID) + (1 |yr),
#     data = tmp.mass.gain,
#     na.action = na.exclude,
#     REML = F
#   )
# m.list$m16  <-
#   lmer(
#     mass_gain ~ poly(birthdate_z, 2) + (1|ID) + (1|yr),
#     data = tmp.mass.gain,
#     na.action = na.exclude,
#     REML = F
#   )
# m.list$m17 <-
#   lmer(
#     mass_gain ~ birthdate_z + (1|ID) + (1|yr),
#     data = tmp.mass.gain,
#     na.action = na.exclude,
#     REML = F
#   )
# # generate AICc table using aictab()
# aic_tab <- aictab(m.list)
# # write.csv(aic_tab, file='output/data/aic.tab.2captures.final.csv')
# # K   AICc Delta_AICc AICcWt Cum.Wt      LL
# # m14 10 721.44       0.00   0.42   0.42 -349.91
# # m13 11 722.76       1.31   0.22   0.64 -349.40
# # m10 13 724.06       2.61   0.11   0.76 -347.66
# # m4   7 725.39       3.95   0.06   0.82 -355.29
# # m8   7 725.41       3.96   0.06   0.88 -355.30
# # m12  9 725.71       4.27   0.05   0.93 -353.20
# # m11  8 726.14       4.69   0.04   0.97 -354.55
# # m9   9 727.33       5.89   0.02   0.99 -354.01
# # m5   6 730.22       8.78   0.01   0.99 -358.81
# # m15  8 731.24       9.79   0.00   1.00 -357.10
# # m16  6 733.18      11.74   0.00   1.00 -360.29
# # m2   5 734.78      13.33   0.00   1.00 -362.18
# # m1   4 739.12      17.68   0.00   1.00 -365.42
# # m17  5 739.18      17.74   0.00   1.00 -364.38
# 
# # Find the row index of the model with the minimum AIC value
# best_model_row <- which.min(aic_tab[, 3])
# 
# # Print the modname of the model with the minimum AIC value
# best_model_name <- aic_tab$Modnames[best_model_row]
# 
# # get the model object corresponding to the best model
# best_model_object <- m.list[[best_model_name]]
# 
# # print summary of the best model, REML=FALSE
# summary(best_model_object)
# 
# # update to REML = TRUE
# best_model_reml_unf <- update(best_model_object, REML = TRUE) # convergence issues here 
# 
# # print the summary of the best model
# summary(best_model_reml_unf)
# 
# # create tidy df of fixed and random effects estimates 
# model_results_2captures <- broom.mixed::tidy(best_model_reml_unf, conf.int=TRUE, conf.method="profile")
# 
# # Extract R-squared
# R_squared <- MuMIn::r.squaredGLMM(best_model_reml_unf)
# 
# colnames(model_results_2captures)
# 
# random_df2 <- data.frame(effect = 'Model fit',
#                         group=NA,
#                         term= c("marg.R squared", 'cond.R squared'),
#                         estimate = c(R_squared[,'R2m'],R_squared[,'R2c']),
#                         std.error = NA,
#                         statistic=NA,
#                         conf.low = NA,
#                         conf.high = NA)
# 
# # Merge data frames
# model_results_2captures <- rbind(model_results_2captures, random_df2); rownames(model_results_2captures) <- NULL 
# 
# # Round numeric variables to 2 digits
# model_results_2captures[, 4:8] <- round(model_results_2captures[, 4:8], 2)
# 
# # Print results for the best model
# print(model_results_2captures)
# 
# # export table 1 for Crozier et al., in csv
# # write.csv(model_results_2captures, file='output/data/results.mass.gain.2captures.final.csv')







# clean up

# # Replace "my_df" with the name of your data frame
# keep_obj <- c("df_scaled", "df_scaled_filtered", "mass_gain_df_filtered", "repro_df_filtered", "summary_df","consec_df_filtered")
# 
# # Get the names of all objects in the current environment
# all_objs <- ls()
# 
# # Remove all objects except for "keep_obj"
# rm(list = setdiff(all_objs, keep_obj))





# GLMMs of mismatch at t on female prob to produce a lamb at t+1 ---------

# attention - need 2 observations per female (mismatch in year t and attempt to reproduce in year t +1)

dfPairs <- df_scaled_unfiltered %>% 
  select(ID, yr, repro_tp1, code_sr, reproduced, birthdate_z,greenup_mismatch_z, mass_fall_z,mass_gain, age_class, wean_mass_z) %>% 
  arrange(ID, yr) %>% 
  group_by(ID) %>% 
  mutate(
    next_yr=lead(yr),
    codesr2=lead(code_sr),
    mass_gain = mass_gain,
    mass_fall_z = mass_fall_z,
    greenup_mismatch_z=greenup_mismatch_z,
    birthdate_z=birthdate_z
  ) %>% 
  # keep only row with consecutive years (had a birthdate in yr t and had a repro or not next yr)
  filter(next_yr ==yr+1) %>% 
  # Remove rows wiht missing predictors or outcome
  filter(!is.na(repro_tp1) &!is.na(mass_gain) &!is.na(greenup_mismatch_z) & !is.na(mass_fall_z)) %>% 
  arrange(ID, yr) %>% # 225 observations
  filter(reproduced == 1) %>%
  ungroup() %>%
  filter(greenup_mismatch_z < 3.3 | is.na(greenup_mismatch_z)) %>% # or scaled is ~3.3
  distinct() %>% droplevels()


nlevels(dfPairs$ID) #76 (or 82 including 1999)


dfPairs %>%
  summarise(across(c(greenup_mismatch_z, mass_fall_z, mass_gain, wean_mass_z, birthdate_z), ~ sd(., na.rm = TRUE)))

# this df contains 225 observations of 65 females who had given birth in year t to have a mismatch value and for which we know her reproductive success next yr

# remove NA in explanatory variables 
tmp.df <- dfPairs %>%
  filter(
    !is.na(mass_gain),
    !is.na(mass_fall_z),
    !is.na(greenup_mismatch_z),
    !is.na(wean_mass_z),
    !is.na(birthdate_z),
    !is.na(age_class),
    !is.na(repro_tp1)
  ) %>%
  mutate(across(c(mass_gain, mass_fall_z, greenup_mismatch_z, wean_mass_z, birthdate_z), as.numeric)) %>%
  # mutate(
  #   greenup_mismatch_z = scale(greenup_mismatch_z),
  #   mass_fall_z = scale(mass_fall_z),
  #   mass_gain_z = scale(mass_gain),
  #   wean_mass_z = scale(wean_mass_z),
  #   birthdate_z = scale(birthdate_z)
  # ) %>%
  droplevels()


vars_to_scale <- c("mass_gain", "mass_fall_z", "greenup_mismatch_z", "wean_mass_z", "birthdate_z")

# Create scaled versions and bind to original df
scaled_vars <- as.data.frame(sapply(tmp.df[, vars_to_scale], scale))
names(scaled_vars) <- paste0(vars_to_scale, "2")  # Rename columns

# Combine with original dataset
tmp.df <- cbind(tmp.df, scaled_vars)
tmp.df$ID <- droplevels(tmp.df$ID)

# check how unbalanced
table(tmp.df$repro_tp1) 
# no yes 
# 11 154 

# put yr back to factor for analyses
tmp.df$yr <- as.factor(as.character(tmp.df$yr))


# note to myself: you have very few datapoints to test mismatch in yr t since sr no lamb or neonatal death will not have a birthdate
# note 2: separation or unbalanced response did not generate error message here. Trying mixed models

library(glmmTMB)
# does most female have one observation? 
table(tmp.df$ID, tmp.df$repro_tp1) # no - some have 9 but most only have 1 observations
table(tmp.df$yr) # no - several obs. per yr
table(tmp.df$ID)



# the variable code.sr seems to be problematic with unequal levels 

# 2   3   5 
# 9  54 102 

# Regrouper en binaire = lactated but not trhoughtwinter 1 = winter 
tmp.df$code_sr_bin <- ifelse(tmp.df$code_sr %in% c(2,3) , 0, 1) # fuck it I don't use it

# Vérifier la répartition
table(tmp.df$code_sr_bin)

# Fit the models
m.list <- list()

# Null model (baseline)
m.list$m0 <- glmmTMB(repro_tp1 ~ 1 + (1|yr), data = tmp.df, family = "binomial")

# Single predictors
m.list$m1 <- glmmTMB(repro_tp1 ~ mass_gain2 + (1|yr), data = tmp.df, family = "binomial")
m.list$m2 <- glmmTMB(repro_tp1 ~ greenup_mismatch_z2 + (1|yr), data = tmp.df, family = "binomial")
m.list$m3 <- glmmTMB(repro_tp1 ~ wean_mass_z2 + (1|yr), data = tmp.df, family = "binomial") # problem with cov.matrix
m.list$m4 <- glmmTMB(repro_tp1 ~ mass_fall_z2 + (1|yr), data = tmp.df, family = "binomial") # problem with cov.matrix

# Additive combinations
m.list$m5 <- glmmTMB(repro_tp1 ~ greenup_mismatch_z2 + wean_mass_z2 + (1|yr), data = tmp.df, family = "binomial") # problem with cov.matrix
m.list$m6 <- glmmTMB(repro_tp1 ~ mass_gain2 + greenup_mismatch_z2 + (1|yr), data = tmp.df, family = "binomial")
m.list$m7 <- glmmTMB(repro_tp1 ~ greenup_mismatch_z2 + age_class + wean_mass_z2 + (1|yr), data = tmp.df, family = "binomial")
m.list$m8 <- glmmTMB(repro_tp1 ~ mass_gain2 + greenup_mismatch_z2 + age_class + wean_mass_z2 + (1|yr), data = tmp.df, family = "binomial")

aic_tab <- aictab(m.list)
aic_tab[order(aic_tab$AICc), ]

# K  AICc Delta_AICc AICcWt Cum.Wt     LL
# m0  2 66.38       0.00   0.24   0.24 -31.15
# m5  5 67.05       0.68   0.17   0.42 -28.34

aic_tab <- aictab(m.list) %>% mutate(across(where(is.numeric), ~ round(.x, 2)))

# export this table
# write.csv(as.data.frame(aic_tab), file='output/data/final/tableS6.aic.tab.final.csv')


# null model is the best followed by4 and 1

summary(m.list$m4)
round(confint(m.list$m4), 2)
# Conditional model:
#   Groups Name        Variance Std.Dev.
# yr     (Intercept) 26.99    5.195   
# Number of obs: 165, groups:  yr, 20
# 
# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)   
# (Intercept)    7.3090     2.6771   2.730  0.00633 **
# mass_fall_z2   0.4393     0.4570   0.961  0.33639  

#                         2.5 % 97.5 % Estimate
# (Intercept)             2.06  12.56     7.31
# mass_fall_z2           -0.46   1.34     0.44
# Std.Dev.(Intercept)|yr  1.44  18.71     5.19

summary(m.list$m1)
round(confint(m.list$m1),2)
# 
# Random effects:
#   
#   Conditional model:
#   Groups Name        Variance Std.Dev.
# yr     (Intercept) 36.83    6.068   
# Number of obs: 165, groups:  yr, 20
# 
# Conditional model:
#   Estimate Std. Error z value Pr(>|z|)   
# (Intercept)   7.8540     2.7600   2.846  0.00443 **
#   mass_gain2   -0.5410     0.6648  -0.814  0.41572   

# 
# 2.5 % 97.5 % Estimate
# (Intercept)             2.44  13.26     7.85
# mass_gain2             -1.84   0.76    -0.54
# Std.Dev.(Intercept)|yr  1.82  20.19     6.07

# LRT of lamb production t+1
# repro_df_filtered$dummy <- as.factor(1) 
# class(repro_df_filtered$repro_tp1) # a 2 level factor
# 
# m0r <-
#   glmer(
#     repro_tp1 ~ greenup_mismatch_z + (1|dummy),
#     data = repro_df_filtered,
#     family = binomial,
#     na.action = na.exclude,
#     control = glmerControl(
#       optimizer = 'Nelder_Mead',
#       check.nlev.gtr.1 = "ignore")
#   ) #ok singular fit - I guess it is normal or is it because so unbalanced?
# 
# m1r<-glmer(repro_tp1~greenup_mismatch_z + (1|ID), data = repro_df_filtered, na.action=na.exclude,family=binomial) # 
# m2r<-glmer(repro_tp1~greenup_mismatch_z + (1|yr), data = repro_df_filtered, na.action=na.exclude, family=binomial) # 
# m3r<-glmer(repro_tp1~greenup_mismatch_z + (1|ID)  + (1|yr), data = repro_df_filtered, na.action=na.exclude, family=binomial)
# 
# anova(m0r, m1r) #p=0
# anova(m0r, m2r) # returns p 0 or of 0.04484*
# anova(m0r, m3r) #3 returns p <0.001
# anova(m1r, m3r) #3 returns p <0.001
# anova(m2r, m3r) #3 returns p <0.001
# 
# # Model selection - fixed effects
# glmm.repro <- list()
# library(brms)
# 
# glmm.repro$m0 <-
#   glmer(
#     repro_tp1 ~ 1 + (1|yr) + (1|ID),
#     family = binomial,
#     data = repro_df_filtered,
#     na.action = na.exclude,
#     control = glmerControl(optimizer = 'bobyqa', nAGQ =20)
#     )
# 


# #check assumptions
# testDispersion(m1)
# simulationOutput <- DHARMa::simulateResiduals(fittedModel = m1, plot = T)
# residuals(simulationOutput)
# plot(simulationOutput) #no significant problems detected
# 
# # create tidy df of fixed and random effects estimates 
# model_results_repro_tp1 <- broom.mixed::tidy(best_model_object, conf.int=TRUE, conf.method="profile")
# 
# # Extract R-squared
# R_squared <- MuMIn::r.squaredGLMM(best_model_object)
# 
# # Store in mini df
# random_df <- data.frame(effect = 'Model fit',
#                         group=NA,
#                         term= c("marg.R squared (theo)", 'cond.R squared (theo)'),
#                         estimate = c(R_squared[1,'R2m'],R_squared[1,'R2c']),
#                         std.error = NA,
#                         statistic=NA,
#                         p.value=NA,
#                         conf.low = NA,
#                         conf.high = NA)
# 
# # Merge data frames
# model_results_repro_tp1 <- rbind(model_results_repro_tp1, random_df); rownames(model_results_repro_tp1) <- NULL 
# 
# # Round numeric variables to 2 digits
# model_results_repro_tp1[, 4:9] <- round(model_results_repro_tp1[, 4:9], 2)
# 
# # Back-transform coefficients to the probability scale
# model_results_repro_tp1$estimate <- boot::inv.logit(model_results_repro_tp1$estimate)
# model_results_repro_tp1$conf.low <- boot::inv.logit(model_results_repro_tp1$conf.low)
# model_results_repro_tp1$conf.high<- boot::inv.logit(model_results_repro_tp1$conf.high)
# 
# # Print results for the best model
# print(model_results_repro_tp1)
# #   effect    group term     estim…¹ std.e…² stati…³ p.value conf.…⁴ conf.…⁵
# #   <chr>     <chr> <chr>      <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
# # 1 fixed     NA    (Interc…   0.982    0.95    4.2     0      0.936   1.00 
# # 2 fixed     NA    greenup…   0.237    0.47   -2.47    0.01   0.104   0.453
# # 3 ran_pars  yr    sd__(In…   0.816   NA      NA      NA      0.587   0.998
# # 4 Model fit NA    marg.R …   0.535   NA      NA      NA     NA      NA    
# # 5 Model fit NA    cond.R …   0.620   NA      NA      NA     NA      NA
# # results directly in ms - no export for this supp table. 
# 

# clean up 
keep_obj <- c("df_scaled", "df_scaled_filtered", "summary_df","consec_df_filtered")

# Get the names of all objects in the current environment
all_objs <- ls()

# Remove all objects except for "keep_obj"
rm(list = setdiff(all_objs, keep_obj))





# LMM of mismatch at t on mismatch at t+1  -----------------------------------------------------------------

# remove NA in explanatory variables 
breedingDf <- consec_df_filtered %>%
 dplyr:: select(ID, yr,mismatch_tp1,mass_gain, mass_fall_z, greenup_mismatch_z,wean_mass_z) %>% 
  filter(
    !is.na(mass_gain),
    !is.na(mass_fall_z),
    !is.na(greenup_mismatch_z),
    !is.na(wean_mass_z),
   # !is.na(age_class),
    !is.na(mismatch_tp1)
  ) %>%
  mutate(across(c(mass_gain, mass_fall_z, greenup_mismatch_z, wean_mass_z,mismatch_tp1), as.numeric)) %>%
  droplevels()


vars_to_scale <- c("mass_gain", "mass_fall_z", "greenup_mismatch_z", "wean_mass_z")

# Create scaled versions and bind to original df
scaled_vars <- as.data.frame(sapply(breedingDf[, vars_to_scale], scale))
names(scaled_vars) <- paste0(vars_to_scale, "2")  # Rename columns

# Combine with original dataset
breedingDf <- cbind(breedingDf, scaled_vars)
breedingDf$ID <- droplevels(breedingDf$ID)

# put yr back to factor for analyses
breedingDf$yr <- as.factor(as.character(breedingDf$yr))

# Select only numeric columns
# cor_df <- dplyr::select_if(breedingDf, is.numeric)
# 
# # Compute the correlation matrix
# mcor <- round(cor(cor_df, method='pearson'), 2)
# 
# # Use ggcorrplot to visualize the correlation matrix
# ggcorrplot(mcor, 
#            method = "square", 
#            type = "upper", 
#            outline.color = "darkgray",
#            colors = c("blue", "white", "orangered"), 
#            lab = TRUE, 
#            lab_size = 3,
#            tl.cex = 12, 
#            # title = "Correlation Matrix",
#            ggtheme = ggplot2::theme_minimal())
# 

# mcor_df <- as.data.frame(as.table(mcor))
# colnames(mcor_df) <- c("Var1", "Var2", "Correlation")


# # filter for high correlations
# high_corr_pairs <- mcor_df %>%
#   filter(abs(Correlation) >= 0.55 & Var1 != Var2) %>% # a bit more conservative here
#   rowwise() %>% # ensure unique pairs
#   mutate(pair = paste(sort(c(Var1, Var2)), collapse = "-")) %>%
#   distinct(pair, .keep_all = TRUE) %>%
#   select(Var1, Var2, Correlation)
# 
# print(high_corr_pairs)
# # Var1         Var2               Correlation
# <fct>        <fct>                    <dbl>
# 1 mismatch_tp1 birthdate_tp1             0.81
# 2 mass_gain    greenup_mismatch_z       -0.55
# 3 mass_gain_z  greenup_mismatch_z       -0.55
# 4 mass_gain_z  mass_gain                 1   
# 5 ewe_age_z    mass_fall_z               0.55
# 6 birthdate_z  wean_mass_z              -0.57



breedingDf$dummy <- as.factor(1) 
breedingDf <- data.frame(breedingDf)

# testing the random effects with likelihood ratio test 
# this should use the full model with all fixed effects 

m0r <- lmer(
  mismatch_tp1 ~ mass_fall_z + greenup_mismatch_z + (1|dummy),
  data = breedingDf,
  na.action = na.exclude,
  REML = TRUE,
  control = lmerControl(optimizer = 'Nelder_Mead',
                        check.nlev.gtr.1 = "ignore"))
m1r <-
  lmer(
    mismatch_tp1 ~ mass_fall_z + greenup_mismatch_z +(1|ID),
    data = breedingDf,
    na.action = na.exclude,
    REML = TRUE
  )
m2r <-
  lmer(
    mismatch_tp1 ~ mass_fall_z + greenup_mismatch_z + (1 |yr),
    data = breedingDf,
    na.action = na.exclude,
    REML = TRUE
  )
m3r <-
  lmer(
    mismatch_tp1 ~ mass_fall_z + greenup_mismatch_z + (1 |ID)  + (1 |yr),
    data = breedingDf,
    na.action = na.exclude,
    REML = TRUE
  )

anova(m0r, m1r) # 0
anova(m0r, m2r) # 0
anova(m0r, m3r) # m3
anova(m1r, m3r) # m3 
anova(m2r, m3r) # m3 looks like both ID and yr are included 


# check replication of ID 

t=table(breedingDf$ID)
View(t)
table(breedingDf$yr)


# model seletcion 

m.list = list()

m.list$m0 <-
  lmer(
    mismatch_tp1 ~ 1 + (1|ID) + (1|yr),
    data = breedingDf,
    na.action = na.exclude,
    REML = F
  )

m.list$m1 <-
  lmer(
    mismatch_tp1 ~ greenup_mismatch_z2 + (1|ID) + (1|yr),
    data = breedingDf,
    na.action = na.exclude,
    REML = F
  )

m.list$m2 <-
  lmer(
    mismatch_tp1 ~ mass_fall_z2 + (1|ID) + (1|yr),
    data = breedingDf,
    na.action = na.exclude,
    REML = F
  )


m.list$m3<-
  lmer(
    mismatch_tp1 ~ mass_fall_z2 + greenup_mismatch_z2 + (1|ID) + (1|yr),
    data = breedingDf,
    na.action = na.exclude,
    REML = F
  )

# m.list$m4<- # individual diff taken into account?
#   lmer(
#     mismatch_tp1 ~ mass_fall_z2 + greenup_mismatch_z2 + age_class + (1|ID) + (1|yr), # mass_gain correlated to gu
#     data = breedingDf,
#     na.action = na.exclude,
#     REML = F
#   )

aic = aictab(cand.set = m.list) #m4 best, 11 equivalent

aic <- as.data.frame(aic) %>% mutate(across(where(is.numeric), ~round(.x,2)))
aic
#        Modnames K   AICc Delta_AICc ModelLik AICcWt      LL Cum.Wt
# 3       m2 5 778.71       0.00     1.00   0.47 -384.02   0.47
# 4       m3 6 779.64       0.94     0.63   0.29 -383.36   0.76
# 5       m4 8 780.71       2.00     0.37   0.17 -381.53   0.94
# 1       m0 4 784.08       5.37     0.07   0.03 -387.82   0.97
# 2       m1 5 784.21       5.50     0.06   0.03 -386.78   1.00


# export table  for Crozier et al., in csv
# write.csv(aic, file='output/data/final/ts7.aictab.mismatch.tp1.final.csv')

# Find the row index of the model with the minimum AIC value
best_model_row <- which.min(aic[, 3])

# Print the modname of the model with the minimum AIC value
best_model_name <- aic$Modnames[best_model_row]

# get the model object corresponding to the best model
best_model_object <- m.list[[best_model_name]]

# update to REML = TRUE
best_model_reml_filt <- update(best_model_object, REML = TRUE) 

# print the summary of the best model
summary(best_model_reml_filt)

# create tidy df of fixed and random effects estimates 
model_results_mismatch_tp1 <- broom.mixed::tidy(best_model_reml_filt, conf.int=TRUE, conf.method="profile")

# Extract R-squared
R_squared <- MuMIn::r.squaredGLMM(best_model_object)

# Store in mini df
random_df <- data.frame(effect = 'Model fit',
                        group=NA,
                        term= c("marg.R squared (theo)", 'cond.R squared (theo)'),
                        estimate = c(R_squared[1,'R2m'],R_squared[1,'R2c']),
                        std.error = NA,
                        statistic=NA,
                        # p.value=NA,
                        conf.low = NA,
                        conf.high = NA)

# Merge data frames
model_results_mismatch_tp1 <- rbind(model_results_mismatch_tp1, random_df); rownames(model_results_mismatch_tp1) <- NULL 

# Round numeric variables to 2 digits
model_results_mismatch_tp1[, 4:8] <- round(model_results_mismatch_tp1[, 4:8], 2)

# Print results for the best model
# print(model_results_mismatch_tp1)
# effect    group    term                  estimate std.error statistic conf.low conf.high
# <chr>     <chr>    <chr>                    <dbl>     <dbl>     <dbl>    <dbl>     <dbl>
# 1 fixed     NA       (Intercept)              11.0       3.4       3.25     4.24     17.9 
# 2 fixed     NA       mass_fall_z              -4.73      1.61     -2.93    -7.93     -1.45
# 3 ran_pars  ID       sd__(Intercept)           9.3      NA        NA        4.1      13.8 
# 4 ran_pars  yr       sd__(Intercept)          11.7      NA        NA        7.74     17.5 
# 5 ran_pars  Residual sd__Observation           8.23     NA        NA        6.64     10.7 
# 6 Model fit NA       marg.R squared (theo)     0.07     NA        NA       NA        NA   
# 7 Model fit NA       cond.R squared (theo)     0.78     NA        NA       NA        NA 

# 
#   effect    group    term     estimate std.error statistic conf.low conf.high
#   <chr>     <chr>    <chr>       <dbl>     <dbl>     <dbl>    <dbl>     <dbl>
# 1 fixed     NA       (Interc…    10.7       3.41      3.13     3.84     17.6 
# 2 fixed     NA       mass_fa…    -4.65      1.59     -2.93    -7.8      -1.42
# 3 ran_pars  ID       sd__(In…     9.3      NA        NA        4.1      13.8 
# 4 ran_pars  yr       sd__(In…    11.7      NA        NA        7.74     17.5 
# 5 ran_pars  Residual sd__Obs…     8.23     NA        NA        6.64     10.7 
# 6 Model fit NA       marg.R …     0.07     NA        NA       NA        NA   
# 7 Model fit NA       cond.R …     0.78     NA        NA       NA        NA   
# > 
# export table  for Crozier et al., in csv
# write.csv(model_results_mismatch_tp1, file='output/data/final/ts8.estimates.mismatch.tp1.final.csv')



# save(list=ls(), file='cache/figDataMismatchtp1.RData')


summary(m.list$m3)

# Random effects:
#   Groups   Name        Variance Std.Dev.
# ID       (Intercept)  78.66    8.869  
# yr       (Intercept) 115.59   10.751  
# Residual              69.38    8.330  
# Number of obs: 97, groups:  ID, 38; yr, 18
# 
# Fixed effects:
#   Estimate Std. Error t value
# (Intercept)           10.469      3.194   3.278
# mass_fall_z2          -4.455      1.578  -2.823
# greenup_mismatch_z2    2.116      1.764   1.200
# 




round(confint(m.list$m3),2)
# 2.5 % 97.5 %
#   .sig01               2.69  13.64
# .sig02               7.29  16.61
# .sigma               6.68  11.06
# (Intercept)          3.96  17.17
# mass_fall_z2        -7.59  -1.18
# greenup_mismatch_z2 -1.49   5.76
# 



save(list=ls(),file = 'cache/fig5adat.RData')
rm(tmp.df)




# GLMMs of mismatch at t on weaning success at t+1 ------------------------

# new variable, new dataset here, outliers removed

# some individuals are there only once because we ended up cutting 1999 and 2020 off. 
# individuals there once are fine because they reproduced AND survived to the next year and thus have a FRS. The second of the two years might thus not be there - see for ex. 28T 
# I added variable wean_tp1 - a 2 level factor 



# remove missing values everywhere 
breedingDf <- consec_df_filtered %>%
  select(ID, yr, mismatch_tp1, wean_tp1, greenup_mismatch_z, mass_gain, mass_fall_z, wean_mass_z) %>% 
  filter(!is.na(mass_gain),
         !is.na(mass_fall_z),
         !is.na(wean_mass_z),
         !is.na(greenup_mismatch_z)) %>% droplevels() # not sure that does anything

# now, there shouln't have any NA in the response variable 
breedingDf <- breedingDf %>%
  filter(!is.na(wean_tp1),
         !is.na(mismatch_tp1)) %>% droplevels()

# check if response is balanced
table(breedingDf$wean_tp1) 

# no yes 
#  12  85 


# rescale this df 
vars_to_scale <- c("mass_gain", "mass_fall_z", "greenup_mismatch_z", 'wean_mass_z')

# Create scaled versions and bind to original df
scaled_vars <- as.data.frame(sapply(breedingDf[, vars_to_scale], scale))
names(scaled_vars) <- paste0(vars_to_scale, "2")  # Rename columns

# Combine with original dataset
breedingDf <- cbind(breedingDf, scaled_vars)
breedingDf$ID <- droplevels(breedingDf$ID)



# put yr back to factor for analyses
breedingDf$yr <- as.factor(as.character(breedingDf$yr))


# check IDs and yr distribution 
t=table(breedingDf$ID)
View(t)


# Likelihood ratio test - direct effect of mismatch at t on lamb survival to weaning t+1
breedingDf$dummy <- as.factor(1)

# Add a dummy variable with three levels
# breedingDf$dummy <- factor(sample(c("1", "2", "3"), 124, replace = TRUE))

breedingDf <- data.frame(breedingDf)

m0r <-
  glmer(
    wean_tp1 ~ greenup_mismatch_z2 + mass_gain2 + mass_fall_z2 + wean_mass_z2+ (1 | dummy),
    data = breedingDf,
    na.action = na.exclude,
    family = binomial,
    control = glmerControl(optimizer = 'Nelder_Mead',
                           check.nlev.gtr.1 = "ignore")
    
  ) # singular fit - 

m1r <-
  glmer(
    wean_tp1 ~  greenup_mismatch_z2 + mass_gain2 + mass_fall_z2 + wean_mass_z2+(1|ID), #+ (1 | dummy), says singular fit - no variance ? 
    data = breedingDf,
    na.action = na.exclude,
    family = binomial
  )

summary(m1r) # var ID =0 
VarCorr(m1r)

m2r <-
  glmer(
    wean_tp1 ~  greenup_mismatch_z2 + mass_gain2 + mass_fall_z2 + wean_mass_z2+(1|yr), # + (1 | dummy),
    data = breedingDf,
    na.action = na.exclude,
    family = binomial
  )

m3r <-
  glmer(
    wean_tp1 ~ greenup_mismatch_z2 + mass_gain2 + mass_fall_z2 + wean_mass_z2+(1|ID) + (1|yr),
    data = breedingDf,
    na.action = na.exclude,
    family = binomial
  )

anova(m0r, m1r) # 0 or ns but not nested 
anova(m0r, m2r) # yes but not nested without dummy
anova(m0r, m3r) # m3 - yesfor yr
anova(m1r, m3r) # m3 yes for yr
anova(m2r, m3r) # ns so just keep yr.

# of what little bit actually worked here, m3r isn't significant so going to remove ID as random effect again



# model selection of the effect of mismatch at t on weaning success at t+1
mod.list.weaning <- list()
mod.list.weaning$m0 <- glmer(wean_tp1 ~ 1 + (1|yr), data = breedingDf,
                             na.action = na.exclude,
                             family = binomial
)
mod.list.weaning$m1 <- glmer(wean_tp1 ~ greenup_mismatch_z2 + (1|yr), data = breedingDf,
                             na.action = na.exclude,
                             family = binomial
)
mod.list.weaning$m2 <- glmer(wean_tp1 ~ mass_gain2 + (1|yr), data = breedingDf,
                             na.action = na.exclude,
                             family = binomial
)
mod.list.weaning$m3 <- glmer(wean_tp1 ~ mass_fall_z2 + (1|yr), data = breedingDf,
                             na.action = na.exclude,
                             family = binomial
)
mod.list.weaning$m4 <- glmer(wean_tp1 ~ wean_mass_z2 + (1|yr), data = breedingDf,
                             na.action = na.exclude,
                             family = binomial
)


mod.list.weaning$m5 <- glmer(wean_tp1 ~ greenup_mismatch_z2 + mass_gain2 + (1|yr), data = breedingDf,
                             na.action = na.exclude,
                             family = binomial
)
mod.list.weaning$m6 <- glmer(wean_tp1 ~ greenup_mismatch_z2 + mass_gain2+ wean_mass_z2 + (1|yr), data = breedingDf,
                             na.action = na.exclude,
                             family = binomial
)
mod.list.weaning$m7 <- glmer(wean_tp1 ~ greenup_mismatch_z2 + mass_fall_z2 + wean_mass_z2 + (1|yr), data = breedingDf,
                             na.action = na.exclude,
                             family = binomial
)
mod.list.weaning$m8 <- glmer(wean_tp1 ~ mass_gain2+ mass_fall_z2 + wean_mass_z2 + (1|yr),data = breedingDf,
                             na.action = na.exclude,
                             family = binomial
)


# # test age? not specified in ms
# mod.list.weaning$m9  <- glmer(wean_tp1 ~ greenup_mismatch_z2 + mass_gain2 + wean_mass_z2 + age_class + (1|yr),data = breedingDf,
#                               na.action = na.exclude,
#                               family = binomial
# )
# mod.list.weaning$m10 <- glmer(wean_tp1 ~ greenup_mismatch_z2 + mass_gain2+ wean_mass_z2 + ewe_age_z + (1|yr), data = breedingDf,
#                               na.action = na.exclude,
#                               family = binomial
# )



# all models converged
aic = aictab(cand.set = mod.list.weaning)
aic <- as.data.frame(aic) %>% mutate(across(where(is.numeric), ~round(.x,2)))

aic

#         Modnames K  AICc Delta_AICc ModelLik AICcWt     LL Cum.Wt
# 1       m0 2 75.65       0.00     1.00   0.27 -35.76   0.27
# 5       m4 3 76.21       0.55     0.76   0.20 -34.97   0.47
# 4       m3 3 76.35       0.69     0.71   0.19 -35.05   0.66
# 3       m2 3 77.41       1.76     0.42   0.11 -35.58   0.77
# 2       m1 3 77.77       2.12     0.35   0.09 -35.76   0.86
# 6       m5 4 79.34       3.68     0.16   0.04 -35.45   0.90
# 8       m7 5 79.58       3.93     0.14   0.04 -34.46   0.94
# 7       m6 5 79.94       4.29     0.12   0.03 -34.64   0.97
# 9       m8 5 79.99       4.33     0.11   0.03 -34.66   1.00

# export table for Crozier et al., in csv
# write.csv(aic, file='output/data/final/ts8.aictab.wean.tp1.final.csv')
# car::vif(lm(wean_tp1 ~ greenup_mismatch_z2 + mass_gain2 + wean_mass_z2 + mass_fall_z2, data = breedingDf))

# 
# # Find the row index of the model with the minimum AIC value
# best_model_row <- which.min(aic[, 3])
# 
# # Print the modname of the model with the minimum AIC value
# best_model_name <- aic$Modnames[best_model_row]
# 
# # get the model object corresponding to the best model
# best_model_object <- mod.list.weaning[[best_model_name]]
# 
# # print summary of the best model, REML=FALSE
# summary(best_model_object)
# 
# #check assumptions
# # dharma::testDispersion(mod.list.weaning$m7)
# # simulationOutput <- DHARMa::simulateResiduals(fittedModel = m12, plot = T)
# # residuals(simulationOutput)
# # plot(simulationOutput) #no significant problems detected
# 
# # create tidy df of fixed and random effects estimates 
# model_results_wean_tp1 <- broom.mixed::tidy(best_model_object, conf.int=TRUE, conf.method="profile")
# 
# # Extract R-squared
# R_squared <- MuMIn::r.squaredGLMM(best_model_object)
# 
# # Store in mini df
# random_df <- data.frame(effect = 'Model fit',
#                         group=NA,
#                         term= c("marg.R squared (theo)", 'cond.R squared (theo)'),
#                         estimate = c(R_squared[1,'R2m'],R_squared[1,'R2c']),
#                         std.error = NA,
#                         statistic=NA,
#                         p.value=NA,
#                         conf.low = NA,
#                         conf.high = NA)
# 
# # Merge data frames
# model_results_wean_tp1 <- rbind(model_results_wean_tp1, random_df); rownames(model_results_wean_tp1) <- NULL 
# 
# # # Round numeric variables to 2 digits
# # model_results_wean_tp1[, 4:9] <- round(model_results_wean_tp1[, 4:9], 2)
# # The logit scale is the default link function for binomial GLMMs, and it relates the linear predictor to the probability of success.
# # Back-transform coefficients to the probability scale
# model_results_wean_tp1$estimate <- boot::inv.logit(model_results_wean_tp1$estimate)
# model_results_wean_tp1$conf.low <- boot::inv.logit(model_results_wean_tp1$conf.low)
# model_results_wean_tp1$conf.high<- boot::inv.logit(model_results_wean_tp1$conf.high)
# 
# # Print results for the best model
# print(model_results_wean_tp1)
# #   effect  group term  estimate std.error statistic p.value conf.low conf.high
# # 1 fixed   NA    (Int…    0.705      0.36      2.44    0.01    0.527     0.844
# # 2 fixed   NA    mass…    0.532      0.27      0.49    0.62    0.399     0.659
# # 3 fixed   NA    wean…    0.652      0.29      2.18    0.03    0.522     0.777
# # 4 ran_pa… yr    sd__…    0.760     NA        NA      NA       0.582     0.908
# # 5 Model … NA    marg…    0.525     NA        NA      NA      NA        NA    
# # 6 Model … NA    cond…    0.589     NA        NA      NA      NA        NA  
# 
# # export table for Crozier et al., in csv
# # write.csv(model_results_wean_tp1, file='output/data/ts6.model.results.wean.tp1.back-transformed.csv')
# 


# equivalent models - final revs 

round(summary(mod.list.weaning$m4)$coefficients,2)

# #               Estimate Std. Error z value Pr(>|z|)
# (Intercept)      2.25       0.51    4.42     0.00
# wean_mass_z2     0.42       0.34    1.24     0.22

# round(boot::inv.logit(summary(mod.list.weaning$m4)$coefficient),2)
# Estimate Std. Error z value Pr(>|z|)
# (Intercept)       0.9       0.62    0.99     0.50
# wean_mass_z2      0.6       0.59    0.77     0.55

round(confint(mod.list.weaning$m4),2)
# 2.5 % 97.5 %
#   .sig01        0.00   2.65
# (Intercept)   1.44   3.69
# wean_mass_z2 -0.24   1.17


round(summary(mod.list.weaning$m3)$coefficients,2)
# Estimate Std. Error z value Pr(>|z|)
# (Intercept)      2.22       0.49    4.53     0.00
# mass_fall_z2     0.42       0.36    1.17     0.24

round(confint(mod.list.weaning$m3),2)
# 2.5 % 97.5 %
#   .sig01        0.00   2.57
# (Intercept)   1.43   3.59
# mass_fall_z2 -0.26   1.19
# 


round(summary(mod.list.weaning$m2)$coefficients,2)
# Estimate Std. Error z value Pr(>|z|)
# (Intercept)     2.20       0.49    4.44     0.00
# mass_gain2      0.24       0.40    0.59     0.56

round(confint(mod.list.weaning$m2),2)

# 2.5 % 97.5 %
#   .sig01       0.00   2.65
# (Intercept)  1.40   3.60
# mass_gain2  -0.49   1.14


# save(list=ls(),file = 'cache/fig5bdat.Rdata')



# EXTRAS ------------------------------------------------------------------




# LMMs mismatch at t on parturition date at t+1 ----------------------
# 
# # clean up 
# keep_obj <- c("df_scaled", "df_scaled_filtered", "summary_df","consec_df_filtered")
# 
# # Get the names of all objects in the current environment
# all_objs <- ls()
# 
# # Remove all objects except for "keep_obj"
# rm(list = setdiff(all_objs, keep_obj))
# 
# # put yr back to factor for analyses
# consec_df_filtered$yr <- as.factor(as.character(consec_df_filtered$yr))
# 
# # remove missing values everywhere 
# # now, there shouln't have any NA in the response variable 
# # birthdate_df <- consec_df_filtered %>%
# #   filter(!is.na(birthdate_tp1)) %>% droplevels()
# 
# # dropping to 38 females - no missing values in response variables
# 
# # Select only numeric columns to rescale since it is a new df
# # birthdate_df <- birthdate_df %>%
#  #  mutate(across(where(is.numeric), scale, .names = "{.col}")) # add nothing to col names
# 
# # testing the random effects of ID and year
# birthdate_df$dummy <- as.factor(1) 
# birthdate_df <- data.frame(birthdate_df)
# 
# # m0r<-lmer(birthdate_tp1~mass_gain_z + (1|dummy), 
# #           data = birthdate_df, 
# #           na.action=na.exclude, 
# #           REML = T, 
# #           control = lmerControl(optimizer = 'Nelder_Mead',
# #                                 check.nlev.gtr.1 = "ignore")) #ok does not converge 
# # m1r <-
# #   lmer(
# #     birthdate_tp1 ~ mass_gain_z + (1|ID),
# #     data = birthdate_df,
# #     na.action = na.exclude,
# #     REML = T
# #   )
# # m2r <-
# #   lmer(
# #     birthdate_tp1 ~ mass_gain_z + (1|yr),
# #     data = birthdate_df,
# #     na.action = na.exclude,
# #     REML = T
# #   )
# # m3r <-
# #   lmer(
# #     birthdate_tp1 ~ mass_gain_z + (1|ID)  + (1|yr),
# #     data = birthdate_df,
# #     na.action = na.exclude,
# #     REML = T
# #   )
# # 
# # anova(m0r, m1r) # 0
# # anova(m0r, m2r) # 0
# # anova(m0r, m3r) # m3
# # anova(m1r, m3r) # m3 
# # anova(m2r, m3r) # m3 looks like both ID and yr are included 
# # 
# # # doing the model selection of fixed effects of mismatch on parturition date at t+1
# # mod.list.bdate <- list()
# # mod.list.bdate$m0 <-
# #   lmer(
# #     birthdate_tp1 ~ 1 + (1|ID)  + (1|yr),
# #     data = birthdate_df,
# #     na.action = na.exclude,
# #     REML = F
# #   )
# # 
# # mod.list.bdate$m1 <-
# #   lmer(
# #     birthdate_tp1 ~ birthdate_z + (1|ID)  + (1|yr),
# #     data = birthdate_df,
# #     na.action = na.exclude,
# #     REML = F
# #   )
# # 
# # mod.list.bdate$m2 <-
# #   lmer(
# #     birthdate_tp1 ~ greenup_mismatch_z + (1|ID)  + (1|yr),
# #     data = birthdate_df,
# #     na.action = na.exclude,
# #     REML = F
# #   )
# # 
# # mod.list.bdate$m3 <-
# #   lmer(
# #     birthdate_tp1 ~ wean_mass_z + (1|ID)  + (1|yr),
# #     data = birthdate_df,
# #     na.action = na.exclude,
# #     REML = F
# #   )
# # 
# # mod.list.bdate$m4 <-
# #   lmer(
# #     birthdate_tp1 ~ mass_fall_z + (1|ID)  + (1|yr), # selected
# #     data = birthdate_df,
# #     na.action = na.exclude,
# #     REML = F
# #   )
# # 
# # mod.list.bdate$m5 <-
# #   lmer(
# #     birthdate_tp1 ~ mass_gain_z + (1|ID)  + (1|yr),
# #     data = birthdate_df,
# #     na.action = na.exclude,
# #     REML = F
# #   )# ok scaled even if not _z
# # 
# # mod.list.bdate$m6 <-
# #   lmer(
# #     birthdate_tp1 ~ greenup_mismatch_z + mass_gain_z + (1|ID)  + (1|yr),
# #     data = birthdate_df,
# #     na.action = na.exclude,
# #     REML = F
# #   )
# # 
# # mod.list.bdate$m7 <-
# #   lmer(
# #     birthdate_tp1 ~ greenup_mismatch_z + wean_mass_z + (1|ID)  + (1|yr),
# #     data = birthdate_df,
# #     na.action = na.exclude,
# #     REML = F
# #   )
# # 
# # mod.list.bdate$m8 <-
# #   lmer(
# #     birthdate_tp1 ~ greenup_mismatch_z + wean_mass_z + birthdate_z + (1|ID)  + (1|yr),
# #     data = birthdate_df,
# #     na.action = na.exclude,
# #     REML = F
# #   )
# # 
# # mod.list.bdate$m9 <-
# #   lmer(
# #     birthdate_tp1 ~ greenup_mismatch_z + birthdate_z + (1|ID)  + (1|yr),
# #     data = birthdate_df,
# #     na.action = na.exclude,
# #     REML = F
# #   )
# # 
# # mod.list.bdate$m10 <-
# #   lmer(
# #     birthdate_tp1 ~ greenup_mismatch_z + wean_mass_z + mass_gain_z + ewe_age_z + (1|ID)  + (1|yr),
# #     data = birthdate_df,
# #     na.action = na.exclude,
# #     REML = F
# #   )
# # 
# # mod.list.bdate$m11 <-
# #   lmer(
# #     birthdate_tp1 ~ mass_fall_z + mass_gain_z + (1|ID)  + (1|yr), # selected
# #     data = birthdate_df,
# #     na.action = na.exclude,
# #     REML = F
# #   )
# # 
# # mod.list.bdate$m12 <-
# #   lmer(
# #     birthdate_tp1 ~ mass_fall_z + mass_gain_z + ewe_age_z + (1|ID)  + (1|yr),
# #     data = birthdate_df,
# #     na.action = na.exclude,
# #     REML = F
# #   )
# # 
# # mod.list.bdate$m13 <-
# #   lmer(
# #     birthdate_tp1 ~ mass_fall_z + greenup_mismatch_z + mass_gain_z + ewe_age_z + (1|ID)  + (1|yr),
# #     data = birthdate_df,
# #     na.action = na.exclude,
# #     REML = F
# #   )
# # 
# # mod.list.bdate$m14 <-
# #   lmer(
# #     birthdate_tp1 ~ mass_fall_z + greenup_mismatch_z + (1|ID)  + (1|yr),
# #     data = birthdate_df,
# #     na.action = na.exclude,
# #     REML = F
# #   )
# # 
# # aic = aictab(cand.set = mod.list.bdate)
# # 
# # #     K   AICc Delta_AICc AICcWt Cum.Wt      LL
# # # m4  5 257.13       0.00   0.42   0.42 -123.24
# # # m11 6 258.49       1.35   0.21   0.63 -122.78
# # # m14 6 259.11       1.98   0.16   0.79 -123.09
# # # m12 7 259.61       2.48   0.12   0.91 -122.18
# # # m13 8 260.55       3.41   0.08   0.99 -121.46
# # # m10 8 266.64       9.51   0.00   0.99 -124.50
# # # m3  5 267.45      10.32   0.00   0.99 -128.40
# # # m1  5 267.55      10.41   0.00   0.99 -128.44
# # # m0  4 267.96      10.83   0.00   1.00 -129.77
# # # m2  5 269.48      12.35   0.00   1.00 -129.41
# # # m7  6 269.55      12.41   0.00   1.00 -128.31
# # # m5  5 269.61      12.48   0.00   1.00 -129.48
# # # m9  6 269.71      12.58   0.00   1.00 -128.39
# # # m8  7 271.11      13.97   0.00   1.00 -127.92
# # # m6  6 271.61      14.47   0.00   1.00 -129.34
# # 
# # 
# # # export table  for Crozier et al., in csv
# # # write.csv(aic, file='output/data/aictab.bdate.tp1.final.csv')
# # 
# # # Find the row index of the model with the minimum AIC value
# # best_model_row <- which.min(aic[, 3])
# # 
# # # Print the modname of the model with the minimum AIC value
# # best_model_name <- aic$Modnames[best_model_row]
# # 
# # # get the model object corresponding to the best model
# # best_model_object <- mod.list.bdate[[best_model_name]]
# # 
# # # update to REML = TRUE
# # best_model_reml_filt <- update(best_model_object, REML = TRUE) 
# # 
# # # print the summary of the best model
# # summary(best_model_reml_filt)
# # 
# # # create tidy df of fixed and random effects estimates 
# # model_results_bdate_tp1 <- broom.mixed::tidy(best_model_reml_filt, conf.int=TRUE, conf.method="profile")
# # 
# # # Extract R-squared
# # R_squared <- MuMIn::r.squaredGLMM(best_model_reml_filt)
# # 
# # # Store in mini df
# # random_df <- data.frame(effect = 'Model fit',
# #                         group=NA,
# #                         term= c("marg.R squared (theo)", 'cond.R squared (theo)'),
# #                         estimate = c(R_squared[1,'R2m'],R_squared[1,'R2c']),
# #                         std.error = NA,
# #                         statistic=NA,
# #                        # p.value=NA,
# #                         conf.low = NA,
# #                         conf.high = NA)
# # 
# # # Merge data frames
# # model_results_bdate_tp1 <- rbind(model_results_bdate_tp1, random_df); rownames(model_results_bdate_tp1) <- NULL 
# # 
# # # Round numeric variables to 2 digits
# # model_results_bdate_tp1[, 4:8] <- round(model_results_bdate_tp1[, 4:8], 2)
# # 
# # # Print results for the best model
# # print(model_results_bdate_tp1)
# # # 
# # # effect    group    term    estimate std.error statistic conf.low conf.high
# # #   <chr>     <chr>    <chr>      <dbl>     <dbl>     <dbl>    <dbl>     <dbl>
# # # 1 fixed     NA       (Inter…     0.16      0.16      1       -0.15      0.5 
# # # 2 fixed     NA       mass_f…    -0.45      0.12     -3.77    -0.68     -0.21
# # # 3 ran_pars  ID       sd__(I…     0.76     NA        NA        0.44      1.06
# # # 4 ran_pars  yr       sd__(I…     0.27     NA        NA        0         0.53
# # # 5 ran_pars  Residual sd__Ob…     0.63     NA        NA        0.52      0.8 
# # # 6 Model fit NA       marg.R…     0.16     NA        NA       NA        NA   
# # # 7 Model fit NA       cond.R…     0.68     NA        NA       NA        N                                                          
# # 
# # # export parameter estiamtes 
# # # write.csv(model_results_bdate_tp1, file='output/data/model.results.bdate.tp1.final.csv')
# # 
# # # also check the second best`, m11
# # # update to REML = TRUE
# # m11 <- update(mod.list.bdate$m11 , REML = TRUE) 
# # 
# # # print the summary of the best model
# # round(summary(m11)$coeff,2)
# # #           Estimate Std. Error t value
# # # (Intercept)     0.16       0.16    0.97
# # # mass_fall_z    -0.50       0.13   -3.81
# # # mass_gain_z     0.09       0.10    0.94
# # 
# #  
# # 
# # # compare to this metric
# # performance::r2_nakagawa(mod.list.bdate$m4)
# # R2 for Mixed Models
# # Conditional R2: 0.669
# # Marginal R2: 0.163
# 
# 
# # compare to other models 
# # broom.mixed::tidy(mod.list.bdate$m4, conf.int=TRUE, conf.method="profile")
# # # A tibble: 5 × 8
# # effect   group    term            estimate std.error statistic conf.low conf.high
# # <chr>    <chr>    <chr>              <dbl>     <dbl>     <dbl>    <dbl>     <dbl>
# # 1 fixed    NA       (Intercept)       154.        1.99     77.3    150.      158.  
# # 2 fixed    NA       mass_fall_z        -5.89      1.50     -3.92    -8.89     -2.87
# # 3 ran_pars ID       sd__(Intercept)     9.29     NA        NA        5.23     13.4 
# # 4 ran_pars yr       sd__(Intercept)     3.13     NA        NA        0         6.76
# # 5 ran_pars Residual sd__Observation     8.41     NA        NA        6.85     10.7 
# 
# # broom.mixed::tidy(mod.list.bdate$m12, conf.int=TRUE, conf.method="profile")
# # # A tibble: 7 × 8
# # effect   group    term            estimate std.error statistic conf.low conf.high
# # <chr>    <chr>    <chr>              <dbl>     <dbl>     <dbl>    <dbl>     <dbl>
# # 1 fixed    NA       (Intercept)       154.        1.94     79.3   150.      158.   
# # 2 fixed    NA       mass_fall_z        -5.66      2.06     -2.75   -9.92     -1.49 
# # 3 fixed    NA       mass_gain           1.83      1.22      1.50   -0.777     4.27 
# # 4 fixed    NA       ewe_age_z          -2.01      1.45     -1.39   -4.89      0.857
# # 5 ran_pars ID       sd__(Intercept)     9.71     NA        NA       6.14     13.7  
# # 6 ran_pars yr       sd__(Intercept)     2.00     NA        NA       0         5.83 
# # 7 ran_pars Residual sd__Observation     8.23     NA        NA       6.73     10.3 
# 
# 
# 
# 
# 
# 
# 
# 
# 
# 














# Extra -------------------------------------------------------------------



# check MODEL assumption KEEP AS EXAMPLE 
E3 <- resid(m1g)
F3 <- fitted(m1g)
MyVar <- c("mass_spring_z")
df_scaled$E3 <- E3
MyMultipanel.ggp2(Z = df_scaled, 
                  varx = MyVar, 
                  vary = "E3", 
                  ylab = "Pearson residuals",
                  addSmoother = TRUE,
                  addRegressionLine = FALSE,
                  addHorizontalLine = TRUE)


par(mfrow = c(2,3), 
    mar = c(5,5,3,3),
    cex.lab = 1.5)
plot(x = F3, 
     y = E3, 
     cex.lab = 1.5,
     xlab = "Fitted values",
     ylab = "Residuals")
abline(h = 0, lty = 2)

plot(x = df_scaled$mass_spring_z, 
     y = E3, 
     cex.lab = 1.5,
     xlab = "Year",
     ylab = "Residuals")
abline(h = 0, lty = 2)

boxplot(E3 ~ factor, data = df_scaled)
abline(h = 0, lty = 2 )

boxplot(E3 ~ factor, data = df_scaled)
abline(h = 0, lty = 2 )

hist(E3, 
     xlab = "", 
     ylab = "", 
     breaks = 10)


# KEEP AS EXAMPLE TO EXTRACT COEFF

# Extract fixed effect coefficients
fixed_effects <- summary(m8gr)$coefficients[,c(1,4)]

# Extract random effect variances
random_variances <- as.data.frame(VarCorr(m8gr))

# Extract R-squared
R_squared <- summary(m8gr)$r.sq

# Store results in a data frame
model_results <- data.frame(Parameter = c(rownames(fixed_effects), rownames(random_variances)),
                            Estimate = c(fixed_effects[,1], random_variances[,1]),
                            Std_Error = c(fixed_effects[,2], NA, NA),
                            p_value = c(fixed_effects[,3], NA, NA),
                            Result_Type = c(rep("Fixed Effect", nrow(fixed_effects)), rep("Random Effect Variance", nrow(random_variances))),
                            R_squared = R_squared)

# Print results
print(model_results)

# checkin model assumptions
plot(m7go$gam)
gam.check(m7go$gam)

plot(m12go$gam)
gam.check(m12go$gam)


# create df with unique IDs  WHY??

mismatch_df <- transform(df_scaled_unfiltered,                                
                         IDx = as.numeric(factor(ID)))
mismatch_df = subset(mismatch_df, select = -c(ID) )
view(mismatch_df)
mismatch_df <-mismatch_df %>% 
  rename(
    ID = IDx
  )
colnames(mismatch_df)
#write.csv(mismatch_df, file = "mismatch_df.csv")



