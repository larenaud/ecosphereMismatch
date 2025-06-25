## ----setup, include=FALSE--------------
knitr::opts_chunk$set(echo = TRUE)
library(tidyverse)
library(ggplot2)
library(boot) # to generate CI
library(cowplot)



# Create a temporary environment
tmp_env <- new.env()

# Load the RData file into that environment
load("data/mine/mismatch_df_2025-05-08.RData", envir = tmp_env)

# Extract only the object you want
summary_df <- tmp_env$summary_df


# spatial fig -------------------------------------------------------------
















# reload data 
# load("data/mine/mismatch_df_2023-08-10.RData")
load("data/mine/mismatch_df_2025-05-08.RData") # this is not good for the model predictions, just raw data plots



# Descriptive figures - potentially for supp 







# now compare the mass in June between pregnant and non-pregnant females
boxplot(mass_spring_f~first_pregnant, data=lrs_f_mass_df)
summary(lme4::lmer(mass_spring_f~first_pregnant + (1|ID) + (1|yr), data=lrs_f_mass_df))$coeff
#                 Estimate Std. Error  t value
# (Intercept)     50.497051  0.7903224 63.89424
# first_pregnant1  5.365888  0.4948083 10.84438

confint(lme4::lmer(mass_spring_f~first_pregnant + (1|ID) + (1|yr), data=lrs_f_mass_df))
# 
# (Intercept)     48.936809 52.067586
# # first_pregnant1  4.393956  6.353100 # there is an effect of pregnancy at first capture on adj. mass in June.  





# Plot the data
# Subsample 10 unique IDs
set.seed(123)  # Set seed for reproducibility
sampled_ids <- sample(unique(df_long$unique_ID), 100)

df_long %>%
  filter(unique_ID %in% sampled_ids) %>%
  ggplot(aes(x = date, y = mass, group = unique_ID,color = first_pregnant)) + # ,
  geom_line()+
  geom_point()#+

# Plot the data to show mass difference between pregnant and non-pregnant females
jn=ggplot(df_long %>% filter(date=='June 5'), aes(x = first_pregnant, y = mass, fill = first_pregnant)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.1, aes(color = first_pregnant)) +
  labs(title = "Mass Comparison Between Pregnant and Non-Pregnant Females",
       x = "First Pregnant Status",
       y = "Mass",
       fill = "Pregnant",
       color = "Pregnant") +
  theme_minimal() +
  theme(legend.position = "none")

# Plot the data to show mass difference between pregnant and non-pregnant females
se=ggplot(df_long %>% filter(date=='September 15'), aes(x = first_pregnant, y = mass, fill = first_pregnant)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.1, aes(color = first_pregnant)) +
  labs(title = "Mass Comparison Between Pregnant and Non-Pregnant Females",
       x = "First Pregnant Status",
       y = "Mass",
       fill = "Pregnant",
       color = "Pregnant") +
  theme_minimal() +
  theme(legend.position = "none")
cowplot::plot_grid(jn, se, label=c('jn','se'), nrow = 1, ncol=2)

# retest effect of pregnancy on mass gain 

# Plot the data to show mass difference between pregnant and non-pregnant females
ggplot(mismatch_df, aes(x = first_pregnant, y = mass_gain, fill = first_pregnant)) +
  geom_boxplot(alpha = 0.5) +
  geom_jitter(width = 0.1, aes(color = first_pregnant)) +
  labs(title = "Mass Comparison Between Pregnant and Non-Pregnant Females",
       x = "First Pregnant Status",
       y = "Mass",
       fill = "Pregnant",
       color = "Pregnant") +
  theme_minimal() +
  theme(legend.position = "none") # looks like pregnant gain less mass. 










# Function to filter dataset by year
filter_start_year <- function(df) {
  filtered_df <- df %>%
    mutate(yr = as.numeric(as.character(yr))) %>%
    filter(yr >= 2000)
  
  return(filtered_df)
}
# Filtering each dataset
df_unscaled <- filter_start_year(df_unscaled)
df_scaled_unfiltered <- filter_start_year(df_scaled_unfiltered)
mass_gain_df_full <- filter_start_year(mass_gain_df_full)
mass_gain_df_filtered <- filter_start_year(mass_gain_df_filtered)
repro_df_full <- filter_start_year(repro_df_full)
repro_df_filtered <- filter_start_year(repro_df_filtered)
consec_df_full <- filter_start_year(consec_df_full)
consec_df_filtered <- filter_start_year(consec_df_filtered)



# Figure 1 is a conceptual figure




# Figure 2 histogram of mismatch values --------------------------------------

FIG1_hist <- ggplot(data=df_unscaled_filtered, aes(x=greenup_mismatch)) +
  geom_histogram(binwidth=2, color="black", fill="lightblue") +
  labs(x="Trophic mismatch (days)", y="Number of births") +
  scale_x_continuous(breaks=seq(-30, 80, 10)) +
 # ylim(0, 20) +
  cowplot::theme_cowplot(14) +
  theme(
    panel.grid.major.x = element_line(size = 0.5, color = "gray80", linetype = "dashed"),
    panel.grid.major.y = element_line(size = 0.5, color = "gray80", linetype = "dashed"),
    axis.text = element_text(size = 12),
    legend.title = element_blank(),
    legend.text = element_text(size = 12),
    plot.margin = margin(1, 1, 1, 1, "cm")
  )+
  geom_vline(xintercept=0, colour="lightcoral", size=1, linetype="longdash") 
FIG1_hist

# ggsave('output/graph/revisions/revised_fig1_mismatch_hist.png', width=10,height = 8, dpi=300)
# ggsave('output/graph/revisions/revised_fig1_mismatch_hist.pdf', width=10,height = 8, dpi=300)





# Figure S1 median parturition date over time ---------------------------------------------------------------
# # Generate sequence of years and store in data frame
# newdat <- data.frame(yr = seq(min(annual_df$yr), max(annual_df$yr), 1))
# # generate precitions with 95% CI
# predic <- predict(m5_bd_gam, type='link',newdata = newdat, interval="confidence", level = 0.95)
# #merge newdata and pred for plotting
# pred<- cbind(newdat, predic)

# generate predictions with 95% CI
p <- predict(m5_bd_gam, newdata=annual_df,type = "link", se.fit = TRUE) 

# If you want CIs
upr <- p$fit + (1.96 * p$se.fit)
lwr <- p$fit - (1.96 * p$se.fit)

# In the GAM, you need to form the confidence interval on the scale of the linear predictor and then transform that to the scale of the response by applying the inverse of the link function:
# https://stats.stackexchange.com/questions/33327/confidence-interval-for-gam-model/33328#33328
annual_df$p <- p$fit
annual_df$upr <- m5_bd_gam$family$linkinv(upr)
annual_df$lwr <- m5_bd_gam$family$linkinv(lwr)

# Calculate quantiles for the raw dat point range
quantiles <- df_unscaled_filtered %>%
  group_by(yr) %>%
  summarise(
    lower_quartile = quantile(birthdate, 0.25, na.rm=T),
    median_birthdate = median(birthdate, na.rm=T),
    upper_quartile = quantile(birthdate, 0.75, na.rm=T)
  )

# Load necessary packages
library(scales)

# Define the transformation function
julian_to_date <- function(julian_day) {
  as.Date(julian_day, origin = "1900-01-01") %>% format("%b-%d") # Assuming Julian days are relative to 1899-12-31
}





# Plot
FIGS1_median_bdate <- ggplot(annual_df, aes(x = yr, y = p)) +
  geom_pointrange(data = quantiles, aes(x = yr, y = median_birthdate,
                                        ymin = lower_quartile, ymax = upper_quartile),
                  size = 0.8, shape = 16, colour = 'grey50') + 
  geom_smooth(size = 1.5, colour = "cadetblue4", method = 'gam', se = FALSE) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), alpha = 0.2, fill = 'cadetblue4') +
  scale_x_continuous(limits = c(2000, 2020), breaks = seq(2000, 2020, 2)) +
  scale_y_continuous(labels = julian_to_date, expand = c(0, 0)) +  # Apply transformation
  labs(x = "Year", y = "Median parturition date") +
  cowplot::theme_cowplot(12) +
  theme(
    panel.grid.major.x = element_line(size = 0.5, color = "gray80", linetype = "dashed"),
    panel.grid.major.y = element_line(size = 0.5, color = "gray80", linetype = "dashed"),
    axis.text = element_text(size = 10),
    legend.title = element_blank(),
    legend.text = element_text(size = 10),
    plot.margin = margin(1, 1, 1, 1, "cm")
  )

FIGS1_median_bdate
# 
# ggsave('output/graph/revisions/revised_figS1_median_birthdate_trend.png', width=7,height = 5, unit='in', dpi=300)
# ggsave('output/graph/revisions/revised_figS1_median_birthdate_trend.pdf', width=7,height = 5, dpi=300)


# Figure 3 panel: a) green-up and b) mismatch over time -------------------------------------

load('cache/figData_Trends.RData')

# Load necessary packages
library(scales)

# Define the transformation function
# julian_to_date <- function(julian_day) {
#   as.Date(julian_day, origin = "1900-01-01") %>% format("%b-%d") # Assuming Julian days are relative to 1899-12-31
# }

# temp to english
julian_to_date <- function(julian_day) {
  old_locale <- Sys.getlocale("LC_TIME")         # Save current locale
  Sys.setlocale("LC_TIME", "C")                  # Set to English
  out <- format(as.Date(julian_day, origin = "1900-01-01"), "%b-%d")
  Sys.setlocale("LC_TIME", old_locale)           # Restore locale
  return(out)
}

# coefficients

# mod.gu.gam <- gam(greenup_date~s(yr),gamma = 1.4, data=unique(annual_df[, c("yr", "greenup_date")]))  
# mod.gu.gam2 <- gam(greenup_date~s(yr),data=unique(annual_df[, c("yr", "greenup_date")]))

# generate predictions with 95% CI
p <- predict(mod.gu.l, newdata=annual_df,type = "response", se.fit = TRUE) 

# If you want CIs

upr <- p$fit + (1.96 * p$se.fit)
lwr <- p$fit - (1.96 * p$se.fit)


# In the GAM, you need to form the confidence interval on the scale of the linear predictor and then transform that to the scale of the response by applying the inverse of the link function:
# https://stats.stackexchange.com/questions/33327/confidence-interval-for-gam-model/33328#33328
annual_df$p <- p$fit
annual_df$upr <- upr
annual_df$lwr <- lwr



# plot it 
fig3A <- ggplot(annual_df, aes(x=yr, y=p)) + 
  geom_point(data=annual_df, aes(x=yr, y=greenup_date),size=1.5, alpha=0.6, colour='black') + 
  geom_smooth(aes(y=p), linewidth = 1.2, se=F,colour='grey10') + 
  geom_ribbon((aes(ymin = lwr, ymax = upr)), alpha=0.2) +
  scale_x_continuous(limits = c(2000, 2020),breaks=seq(2000, 2020, 2)) + # put argument limits with breaks for better x axis
  scale_y_continuous(labels = julian_to_date, expand = c(0, 1)) +  # Apply transformation
    labs(x="Year",y="Green-up date") +  
  
  # Theme 
  cowplot::theme_cowplot() +
  theme(
    panel.grid.major.y = element_line(size = 0.4, color = "grey90", linetype = "dashed"),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = 'top',
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 10),
    # plot.margin = margin(1, 1, 1, 1, "cm"),
    plot.background = element_rect(fill = 'white', color = NA)
  ) 

fig3A
  
# cowplot::theme_cowplot(12) +
  # theme(
  #   panel.grid.major.x = element_line(size = 0.5, color = "gray80", linetype = "dashed"),
  #   panel.grid.major.y = element_line(size = 0.5, color = "gray80", linetype = "dashed"),
  #   axis.text = element_text(size = 10),
  #   legend.title = element_blank(),
  #   legend.text = element_text(size = 10)
  #  # plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  # )




# plot the raw data 
plt_labs <- labs(y = 'Trophic mismatch (days)',
                 x = 'Year',
                 colour = 'ID')

# plot the prediction of m5_gam 
new_data <- tidyr::expand(df_gu,ID,
                          yr = unique(yr))

m2_pred <- bind_cols(new_data,
                     as.data.frame(predict(m2_gam, newdata = new_data,
                                           type = "link", se.fit = TRUE)))

# If you want CIs
upr <- m2_pred$fit + (2 * m2_pred$se.fit)
lwr <- m2_pred$fit - (2 * m2_pred$se.fit)

m2_pred$upr <- m2_gam$family$linkinv(upr)
m2_pred$lwr <- m2_gam$family$linkinv(lwr)


fig3B <- ggplot(data=m2_pred, aes(x=yr, y=fit)) + 
  geom_point(data=df_gu, aes(x=yr, y=greenup_mismatch), size=1.5, shape = 21, alpha=0.6, colour='black') + 
  geom_smooth(colour='grey40', linewidth = 0.8, se=F) +
  geom_ribbon((aes(ymin = lwr, ymax = upr)), alpha=0.2) +
   scale_x_continuous(limits = c(2000, 2020),breaks=seq(2000, 2020, 2)) +plt_labs +
  scale_y_continuous(limits = c(-30, 60), breaks = seq(-30, 60, 10)) +

  # Theme
  cowplot::theme_cowplot() +
  theme(
    panel.grid.major.y = element_line(size = 0.4, color = "grey90", linetype = "dashed"),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = 'top',
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 10),
    # plot.margin = margin(1, 1, 1, 1, "cm"),
    plot.background = element_rect(fill = 'white', color = NA)
  ) 
  
fig3B

  # cowplot::theme_cowplot(12) +
  # theme(
  #   panel.grid.major.x = element_line(size = 0.5, color = "gray80", linetype = "dashed"),
  #   panel.grid.major.y = element_line(size = 0.5, color = "gray80", linetype = "dashed"),
  #   axis.text = element_text(size = 10),
  #   legend.title = element_blank(),
  #   legend.text = element_text(size = 10)
  #  # plot.margin = margin(0.5, 0.5, 0.5, 0.5, "cm")
  # )



# create panel
panel3AB <- cowplot::plot_grid(fig3A, fig3B, labels='auto', label_size = 14,
                               align = 'v', rel_widths = c(1,1))
panel3AB


# saving with minimum 300 dpi 
# ggsave("output/graph/final/FIG3AB.temporal.trends.final.grey.png", width = 11, height = 5, units = "in", device='png', dpi=300, bg='white')
# 
# cowplot::save_plot("output/graph/final/fig3AB_temporal_trends_cowplot.grey.pdf", panel3AB,
# 								 ncol = 2, #
# 								 nrow = 1, #
# 								 base_aspect_ratio = 1.3) # aspect ratio 'stretches' the figure. Up to you. Sometimes you need to adjust margins.

# ggsave(filename = "output/graph/final/FIG4_gainAge.png",
#        plot = FIG4_gainAge,
#        width = 10,
#        height = 5,
#        units = "in",
#        dpi = 300)


# Figure 4 of mass gain --------------------------------------


load('cache/massModels.RData')

best_model_name

# Print results for the best model
print(model_results_subset)
m.list$m14

# print the summary of the best model
summary(best_model_reml_filt)

mass_gain_df_filtered$yr <- as.factor(mass_gain_df_filtered$yr) 
mass_gain_df_filtered$ID <- as.factor(mass_gain_df_filtered$ID) 


# this model was selected 
m14  <-
  lmer(
    mass_gain ~ greenup_mismatch_z + I(greenup_mismatch_z^2)+ first_pregnant+mass_spring_z + pred + age_class +  # *
      (1 |ID) + (1 |yr),
    data = mass_gain_df_filtered,
    na.action = na.exclude,
    REML = TRUE
  )
summary(m14)

# create prediction data 
newdat1 <- expand.grid(greenup_mismatch_z=seq(min(mass_gain_df_filtered$greenup_mismatch_z, na.rm=T), 
                                              max(mass_gain_df_filtered$greenup_mismatch_z, na.rm = T), length = 200),
                       mass_spring_z=mean(mass_gain_df_filtered$mass_spring_z,na.rm=T),
                       #    wean_mass_z = mean(mass_gain_df_filtered$wean_mass_z, na.rm=T),
                       age_class = c("prim","adult","senescent"),
                       yr = 9999, # average year 
                       ID = "E12", # average female
                       pred = "0",
                       first_pregnant = c("0",'1')
) 

# you are in a lmer context. need to generate CI with appropriate function by bootstrap
myfun <- function(x) predict(x,newdata=newdat1,type="response",re.form=NA) # removed allow.new.levels=T

boo <- bootMer(m14, myfun, 1000, seed = NULL, use.u = FALSE, re.form=NA,
               verbose = T)
boo <- data.frame(boo)
str(boo)

newdat1$predi <- apply(boo, 2, mean) # removed boo$t because not in seq of x anymore
newdat1$max <- apply(boo, 2, function(x) quantile(x, 0.975))
newdat1$min <- apply(boo, 2, function(x) quantile(x, 0.025))

# extract only summary DF from initial data 


# unscale using values stored in summary_df USE the FULL df here
sd <- summary_df[summary_df$variable=="greenup_mismatch",'sd']
mean <- summary_df[summary_df$variable=="greenup_mismatch",'mean']
newdat1$greenup_mismatch_unsc <- (newdat1$greenup_mismatch_z * sd) + mean
# do the same for the raw data points (scaled)
mass_gain_df_filtered$greenup_mismatch_unsc <- (mass_gain_df_filtered$greenup_mismatch_z * sd) + mean


# Improved color palette
my_colors <- c("No" = "#E69F00", "Yes" = "#56B4E9")  # Okabe-Ito palette

FIG4_gainAge <-
  ggplot(data = newdat1,
         aes(
           x = greenup_mismatch_unsc,
           y = predi,
           color = factor(first_pregnant, labels = c("No", "Yes")),
           group = interaction(age_class, first_pregnant)
         )) +
  # Raw data points
  geom_point(
    data = mass_gain_df_filtered,
    aes(
      x = greenup_mismatch_unsc,
      y = mass_gain,
      color = factor(first_pregnant, labels = c("No", "Yes"))
    ),
    size = 2,
    alpha = 0.5
  ) +
  # Smoothed pred line
  geom_smooth(se = F, linewidth = 1) +
  # CI
  geom_ribbon(aes(
    ymax = max,
    ymin = min,
    fill = factor(first_pregnant, labels = c("No", "Yes"))
  ),
  alpha = 0.2,
  colour = NA)  +
  
  # highlight matched zone
  annotate(
    'rect',
    xmin = -5,
    xmax = 5,
    ymin = 0,
    ymax = 40,
    alpha = 0.1,
    fill = 'grey80'
  ) +
  geom_vline(
    xintercept = 0,
    linetype = 'dashed',
    color = 'grey',
    linewidth = 0.7
  ) +
  
  # labels for timing
  annotate(
    'text',
    x = -20,
    y = 1,
    label = 'Early birth',
    size = 3
  ) +
  annotate(
    'text',
    x = 20,
    y = 1,
    label = 'Late birth',
    size = 3
  ) +
  annotate(
    "text",
    x = 0,
    y = 1,
    label = "Matched",
    size = 3,
    fontface = "italic"
  ) +
  
  # Axes and legends
  #  scale_y_continuous(limits = c(0, 40), breaks = seq(0, 40, 5)) +
  # scale_x_continuous(limits = c(-25, 55), breaks = seq(-25, 55, 10)) +
  scale_color_manual(name = "First captured pregnant",
                     #labels = c("No", "Yes"),
                     values = my_colors) +
  scale_fill_manual(name = "First captured pregnant",
                    #  labels = c("No", "Yes"),
                    values = my_colors) +
  
  labs(x = "Mismatch (days)", y = "Summer mass gain (kg)") +
  
  # Facets with labels
  facet_wrap( ~ as.factor(age_class), 
              labeller = labeller(
                `as.factor(age_class)` = c(
                  "prim" = "Primiparous",
                  "senescent" = "Senescent",
                  "adult" = "Prime-aged"
                )
              ),
              nrow=1, 
              strip.position = 'top'
  ) + 
  
  # Theme
  
  cowplot::theme_cowplot(12) +
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
  ) 
# guides(color = guide_legend(title.position = "top", title.hjust = 0.5),
#        fill = guide_legend(title.position = "top", title.hjust = 0.5))

  
FIG4_gainAge

# Save the plot with ggsave()
ggsave(filename = "output/graph/final/FIG4_gainAgeV2.png",
       plot = FIG4_gainAge,
       width = 10,
       height = 5,
       units = "in",
       dpi = 300)

# 'bad' fit was due to NA remaining in datapoints, while excluded from prediction. 

# # create inset figure to show extent of outliers in the raw data
# # just fit the best model with unfiltered dataframe
# m10  <- lmer(mass_gain ~ poly(greenup_mismatch_z, 2)+ pregnant + wean_mass_z + mass_spring_z + pred + age_class + (1|ID) + (1|yr), data=mass_gain_df_full, na.action=na.exclude, REML=T) 
# 
# # showing models with outliers
# newdat2 <- data.frame(greenup_mismatch_z=seq(min(mass_gain_df_full$greenup_mismatch_z, na.rm=T), 
#                                              max(mass_gain_df_full$greenup_mismatch_z, na.rm = T), length = 200),
#                       mass_spring_z=mean(mass_gain_df_full$mass_spring_z,na.rm=T),
#                       wean_mass_z = mean(mass_gain_df_full$wean_mass_z, na.rm=T),
#                       age_class = "adult",
#                       # prs = "weaned",
#                       yr = 9999, # average year 
#                       ID = "E12", # average female
#                       pred = "0",
#                       pregnant = "0" 
#                      # lamb_sex_tm1 = "M"
# ) 
# 
# # bootstrap 
# myfun <- function(x) predict(x,newdata=newdat2,type="response",re.form=NA) # removed allow.new.levels=T
# boo <- bootMer(m10, myfun, 500, seed = NULL, use.u = FALSE, re.form=NA,
# 							 verbose = T)
# boo <- data.frame(boo)
# str(boo)
# 
# newdat2$predi <- apply(boo, 2, mean) # removed boo$t because not in seq of x anymore
# newdat2$max <- apply(boo, 2, function(x) quantile(x, 0.975))
# newdat2$min <- apply(boo, 2, function(x) quantile(x, 0.025))
# 
# # unscale using values stored in summary_df USE the FULL df here
# sd <- summary_df[summary_df$variable=="greenup_mismatch",'sd']
# mean <- summary_df[summary_df$variable=="greenup_mismatch",'mean']
# newdat2$greenup_mismatch_unsc <- (newdat2$greenup_mismatch_z * sd) + mean
# 
# # plot the inset 
# fig_mass_inset<- ggplot(data=newdat2, aes(x=greenup_mismatch_unsc, y= predi)) + 
#   geom_point(data=df_unscaled, aes(x=greenup_mismatch, y=mass_gain), size=0.8) +
#   geom_point(data=df_unscaled[df_unscaled$greenup_mismatch>65, ], aes(x=greenup_mismatch, y=mass_gain), colour="red", size=1) + 
#   geom_smooth(se=F, colour = "cadetblue4") +
#   geom_ribbon(aes(ymax=max, ymin=min), alpha = 0.2) +
#   scale_y_continuous(limits=c(0,30), breaks=seq(0,30,5)) +
#   scale_x_continuous(limits=c(-25,120),breaks=seq(-25,120,20)) +
#   labs(x="",y="") +
#   cowplot :: theme_cowplot(6) + 
#   theme(plot.background = element_rect(fill = 'white'))
# fig_mass_inset 
# 
# # combine
# plot.with.inset <-
#   ggdraw() +
#   draw_plot(fig_mass_main) +
#   draw_plot(fig_mass_inset, x = 0.6, y = .65, width = .35, height = .35)
# 
# 
# # Save the plot with ggsave()
# # ggsave(filename = "output/graph/plot.with.inset.3.png",
# #        plot = plot.with.inset,
# #        width = 17,
# #        height = 12,
# #        units = "cm",
# #        dpi = 300)






# Figure 5, 2panel for t+1  --------------------------------------

load('cache/fig5adat.RData')
# panel a - mismatch t+1

m.list$m2 <-
  lmer(
    mismatch_tp1 ~ mass_fall_z2 + (1|ID) + (1|yr),
    data = breedingDf,
    na.action = na.exclude,
    REML = TRUE
  )

# create prediction data 
datA <-
  expand.grid(
    mass_fall_z2 = seq(
      min(breedingDf$mass_fall_z2, na.rm = T),
      max(breedingDf$mass_fall_z2, na.rm = T),
      length = 200
    ),
    yr = 99999,
    ID = "E12"
  )  

# generate CIs
myfun <- function(x) predict(x,newdata=datA,type="response",re.form=NA) 
boo <- bootMer(m.list$m2, myfun, 1000, seed = NULL, use.u = FALSE, re.form=NA,
               verbose = T)

boo <- data.frame(boo)
str(boo)

datA$predi <- apply(boo, 2, mean) 
datA$max <- apply(boo, 2, function(x) quantile(x, 0.975))
datA$min <- apply(boo, 2, function(x) quantile(x, 0.025))

# unscale using values stored in summary_df USE the FULL df here
sd <- summary_df[summary_df$variable=="mass_fall",'sd'] # 7.626993
mean <- summary_df[summary_df$variable=="mass_fall",'mean'] # 69.38365
datA$mass_fall_unsc <- (datA$mass_fall_z2 * sd) + mean
breedingDf$mass_fall_raw <- (breedingDf$mass_fall_z2 * sd) + mean



a <- ggplot(data=datA, aes(x=mass_fall_unsc, y= predi)) +
  geom_point(data=breedingDf, aes(x=mass_fall_raw, y=mismatch_tp1), size=1.5, shape =21,alpha=0.6) +
  geom_smooth(se=F,linewidth = 1, colour='grey10') +
  geom_ribbon(aes(ymax=max, ymin=min), alpha = 0.2) +
  scale_y_continuous(limits=c(-20,80), breaks=seq(-20,80,10)) +
  scale_x_continuous(limits=c(50,90), breaks=seq(50,90,10)) +
  labs(x=expression("Female mass at conception (kg) at" [ ~ t]),
       y= expression("Trophic mismatch (days) t" [ ~ t + 2])) + 
  cowplot::theme_cowplot() +
  theme(
    panel.grid.major.y = element_line(size = 0.4, color = "grey90", linetype = "dashed"),
    panel.grid.major.x = element_line(size = 0.4, color = "grey90", linetype = "dashed"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = 'top',
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 10),
    # plot.margin = margin(1, 1, 1, 1, "cm"),
    plot.background = element_rect(fill = 'white', color = NA)
  ) 

a






# panel b - wean t+2
load('cache/fig5bdat.Rdata')

mod.list.weaning$m3 <- glmer(wean_tp1 ~ mass_fall_z2 + (1|yr), data = breedingDf,
                             na.action = na.exclude,
                             family = binomial)
summary(mod.list.weaning$m3 )


# Scaled residuals: 
#   Min      1Q  Median      3Q     Max 
# -3.4026  0.2306  0.2864  0.3596  0.7872 
# 
# Random effects:
#   Groups Name        Variance Std.Dev.
# yr     (Intercept) 0.8746   0.9352  
# Number of obs: 97, groups:  yr, 18
# 
# Fixed effects:
#   Estimate Std. Error z value Pr(>|z|)    
# (Intercept)    2.2200     0.4903   4.527 5.97e-06 ***
#   mass_fall_z2   0.4205     0.3602   1.167    0.243    
# ---


# create prediction data 
# attention to the variable  rescaled v2

datb <-
  expand.grid(
    mass_fall_z2 = seq(
      min(breedingDf$mass_fall_z2, na.rm =T),
      max(breedingDf$mass_fall_z2, na.rm =T),
      length = 200
    ),    #-1.75, 2.25,
    yr = 9999
  )

# you are in a lmer context. need to generate CI with appropriate function by bootstrap
myfun <- function(x) predict(x,newdata=datb,type="link",re.form=NA) # you are on the logit scale 
boo <- bootMer(mod.list.weaning$m3, myfun, 1000, seed = NULL, use.u = FALSE, re.form=NA, # replace by good model here 
               verbose = T)

boo <- data.frame(boo)
str(boo)

# extract the mean, quantiles, and compute the linear predictor and back-transform
datb$predi <- plogis(apply(boo, 2, mean))
datb$max <- plogis(apply(boo, 2, function(x) quantile(x, 0.975)))
datb$min <- plogis(apply(boo, 2, function(x) quantile(x, 0.025)))

# unscale using values stored in summary_df USE the FULL df here
sd <- summary_df[summary_df$variable == "mass_fall", 'sd']
mean <- summary_df[summary_df$variable == "mass_fall", 'mean']
datb$mass_fall_unsc <- (datb$mass_fall_z2 * sd) + mean

# take the good df and the variable - rescaled
breedingDf$mass_fall_raw <- (breedingDf$mass_fall_z2 * sd) + mean


# STEP 1: Recreate the raw (unscaled) variable
breedingDf$mass_fall_raw <- (breedingDf$mass_fall_z2 * sd) + mean

bin_width <- 1
bin_breaks <- seq(50, 85, by = bin_width)

# Create histogram data
hist_data <- breedingDf %>%
  filter(!is.na(wean_tp1)) %>%
  mutate(
    wean_tp1_num = ifelse(wean_tp1 == "yes", 1, 0)
  ) %>%
  mutate(
    bin = cut(mass_fall_raw, breaks = bin_breaks, include.lowest = TRUE),
    bin_start = as.numeric(gsub("\\((.+),.*", "\\1", bin)),
    bin_end   = as.numeric(gsub(".*,(.+)\\]", "\\1", bin)),
    bin_mid   = (bin_start + bin_end) / 2
  ) %>%
  group_by(wean_tp1, bin_mid) %>%
  summarise(n = n(), .groups = "drop") %>%
  mutate(
    y_base = ifelse(wean_tp1 == "yes", 1, 0),  # start position
    y = (n / max(n)) * 0.4                 # scaled height (adjust if too small)
  )

# base +
#   geom_rect(
#     data = hist_data,
#     aes(
#       xmin = bin_mid - bin_width / 2,
#       xmax = bin_mid + bin_width / 2,
#       ymin = y_base,
#       ymax = y_base + ifelse(wean_tp1 == "yes", y, -y), 
#       # So for wean_tp1 == "yes" rectangles go up from 1 (y_base=1), and for no they go down from 0 (y_base=0).
#       #  That looks reversed. Usually, "yes" (1) should be near y=1, but bars should extend downward from 1, and "no" (0) bars should extend upward from 0.
#       fill = wean_tp1
#     ),
#     alpha = 0.4,
#     inherit.aes = FALSE
#   ) +
#   scale_fill_manual(
#     values = c("no" = "#999999", "yes" = "#56B4E9"),
#     labels = c("Not weaned", "Weaned"),
#     name = NULL
#   )

rect_width <- bin_width * 0.3


b<-
  ggplot(data = datb, aes(x = mass_fall_unsc, y = predi))+
  geom_rect(
    data = hist_data,
    aes(
      xmin = bin_mid - bin_width / 3,
      xmax = bin_mid + bin_width / 3,
      ymin =  ifelse(wean_tp1 == "yes", y_base - y, y_base),
      ymax = ifelse(wean_tp1 == "yes", y_base, y_base + y)),
    alpha = 0.2,
    inherit.aes = FALSE
  ) +
  # geom_point(
  #   data = breedingDf,
  #   aes(x = mass_fall_raw, y = as.numeric(as.factor(wean_tp1)) - 1),
  #   # trick to put a yes-no into numeric
  #   size = 2, colour = 'grey20', alpha = 0.4
  # ) +
  geom_smooth(se = F,
              size = 1,
              colour = 'black') +
  geom_ribbon(aes(ymax = max, ymin = min), alpha = 0.2, fill='#56B4E9') +
  scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.2)) +
  scale_x_continuous(limits = c(50, 90), breaks = seq(50, 90, 10)) +
  labs(x=expression("Female mass at conception (kg) at" [ ~ t]),
       y= expression("Probability to survive to weaning at " [ ~ t + 2])) + 
  # custom theme 
  cowplot::theme_cowplot() +
  theme(
    panel.grid.major.y = element_line(size = 0.4, color = "grey90", linetype = "dashed"),
    panel.grid.major.x = element_line(size = 0.4, color = "grey90", linetype = "dashed"),
    axis.text = element_text(size = 10),
    axis.title = element_text(size = 12),
    legend.position = 'top',
    legend.title = element_text(size = 10),
    legend.text = element_text(size = 10),
    strip.text = element_text(size = 10),
    # plot.margin = margin(1, 1, 1, 1, "cm"),
    plot.background = element_rect(fill = 'white', color = NA)
  ) 
b


# create panel
FIG5 <- cowplot::plot_grid(a,b, labels='auto', label_size = 14,
                               align = 'v', rel_widths = c(1,1))
FIG5


# saving with minimum 300 dpi 
ggsave("output/graph/final/FIG5.png", width = 11, height = 5, units = "in", device='png', dpi=300, bg='white')






# #make it 2-panel for the consequences
# # panelAB <- cowplot::plot_grid(a, b, labels='auto', 
# #                                align = 'h', ncol=2)
# # panelAB
# # 
# # cowplot::save_plot("output/graph/revisions/panelAB.png", panelAB,
# # 								 ncol = 2, #
# # 								 nrow = 1, #
# # 								 base_aspect_ratio = 1.3) # aspect ratio 'stretches' the figure. Up to you. Sometimes you need to adjust margins.
# 
# 
# # Overlay panelBC onto plot_with_inset
# final_plot <- 
#   ggdraw() +
#   draw_plot(fig_mass_main, 0, 0, 0.6, 1) +  
#   draw_plot_label(label = "A", x = 0, y = 0.99, size = 14, hjust = 0, vjust = 1) +
#   draw_plot(panelBC, x = 0.6, y = 0, width = 0.4, height = 1) 
# 
# cowplot::save_plot("output/graph/revisions/panelABC.png", final_plot,
# 								 ncol =1, #
# 								 nrow = 1, #
# 								 base_aspect_ratio = 1) # aspect ratio 'stretches' the figure. Up to you. Sometimes you need to adjust margins.
# 
# 
# # Save the plot with ggsave()
# ggsave(filename = "output/graph/final_plot.png",
#        plot = final_plot,
#        width = 17,
#        height = 12,
#        units = "cm",
#        dpi = 300)
# 
# 
# # Save the plot with ggsave()
# ggsave(filename = "output/graph/revisions/FIG4ABC_panel_plot.png",
#        plot = final_plot,
#        width = 35,
#        height = 20,
#        units = "cm",
#        dpi = 300)







# EXTRAS ------------------------------------------------------------------


# Birthdate t+1 -----------------------------------------------------------

## ----predictions - effect on birthdate t+1------------------
newd <- data.frame()
nd <- expand.grid(mass_fall_z=seq(min(mismatchlamb_tp1$mass_fall_z, na.rm = T), max(mismatchlamb_tp1$mass_fall_z, na.rm =T), length = 100),
                  wean_mass_z = mean(mismatchlamb_tp1$wean_mass_z, na.rm=T),
                  prs = "weaned",   
                  yr = 99999, 
                  ID = "E12", 
                  pred = "0",
                  preg = "0"
) 
newd <- rbind(newd,nd) 

myfun <- function(x) predict(x,newdata=newd,type="response",re.form=NA,allow.new.levels=T) 
boo <- bootMer(m4br, myfun, 1000, seed = NULL, use.u = FALSE, re.form=NA,verbose = F) 
boo <- data.frame(boo)
newd$predi <- apply(boo, 2, mean) 
newd$upr <- apply(boo, 2, function(x) quantile(x, 0.975))
newd$lwr <- apply(boo, 2, function(x) quantile(x, 0.025))

max(newd$predi)

#unscale
newd$mass_fall_unsc <- ((newd$mass_fall_z * sd(mismatchlamb_tp1$mass_fall, na.rm=T))) + mean(mismatchlamb_tp1$mass_fall, na.rm=T) 

graph5 <- ggplot(data=newd, aes(x=mass_fall_unsc, y= predi)) + 
  geom_smooth(se=F,size = 3, colour = "cadetblue4")+
  geom_ribbon(aes(ymax=upr, ymin=lwr), alpha = 0.2) +
  geom_point(data=mismatchlamb_tp1, aes(x=mass_fall, y=birthdate_tp1), size=2) + 
  geom_hline(yintercept=144.5, colour="lightcoral", size=2, linetype="dashed") +
  geom_hline(yintercept=163.5, colour="lightcoral", size=2, linetype="dashed") +
  scale_y_continuous(breaks=seq(130, 200, 10)) +
  scale_x_continuous(breaks=seq(60,85, 5)) +
  labs(x="Fall Mass t (kg)",y="Parturition date (t+1)") +  
  theme(legend.position ="topright") +
  cowplot :: theme_cowplot() +
  theme(panel.grid.major = element_line(color="lightgrey", linetype = 'dotted')) +
  theme(axis.text=element_text(size=25)) +
  theme(axis.title = element_text(size =25)) +
  coord_cartesian(clip = "off")
graph5 



# Mismatch t+1 ------------------------------------------------------------

## ----predictions - effect on mismatch t+1-------------------
newd <- data.frame()
nd <- expand.grid(mass_fall_z=seq(min(mismatchlamb_tp1$mass_fall_z, na.rm = T), max(mismatchlamb_tp1$mass_fall_z, na.rm =T), length = 100),
                  yr = 99999, 
                  ID = "E12"
) 
newd <- rbind(newd,nd) 

myfun <- function(x) predict(x,newdata=newd,type="response",re.form=NA,allow.new.levels=T) 
boo <- bootMer(m4mr, myfun, 1000, seed = NULL, use.u = FALSE, re.form=NA,verbose = F) 
boo <- data.frame(boo)
newd$predi <- apply(boo, 2, mean) 
newd$upr <- apply(boo, 2, function(x) quantile(x, 0.975))
newd$lwr <- apply(boo, 2, function(x) quantile(x, 0.025))

max(newd$predi)

#unscale
newd$mass_fall_unsc <- ((newd$mass_fall_z * sd(mismatchlamb_tp1$mass_fall, na.rm=T))) + mean(mismatchlamb_tp1$mass_fall, na.rm=T) 

graph6 <- ggplot(data=newd, aes(x=mass_fall_unsc, y= predi)) + 
  geom_smooth(se=F,size = 2.5, colour = "cadetblue4")+
  geom_ribbon(aes(ymax=upr, ymin=lwr), alpha = 0.2) +
  geom_point(data=mismatchlamb_tp1, aes(x=mass_fall, y=greenupmis_tp1), size=2) + 
  geom_hline(yintercept=17, colour="lightcoral", size=2, linetype="dashed") +
  geom_hline(yintercept=2.5, colour="lightcoral", size=2, linetype="dashed") +
  scale_y_continuous(breaks=seq(-30, 80, 20)) +
  scale_x_continuous(breaks=seq(55,85, 5)) +
  labs(x="Fall Mass t (kg)",y="Mismatch t+1 (days)") +  
  theme(legend.position ="topright") +
  cowplot :: theme_cowplot() +
  theme(panel.grid.major = element_line(color="lightgrey", linetype = 'dotted')) +
  theme(axis.text=element_text(size=25)) +
  theme(axis.title = element_text(size =25)) +
  coord_cartesian(clip = "off")
graph6 





# EXTRAS ------------------------------------------------------------------


# extra<-
#   ggplot(data = newdat_a, aes(x = wean_mass_unsc, y = predi)) + # show all age class in Sup
#   geom_smooth(se = F,
#               size = 1.5,
#               colour = 'cadetblue') +
#   geom_ribbon(aes(ymax = max, ymin = min), alpha = 0.2) +
#   # geom_point(data=consec_df_filtered, # remove NA from response, and outliers mismatch
#   #            aes(x=mass_fall_z, y=as.numeric(as.factor(wean_tp1))-1), # trick to put a yes-no into numeric
#   #            size=1.5)
#   geom_point(
#     data = consec_df_filtered,
#     aes(x = mass_fall_raw, y = as.numeric(as.factor(wean_tp1)) - 1),
#     # trick to put a yes-no into numeric
#     size = 1.5
#   ) +
#   scale_y_continuous(limits = c(0, 1), breaks = seq(0, 1, 0.1)) +
#   scale_x_continuous(limits = c(45, 85), breaks = seq(45, 85, 5)) +
#   labs(x = "Lamb weaning mass (kg)",
#        y = expression("Probability to survive to weaning" [ ~ t + 1])) +
#   cowplot::theme_cowplot(10) +
#   theme(
#     panel.grid.major = element_line(color = "lightgrey", linetype = 'dotted'),
#     plot.background = element_rect(fill = 'white')
#   )
# extra

#ggsave('extra.figure.png',width=7, height=5, units='in',dpi=300)





# ATTENTION THIS IS NOT EXACTLY THE SAME DF 
# rename in analysis script




# panel b - birthdate t+1
# selected model for birthdate_tp1
# m11 <- lmer(birthdate_tp1~mass_fall_z + mass_gain + (1|ID) + (1|yr), data = consec_df_filtered, na.action=na.exclude, REML=T)
# 
# m4 <-
#   lmer(
#     birthdate_tp1 ~ mass_fall_z + (1|ID)  + (1|yr), # selected
#     data = birthdate_df,
#     na.action = na.exclude,
#     REML = TRUE
#   )
# 
# 
# # create prediction data 
# newdat_b <-
#   expand.grid(
#     mass_fall_z = seq(
#       min(birthdate_df$mass_fall_z, na.rm = T),
#       max(birthdate_df$mass_fall_z, na.rm = T),
#       length = 200
#     ),
#     yr = 99999,
#     ID = "E12"
#   )
# 
# # generate CIs
# myfun <- function(x) predict(x,newdata=newdat_b,type="response",re.form=NA) 
# boo <- bootMer(m4, myfun, 1000, seed = NULL, use.u = FALSE, re.form=NA,
#                verbose = T)
# 
# boo <- data.frame(boo)
# str(boo)
# 
# newdat_b$predi <- apply(boo, 2, mean) 
# newdat_b$max <- apply(boo, 2, function(x) quantile(x, 0.975))
# newdat_b$min <- apply(boo, 2, function(x) quantile(x, 0.025))
# 
# # unscale using values stored in summary_df USE the FULL df here
# sd <- summary_df[summary_df$variable=="mass_fall",'sd']
# mean <- summary_df[summary_df$variable=="mass_fall",'mean']
# newdat_b$mass_fall_unsc <- (newdat_b$mass_fall_z * sd) + mean
# birthdate_df$mass_fall_raw <- (birthdate_df$mass_fall_z * sd) + mean
# 
# fig5B <- ggplot(data=newdat_b, aes(x=mass_fall_unsc, y= predi)) +
#   geom_smooth(se=F,size = 1.5, colour='cadetblue') +
#   geom_ribbon(aes(ymax=max, ymin=min), alpha = 0.2) +
#   geom_point(data=birthdate_df, aes(x=mass_fall_raw, y=birthdate_tp1), size=1.5) + 
#   scale_y_continuous(limits=c(130,205), breaks=seq(130,205,5)) +
#   scale_x_continuous(limits=c(50,85), breaks=seq(50,85,5)) +
#   labs(x="Female autumn mass (kg)",
#        y= expression("Parturition date" [ ~ t + 1])) + 
#   cowplot :: theme_cowplot(10) +
#   theme(panel.grid.major = element_line(color="lightgrey", linetype = 'dotted'),
#         plot.background = element_rect(fill = 'white'))
# 
# fig5B

