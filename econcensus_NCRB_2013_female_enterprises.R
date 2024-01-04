setwd("/Users/saeehatwalne/Desktop/APU 2023-25/shrug-ec13-dta")
library(haven)
library(tidyverse)
library(ggplot2)
library(dplyr)
library(ggpubr)
library(tidyr)
library(car)
library(sf)
library(igraph)
library(corrplot)
# install.packages("psych")
library(psych)
library(Hmisc)
library(zoo)
library(stargazer)
# install.packages("writexl")
library(writexl)
# install.packages("olsrr")
library(olsrr)
library(lmtest)
rm(list=ls())
#MERGINGS

#name versus ID data - unique MYID creation
shrid_loc_names <- read_dta("shrid_loc_names.dta")
shrid_loc_names <- shrid_loc_names%>%
  select("shrid2","state_name","district_name")
shrid_loc_names$MYID <- shrid_loc_names$shrid2
shrid_loc_names$MYID <- as.character(shrid_loc_names$MYID)
shrid_loc_names$MYID <- substring(shrid_loc_names$MYID, 4)
# shrid_loc_names$MYID <- substr(shrid_loc_names$MYID,4 ,6)
shrid_loc_names$MYID <- substr(shrid_loc_names$MYID, 1,6)

shrid_loc_names <- shrid_loc_names %>%
  select('MYID', "state_name", "district_name")
unique_pairs <- unique(shrid_loc_names[, c("MYID", "district_name", "state_name")])

#econ census data - unique MYID creation
load("ec13_pc11dist.RData")
ec13data <- ec13_pc11dist[,1:46]
ec13data$MYID <- paste(ec13data$pc11_state_id, ec13data$pc11_district_id, sep = "-")

#cleaning district level census data
district_census <- read_csv("india-districts-census-2011.csv")
district_census$`District name` <- tolower(district_census$`District name`)
district_census <- district_census %>%
  rename("district_name" = "District name")
district_census <- district_census %>%
  rename("state_name" = "State name")
district_census[district_census == "leh(ladakh)"] <- "leh ladakh"


#Merging district census with unique names dataframe
df1 <- merge(district_census, unique_pairs, by.x = 'district_name', by.y = 'district_name')

#Merging this merged dataframe with economic census dataframe
df2 <- merge(df1, ec13data, by.x = 'MYID', by.y = 'MYID')

df2 <- df2 %>%
  select(Population, Male, Female, Literate, Male_Literate, Female_Literate,
         SC, Male_SC, Female_SC, ST, Male_ST, Female_ST, Workers, Male_Workers,
         Female_Workers, Household_Workers, Hindus, Muslims, Christians,
         Sikhs, Buddhists, Jains, Others_Religions, Religion_Not_Stated,
         Households, Rural_Households, Urban_Households,
         state_name.x, state_name.y, ec13_emp_all, ec13_emp_f, ec13_emp_m, ec13_count_all,
         ec13_count_own_m, ec13_count_own_f, ec13_count_own_other, MYID, district_name,
         `District code`,Married_couples_1_Households, Households_with_Internet,
         Households_with_Scooter_Motorcycle_Moped, Workers, Household_Workers)

# rm(df1, shrid_loc_names, unique_pairs, district_census, ec13_pc11dist, ec13data)

#converting to proportion
df3 <- df2 %>%
  mutate(prop_emp_f = (df2$ec13_emp_f*100)/df2$ec13_emp_all) %>%
  mutate(prop_emp_m = (df2$ec13_emp_m*100)/df2$ec13_emp_all) %>%
  mutate(prop_own_f = (df2$ec13_count_own_f*100)/df2$ec13_count_all) %>%
  mutate(prop_own_m = (df2$ec13_count_own_m*100)/df2$ec13_count_all) %>%
  mutate(prop_f = (df2$Female*100)/df2$Population) %>%
  mutate(prop_m = (df2$Male*100)/df2$Population) %>%
  mutate(prop_lit_f = (df2$Female_Literate*100)/df2$Literate)%>%
  mutate(prop_lit_m = (df2$Male_Literate*100)/df2$Literate) %>%
  mutate(prop_hh_r = (df2$Rural_Households*100)/df2$Households) %>%
  mutate(prop_hh_u = (df2$Urban_Households*100)/df2$Households) %>%
  mutate(prop_hindu = (df2$Hindus*100)/df2$Population) %>%
  mutate(prop_muslim = (df2$Muslims*100)/df2$Population) %>%
  mutate(prop_hh_scooter = (df2$Households_with_Scooter_Motorcycle_Moped*100)/df2$Households) %>%
  mutate(prop_hh_internet = (df2$Households_with_Internet*100)/df2$Households) %>%
  mutate(prop_sc = (df2$SC*100)/df2$Population) %>%
  mutate(prop_hh_workers = (df2$Household_Workers*100)/df2$Workers)
df4 <- df3 %>%
  select(MYID, district_name, `District code`, state_name.x, state_name.y,
         prop_emp_f, prop_emp_m, prop_own_f, prop_own_m, prop_m, prop_f, prop_lit_f,
         prop_lit_m, prop_hh_r, prop_hindu, 
         prop_hh_internet, prop_hh_scooter, prop_sc, prop_hh_workers, prop_muslim, Population)


# #Merging this merged dataframe with crime dataframe
cr13 <- read_dta("crimes2013.dta")
cr13$district <- tolower(cr13$district)
cr13 <- cr13 %>%
  mutate(cr_women = cr13$rape + cr13$custodialrape + cr13$otherrape +
           cr13$kidnappingandabductionofwome + cr13$assaultonwomenwithintentto +
           cr13$insulttomodestyofwomen + cr13$crueltybyhusbandorhisrelati +
           cr13$importationofgirlsfromforeig)
# cr_df <- cr13 %>%
  
# cr13 <- cr13 %>%
#   mutate(prop_cr_women = (cr13$cr_women*100)/cr13$totalipccrimes)
# cr13 <- cr13 %>%
#   select(prop_cr_women, district, stateut, year)

#merging with crime data
df5 <- merge(df4, cr13, by.x = "district_name", by.y = "district")
df5 <- df5 %>%
  mutate(cr_rate_women = (df5$cr_women*100000)/df5$Population)
#selected variables only df for corr matrix
df_corr <- df5 %>%
  select(cr_rate_women, prop_emp_f, prop_own_f, prop_lit_f,prop_hindu,
         prop_hh_internet, prop_hh_scooter, prop_hh_workers, prop_muslim,
         prop_f, prop_hh_r, prop_sc, Population)
#correlation matrix
mcor <- cor(df_corr)
corrplot(mcor, method = "shade", shade.col = NA, 
         tl.col = "black", tl.srt = 45, tl.cex = 0.6,
         addCoef.col = "black", number.cex=0.5)

#correlation matrix
mcor <- cor(df_corr)
corrplot(mcor, method = "shade", shade.col = NA, 
         tl.col = "black", tl.srt = 45, tl.cex = 0.6,
         addCoef.col = "black", number.cex=0.5)

par(mfrow = c(1,1))

# #HEATMAPS FOR SELECTED VARIABLES
district_shp <- st_read("/Users/saeehatwalne/Desktop/APU 2023-25/shrug-ec13-dta/disrtict_shapefiles/2011_Dist.shp")
ggplot(district_shp) + geom_sf()
load("/Users/saeehatwalne/Desktop/APU 2023-25/Econometrics_wR/state_codes.RData")

district_codes <- df4 %>%
  select(`District code`, district_name)

#heatmap without crime data - 633 districts
district_df1 <- merge(df4, district_codes, by.x = "District code", by.y = "District code")
district_df1_map = merge(district_shp, district_df1, by.x = "censuscode", by.y = "District code", all.x = TRUE)

ggplot(district_df1_map) + geom_sf(aes(fill = prop_own_f)) + scale_fill_viridis_c() +
  labs(title="Percentage of female enterprise ownership in districts in India")

#Heatmap for crime and other data - only 469 districts
district_df2 <- merge(df5, district_codes, by.x = "District code", by.y = "District code")
district_df2_map = merge(district_shp, district_df2, by.x = "censuscode", by.y = "District code", all.x = TRUE)

ggplot(district_df2_map) + geom_sf(aes(fill = cr_rate_women)) + scale_fill_viridis_c() +
  labs(title="Crime rate against women (per 1 lakh)")
ggplot(district_df2_map) + geom_sf(aes(fill = prop_own_f)) + scale_fill_viridis_c() +
  labs(title="Percentage of female enterprise ownership in districts in India")

#SUMMARY STATISTICS
#dataframe for summary statistics without the state names etc.
#only district names and variables kept
psych::describe(df_corr)
summary_df1 <- psych::describe(df_corr)
# Print or save the summary table
print(summary_df1)
write_xlsx(summary_df1,"/Users/saeehatwalne/Desktop/APU 2023-25/shrug-ec13-dta/summarytable.xlsx")
ggplot(df_corr, aes(x = prop_own_f)) +
  geom_density() + labs(title = "Density plot for percentage female owners")
#histograms for some variables
par(mfrow = c(3,2))
hist(df_corr$cr_rate_women, xlab = "crime rate against women", main = "crime rate against women" )
hist(df_corr$prop_emp_f, xlab = "proportion of females employed", main = "proportion of females employed")
hist(df_corr$prop_own_f, xlab = "proportion of female owners", main = "proportion of female owners")
hist(df_corr$prop_lit_f, xlab = "proportion of literate females", main = "proportion of literate females")
hist(df_corr$prop_hh_r, xlab = "proportion of rural households", main = "proportion of rural households" )
hist(df_corr$prop_f, xlab = "proportion of females", main = "proportion of females")

#maximum districts
df5$state_name.x[which.max(df5$cr_rate_women)]
#maxiumum states
state_with_max_population <- df5$stateut[which.max(df5$prop_emp_f)]
#maximum crime rate
state_with_max_population <- df5$stateut[which.max(df5$prop_emp_f)]

#mean proportion of ownership by females
mean(df5$prop_own_f) #10.37
median(df5$prop_own_f) #8.30

#hypothesis testing
#is there any difference in mean prop_emp_f for in districts where prop_own_f > 10% and <=10%?
#H_0 : mean_prop_emp_f_over10 = mean_prop_emp_f_under10 i.e. mean_prop_emp_f_over10 - mean_prop_emp_f_under10 = 0
#this means H-0 : No significant difference in mean prop_emp_f for in districts where prop_own_f > 20% and <=20%

mean_prop_emp_f_over8 <- df5 %>%
  filter(prop_own_f > 8.3) %>%
  select(prop_emp_f)
#note: age>= 20 assumed
mean_prop_emp_f_under8 <- df5 %>%
  filter(prop_own_f <= 8.3) %>%
  select(prop_emp_f)
#note: 95% confidence level taken - just wanted to see
t.test(mean_prop_emp_f_over8,mean_prop_emp_f_under8,
       alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95
)

#literacy and employment
mean(df5$prop_lit_f) #42.2
mean_prop_lit_f_over42 <- df5 %>%
  filter(prop_lit_f > 42.23) %>%
  select(prop_emp_f)
#note: age>= 20 assumed
mean_prop_lit_f_under42 <- df5 %>%
  filter(prop_lit_f <= 42.23) %>%
  select(prop_emp_f)
#note: 95% confidence level taken - just wanted to see
t.test(mean_prop_lit_f_over42,mean_prop_lit_f_under42,
       alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95
)

#crime rate and employment
median(df5$cr_rate_women) #21.79
mean(df5$cr_rate_women) #76.08
median_cr_rate_women_under21 <- df5 %>%
  filter(cr_rate_women < 21.79) %>%
  select(prop_emp_f)
#note: age>= 20 assumed
median_cr_rate_women_over21 <- df5 %>%
  filter(cr_rate_women > 21.79) %>%
  select(prop_emp_f)
#note: 95% confidence level taken - just wanted to see
t.test(median_cr_rate_women_under21,median_cr_rate_women_over21,
       alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95
)

#crime rate and ownership
median_cr_rate_women_under21 <- df5 %>%
  filter(cr_rate_women < 21.79) %>%
  select(prop_own_f)
#note: age>= 20 assumed
median_cr_rate_women_over21 <- df5 %>%
  filter(cr_rate_women > 21.79) %>%
  select(prop_own_f)
#note: 95% confidence level taken - just wanted to see
t.test(median_cr_rate_women_under21,median_cr_rate_women_over21,
       alternative = c("two.sided"),
       mu = 0, paired = FALSE, var.equal = FALSE,
       conf.level = 0.95
)


#scatter plots
par(mfrow = c(1, 1))
#scatter plot - only females
ggplot(df5, aes(x=prop_own_f, y = prop_emp_f)) + geom_point() + 
  geom_smooth(method = "lm") + stat_regline_equation() +
  labs(title = "Proportion of female ownership of enterprises \n & proportion of female employment", 
       x = "Proportion of female owners of enterprises", 
       y = "Proportion of female employees") + 
  theme(plot.title = element_text(
         size = rel(1.5), lineheight = .9,
         family = "Times", face = "bold.italic", colour = "black"
       )) + theme(axis.title.x = element_text(
         size = 16, lineheight = .9
       )) + theme(axis.title.y = element_text(
         size = 16, lineheight = .9
       ))
#scatter plot - only males and only females
ggplot(df5, aes(x=prop_own_f, y = prop_emp_f)) + geom_point() + 
  geom_smooth(method = "lm") + stat_regline_equation() +
  labs(title = "Proportion of female ownership of enterprises \n & proportion of female employment", 
       x = "Proportion of female owners of enterprises", 
       y = "Proportion of female employees") + 
  theme(plot.title = element_text(
    size = rel(1.5), lineheight = .9,
    family = "Times", face = "bold.italic", colour = "black"
  )) + theme(axis.title.x = element_text(
    size = 16, lineheight = .9
  )) + theme(axis.title.y = element_text(
    size = 16, lineheight = .9
  ))

#scatter plot - males and females
ggplot(df5, aes(x = prop_own_f, y = prop_emp_f)) +
  geom_point(aes(color = "Female")) + 
  geom_point(aes(x = prop_own_m, y = prop_emp_m, color = "Male")) +
  labs(x = "Ownership Proportion", y = "Employment Proportion", title = "Scatter Plot of percentage ownership \n vs percentage employment") +
  scale_color_manual(values = c("Female" = "hotpink", "Male" = "blue"), guide = guide_legend(title = "Gender")) +
  theme(plot.title = element_text(
    size = rel(1.5), lineheight = .9,
    family = "Times", face = "bold.italic", colour = "black"
  )) + theme(axis.title.x = element_text(
    size = 16, lineheight = .9
  )) + theme(axis.title.y = element_text(
    size = 16, lineheight = .9
  ))

#regression trial
fit1 = lm(prop_emp_f ~ prop_own_f, data = df5)
summary(fit1)
summary_table_fit1 <- stargazer(fit1, type = "html", title = "Regression Summary")
# Print or save the summary table
print(summary_table_fit1)
writeLines(summary_table_fit1, "regression_summary.html")

#step wise regression building
mymodel = lm(prop_emp_f ~ prop_own_f + prop_lit_f + prop_hh_r + 
               prop_hindu + prop_hh_internet + cr_rate_women, data = df5)
summary(mymodel)
forwardfit.p <- ols_step_forward_p(mymodel, penter = 0.05)
forwardfit.p

#stepwise regression models
m1 <- lm(prop_emp_f ~ prop_own_f, data = df5)
m2 <- lm(prop_emp_f ~ prop_own_f + prop_lit_f + prop_hh_r + prop_hh_internet + cr_rate_women, data = df5)
m3 <- lm(prop_emp_f ~ prop_own_f + prop_lit_f + prop_hh_r + prop_hh_internet + cr_rate_women + prop_hh_scooter + prop_hindu + prop_muslim + prop_sc, data = df5)
tablefit <- stargazer (m1, m2, m3, type="html")
summary(tablefit)
print(tablefit)
writeLines(tablefit, "regression_summary4.html")
#LR test of the models
lrtest<-lrtest(m1, m2, m3)
lrtest
#AIC of the models
AIC(m1, m2,m3)
#ANOVA of the models
anova <- anova(m1,m2,m3)
anova
#multicollinearity - variance inflation factor
viftable <- as.data.frame(vif(m3))
print(viftable)
par(mfrow = c(1,1))

#Bootstrapped confidence intervals and standard errors
boot_reg <- function(data, idx){
  m3 <- lm(prop_emp_f ~ prop_own_f + prop_lit_f + prop_hh_r + prop_hh_internet + cr_rate_women + prop_hh_scooter + prop_hindu + prop_muslim + prop_sc, data = df5[idx,])
  coef(m3)
}
b <- boot::boot(df5, boot_reg, 1000)
boot::boot.ci(b, type = "perc")
boot::boot.ci(b, index = 2, type = "perc")
summary(m3)
summary(b)
#Printing bootstrapped standard errors
boot_se <- apply(b$t, 2, sd)
print(boot_se)
# Print original OLS standard errors
(summary(m3)$coefficients[, "Std. Error"])
#original confidence intervals
confint(m3)


ols_coef <- coef(m3)
ols_se <- summary(m3)$coefficients[, "Std. Error"]

# Extract bootstrap coefficients
boot_coef <- colMeans(b$t)
plot_data <- data.frame(
  Coefficient = names(ols_coef),
  OLS_Estimate = ols_coef,
  Bootstrap_Mean = boot_coef
)
plot_data_long <- gather(plot_data, key = "Estimate_Type", value = "Estimate", -Coefficient)



# par(mfrow = c(1,1))
# ggplot(plot_data, aes(x = Coefficient, y = OLS_Estimate)) +
#   geom_bar(stat = "identity", fill = "blue") +
#   ggtitle("OLS Coefficients") +
#   ylim(range(c(plot_data$OLS_Estimate, plot_data$Bootstrap_Mean))) +
#   ylab("Estimate") +
#   theme_minimal()
# 
# ggplot(plot_data, aes(x = Coefficient, y = Bootstrap_Mean)) +
#   geom_bar(stat = "identity", fill = "green") +
#   ggtitle("Bootstrap Coefficients") +
#   ylim(range(c(plot_data$OLS_Estimate, plot_data$Bootstrap_Mean))) +
#   ylab("Estimate") +
#   theme_minimal()


#Weighted least squares regression
#create residual vs. fitted plot
par(mfrow = c(1,1))
plot(fitted(m3), resid(m3), xlab='Fitted Values', ylab='Residuals', main = "Variance of errors in M3")
#add a horizontal line at 0 
abline(0,0)
#Breusch Pagan Test
# bptest(m3)
# m2 <- lm(prop_emp_f ~ prop_own_f + prop_lit_f + prop_hh_r + prop_hh_internet + cr_rate_women, data = df5)
# bptest(m2, ~ prop_own_f*prop_lit_f + prop_, data = mtcars)
m3 <- lm(prop_emp_f ~ prop_own_f + prop_lit_f + prop_hh_r + prop_hh_internet + cr_rate_women + prop_hh_scooter + prop_hindu + prop_muslim + prop_sc, data = df5)
wt <- 1 / lm(abs(m3$residuals) ~ m3$fitted.values)$fitted.values^2
#perform weighted least squares regression
wls_model <- lm(prop_emp_f ~ prop_own_f + prop_lit_f + prop_hh_r + 
                  prop_hh_internet + cr_rate_women + prop_hh_scooter + 
                  prop_hindu + prop_muslim + prop_sc, data = df5, weights=wt)
summary(wls_model)
tablefit <- stargazer (wls_model, type="html")
summary(tablefit)
print(tablefit)
writeLines(tablefit, "regression_summarywls.html")
