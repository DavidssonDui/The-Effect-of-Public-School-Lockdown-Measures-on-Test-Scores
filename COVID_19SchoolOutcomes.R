rm(list=ls()) 

# Load Packages
library(tidyverse)
library(modelsummary)
library(plm)
library(stargazer)
library(tableone)
library(knitr)
library(MatchIt)
library(cobalt)
library(maps)
library(patchwork)
#####

# Set working directory
# only line to change 
yourfiledirec=paste("/Users/computerboi/Downloads/ReplicationPackage")



inputdirecraw=paste(yourfiledirec,"/Microdata/raw/",sep="")
inputdirecderived=paste(yourfiledirec,"/Microdata/derived/",sep="")

codedirec=paste(yourfiledirec,"/Code/",sep="")
outputdirec=paste(yourfiledirec,"/Output/",sep="")
############

# 1. Clean Data 

setwd(inputdirecraw)

## Read in raw data
polidata <- read_csv("countypres_2000-2020.csv")
seda2024 <- read_csv("seda_admindist_long_gys_2024.1.csv")
CSHD <- read_csv("District_Monthly_Shares_03.08.23.csv")
covidcases <- read_csv("us-counties.csv")
seda2024cov <- read_csv("seda_cov_admindist_annual_2024.1.csv")

## Read in NCES (modified outside of R to remove heading)
setwd(inputdirecderived)
nces <- read_csv("ELSI_csv_export_6387938257895347813504.csv")

## Filter SEDA2024 for years 2018-2024 (excluding 2020, 2021), mathscores, and standardize district name
seda2024mth <- seda2024 |> 
  filter(year %in% c(2016 ,2017 ,2018, 2019, 2022, 2023, 2024)) |> 
  filter(subject == "mth") |> 
  rename(district = sedaadminname) |> 
  mutate(blkgys = if_else(is.na(gys_mn_blk), 1, 0))

seda2024mth$district <- tolower(seda2024mth$district)


## Summarize number of months that district had over 80% of schools in lockdown in the COVID-19 School Data Hub
CSHD <- CSHD |> 
  mutate(DistrictName = tolower(DistrictName)) |> 
  rename(stateabb = StateAbbrev) |> 
  rename(district = DistrictName) |> 
  group_by(stateabb, district) |> 
  summarise(months_virtual = sum(share_virtual>0.8, na.rm = TRUE))


## Calculate mean average for COVID-19 cases and deaths per 100k across time for each county
covidcases <- covidcases |> 
  mutate(county = tolower(county)) |> 
  mutate(state = tolower(state)) |> 
  group_by(county, state) |> 
  summarise(caseavgp100k = mean(cases_avg_per_100k, na.rm = TRUE), deathavgp100k = mean(deaths_avg_per_100k, na.rm = TRUE))

## Clean NCES data such that districts can be linked to voting and covid data through county name
nces <- nces |> 
  rename(district = `Agency Name`) |> 
  rename(stateabb = `State Abbr [District] Latest available year`) |> 
  rename(state = `State Name [District] Latest available year`) |> 
  rename(pupltch = `Pupil/Teacher Ratio [District] 2021-22`) |> 
  rename(instexpstd = `Instructional Expenditures (E13) per Pupil (V33) [District Finance] 2021-22`) |> 
  mutate(across(c(`County Name [District] 2017-18`, `County Name [District] 2018-19`,
                  `County Name [District] 2021-22`, `County Name [District] 2022-23`,`County Name [District] 2023-24`),
                ~ na_if(.x, "†"))) |> 
  mutate(county = coalesce(`County Name [District] 2017-18`, `County Name [District] 2018-19`,
                           `County Name [District] 2021-22`, `County Name [District] 2022-23`, `County Name [District] 2023-24`)) |> 
  mutate(county = str_remove(county, regex("County", ignore_case = TRUE)) |> str_squish()) |> 
  mutate(state = tolower(state)) |> 
  mutate(county = tolower(county)) |> 
  mutate(district = tolower(district)) |> 
  select(-c(`County Name [District] 2017-18`, `County Name [District] 2018-19`,
            `County Name [District] 2021-22`, `County Name [District] 2022-23`, `County Name [District] 2023-24`)) |> 
  mutate(pupltch = as.numeric(pupltch)) |> 
  mutate(instexpstd = as.numeric(instexpstd))


## Filter out SEDA2024 school demographic data for relevant years
seda2024cov <- seda2024cov |> 
  filter(year %in% c(2016,2017,2018, 2019, 2022, 2023, 2024))

## Clean up of voting data and calculation of share of county that voted for the democratic party
polidata <- polidata |> 
  filter(year == 2020) |> 
  rename(stateabb = state_po) |> 
  rename(county = county_name) |> 
  filter(party == "DEMOCRAT") |> 
  group_by(year, stateabb, county, candidate) |> 
  summarise(total_candidatevotes = sum(candidatevotes, na.rm = TRUE), 
            totalvotes = max(totalvotes, na.rm = TRUE), 
            .groups = "drop")

polidata <- polidata |> 
  group_by(county, stateabb) |> 
  summarise(demshare = total_candidatevotes/totalvotes)

polidata$county <- tolower(polidata$county)


## Merging of datasets

sedamerged <- merge(seda2024mth, CSHD, by = c("district", "stateabb")) 

sedamerged <- merge(sedamerged, nces, by = c("district", "stateabb"))

sedamerged <- merge(sedamerged, covidcases, by = c("county", "state"))

sedamerged <- merge(sedamerged, polidata, by = c("county", "stateabb"))

sedamerged <- merge(sedamerged, seda2024cov, by = c("year", "sedaadmin"))

sedamerged <- sedamerged |> 
  filter(totenrl >= 1000)

# save file to the derived directory 
write.csv(sedamerged, paste(inputdirecderived,"sedamerged.csv", sep="") )


############## Plots and Regressions: 
# Read in the clean file 
df=read.csv(paste(inputdirecderived,"sedamerged.csv", sep=""))

## Plotting of U.S map of sample location and districution

county_counts <- sedamerged |> 
  filter(grade == 3) |> 
  filter(year == 2019) |> 
  count(state, county) |> 
  mutate(state = tolower(state),
         county = tolower(county))

us_counties <- map_data("county")  # from base 'maps' package


map_data_joined <- us_counties |> 
  left_join(county_counts, by = c("region" = "state", "subregion" = "county"))

ggplot(map_data_joined, aes(long, lat, group = group, fill = n)) +
  geom_polygon(color = "white", size = 0.1) +
  coord_fixed(1.3) +
  scale_fill_viridis_c(option = "magma", na.value = "gray90") +
  labs(title = "Distribtion of School District by County") + 
  theme_minimal() +
  theme(
    panel.grid = element_blank(), 
    panel.background = element_blank(), 
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none"
  )
ggsave( paste(outputdirec,"Figure1.pdf"))

## Summary Statistics before and after cleaning/merging

vars1 <- c("gys_mn_all", "gys_mn_blk", "gys_mn_hsp", "gys_mn_wht", "gys_mn_ecd")
vars2 <- c("months_virtual")
vars3 <- c("pupltch", "instexpstd")
vars4 <- c("caseavgp100k", "deathavgp100k")
vars5 <- c("totenrl", "enrl38", "perblk", "perhsp", "perfrl", "perwht", "urban")
vars6 <- c("demshare")

# All variables to compare

all_vars <- c(vars1, vars2, vars3, vars4, vars5, vars6)

# Labels to use later for dataset grouping
group_labels <- c(
  rep("SEDA2024", length(vars1)),
  rep("COVID-19 School Data Hub", length(vars2)),
  rep("NCES Common Core Data", length(vars3)),
  rep("NYT COVID-19 Data Repository", length(vars4)),
  rep("SEDA2024 Covariates", length(vars5)),
  rep("MIT Election Labs", length(vars6))
)

mean_raw <- sapply(seda2024[vars1], mean, na.rm = TRUE)
mean_clean <- sapply(sedamerged[vars1], mean, na.rm = TRUE)

mean_raw <- c(mean_raw, sapply(CSHD[vars2], mean, na.rm = TRUE))
mean_clean <- c(mean_clean, sapply(sedamerged[vars2], mean, na.rm = TRUE))

mean_raw <- c(mean_raw, sapply(nces[vars3], mean, na.rm = TRUE))
mean_clean <- c(mean_clean, sapply(sedamerged[vars3], mean, na.rm = TRUE))

mean_raw <- c(mean_raw, sapply(covidcases[vars4], mean, na.rm = TRUE))
mean_clean <- c(mean_clean, sapply(sedamerged[vars4], mean, na.rm = TRUE))

mean_raw <- c(mean_raw, sapply(seda2024cov[vars5], mean, na.rm = TRUE))
mean_clean <- c(mean_clean, sapply(sedamerged[vars5], mean, na.rm = TRUE))

mean_raw <- c(mean_raw, sapply(polidata[vars6], mean, na.rm = TRUE))
mean_clean <- c(mean_clean, sapply(sedamerged[vars6], mean, na.rm = TRUE))

summarystats <- data.frame(
  Dataset = group_labels,
  Variable = all_vars,
  Mean_Before = mean_raw,
  Mean_After = mean_clean
)

summarystats <- summarystats[order(summarystats$Dataset), ]

stargazer(summarystats, out=paste(outputdirec,"Table1.txt"), summary = FALSE, rownames = FALSE,
          title = "Comparison of Means Before and After Cleaning by Dataset")

## Plotting of unweighted and weighted averages of GYS district mean test score by months in remote/virtual learning

weighted_mean_fun <- function(x, w) {
  sum(x * w) / sum(w)
}

density <- sedamerged |> 
  group_by(months_virtual) |> 
  mutate(n = n()) |>   # count of districts per x value
  ungroup()

df_weighted <- density |> 
  filter(!(is.na(months_virtual))) |> 
  group_by(months_virtual, n) |> 
  summarise(weighted_avg = sum(gys_mn_all*totenrl) / sum(totenrl))


 ggplot(density, aes(x = months_virtual, y = gys_mn_all)) +
  stat_summary(fun = mean, aes(color = n), size = 0.5) +  
  labs(title = "District Test Scores by Months in Lockdown", x = "Total Months When Over 80% of Schools in Remote Learning", y = "District Mean Test Score, Grade-Subject-Year, All Students") +
  scale_color_viridis_c() +
  theme_minimal() + 
  theme(
    panel.grid = element_blank(), 
    panel.background = element_blank(),  
    axis.line = element_line(color = "black"),
  )
 
 ggsave( paste(outputdirec,"Figure2.pdf"))
 
 ggplot(df_weighted, aes(x = months_virtual, y = weighted_avg)) +
  geom_point(aes(color = n), size = 3) +
  labs(title = "District Test Scores by Months in Hyrbid, Weighted by Number of Students", x = "Total Months When Over 80% of Schools in Hyrbid Learning", y = "District Mean Test Score, Grade-Subject-Year, All Students", color = "Number of Districts") +
  scale_color_viridis_c() +
  theme_minimal() +  
  theme(
    panel.grid = element_blank(),  
    panel.background = element_blank(), 
    axis.line = element_line(color = "black")  
  )

 ggsave( paste(outputdirec,"Figure3.pdf"))

## Plotting of months in remote learning on socio-economic indiactors
 
 ggplot(sedamerged, aes(x = months_virtual, y = pupltch)) +
   stat_summary(fun = mean, fill = "black", size = 0.5) +  
   labs(title = "Pupil-Teacher Ratio by Months in Lockdown", x = "Total Months When Over 80% of Schools in Remote Learning", y = "Pupil-Teacher Ratio") +
   scale_color_viridis_c() +
   theme_minimal() + 
   theme(
     panel.grid = element_blank(), 
     panel.background = element_blank(),  
     axis.line = element_line(color = "black"))
 ggsave( paste(outputdirec,"Figure4.pdf"))
 
 ggplot(sedamerged, aes(x = months_virtual, y = single_momavgall)) +
   stat_summary(fun = mean, fill = "black", size = 0.5) +  
   labs(title = "District Single Mother HH Rate by Months in Lockdown", x = "Total Months When Over 80% of Schools in Remote Learning", y = "Single Mother HH Rate, All Families, 2005-09, 2010-14 & 2015-19 avg") +
   scale_color_viridis_c() +
   theme_minimal() + 
   theme(
     panel.grid = element_blank(), 
     panel.background = element_blank(),  
     axis.line = element_line(color = "black"))
 ggsave( paste(outputdirec,"Figure5.pdf"))
 
 ggplot(sedamerged, aes(x = months_virtual, y = perhsp)) +
   stat_summary(fun = mean, fill = "black", size = 0.5) +  
   labs(title = "District Percentage of Hispanic Students by Months in Lockdown", x = "Total Months When Over 80% of Schools in Remote Learning", y = "Percentage of Hispanic Students") +
   scale_color_viridis_c() +
   theme_minimal() + 
   theme(
     panel.grid = element_blank(), 
     panel.background = element_blank(),  
     axis.line = element_line(color = "black"))
 ggsave( paste(outputdirec,"Figure8.pdf"))
 
## Plotting of distribution of schools over/equal to or under six months in remote/virtual learning
 
 seda6months <- sedamerged |> 
   distinct(district, .keep_all = TRUE) |> 
   mutate(treat = if_else(months_virtual >= 6, 1, 0))
 
 ggplot(seda6months, aes(x = factor(treat, levels = c(0, 1), labels = c("Under 6 Months", "Over/Equal to 6 Months")))) +
   geom_bar(width = 0.6, fill = c("red","blue") , bins = 2) +
   theme_classic() +
   theme(
     panel.background = element_blank(),
     plot.background = element_blank(),
     axis.line = element_blank(),
     axis.ticks = element_blank()
   ) +
   labs(
     x = "Months when over 80% of schools in remote leaning",
     y = "Number of Dsitricts",
     title = "Distribution of Districts in Remote Learning"
   ) + scale_x_discrete(labels = c("0.0" = "Under 6 Months", "1.0" = "Over 6 Months"))
 ggsave( paste(outputdirec,"Figure6.pdf"))
 
 seda1month <- sedamerged |> 
   distinct(district, .keep_all = TRUE) |> 
   mutate(treat = if_else(months_virtual >= 1, 1, 0))
 
 ggplot(seda1month, aes(x = factor(treat, levels = c(0, 1), labels = c("Zero Months", "At Least One Month")))) +
   geom_bar(width = 0.6, fill = c("red","blue") , bins = 2) +
   theme_classic() +
   theme(
     panel.background = element_blank(),
     plot.background = element_blank(),
     axis.line = element_blank(),
     axis.ticks = element_blank()
   ) +
   labs(
     x = "Months when over 80% of schools in remote leaning",
     y = "Number of Dsitricts",
   ) + scale_x_discrete(labels = c("0.0" = "Zero Months", "1.0" = "At Least One Month"))
 ggsave( paste(outputdirec,"Figure7.pdf"))
 
## Filter out for test scores on grade 3 students
seda2024merggr3 <- sedamerged |> 
  filter(grade == 3)


## OLS regressions of months spent in remote/virtual learning on GYS (Grade-Year-Standardized) district mean test scores 
## for grade 3 students, with and without district controls

olsmthnc <- lm(data = seda2024merggr3, gys_mn_all ~ months_virtual)

olsmthdc <- lm(data = seda2024merggr3, gys_mn_all ~ months_virtual + pupltch + caseavgp100k + deathavgp100k + enrl38 + instexpstd + urban + demshare + perfrl + perblk + perhsp)

## OLS two-way fixed effects regression of months in remote/virtual learning on GYS (Grade-Year-Standardized) district mean test scores 
## (all students, black students, hispanic students, economically disadvantaged students)

pdataseda2024 <- pdata.frame(seda2024merggr3, index = c("district", "state", "year"))

pdataseda2024 <- pdataseda2024[!duplicated(pdataseda2024[, c("district", "state", "year")]), ]

olsblk <-  plm(gys_mn_blk ~ months_virtual + pupltch + caseavgp100k + deathavgp100k + enrl38 + instexpstd + urban + demshare + perfrl + perblk + perhsp, data = pdataseda2024, model = "within", effect = "twoways")

olshsp <-  plm(gys_mn_hsp ~ months_virtual + pupltch + caseavgp100k + deathavgp100k + enrl38 + instexpstd + urban + demshare + perfrl + perblk + perhsp, data = pdataseda2024, model = "within", effect = "twoways")

olsasn <- plm(gys_mn_asn ~ months_virtual + pupltch + caseavgp100k + deathavgp100k + enrl38 + instexpstd + urban + demshare + perfrl + perblk + perhsp, data = pdataseda2024, model = "within", effect = "twoways")

olsECD <-  plm(gys_mn_ecd ~ months_virtual + pupltch + caseavgp100k + deathavgp100k + enrl38 + instexpstd + urban + demshare + perfrl + perblk + perhsp, data = pdataseda2024, model = "within", effect = "twoways")

olstwfe <- plm(gys_mn_all ~ months_virtual + pupltch + caseavgp100k + deathavgp100k + enrl38 + instexpstd + urban + demshare + perfrl + perblk + perhsp, data = pdataseda2024, model = "within", effect = "twoways")

## Summary of OLS regression results

stargazer(olsmthnc, olsmthdc, olstwfe, olsblk, olshsp, olsECD, out=paste(outputdirec,"Table2.txt"),
          title = "Regression Results",
          dep.var.labels = "Dependent Variable: Mean Test Score, all students",
          column.labels = c("No Controls", "District Controls", "DC & TW-FE", "Black", "Hispanic", "ECD"),
          digits = 3,                 # Limits decimal places for coefficients
          digits.extra = 2,
          omit = c("pupltch", "instexpstd", "enrl38"),  
          omit.stat = c("f", "ser"))


## Creation of Balence Table for districts with less than 6 months in remote/vitrual learning, and districts with over 6 months in remote/vitrual learning

seda2024mergbt <- sedamerged |> 
  filter(grade == 3) |> 
  mutate(treat = if_else(months_virtual >= 6, 1, 0))

covariates <- c("gys_mn_all", "pupltch", "caseavgp100k", "deathavgp100k", "instexpstd", "perblk", "perhsp", 
                "enrl38", "avgrd38", "perfrl", "perasn", "pernam", "peroth", "perwht", "totenrl", "sesavgall",       
                "lninc50avgall", "baplusavgall", "unempavgall", "snapavgall", "povertyavgall", "single_momavgall", "urban",            
                "suburb", "town", "rural")

seda2024mergbt <- seda2024mergbt |> 
  mutate(treat = case_when(
    treat %in% c("Yes", "yes", 1) ~ 1,
    treat %in% c("No", "no", 0) ~ 0,
    TRUE ~ NA_real_
  ))

balance_table <- CreateTableOne(vars = covariates, strata = "treat", data = seda2024mergbt, test = TRUE)

tab1_df <- print(balance_table, printToggle = FALSE, noSpaces = TRUE, smd = TRUE)

tab1_stargazer <- as.data.frame(tab1_df) |> 
  tibble::rownames_to_column("Variable") # Keep row names as a column

# Balance table results
stargazer(tab1_stargazer, out=paste(outputdirec,"Table3.txt") , summary = FALSE, title = "Balence Test Table", smd = TRUE)

## Plotting of binned scatterplot for single_motherhood rate on district mean test scores


df_binned <- sedamerged |> 
  mutate(bin = ntile(perfrl, 20)) |>  
  group_by(bin) |> 
  summarise(
    age_bin_mean = mean(perfrl, na.rm = TRUE),
    outcome_mean = mean(gys_mn_all, na.rm = TRUE)
  )

ggplot(df_binned, aes(x = age_bin_mean, y = outcome_mean)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Percentage of Students on FRL vs. GYS Test scores ",
       x = "Percentage of Students on FRL",
       y = "Average District GYS Test Score, All Students") +
  theme_minimal() + theme(
    panel.grid = element_blank(),  
    panel.background = element_blank(),  
    axis.line = element_line(color = "black")  
  )
ggsave( paste(outputdirec,"Figure11.pdf"))

df_binned <- sedamerged |> 
  mutate(bin = ntile(single_momavgall, 20)) |>  
  group_by(bin) |> 
  summarise(
    age_bin_mean = mean(single_momavgall, na.rm = TRUE),
    outcome_mean = mean(gys_mn_all, na.rm = TRUE)
  )

ggplot(df_binned, aes(x = age_bin_mean, y = outcome_mean)) +
  geom_point(size = 2) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(title = "Average Single Motherhood Rate vs. GYS Test Scores",
       x = "Single Mother Household Rate (2005-09, 2010-14 & 2015-19 Averages)",
       y = "Average District GYS Test Score, All Students") +
  theme_minimal() + theme(
    panel.grid = element_blank(),  
    panel.background = element_blank(),  
    axis.line = element_line(color = "black")  
  )
ggsave( paste(outputdirec,"Figure12.pdf"))

## Filtering of new merged data set for propensity score matching and DiD regression

sedamergpsm <- sedamerged |> 
  filter(totenrl >= 1000) |> 
  filter(grade == 3) |> 
  filter(!is.na(instexpstd)) |> 
  filter(!is.na(pupltch)) |> 
  filter(!is.na(lninc50avgall)) |> 
  filter(!is.na(sesavgall)) |> 
  filter(!is.na(baplusavgall)) |> 
  filter(!is.na(unempavgall)) |> 
  filter(!is.na(snapavgall)) |> 
  filter(!is.na(povertyavgall)) |> 
  filter(!is.na(single_momavgall)) |> 
  mutate(treat = if_else(months_virtual >= 1, 1, 0))

## Propensity score matching by various covariates

matchit_result <- matchit(treat ~
                            demshare + unempavgall + pupltch + single_momavgall +
                            perhsp + perasn + perfrl + povertyavgall + urban,
                          data = sedamergpsm,
                          method = "nearest",  # Options: nearest, optimal, full, genetic, etc.
                          ratio = 1)

matched_data <- match.data(matchit_result)

matched_data1 <- matched_data |> 
  mutate(post = if_else(year >= 2022, 1, 0)) |> 
  select(post, everything())

## Plotting of propensity score matching balance test

bal <- bal.tab(matchit_result, un = TRUE)
summary_df <- bal$Balance
summary_df$Variable <- rownames(summary_df)
rownames(summary_df) <- NULL


long_df <- pivot_longer(summary_df,
                        cols = c("Diff.Un", "Diff.Adj"),
                        names_to = "Time",
                        values_to = "SMD")

ggplot(long_df, aes(x = reorder(Variable, SMD), y = SMD, fill = Time)) +
  geom_col(position = "dodge") +
  scale_x_discrete(limits = rev(levels(reorder(long_df$Variable, long_df$SMD)))) +  # This reverses the x-axis order
  labs(x = "Covariates", y = "Absolute Std. Mean Difference",
       fill = "Before/After Matching") +
  theme(
    panel.grid = element_blank(),  
    panel.background = element_blank(),  
    axis.line = element_line(color = "black")
  ) +
  scale_fill_manual(values = c("Diff.Un" = "blue",  
                               "Diff.Adj" = "red"),
                    labels = c("Diff.Un" = "Unmatched Difference",
                               "Diff.Adj" = "Matched Difference"))
 ggsave( paste(outputdirec,"Figure9.pdf"))


balance <- bal.tab(matchit_result, un = TRUE, m.threshold = 0.1)
print(balance)

before <- balance$Balance$Diff.Un
after <- balance$Balance$Diff.Adj

# Perform paired t-test
t.test(before, after, paired = TRUE)

## DiD regression of whether a school spent time in remote/virtual learning on GYS district mean test scores before and after the 2020-2021 school year.
## Regressions for all students, with and without controls, as well as with district fixed effects for all students, black students, hispanic students,
## and economically disadvantaged students.

olsmth_dc <- lm(data = matched_data1, gys_mn_all ~ treat + post + treat*post)

pdataseda2024 <- pdata.frame(matched_data1, index = c("district", "state", "year"))

pdataseda2024 <- pdataseda2024[!duplicated(pdataseda2024[, c("district", "state", "year")]), ]

olsmth_dc1 <- lm(data = matched_data1, gys_mn_all ~ treat + post + treat*post + pupltch + caseavgp100k + deathavgp100k + enrl38 + instexpstd + urban + demshare + perfrl + perblk + perhsp)

olstwfe <- plm(gys_mn_all ~ treat + post + treat*post + pupltch + caseavgp100k + deathavgp100k + enrl38 + instexpstd + urban + demshare + perfrl + perblk + perhsp, data = pdataseda2024, model = "within", effect = "individual")

olstwfeblk <- plm(gys_mn_blk ~ treat + post + treat*post + pupltch + caseavgp100k + deathavgp100k + enrl38 + instexpstd + urban + demshare + perfrl + perblk + perhsp, data = pdataseda2024, model = "within", effect = "individual")

olstwfehsp <- plm(gys_mn_hsp ~ treat + post + treat*post + pupltch + caseavgp100k + deathavgp100k + enrl38 + instexpstd + urban + demshare + perfrl + perblk + perhsp, data = pdataseda2024, model = "within", effect = "individual")

olstwfeed <- plm(gys_mn_ecd ~ treat + post + treat*post + pupltch + caseavgp100k + deathavgp100k + enrl38 + instexpstd + urban + demshare + perfrl + perblk + perhsp, data = pdataseda2024, model = "within", effect = "individual")

olstwfewht <- plm(gys_mn_wht ~ treat + post + treat*post + pupltch + caseavgp100k + deathavgp100k + enrl38 + instexpstd + urban + demshare + perfrl + perblk + perhsp, data = pdataseda2024, model = "within", effect = "individual")

## Results from DiD regressions

stargazer(olsmth_dc, olsmth_dc1, olstwfe, olstwfeblk, olstwfehsp, out=paste(outputdirec,"Table4.txt"),
          title = "Regression Results",
          dep.var.labels = "Dependent Variable: Mean Test Score, all students",
          column.labels = c("No Controls", "District Controls", "DC & TW-FE", "Black", "Hispanic", "ECD"),
          digits = 3,                 # Limits decimal places for coefficients
          digits.extra = 2,
          omit = c("pupltch", "instexpstd", "enrl38"),  
          omit.stat = c("f", "ser"))


## Ploting of trends between the treated and control group

pt_means <- matched_data1 |> 
  group_by(year, treat) |> 
  summarise(
    mean = mean(gys_mn_all, na.rm = TRUE),
    se = sd(gys_mn_all, na.rm = TRUE) / sqrt(n()),
    .groups = "drop"
  ) |> 
  mutate(group = ifelse(treat == 1, "Treatment", "Control")) |> 
  select(-treat)

pt_diff <- pt_means |> 
  pivot_wider(names_from = group, values_from = c(mean, se)) %>%
  mutate(
    diff = mean_Treatment - mean_Control,
    se_diff = sqrt(se_Treatment^2 + se_Control^2),
    ci_lower = diff - 1.96 * se_diff,
    ci_upper = diff + 1.96 * se_diff
  )

ggplot(pt_diff, aes(x = year, y = diff)) +
  geom_line(color = "black", size = 1.2) +
  stat_summary(fun = mean, fill = "steelblue", color = "black", size = 0.5) +
  geom_line(aes(y = ci_upper), linetype = "dotted", color = "black") +
  geom_line(aes(y = ci_lower), linetype = "dotted", color = "black") +
  geom_hline(yintercept = pt_diff$diff[1], linetype = "solid", color = "red") +
  labs(
    title = "Difference in District Mean Test Scores between Treatment and Control Group",
    x = "Year",
    y = "Difference in GYS District Mean Test Score (Treatment - Control)"
  ) +
  theme(
    panel.grid = element_blank(), 
    panel.background = element_blank(), 
    axis.line = element_line(color = "black")  
  )
ggsave( paste(outputdirec,"Figure10.pdf"))

