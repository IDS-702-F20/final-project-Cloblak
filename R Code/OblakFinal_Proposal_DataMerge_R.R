dev.off()

library(ggplot2)
library(readxl) 
library(tibble)
library(tidyverse)
library(dbplyr)
library(rio)
library(tidyr)
library(readr)
library(jtools)
library(reshape)
library(sjPlot)
library(lme4)
library(pander)

getwd()
setwd("C:/Users/Chris/Desktop/MIDS/Fall 2020/IDS 702 Modeling and Rep of Data/FinalProject")

#Input and clean Police Death Dataset in preparation for merging
policebrut_df = read.csv("data/MPVDatasetDownload.csv")[ ,c('Victim.s.gender', 'Date.of.Incident..month.day.year.',
                                                            'City', 'State', 'County',
                                                            'Agency.responsible.for.death', 'Cause.of.death',
                                                            'Criminal.Charges.', 'Unarmed.Did.Not.Have.an.Actual.Weapon',
                                                            'Alleged.Threat.Level..Source..WaPo.', 
                                                            'Alleged.Weapon..Source..WaPo.and.Review.of.Cases.Not.Included.in.WaPo.Database.'
                                                            ,'Body.Camera..Source..WaPo.')]
#inspect data set
head(policebrut_df)

#redefine data set columns to more readable data
colnames(policebrut_df)[1]  <- "VictimGender"
colnames(policebrut_df)[2]  <- "DateofDeath"
colnames(policebrut_df)[6]  <- "PoliceAgencyName"
colnames(policebrut_df)[7]  <- "CauseOfDeath"
colnames(policebrut_df)[8]  <- "CriminalCharges"
colnames(policebrut_df)[9]  <- "Unarmed"
colnames(policebrut_df)[10] <- "AllegedTheatLevel"
colnames(policebrut_df)[11] <- "AllegedWeapon"
colnames(policebrut_df)[12] <- "BodyCamera"

#inspect and reassign the 
typeof(policebrut_df$DateofDeath)
as.Date(policebrut_df$DateofDeath, "%m/%d/%Y")

policebrut_df$DateofDeath <- as.Date(policebrut_df$DateofDeath, "%m/%d/%Y")
policebrut_df$YearofDeath <- format(as.Date(policebrut_df$DateofDeath), "%Y")

head(policebrut_df)

#confirm columns names have changed
colnames(policebrut_df)

premerge_Police_df <- policebrut_df %>% group_by(State, County, PoliceAgencyName, YearofDeath) %>% 
  summarise(DeathCount = n_distinct(DateofDeath),
            GunShotDeathCount = sum(CauseOfDeath=="Gunshot"), 
            TazerDeathCount = sum(CauseOfDeath=="Gunshot, Taser"),
            BodyCameraNo = sum(BodyCamera=="No", na.rm = TRUE))

colnames(premerge_Police_df)

colnames(premerge_Police_df)[4]  <- "Year"

grouped_police_df <- premerge_Police_df %>% 
  group_by(State, Year) %>%
  summarise(TotalDeathCount = sum(DeathCount), 
            GunShotDeathCount = sum(GunShotDeathCount), 
            TazerDeathCount = sum(TazerDeathCount),
            BodyCameraNo = sum(BodyCameraNo),
            #PoliceDeath = sum(DeathCount[str_count(PoliceAgencyName, "Police|PD|Troopers")]),
            PoliceTrooperDeath = sum(DeathCount[grepl("Police|Troopers", PoliceAgencyName)]),
            #SheriffTrooperDeath = sum(DeathCount[str_count(PoliceAgencyName, "Sheriff")]),
            SheriffMarshPatrolDeath = sum(DeathCount[grepl("Sheriff|Marshals|Patrol", PoliceAgencyName)]),
            #SafetyOtherDeath = sum(DeathCount[str_count(PoliceAgencyName, "Safety|Game")])
            SafetyOtherDeath = sum(DeathCount[grepl("Safety|Game|Border|Forest", PoliceAgencyName)])
            )

  

#Input and clean military surplus Dataset in preparation for merging

surplus_df <- excel_sheets("data/DISP_AllStatesAndTerritories_09302020.xlsx") %>% 
  map_df(~read_xlsx("data/DISP_AllStatesAndTerritories_09302020.xlsx",.))

colnames(surplus_df)

colnames(surplus_df)[2]  <- "RecivedStation"
colnames(surplus_df)[4]  <- "ItemName"
colnames(surplus_df)[7]  <- "AcquisitionValue"
colnames(surplus_df)[8]  <- "DEMIL_Code"
colnames(surplus_df)[9]  <- "DEMIL_IC"
colnames(surplus_df)[10] <- "ShipYear"
colnames(surplus_df)[11] <- "StationType"

surplus_df$ShipYear <- format(as.Date(surplus_df$ShipYear), "%Y")

truckstr = c("TRUCK", "VEHICLE")
Safety_df = c("SAFETY", "GAME")

premerge_surplus_df <- surplus_df %>% group_by(State, ShipYear) %>% 
  summarise(TotalAcquisitionValueSum = sum(AcquisitionValue),
            PoliceAcquSum = sum(AcquisitionValue[grepl("POLICE|PD|TROOPERS", RecivedStation)]),
            PoliceCount = sum(str_count(unique(RecivedStation),"POLICE|PD|TROOPERS")), 
            SheriffAcquSum = sum(AcquisitionValue[grepl("SHERIFF|MARSHALS|PATROL", RecivedStation)]),
            SheriffCount = sum(str_count(unique(RecivedStation), "SHERIFF|MARSHALS|PATROL")),
            PublicSafetyAcquSum = sum(AcquisitionValue[grepl("SAFETY|GAME|BORDER|FOREST", RecivedStation)]),
            PublicSafety = sum(str_count(unique(RecivedStation), "SAFETY|GAME|BORDER|FOREST")),
            #RifleCount = sum(Quantity[str_count(ItemName, "RIFLE")]),
            #PistolCount = sum(Quantity[str_count(ItemName, "PISTOL")]),
            #VehicleCount = sum(Quantity[str_count(ItemName, "VEHICLE")]),
            #TruckCount = sum(Quantity[str_count(ItemName, "TRUCK")]),
            DEMIL_Code_A = sum(Quantity[str_count(DEMIL_Code, "A")]), #Mis Equipment (tents, fridge, assualt packs, ect)
            DEMIL_Code_B = sum(Quantity[str_count(DEMIL_Code, "B")]), #tools and aircraft equipment
            DEMIL_Code_C = sum(Quantity[str_count(DEMIL_Code, "C")]), #Vehicles
            DEMIL_Code_D = sum(Quantity[str_count(DEMIL_Code, "D")]), #Weapons
            DEMIL_Code_E = sum(Quantity[str_count(DEMIL_Code, "E")]), #Uniforms 
            DEMIL_Code_F = sum(Quantity[str_count(DEMIL_Code, "F")]), #Sights/Scopes
            DEMIL_Code_G = sum(Quantity[str_count(DEMIL_Code, "G")]),
            DEMIL_Code_P = sum(Quantity[str_count(DEMIL_Code, "P")]),
            DEMIL_Code_Q = sum(Quantity[str_count(DEMIL_Code, "Q")])
            )

#Merge our two datasets

colnames(premerge_surplus_df)[2]  <- "Year"
merged_df <- merge(grouped_police_df, premerge_surplus_df, by = c("State", "Year"))

#Pull in population datasets

pop_df <- read_xlsx("data/nst-est2019-01.xlsx", skip = 3)
colnames(pop_df)[1]  <- "State"
pop_df$State <- str_replace(pop_df$State, ".", "" )
pop_df <- pop_df[6:56,]
pop_df <- pop_df[-c(9), ]
pop_df <- pop_df[,-c(2,3,4,5,6)]

pop_df$State <- state.abb[match(pop_df$State, state.name)]

pop_df <- pop_df %>%
  pivot_longer(!State, names_to = "Year", values_to = "Population")

#Merge population data and the rest of the dat

final_merged_df <- merge(merged_df, pop_df, by = c("State", "Year"))

colnames(final_merged_df)

final_merged_df$NormalizedDeaths          = final_merged_df$TotalDeathCount / final_merged_df$Population
final_merged_df$NormalizedMiliSurSpending = final_merged_df$TotalAcquisitionValueSum / final_merged_df$Population

summary(final_merged_df)

#EDA: Now that we have out final data we can begin looking at it holistically, and shape it into 
# what we think will work best with our analysis. 

str(final_merged_df)

# We need to change our data types to something that we can use

final_merged_df$State   = factor(final_merged_df$State)
final_merged_df$Year    = as.integer(final_merged_df$Year)
final_merged_df$Year14  = final_merged_df$Year - min(final_merged_df$Year)
final_merged_df <- within(final_merged_df, State <- relevel(State, ref = "NC"))

final_merged_df$Year14

# Ratio Variables

final_merged_df$PoliceSpentRatio = (final_merged_df$PoliceAcquSum / final_merged_df$TotalAcquisitionValueSum)
final_merged_df$SheriffSpentRatio = (final_merged_df$SheriffAcquSum / final_merged_df$TotalAcquisitionValueSum)

# Categorical Police/Sheriff Ratios

final_merged_df$PoliceSpendingCat = cut(final_merged_df$PoliceSpentRatio, breaks = c(-Inf,.4,.6, 1),
                                        labels = c("0-40%", "40-60%", "60-100%"))

final_merged_df$SheriffSpendingCat = cut(final_merged_df$SheriffSpentRatio, breaks = c(-Inf,.4,.6, 1),
                                        labels = c("0-40%", "40-60%", "60-100%"))

final_merged_df$BodyCamUsage =  (final_merged_df$BodyCameraNo / final_merged_df$TotalDeathCount)
final_merged_df$BodyCamUsageCat = cut(final_merged_df$BodyCamUsage, breaks = c(-Inf,.4, .55 ,.7, .85, 1),
                                      labels = c("0-40%", "40-55%", "55-70%", "70-85%", "85-100%"))


final_merged_df = final_merged_df[, c("Year", "State", "TotalDeathCount", "Population","BodyCamUsageCat", "TotalAcquisitionValueSum", "NormalizedDeaths",
                                      "NormalizedMiliSurSpending", "PoliceSpendingCat", "SheriffSpendingCat",
                                      "DEMIL_Code_A", "DEMIL_Code_B", "DEMIL_Code_C", "DEMIL_Code_D", "DEMIL_Code_E",
                                      "DEMIL_Code_F", "DEMIL_Code_Q", "PoliceAcquSum",
                                      "PoliceCount", "SheriffAcquSum", "SheriffCount", "PublicSafetyAcquSum",
                                      "PoliceTrooperDeath", "SheriffMarshPatrolDeath","SafetyOtherDeath", "Year14")]

# EDA

# list the columns names for future reference
colnames(final_merged_df)

str(final_merged_df)
# distribution of the response variables: We have data for most states, there are only a couple
# that seem to have a missing year or two.  RI is missing 5 of the 7 years.
table(final_merged_df$State, final_merged_df$Year)
table(final_merged_df$Year)

# Explore the relationship between deaths and our independent variables. 

ggplot(final_merged_df, aes(y = NormalizedDeaths, x = NormalizedMiliSurSpending)) +
  geom_point() +
  geom_smooth(method = lm) +
  ggtitle("Normalized Deaths vs. Normalized Spending") +
  theme(text = element_text(size = 20), 
        axis.text.x = element_text(face="bold", color="black", size=14, angle=0),
        axis.text.y = element_text(face="bold", color="black", size=14, angle=45))

ggplot(final_merged_df, aes(y = TotalDeathCount, x = TotalAcquisitionValueSum)) +
  geom_point() +
  geom_smooth(method = lm) +
  ggtitle("Total Death Counts Per State and Year vs. \n Total Mil Surplus Spending (in $)") +
  theme(text = element_text(size = 20), 
        axis.text.x = element_text(face="bold", color="black", size=14, angle=0),
        axis.text.y = element_text(face="bold", color="black", size=14, angle=45))

# Number of weapons purchased and deaths by gunshot
#   There is an upward trend but the graph itself is not very convincing,
#   maybe a couple of outlier are pulling the line up

ggplot(final_merged_df[(final_merged_df$DEMIL_Code_D!=max(final_merged_df$DEMIL_Code_D)), ], 
       aes(y = TotalDeathCount, x = DEMIL_Code_D)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method = lm, color ="red3") +
  ggtitle("Number of Gunshot Deaths vs Weapons Aquired") +
  theme(text = element_text(size = 20), 
        axis.text.x = element_text(face="bold", color="black", size=14, angle=0),
        axis.text.y = element_text(face="bold", color="black", size=14, angle=45))

ggplot(final_merged_df, aes(x = State, y = TotalDeathCount,fill=State)) +
  geom_boxplot() +
  labs(title="")

ggplot(final_merged_df, aes(x = State, y = NormalizedDeaths ,fill=State)) +
  geom_boxplot() +
  labs(title="Normaized Deaths by State") +
  theme(legend.position="none")
  

ggplot(final_merged_df, aes(x = Year, y = NormalizedDeaths, group = Year)) +
  geom_boxplot() +
  labs(title="Normaized Deaths by Year") 

ggplot(final_merged_df, aes(x = Year, y = NormalizedMiliSurSpending, group = Year)) +
  geom_boxplot(alpha = .5,colour="blue4") +
  labs(title="Normaized Deaths by State")

ggplot(final_merged_df, aes(x = Year, y = NormalizedMiliSurSpending)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="loess",col="red3") +
  labs(title="NormalizedSpenidng On Military Surplus From 2013-2019")

ggplot(final_merged_df, aes(x = Year, y = NormalizedDeaths)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="loess",col="red3") +
  labs(title="NormalizedSpenidng On Military Surplus From 2013-2019")

ggplot(final_merged_df, aes(x = Year, y = TotalDeathCount)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="loess",col="red3") +
  labs(title="NormalizedSpenidng On Military Surplus From 2013-2019")

ggplot(final_merged_df, aes(x = Year, y = TotalAcquisitionValueSum)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method="loess",col="red3") +
  labs(title="NormalizedSpenidng On Military Surplus From 2013-2019")

# Compare number of deaths cause by different types of policing departments and the
# amount of surplus aquired.  Police and Sheriff show a strong corrilation. 

ggplot(final_merged_df, aes(y = SheriffMarshPatrolDeath, x = SheriffAcquSum)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method = lm, color ="red3") +
  ggtitle("Deaths Caused by Sheriffs vs \n Amount of Military Surplus Aquired (in $)")+
  theme(text = element_text(size = 20), 
        axis.text.x = element_text(face="bold", color="black", size=14, angle=0),
        axis.text.y = element_text(face="bold", color="black", size=14, angle=45))

ggplot(final_merged_df, aes(y = PoliceTrooperDeath, x = TotalAcquisitionValueSum, group = State)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method = lm, color ="red3") +
  ggtitle("Deaths Caused by Police/Troopers vs \n Amount of Military Surplus Aquired (in $)")+
  theme(text = element_text(size = 20), 
        axis.text.x = element_text(face="bold", color="black", size=14, angle=0),
        axis.text.y = element_text(face="bold", color="black", size=14, angle=45))

ggplot(final_merged_df[(final_merged_df$PublicSafetyAcquSum!=max(final_merged_df$PublicSafetyAcquSum)), ], 
       aes(y = SafetyOtherDeath, x = PublicSafetyAcquSum)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method = lm, color ="red3") +
  ggtitle("Deaths Caused by Safety/Forestry/Game Depts vs \n Amount of Military Surplus Aquired (in $)")+
  theme(plot.title = element_text(hjust = 0.5))

# COMPARE BY STATE y = PoliceTrooperDeath, x = PoliceAcquSum

# California
ggplot(final_merged_df, aes(y = PoliceTrooperDeath, x = PoliceAcquSum)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method = lm, color ="red3") +
  facet_wrap(~ State == "CA" ) + #== "IL"
  ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5))

# Texas
ggplot(final_merged_df, aes(y = PoliceTrooperDeath, x = PoliceAcquSum)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method = lm, color ="red3") +
  facet_wrap(~ State == "TX" ) + #== "IL"
  ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5))

# Alabama
ggplot(final_merged_df, aes(y = PoliceTrooperDeath, x = PoliceAcquSum)) +
  geom_point(alpha = .5,colour="blue4") +
  geom_smooth(method = lm, color ="red3") +
  facet_wrap(~ State == "AL") + #== "AL"
  ggtitle("") +
  theme(plot.title = element_text(hjust = 0.5))


# Compare ratio categories and states

ggplot(final_merged_df, aes(y = TotalDeathCount, x = SheriffSpendingCat)) +
  geom_boxplot(alpha = .5,colour="blue4") +
  geom_smooth(method = lm, color ="red3") +
  #facet_wrap(~ SheriffSpendingCat) + #== "AL"
  ggtitle("Comparing Ratio of States Total Spending by Sheriff Dept and\n Total Deaths Cause") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(final_merged_df, aes(y = NormalizedDeaths, x = SheriffSpendingCat)) +
  geom_boxplot(alpha = .5,colour="blue4") +
  geom_smooth(method = lm, color ="red3") +
  #facet_wrap(~ SheriffSpendingCat) + #== "AL"
  ggtitle("Comparing Ratio of States Total Spending by Sheriff Dept and\n Total Deaths Cause") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(final_merged_df, aes(y = TotalDeathCount, x = PoliceSpendingCat)) +
  geom_boxplot(alpha = .5,colour="blue4") +
  geom_smooth(method = lm, color ="red3") +
  #facet_wrap(~ SheriffSpendingCat) + #== "AL"
  ggtitle("Comparing Ratio of States Total Spending by Police Dept and\n Total Deaths Cause") +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(final_merged_df, aes(y = NormalizedDeaths, x = PoliceSpendingCat)) +
  geom_boxplot(alpha = .5,colour="blue4") +
  geom_smooth(method = lm, color ="red3") +
  #facet_wrap(~ SheriffSpendingCat) + #== "AL"
  ggtitle("Comparing Ratio of States Total Spending by Police Dept and\n Total Deaths Cause") +
  theme(plot.title = element_text(hjust = 0.5))

######## Initial Model 
cov_names <- names(final_merged_df)
p_formula <- as.formula(paste("TotalDeathCount ~",
                              paste(cov_names[!cov_names %in% c("Year", "NormalizedMiliSurSpending", "NormalizedDeaths", "Population", "TotalDeathCount", 
                                                                "PoliceTrooperDeath", "SheriffMarshPatrolDeath", "SafetyOtherDeath")],
                                    collapse = " + ")))

model_1 <- glm(p_formula, data = final_merged_df, offset = log(Population), family=poisson)
summary(model_1)



model_2 <- glm(TotalDeathCount ~ State + Year + DEMIL_Code_A + DEMIL_Code_B + DEMIL_Code_C + DEMIL_Code_D + 
                 DEMIL_Code_E + DEMIL_Code_F + NormalizedMiliSurSpending + PoliceSpendingCat + SheriffSpendingCat, 
               data = final_merged_df, offset = log(Population) ,family=poisson)
summary(model_2)
tab_model(model_2)

plot(model_2, which = 5)

final_merged_df_2 = final_merged_df[-c(279), ]

model_3 <- glmer(TotalDeathCount ~ (1|State) + Year + DEMIL_Code_A + DEMIL_Code_B + DEMIL_Code_C + DEMIL_Code_D + 
                 DEMIL_Code_E + DEMIL_Code_F + NormalizedMiliSurSpending + PoliceSpendingCat + SheriffSpendingCat, 
               data = final_merged_df_2, offset = log(Population) ,family=poisson)
summary(model_3)
tab_model(model_3)

plot(model_3, which = 5)

#Model Selection

#data = na.omit(final_merged_df_2)
#replace_na(final_merged_df, value = BodyCamUsageCat, tagged.na = "0%")
#data - final_merged_df
model_1_first = glm(TotalDeathCount ~ 1, data = final_merged_df, offset = log(Population), family=poisson)
model_1_full = glm(TotalDeathCount ~ BodyCamUsageCat + TotalAcquisitionValueSum_CenterScale + 
                      PoliceSpendingCat + SheriffSpendingCat + DEMIL_Code_A_CenterScale + DEMIL_Code_B + 
                      DEMIL_Code_C_CenterScale + DEMIL_Code_D_CenterScale + DEMIL_Code_E + DEMIL_Code_F + 
                      DEMIL_Code_Q + Year14, data = final_merged_df, 
                      offset = log(Population) ,family=poisson)
tab_model(model_1_full)
step.model <- step(model_1_first, direction="both", scope=formula(model_1_full))
step.model

#Final Model 
roundUpNice <- function(x, nice=c(1,2,4,5,6,8,10)) {
  if(length(x) != 1) stop("'x' must be of length 1")
  10^floor(log10(x)) * nice[[which(x <= 10^floor(log10(x)) * nice)[[1]]]]
}

scaler_TotalSum = roundUpNice((mean(final_merged_df$TotalAcquisitionValueSum) / 100))
final_merged_df$TotalAcquisitionValueSum_scaled = (final_merged_df$TotalAcquisitionValueSum / mean(final_merged_df$TotalAcquisitionValueSum))

scaler_DEMIL_C = mean(final_merged_df$DEMIL_Cod)
final_merged_df$DEMIL_Code_C_scaled = (final_merged_df$DEMIL_Code_C / scaler_DEMIL_C)

final_merged_df$DEMIL_Code_C_c = (final_merged_df$DEMIL_Code_C - mean(final_merged_df$DEMIL_Code_C))
str(final_merged_df)

#scaling varialbes

final_merged_df$TotalAcquisitionValueSum_CenterScale = scale(final_merged_df$TotalAcquisitionValueSum, center = TRUE, scale = TRUE)
final_merged_df$DEMIL_Code_C_CenterScale = scale(final_merged_df$DEMIL_Code_C, center = FALSE, scale = TRUE)
final_merged_df$DEMIL_Code_A_CenterScale = scale(final_merged_df$DEMIL_Code_A, center = FALSE, scale = TRUE)
final_merged_df$DEMIL_Code_D_CenterScale = scale(final_merged_df$DEMIL_Code_D, center = FALSE, scale = TRUE)
final_merged_df$DEMIL_Code_Q_CenterScale = scale(final_merged_df$DEMIL_Code_Q, center = FALSE, scale = TRUE)
final_merged_df$DEMIL_Code_B_CenterScale = scale(final_merged_df$DEMIL_Code_B, center = FALSE, scale = TRUE)
final_merged_df$DEMIL_Code_F_CenterScale = scale(final_merged_df$DEMIL_Code_F, center = FALSE, scale = TRUE)
final_merged_df$DEMIL_Code_E_CenterScale = scale(final_merged_df$DEMIL_Code_E, center = FALSE, scale = TRUE)
final_merged_df = within(final_merged_df, BodyCamUsageCat <- relevel(BodyCamUsageCat, ref = "0-40%"))
final_merged_df = within(final_merged_df, PoliceSpendingCat <- relevel(PoliceSpendingCat, ref = "0-40%"))


#Total Death Model without grouping by state
final_model_total_nogroup = glm(TotalDeathCount ~ State + PoliceSpendingCat + DEMIL_Code_C + TotalAcquisitionValueSum + 
                    DEMIL_Code_A + BodyCamUsageCat + DEMIL_Code_D, 
                  data = final_merged_df, offset = log(Population), family=poisson)
summary(final_model_total_nogroup)
tab_model(final_model_total_nogroup)



#Total Death Model with grouping by state
final_model_total_group = glmer(TotalDeathCount ~ (1|State) + PoliceSpendingCat + DEMIL_Code_C_CenterScale + TotalAcquisitionValueSum_CenterScale + 
                                  DEMIL_Code_A_CenterScale + BodyCamUsageCat + DEMIL_Code_D_CenterScale, 
                                data = final_merged_df, offset = log(Population), family=poisson)
summary(final_model_total_group)
tab_model(final_model_total_group)



##### Refitting model for calculating police deaths only
model_2_first = glm(PoliceCount ~ State, data = final_merged_df, offset = log(Population), family=poisson)
model_2_full = glm(PoliceCount ~ State + BodyCamUsageCat + NormalizedMiliSurSpending +
                     PoliceSpendingCat + SheriffSpendingCat + DEMIL_Code_A_CenterScale + 
                     DEMIL_Code_B_CenterScale + DEMIL_Code_C_CenterScale + DEMIL_Code_D_CenterScale + 
                     DEMIL_Code_E_CenterScale + DEMIL_Code_F_CenterScale + 
                     DEMIL_Code_Q_CenterScale + Year14, data = final_merged_df, 
                   offset = log(Population) ,family=poisson)

step.model <- step(model_2_first, direction="both", scope=formula(model_2_full))
step.model


##### Police Count Model without grouping by state
police_count_model_v2 = glm(PoliceCount ~ State + BodyCamUsageCat + NormalizedMiliSurSpending + 
                           SheriffSpendingCat + PoliceSpendingCat + DEMIL_Code_Q_CenterScale + 
                           DEMIL_Code_C_CenterScale,
                           data = final_merged_df, offset = log(Population), family=poisson)
summary(police_count_model_v2)
tab_model(police_count_model_v2)


##### Police Count Model without grouping by state
police_count_model_1 = glmer(PoliceCount ~ (1|State) + BodyCamUsageCat + NormalizedMiliSurSpending + 
                           PoliceSpendingCat + DEMIL_Code_Q_CenterScale + DEMIL_Code_F_CenterScale + DEMIL_Code_E_CenterScale,
                         data = final_merged_df, offset = log(Population), family=poisson)
summary(police_count_model_1)
tab_model(police_count_model_1)
stargazer(police_count_model_1, apply.se=police_count_model_1)
tab_model(police_count_model, show.se = TRUE, title = "Final Model Summary",
          digits = 3, digits.re = 3, show.ngroups = TRUE, show.aic = TRUE, show.re.var = TRUE)

resid_panel(police_count_model_1, plots = c("resid", "qq"), 
            qqbands = TRUE, theme = "classic")

police_count_model_2 = glmer(PoliceCount ~ (1|State) + BodyCamUsageCat + NormalizedMiliSurSpending + 
                               PoliceSpendingCat + DEMIL_Code_Q_CenterScale + DEMIL_Code_F_CenterScale +
                               DEMIL_Code_E_CenterScale + DEMIL_Code_D_CenterScale,
                             data = final_merged_df, offset = log(Population), family=poisson)
tab_model(police_count_model_2)

pander(anova(police_count_model_1, police_count_model_2, test="Chisq"))


coef(police_count_model)
library(stargazer)
stargazer(police_count_model, type = 'text', ci.level = 0.95)
install.packages("qqplotr")
library(qqplotr)
p1 = qqnorm(resid(police_count_model)) + qqline(resid(police_count_model))

plot(p1)
p8

library(lattice)
dotplot(ranef(police_count_model, cordVar =TRUE))


par(mfrow = c(1, 1))
plot(police_count_model, which = 6)
anova(police_count_model, test="Chisq")

confint(police_count_model)

library(car)
plot(police_count_model_v2)

plot(police_count_model_1)

p6 = qqnorm(resid(police_count_model_1), ylab = "Precipitation [in/yr] for 70 US cities")
p7 = plot(police_count_model_1, which = 6)
lay = rbind(c(1, 2))

grid.arrange(p6, p7, layout_matrix= lay, bottom = "Figure 3: Model Validation Plots")

police_count_res <- resid(police_count_model, type = "pearson")
police_count_pred <- predict(police_count_model,type="response")

#residuals vs fitted
qplot(y=police_count_res, x=police_count_pred, data=final_merged_df, col=PoliceCount, geom="point",
      xlab = "Predicted Counts", ylab = "Pearson Residuals")
