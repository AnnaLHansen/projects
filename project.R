source("help_func_datainspec.R")
source("help_func_standardization.R")

train <- readRDS("~/anonym_data_kursus.rds")
train<- klargoer_salg(train) # tilbage imputerer og fjerner irrelevante attributter
# Laver et subset af salgene så det kun er de almindelige parcelhuse som optræder i data
# Herefter bliver der lavet en oversigt over de variable som findes i data 
# samt hvor mange missing values der er.
attribut <- undersoeger_attributter(train) # Laver oversigt over de attributter som er tilgængelige
# Missing values i data skyldes flere forskellige ting
# Der er mange missing værdier som fordi felterne er valgfrie felter når de udfyldes i bbr. 
# For afstandsvariablene er der missing værdier afstandene ud over en bestemt trunkeringsafstand ikke
# er blevet målt.
train <- samler_variable(train)
train <- missing_values_pct95(attribut, train)
attribut <- undersoeger_attributter(train) 

train <- standardize(train)
