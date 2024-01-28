setwd('C:/Users/mayjaikaew/R-project/sales-analysis/sales-analysis')

#Load neccessary library
library(dplyr)
library(tidyverse)
library(tidyr)
library(lubridate)

#import data
tiredata <- read.csv("TiresDataFile_DSCI490_2024AY.csv")

# function to check which column has na or blank
find.na <- function(data){
  na.columns<- c()
  for (i in 1:ncol(data)) {
    if (any(is.na(data[, i]) | data[, i] == "")) {
      na.columns <- c(na.columns, i)
    }
    }
  return(na.columns)
}
na.columns<-find.na(tiredata)
colnames(tiredata[na.columns])

# Deleting empty columns

empty_cols <- c("Additional.vehicle.s.", "X", "Cust..Education", 
                "Number.of.drivers.in.home")
tiredata <- tiredata[, !(names(tiredata) %in% empty_cols)]


# Cleaning and renaming the customer birth year column to customer age

tiredata$Cust.Age <- 2024 - tiredata$Cust..Year_Birth
tiredata$Cust.Age <- as.numeric(tiredata$Cust.Age)


# Impute missing ages with the mean age, avoiding a loop
mean_age <- mean(tiredata$Cust.Age, na.rm = TRUE)
tiredata$Cust.Age[is.na(tiredata$Cust.Age)] <- mean_age

# Handling household income
# Assuming the median can be calculated directly, 
# Impute missing incomes with the median income range
median_income_range <- median(tiredata$Household.income, na.rm = TRUE)
tiredata$Household.income[is.na(tiredata$Household.income)] <- as.character(median_income_range)

# Convert household income to factor
tiredata$Household.income <- factor(tiredata$Household.income)



# Standardizing discount codes

# Set all non-specific codes to '0' and impute NA values
specific_codes <- c("UV21342ww", "VJ765098np", "wq1143276s")
tiredata$Discount.code[!tiredata$Discount.code %in% specific_codes] <- '0'
tiredata$Discount.code[is.na(tiredata$Discount.code)] <- '0'


# Standardizing responses to promotions
promotion_columns <- grep("Responded.*Promo", names(tiredata), value = TRUE)
for(col in promotion_columns){
  tiredata[[col]][tiredata[[col]] != "Y"] <- "N"
}

# Print unique discount codes for verification
unique(tiredata$Discount.code)


library(dplyr)
library(tidyr)
library(lubridate)

# Impute "no vehicle" for primary vehicle if the number of vehicles at home is empty
tiredata <- tiredata %>%
  mutate(
    Primary.Vehicle.type = ifelse(Number.of.vehicles.in.household == "0" & Primary.Vehicle.type == "", "No vehicle", Primary.Vehicle.type),
    X2nd.Vechicle.type = ifelse(Number.of.vehicles.in.household %in% c("0", "1") & X2nd.Vechicle.type == "", "No second vehicle", X2nd.Vechicle.type),
    X3rd.Vehicle.type = ifelse(Number.of.vehicles.in.household %in% c("0", "1", "2") & X3rd.Vehicle.type == "", "No 3rd vehicle", X3rd.Vehicle.type),
    X4th.Vehicle.type = ifelse(Number.of.vehicles.in.household %in% c("0", "1", "2", "3") & X4th.Vehicle.type == "", "No 4th vehicle", X4th.Vehicle.type)
  )

# Identify rows with empty Primary Vehicle type but non-zero vehicles
rows_with_issues <- which(tiredata$Primary.Vehicle.type == "" & tiredata$Number.of.vehicles.in.household != "0")
if(length(rows_with_issues) > 0) {
  message("These people have a car but did not initialize the vehicle type, rows: ", paste(rows_with_issues, collapse=", "))
}

# Standardize responses to promotions by replacing empty values with "N"
promotion_cols <- grep("Responded.*Promo", names(tiredata), value = TRUE)
tiredata[promotion_cols] <- lapply(tiredata[promotion_cols], function(x) ifelse(x != "Y", "N", x))

# Convert household size to numeric
tiredata$Household.size <- parse_number(tiredata$Household.size)

# Convert certain columns to numeric
num_cols <- c('Customer.ID', 'Cust.Age', 'Rough.Approx.km.per.year', 
              'Number.of.children.18.years.or.younger.in.the.home', 
              'Number.of.adults.in.home', 'X.adult.drivers', 
              'Number.of.vehicles.in.household', 'Summer.tires.....sets.purchased.', 
              'Winter.tires.....sets.purchased.', 'All.seasons....sets.purchased.', 
              'Wheel.rim.purchase', 'Other.automotive.services.parts.accessories.purchases.made.in.the.last.year..at.OTHER.retailers', 
              'Number.of.purchases.made.in.the.store', 'Number.of.online.purchases', 
              'Household.size')
tiredata[num_cols] <- lapply(tiredata[num_cols], as.numeric)

# Fixing date format and converting to date type
date_cols <- c('Last.Date.of.purchase.summer', 'Last.Date.of.purchase.winter', 'Last.Date.of.purchase.all.seasons')
tiredata[date_cols] <- lapply(tiredata[date_cols], mdy)
tiredata[date_cols] <- lapply(tiredata[date_cols], as.Date)

# Renaming columns
new_colnames <- c('Account Number', 'Customer ID', 'Age', 'City', 
                  'Rough/Approx km per year', 'Household size', 
                  'Number of children 18 years or younger in the home', 
                  'Young Drivers', 'Number of adults in home', 
                  'Adult drivers', 'Household income', 'Vehicles Number', 
                  'Primary Vehicle type', '2nd Vehicle type', '3rd Vehicle type', 
                  '4th Vehicle type', 'Summer Tires Sets Purchased', 
                  'Winter Tires Sets Purchased', 'All season Tires Sets Purchased', 
                  'Last Date of purchase summer', 'Last Date of purchase winter', 
                  'Last Date of purchase all seasons', 'Wheel/rim purchase', 
                  'Recent Highest Quality Rim Purchase', 'Warranty', 
                  'Other Automotive Purchases (Other Retailers)', 
                  'In-Store Purchases', 'Online Purchases', 'Discount code obtained', 
                  'Discount code', 'Summer Tire March Promotion Response', 
                  'All-Season Tire March Promotion Response', 'All-Season Tire August Promotion Response', 
                  'Winter Tire October Promotion Response', 'Winter Tire November Promotion Response')
names(tiredata) <- new_colnames



# Replace blank values with "N" and convert to logical
for (i in 31:35){
  tiredata[, i] <- ifelse(tiredata[, i] == "", "N", tiredata[, i])
  tiredata[, i] <- tiredata[, i] == "Y"
}

tiredata$Warranty <- ifelse(tolower(tiredata$Warranty) == "y", TRUE, FALSE)

view(tiredata)
str(tiredata)



write.csv(tiredata, "tires_data02.csv", row.names = FALSE)

#Remove irrelevant future date
tiredata01 <- subset(tiredata, 
                     `Last Date of purchase summer` <= as.Date("2023-12-30") & 
                       `Last Date of purchase winter` <= as.Date("2023-12-30") & 
                       `Last Date of purchase all seasons` <= as.Date("2023-12-30"))

#export data
write.csv(tiredata01, "tires_data03.csv", row.names = FALSE)
