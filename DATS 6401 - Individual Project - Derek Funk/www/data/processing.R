setwd(dirname(rstudioapi::getSourceEditorContext()$path))

countries <- c(
  "Australia", "Brazil", "France", "Germany",
  "Indonesia", "Italy", "Japan", "Mexico",
  "South Africa", "Korea, Rep.", "United Kingdom", "United States"
)

#file reads
worldBankReader <- function(filename) {
  #filename includes extension
  x <- read_csv(
    file = paste0("data\\raw\\", filename),
    skip = 4
  )
  x <- x[x$`Country Name` %in% countries,]
  x <- x[,c(1,55:60)]
  return(x)
}

#just 12 countries and years 2010-2015
data_militarySpend_USD <- worldBankReader("militarySpend_Usd.csv") #1
data_population <- worldBankReader("population.csv") #2
data_expenses_percentOfGdp <- worldBankReader("expenses_percentOfGdp.csv") #3
data_educationSpend_percentOfGdp <- worldBankReader("educationSpend_percentOfGdp.csv") #4 - needs imputation
data_gdp_USD <- worldBankReader("gdp_USD.csv") #5
data_healthSpend_percentOfGdp <- worldBankReader("healthSpend_percentOfGdp.csv") #6

#impute data_educationSpend_percentOfGdp
data_educationSpend_percentOfGdp[5,4] <- (data_educationSpend_percentOfGdp[5,3] + data_educationSpend_percentOfGdp[5,5])/2 #UK 2012
data_educationSpend_percentOfGdp[7,4] <- (data_educationSpend_percentOfGdp[7,3] + data_educationSpend_percentOfGdp[7,5])/2 #Italy 2012
data_educationSpend_percentOfGdp[8,7] <- (data_educationSpend_percentOfGdp[8,6] + 3.46746)/2 #Japan 2015
data_educationSpend_percentOfGdp[9,2] <- (4.66983 + data_educationSpend_percentOfGdp[9,3])/2 #Korea 2010
data_educationSpend_percentOfGdp[11,7] <- (data_educationSpend_percentOfGdp[11,5] + data_educationSpend_percentOfGdp[11,6])/2 #US 2015

#derivations
##start with "blank slates"
data_militarySpend_perCapita <- data_militarySpend_USD #7
data_educationSpend_USD <- data_militarySpend_USD #8
data_educationSpend_perCapita <- data_militarySpend_USD #9
data_healthSpend_USD <- data_militarySpend_USD #10
data_expenses_USD <- data_militarySpend_USD #11
data_educationSpend_PercentOfExpenses <- data_militarySpend_USD #12
data_healthSpend_percentOfExpenses <- data_militarySpend_USD #13
data_healthSpend_perCapita <- data_militarySpend_USD #14
data_militarySpend_percentOfExpenses <- data_militarySpend_USD #15
data_gdp_perCapita <- data_militarySpend_USD #16
data_otherSpend_USD <- data_militarySpend_USD

##then manipulate
data_militarySpend_perCapita[,2:7] <- data_militarySpend_perCapita[,2:7] / data_population[,2:7] #7
data_educationSpend_USD[,2:7] <- data_educationSpend_percentOfGdp[,2:7]/100 * data_gdp_USD[,2:7] #8
data_educationSpend_perCapita[,2:7] <- data_educationSpend_USD[,2:7] / data_population[,2:7] #9
data_healthSpend_USD[,2:7] <- data_healthSpend_percentOfGdp[,2:7]/100 * data_gdp_USD[,2:7] #10
data_expenses_USD[,2:7] <- data_expenses_percentOfGdp[,2:7]/100 * data_gdp_USD[,2:7] #11
data_educationSpend_PercentOfExpenses[,2:7] <- data_educationSpend_USD[,2:7] / data_expenses_USD[,2:7] #12
data_healthSpend_percentOfExpenses[,2:7] <- data_healthSpend_USD[,2:7] / data_expenses_USD[,2:7] #13
data_healthSpend_perCapita[,2:7] <- data_healthSpend_USD[,2:7] / data_population[,2:7] #14
data_militarySpend_percentOfExpenses[,2:7] <- data_militarySpend_USD[,2:7] / data_expenses_USD[,2:7] #15
data_gdp_perCapita[,2:7] <- data_gdp_USD[,2:7] / data_population[,2:7] #16
# data_otherSpend_USD[,2:7] <- data_expenses_USD[,2:7] - (data_educationSpend_USD[,2:7] + data_healthSpend_USD[,2:7] + data_militarySpend_USD[,2:7])

#standardize for visualizations (raws to billions/trillions and percents to whole numbers)
data_educationSpend_USD[,2:7] <- data_educationSpend_USD[,2:7] / 1000000000
data_healthSpend_USD[,2:7] <- data_healthSpend_USD[,2:7]/ 1000000000
data_militarySpend_USD[,2:7] <- data_militarySpend_USD[,2:7] / 1000000000
# data_otherSpend_USD[,2:7] <- data_otherSpend_USD[,2:7] / 1000000000
data_gdp_USD[,2:7] <- data_gdp_USD[,2:7] / 1000000000000
data_expenses_USD[,2:7] <- data_expenses_USD[,2:7] / 1000000000000
data_educationSpend_PercentOfExpenses[,2:7] <- data_educationSpend_PercentOfExpenses[,2:7] * 100
data_healthSpend_percentOfExpenses[,2:7] <- data_healthSpend_percentOfExpenses[,2:7] * 100
data_militarySpend_percentOfExpenses[,2:7]<- data_militarySpend_percentOfExpenses[,2:7] * 100

#special data frames for comparisons
data_gdpVsExpenses2015 <- cbind(data_gdp_USD[,c(1,7)], data_expenses_USD[,7])
names(data_gdpVsExpenses2015) <- c("Country Name", "GDP" ,"Expenses")
data_perCapita2015 <- cbind(
  data_gdp_perCapita[,c(1,7)],
  data_healthSpend_perCapita[,7],
  data_educationSpend_perCapita[,7],
  data_militarySpend_perCapita[,7]
)
names(data_perCapita2015) <- c("Country Name", "GDP", "Health", "Education", "Military")
data_expensesBreakdown2015 <- cbind(
  data_educationSpend_USD[,c(1,7)],
  data_healthSpend_USD[,7],
  data_militarySpend_USD[,7]
)
names(data_expensesBreakdown2015) <- c("Country Name", "Education", "Health", "Military")
data_expensesBreakdown2015 <- data.frame(t(data_expensesBreakdown2015))
x <- data_expensesBreakdown2015[1,]
data_expensesBreakdown2015 <- data_expensesBreakdown2015[-1,]
names(data_expensesBreakdown2015) <- t(x)
data_expensesBreakdown2015 <- cbind(data.frame(category=c("Education", "Health", "Military")), data_expensesBreakdown2015)

#write to google sheets
library(googlesheets)
library(openssl)
gs_auth(new_user = TRUE)
gs_new(
  title = "data", ws_title = "educationSpend_USD", input = data_educationSpend_USD
)
gs_file <- gs_title("data")
gs_ws_new(gs_file, ws_title = "healthSpend_USD", input = data_healthSpend_USD)
gs_ws_new(gs_file, ws_title = "militarySpend_USD", input = data_militarySpend_USD)
gs_ws_new(gs_file, ws_title = "educationSpend_PercentOfExpenses", input = data_educationSpend_PercentOfExpenses)
gs_ws_new(gs_file, ws_title = "healthSpend_percentOfExpenses", input = data_healthSpend_percentOfExpenses)
gs_ws_new(gs_file, ws_title = "militarySpend_percentOfExpenses", input = data_militarySpend_percentOfExpenses)
gs_ws_new(gs_file, ws_title = "educationSpend_perCapita", input = data_educationSpend_perCapita)
gs_ws_new(gs_file, ws_title = "healthSpend_perCapita", input = data_healthSpend_perCapita)
gs_ws_new(gs_file, ws_title = "militarySpend_perCapita", input = data_militarySpend_perCapita)
gs_ws_new(gs_file, ws_title = "gdpVsExpenses2015", input = data_gdpVsExpenses2015)
gs_ws_new(gs_file, ws_title = "perCapita2015", input = data_perCapita2015)
gs_ws_new(gs_file, ws_title = "expensesBreakdown2015", input = data_expensesBreakdown2015)
