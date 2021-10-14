library(tidyverse)
library(tidycensus)
library(sf)
library(kableExtra)
library(dplyr)
library(ggcorrplot)
library(caret)


#######################################################
# Getting data from the ACS tracts
#######################################################

varlist_2019 <- load_variables(2019, "acs5", cache = TRUE)

#Total population
#Total employed population
#Median household income
#Population with income below poverty level
#White population
#Vacant occupancy
#Owner-occupied housing units: bachelor's degree or higher
#Aggregate travel time to work
#Total number of bachelor’s degrees in science and engineering related fields

tracts19 <- get_acs(geography = "tract",                         
                    variables = c("B01001_001","B23025_004","B06011_001",
                                  "B06012_002","B02001_002","B25002_003",
                                  "B25013_006","B08013_001","B15012_009"), 
                    year=2019, 
                    state=08, 
                    county=013,
                    output = "wide",
                    geometry=TRUE) %>% 
            st_transform('ESRI:102254') %>%
            select( c("B01001_001E","B23025_004E","B06011_001E",
                      "B06012_002E","B02001_002E","B25002_003E",
                      "B25013_006E","B08013_001E","B15012_009E") ) %>%
            rename(tot_pop = "B01001_001E",
                   empl_pop = "B23025_004E",
                   med_inc = "B06011_001E",
                   pvty_pop = "B06012_002E",
                   white_pop = "B02001_002E",
                   vac_occ = "B25002_003E",
                   own_occ_bach = "B25013_006E",
                   tt_work = "B08013_001E",
                   sci_bach = "B15012_009E")

#######################################################
# Loading data, finding correlation within studentData
#######################################################

# This loads all the data into "studentData".
studentData <- st_read("studentData.geojson", crs = 'ESRI:102254')

# Attach ACS data
studentData <- st_join(studentData, tracts19, join = st_within)

# This selects all numeric variables as preparation for the
# correlation analysis that follows.
cleanData <- 
  select_if(st_drop_geometry(studentData), is.numeric) %>% 
  select(!c(ExtWallSec, IntWall, Roof_Cover, Stories, UnitCount, MUSA_ID)) %>% 
  na.omit()

# The test data only includes rows comprising the test set.
testData <- filter(cleanData, toPredict == 0)

# This function here attempts to fit a linear model to
# the relationship between "testData" variables.
ggplot(data = testData, aes(mainfloorSF, price)) +
       geom_point(size = .5) + 
       geom_smooth(method = "lm")

# Correlation analysis: pearson for each relationship
ggcorrplot(
    round(cor(testData), 1), 
    p.mat = cor_pmat(testData),
    colors = c("#25CB10", "white", "#FA7800"),
    type="lower",
    insig = "blank") +  
    labs(title = "Correlation across numeric variables") 

# This function allows for the plug-in of variables from "studentData".
testSignificance <- lm(price ~ ., data = cleanData %>% 
                    dplyr::select(price, 
                                  qualityCode,
                                  TotalFinishedSF,
                                  mainfloorSF))

# This gives us our r-squared value, which measures fit to the training data.
summary(testSignificance)

# We'll need to try cross-validation to see how well the model predicts for
# data it has never seen before, which will be more useful than r squared.

# This sets up k-fold cross-validation.
k = 10
fitControl <- trainControl(method = "cv", number = k)
set.seed(324)

# variables in the "select(...)" function are considered in the analysis here.
regression.10foldcv <- 
  train(price ~ ., data = cleanData %>% 
                        select(price, 
                        qualityCode,
                        TotalFinishedSF,
                        mainfloorSF,
                        builtYear,
                        year,
                        Heating,
                        med_inc,
                        tot_pop,
                        tt_work,
                        white_pop,
                        vac_occ), 
        method = "lm", trControl = fitControl, na.action = na.pass)

# The resulting Mean Absolute Error (MAE) of running this line tells us how
# successful our model is at predicting unknown data. 
regression.10foldcv

