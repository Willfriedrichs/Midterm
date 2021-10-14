library(tidyverse)
library(tidycensus)
library(sf)
library(kableExtra)
library(dplyr)
library(ggcorrplot)

#######################################################
# Loading data, finding correlation within studentData
#######################################################

# This loads all the data into "studentData".
studentData <- st_read("studentData.geojson", crs = 'ESRI:102254')

# The test data only includes rows comprising the test set.
testData <- filter(studentData, toPredict == 0)

# This function here attempts to fit a linear model to
# the relationship between "testData" variables.
ggplot(data = testData, aes(mainfloorSF, price)) +
       geom_point(size = .5) + 
       geom_smooth(method = "lm")

# This selects all numeric variables as preparation for the
# correlation analysis that follows.
numericVars <- 
  select_if(st_drop_geometry(testData), is.numeric) %>%
  select(!c(ExtWallSec, IntWall, Roof_Cover, Stories, UnitCount, MUSA_ID)) %>% 
  na.omit()

# Correlation analysis: pearson for each relationship
ggcorrplot(
  round(cor(numericVars), 1), 
  p.mat = cor_pmat(numericVars),
  colors = c("#25CB10", "white", "#FA7800"),
  type="lower",
  insig = "blank") +  
  labs(title = "Correlation across numeric variables") 

# This function allows for the plug-in of variables from "studentData".
testSignificance <- lm(price ~ ., data = numericVars %>% 
                    dplyr::select(price, 
                                  qualityCode,
                                  TotalFinishedSF,
                                  mainfloorSF))

# This gives us our r-squared value, which measures fit to the training data.
summary(testSignificance)

# Later, we'll need to try cross-validation to see how well the model predicts 
# for data it has never seen before. 