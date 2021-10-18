library(tidyverse)
library(tidycensus)
library(sf)
library(kableExtra)
library(dplyr)
library(ggcorrplot)
library(caret)
library(spdep)
library(ckanr)
library(FNN)
library(grid)
library(gridExtra)
library(jtools)     
library(ggstance)
library(rpart)
library(ggplot2)

#############################
#Loading functions and color palettes
#############################

root.dir = "https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/DATA/"

source("https://raw.githubusercontent.com/urbanSpatial/Public-Policy-Analytics-Landing/master/functions.r")

q5 <- function(variable) {as.factor(ntile(variable, 5))}

qbr <- function(df, variable, rnd) {
  if (missing(rnd)) {
    as.character(quantile(round(df[[variable]],0),
                          c(.01,.2,.4,.6,.8), na.rm=T))
  } else if (rnd == FALSE | rnd == F) {
    as.character(formatC(quantile(df[[variable]]), digits = 3),
                 c(.01,.2,.4,.6,.8), na.rm=T)
  }
}

palette5 <- c("#25CB10", "#5AB60C", "#8FA108",   "#C48C04", "#FA7800")

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
#Total number of bachelor's degrees in science and engineering related fields
census_api_key("94efffd19b56ad527e379faea1653ee74dc3de4a",overwrite = TRUE)

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
                      "B25013_006E","B08013_001E","B15012_009E","geometry") ) %>%
            rename(tot_pop = "B01001_001E",
                   empl_pop = "B23025_004E",
                   med_inc = "B06011_001E",
                   pvty_pop = "B06012_002E",
                   white_pop = "B02001_002E",
                   vac_occ = "B25002_003E",
                   own_occ_bach = "B25013_006E",
                   tt_work = "B08013_001E",
                   sci_bach = "B15012_009E") %>%
            mutate(area = as.numeric(st_area(geometry)/1000000))%>%
            mutate(pop_den = tot_pop/area)

  
              
#######################################################
# Loading data, finding correlation within studentData
#######################################################

# This loads all the data into "studentData".
studentData <- st_read("studentData.geojson", crs = 'ESRI:102254')%>%
  mutate(Age = 2021 - builtYear) %>%
  mutate(TotalBath = nbrThreeQtrBaths + nbrFullBaths + nbrHalfBaths)

studentData %>%
st_make_valid(geometry)

# Attach ACS data
studentData <- st_join(studentData, tracts19, join = st_within)

#Boulder County Boundary
BoulderCounty_Bundary <-
st_read("https://opendata.arcgis.com/datasets/964b8f3b3dbe401bb28d49ac93d29dc4_0.geojson")%>%
  select(geometry) %>%
  st_transform('ESRI:102254')

#Boulder Municipal Boundary
BoulderMuni_Boundary <-
  st_read("https://opendata.arcgis.com/datasets/9597d3916aba47e887ca563d5ac15938_0.geojson")%>%
  st_transform('ESRI:102254')%>%
  rename(Municipality = ZONEDESC)
  

#Map of Boulder County with Housing Sales Price and municipal boundary
ggplot()+
  geom_sf(data = BoulderCounty_Bundary, fill = "grey70") +
  geom_sf(data = BoulderMuni_Boundary, aes(fill = Municipality, alpha=0.5),colour = "white") +
  geom_sf(data = studentData, aes(colour = q5(price)),size=.85)+
  scale_colour_manual(values = palette5,
                      labels=qbr(studentData,"price"),
                      name = "Sale Price") +
  geom_sf(data = TrailHead, fill = "black")

# Median Housing Price in Each Municipality
House_in_muni_boundary <- st_intersection(studentData, BoulderMuni_Boundary) %>%
  mutate(pct_pvty = pvty_pop/tot_pop * 100,
         pct_white = white_pop/tot_pop * 100,
         )

Municipality.Summary <-
  st_drop_geometry(House_in_muni_boundary) %>%
  group_by(Municipality) %>%
  summarize(Medium.Price = median(price, na.rm = T),
            Mean.Price = mean(price, na.rm = T),
            Population = mean(tot_pop, na.rm = T),
            "Poplation Density per km^2"= mean(pop_den,na.rm=T),
            "Median Income" = mean(med_inc, na.rm = T),
            "Poverty Rate" = mean(pct_pvty, na.rm = T),
             "White Poplation in %" = mean(pct_white, na.rm = T),
            "Building Age" = mean(Age,na.rm = T)) %>%
  arrange(desc(Medium.Price))

kable(Municipality.Summary, digits = 2) %>%
  kable_styling() %>%
  footnote(general_title = "\n",
           general = "Table 1")

# Creating dummy variables for municipalities
studentData <- st_join(studentData, BoulderMuni_Boundary, join = st_within)

studentData <- 
studentData %>%
  select(-c(OBJECTID, ZONECLASS, LASTUPDATE,LASTEDITOR, REG_PDF_URL, Shape_STArea__, 
            Shape_STLength__, ShapeSTArea,ShapeSTLength
            ))

studentData <- mutate(studentData, Loui_dummy = case_when(Municipality=="Louisville"~ 1, Municipality!="Louisville"~ 0))
studentData <- mutate(studentData, Ward_dummy = case_when(Municipality=="Ward"~ 1, Municipality!="Ward"~ 0))
studentData <- mutate(studentData, Jame_dummy = case_when(Municipality=="Jamestown"~ 1, Municipality!="Jamestown"~ 0))
studentData <- mutate(studentData, Nede_dummy = case_when(Municipality=="Nederland"~ 1, Municipality!="Nederland"~ 0))
studentData <- mutate(studentData, Boul_dummy = case_when(Municipality=="Boulder"~ 1, Municipality!="Boulder"~ 0))
studentData <- mutate(studentData, Erie_dummy = case_when(Municipality=="Erie"~ 1, Municipality!="Erie"~ 0))
studentData <- mutate(studentData, Lafa_dummy = case_when(Municipality=="Lafayette"~ 1, Municipality!="Lafayette"~ 0))
studentData <- mutate(studentData, Long_dummy = case_when(Municipality=="Longmont"~ 1, Municipality!="Longmont"~ 0))
studentData <- mutate(studentData, Lyon_dummy = case_when(Municipality=="Lyons"~ 1, Municipality!="Lyons"~ 0))
studentData <- mutate(studentData, Supe_dummy = case_when(Municipality=="Superior"~ 1, Municipality!="Superior"~ 0))

studentData$Loui_dummy[is.na(studentData$Loui_dummy)] <- 0
studentData$Ward_dummy[is.na(studentData$Ward_dummy)] <- 0
studentData$Jame_dummy[is.na(studentData$Jame_dummy)] <- 0
studentData$Nede_dummy[is.na(studentData$Nede_dummy)] <- 0
studentData$Boul_dummy[is.na(studentData$Boul_dummy)] <- 0
studentData$Erie_dummy[is.na(studentData$Erie_dummy)] <- 0
studentData$Lafa_dummy[is.na(studentData$Lafa_dummy)] <- 0
studentData$Long_dummy[is.na(studentData$Long_dummy)] <- 0
studentData$Lyon_dummy[is.na(studentData$Lyon_dummy)] <- 0
studentData$Supe_dummy[is.na(studentData$Supe_dummy)] <- 0

# Load Park data
GreenSpacePolygon <- st_read("County_Open_Space.geojson") %>%
                     st_transform('ESRI:102254')

Park <- GreenSpacePolygon[!is.na(GreenSpacePolygon$PARK_GROUP),] %>%
        st_centroid()
  

st_c <- st_coordinates



studentData <-
  studentData %>% 
  mutate(park_nn1 = nn_function(st_c(studentData), st_c(Park), 1),
         park_nn2 = nn_function(st_c(studentData), st_c(Park), 2),
         park_nn3 = nn_function(st_c(studentData), st_c(Park), 3),
         park_dist = st_distance(studentData,Park))

Park_in_buffer <-
  st_join(Park,st_buffer(studentData,500),join = st_within)

PKCount <-
  Park_in_buffer %>%
  count(MUSA_ID)%>%
  st_drop_geometry()

studentData <-
  left_join(studentData, PKCount, by = "MUSA_ID" )

studentData <-
  studentData %>%
  rename(PKCount500m = n)

studentData$PKCount500m[is.na(studentData$PKCount500m)] <- 0

# attach distance to green space data
# studentData %>% mutate(green_dis = st_distance(studentData, GreenSpacePolygon))

# Load Landmarks data
landmarksPolygon <- st_union(st_read("Natural_Landmarks.geojson")) %>%
  st_transform('ESRI:102254')

# attach distance to land marks data
studentData <- mutate(studentData, landmark_dist = st_distance(studentData, landmarksPolygon))

# Loading Trail Heads Locations
TrailHead <- 
  st_read("https://opendata.arcgis.com/datasets/5ade4ef915c54430a32026bcb03fe1d7_0.geojson") %>%
  st_transform('ESRI:102254')%>%
  select(geometry)

#Apply buffer counts and nearest neighbor function on trail heads
Trailhead_in_buffer <-
  st_join(TrailHead,st_buffer(studentData,1000),join = st_within)

TrailHeadCount <-
  Trailhead_in_buffer %>%
  count(MUSA_ID)%>%
  st_drop_geometry()

studentData <-
  left_join(studentData, TrailHeadCount, by = "MUSA_ID" )

studentData <-
  studentData %>%
  rename(TrailHead1000m = n)

studentData$TrailHead1000m[is.na(studentData$TrailHead1000m)] <- 0

studentData <-
  studentData %>% 
  mutate(
    head_nn1 = nn_function(st_c(studentData), st_c(TrailHead), 1),
    head_nn2 = nn_function(st_c(studentData), st_c(TrailHead), 2), 
    head_nn3 = nn_function(st_c(studentData), st_c(TrailHead), 3),
    head_nn4 = nn_function(st_c(studentData), st_c(TrailHead), 4), 
    head_nn5 = nn_function(st_c(studentData), st_c(TrailHead), 5))

studentData <-
  studentData %>%
  mutate(trail_dist = st_distance(studentData, TrailHead))

#Loading Playground Locations
Playground <- 
  st_read("Playground_Sites_Points.GEOJSON") %>%
  st_transform('ESRI:102254')%>%
  drop_na(PROPID)%>%
  select(geometry)

Playground.sf <-
  st_join(Playground,st_buffer(studentData,500),join = st_within)

PGCount <-
  Playground.sf %>%
  count(MUSA_ID)%>%
  st_drop_geometry()

studentData <-
  left_join(studentData, PGCount, by = "MUSA_ID" )
  
studentData <-
  studentData %>%
  rename(pgcount500m = n)

studentData$pgcount500m[is.na(studentData$pgcount500m)] <- 0
  

#Loading School Locations
Schools <- 
  st_read("CDPHE_CDOE_School_Locations_and_District_Office_Locations.GEOJSON")%>%
  st_transform('ESRI:102254')%>%
  filter(COUNTY == "BOULDER")
  
Private_School <-
  filter(Schools, startsWith(Type_, "Non-"))

studentData <-
  studentData %>% 
  mutate(
  school_nn1 = nn_function(st_c(studentData), st_c(Schools), 1))

studentData <-
  studentData %>%
  mutate(
    privatesch_nn1 = nn_function(st_c(studentData),st_c(Private_School),1),
    privatesch_nn2 = nn_function(st_c(studentData),st_c(Private_School),2),
    privatesch_nn3 = nn_function(st_c(studentData),st_c(Private_School),3),
    privat_dist = st_distance(studentData,Private_School))

#Loading Flood Plain
Floodplain <-
 st_read("https://opendata.arcgis.com/datasets/30674682e55e4e1b8b0e407b0fe23b9a_0.geojson") %>%
 st_transform('ESRI:102254')%>%
  st_union()

studentData <-
  studentData %>%
  mutate(flood_dist = st_distance(studentData, Floodplain))


# This selects all numeric variables as preparation for the
# correlation analysis that follows.

cleanData <- 
  select_if(st_drop_geometry(studentData), is.numeric) %>%
  select(!c(year, ExtWallSec, IntWall, Roof_Cover, Stories, UnitCount, MUSA_ID))

cleanData$Ac[(cleanData$Ac)== 0] <- NA
cleanData$Heating[(cleanData$Heating) == 0] <- NA



# The test data only includes rows comprising the test set.
# Remove Outliars
train.Data <- filter(cleanData, toPredict == 0) %>%
  filter(price < 30000000)%>%
  na.omit()
  
  
  
test.Data <- filter(cleanData, toPredict == 1)


# This function here attempts to fit a linear model to
# the relationship between "testData" variables.
ggplot(data = studentData, aes(Heating, price)) +
       geom_point(size = .5) + 
       geom_smooth(method = "lm")

# Correlation analysis: pearson for each relationship
ggcorrplot(
  round(cor(testData), 1), 
  p.mat = cor_pmat(testData),show.diag = TRUE,
  colors = c("#25CB10", "white", "#FA7800"),
  type="lower",
  insig = "blank") +  
  labs(title = "Correlation across numeric variables") 

# This function allows for the plug-in of variables from "studentData".
testSignificance <- lm(price ~ ., data = test.Data %>% 
                    dplyr::select(price, 
                                  qualityCode,
                                  TotalFinishedSF,
                                  mainfloorSF,
                                  Age,
                                  ))

# This gives us our r-squared value, which measures fit to the training data.
summary(testSignificance)

# We'll need to try cross-validation to see how well the model predicts for
# data it has never seen before, which will be more useful than r squared.

# This sets up k-fold cross-validation.
k = 100
fitControl <- trainControl(method = "cv", number = k)
set.seed(825)

# Multivariate regression 
reg1 <- lm(price ~ ., data = train.Data %>% 
             dplyr::select(price, 
                           section_num,
                           qualityCode,
                           TotalFinishedSF,
                           Age,
                           Ac,
                           Heating,
                           med_inc,
                           nbrRoomsNobath,
                           vac_occ,
                           landmark_dist,
                           white_pop,
                           tot_pop,
                           pvty_pop,
                           privat_dist,
                           head_nn5,
                           park_nn1,
                           Loui_dummy,
                           Ward_dummy,
                           Jame_dummy,
                           Nede_dummy,
                           Boul_dummy,
                           Erie_dummy,
                           Lafa_dummy,
                           Long_dummy,
                           TotalBath,
                           pgcount500m,
                           flood_dist))

summary(reg1)

# variables in the "select(...)" function are considered in the analysis here.
regression.100foldcv <- 
  train(price ~ ., data = train.Data %>% 
          select(price, 
                 section_num,
                 qualityCode,
                 TotalFinishedSF,
                 Ac,
                 Heating,
                 Age,
                 med_inc,
                 landmark_dist,
                 nbrRoomsNobath,
                 vac_occ,
                 tot_pop,
                 pvty_pop,
                 privat_dist,
                 head_nn5,
                 park_nn1,
                 Loui_dummy,
                 Nede_dummy,
                 Boul_dummy,
                 Erie_dummy,
                 Long_dummy,
                 Lyon_dummy,
                 pgcount500m,
                 TotalBath,
                 flood_dist
          ),
  method = "lm", trControl = fitControl, na.action = na.pass)



# The resulting Mean Absolute Error (MAE) of running this line tells us how
# successful our model is at predicting unknown data. 
regression.100foldcv

#################
#MAE Histogram
#This allows you to see the distribution of MAE of the folds

ggplot(regression.100foldcv$resample, aes(x=MAE))+
  geom_histogram(colour = "white",fill="orange")+
  labs(title="Distribution of MAE",x="Mean Absolute Error", y = "Count",
       subtitle = "k-fold cross-validation; k = 100",
       caption = "Figure 2")+
  theme_apa()

#################
#Run model on unpredicted dataset

test.Data <-
  test.Data %>%
  mutate(SalePrice.Predict = predict(reg1, test.Data))
         
test.Data <-
  test.Data %>%
        mutate(SalePrice.Error = SalePrice.Predict - price,
         SalePrice.AbsError = abs(SalePrice.Predict - price),
         SalePrice.APE = (abs(SalePrice.Predict - price)) / SalePrice.Predict)%>%
  filter(SalePrice < 5000000)

