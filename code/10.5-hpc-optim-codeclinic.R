#### rubberexpansion-gridsearch-hpc for CODE CLINIC #####

#packages
#library(bbmle) # Ben Bolker's Maximum likelihood optimiser
library(parallel)
library(dplyr)

#library(sf) #unable to install on hpc


# Folder paths
setwd('/data/bop17mw/ch2/')
OutFolder <- "output/"
IntFolder <-"output/intermediate/"
TTFolder <-"output/intermediate/traintest"


#### -------Parameters ------####

# Define start and end years
startYr <- 2001
endYr <- 2014
currentYr <- 2018

# Labour Constants
plantingDensity = (555 + 400 +500 + 577 + 500 + 400 + 550 + 450 + 760/1.6)/9
treesPerDay = 760
manDayPerHa.bytree = plantingDensity / treesPerDay
manDayPerHa.XSBN =  (1/1.3 + 1/3.1)
timesPerWeek = 2
manDayPerHaWeek = (manDayPerHa.bytree + manDayPerHa.XSBN) * timesPerWeek
manDayPerHaPerYear = ((manDayPerHaWeek * 52) + 115.7)/2

# Fertilizer Constants
fertInputTonPerHa = (0.9435 + 0.175 + 0.2885 + 0.422 +0.105 + 0.06664 + 0.0385936 + 75/1000)/8

# Transportation Constants
truckCapacity = 18;
rubberDensity = 0.9112;
tonsPerTruck = (truckCapacity*rubberDensity)
tonsPerTruck = 1.232
avgSpeedTruck = 60;
fuelConsumption = 43.5/100

#Timber Constant
tHoriz = 25;

# Summary of Required Constants for NPVcalc (same for all regions), defined here
manDayPerHaPerYear; fertInputTonPerHa; tHoriz; plantingDensity
truckCapacity; rubberDensity; avgSpeedTruck; fuelConsumption



#### -------Datasets: Cells ------####

inputlist <- list.files(TTFolder, pattern="train10_buffer.csv", full.names=TRUE)

inputcells <- read.csv(inputlist[[1]]) #36715obs, CHN 10% #320978, KHM

inputcells <- inputcells %>% mutate(forest = ifelse(.$lcc2000 %in% c(2,3), 1, 0))
inputcells <- inputcells %>% mutate(convtd.in.2000 = ifelse(.$lcc2000==8, 1, 0))



#### -------Datasets: Agri-Rent Table ------####
# this table was created in a previous script, 02-agrent-mmsea.R
MMSEA_AgriRent4 <- read.csv(paste0(IntFolder, 'MMSEA_AgriRent.csv'))



#### ---- country average for agri-rent (not used, but left here as default values) #######

meanYieldAve <- mean(MMSEA_AgriRent4$meanYield[MMSEA_AgriRent4$Year %in% (startYr-1):endYr] )
wageAve <- mean(MMSEA_AgriRent4$USD_day_wageDfl[MMSEA_AgriRent4$Year %in% (startYr-1):endYr] )
priceFertAve <- mean(MMSEA_AgriRent4$FertPriceDfl[MMSEA_AgriRent4$Year %in% (startYr-1):endYr] )
priceDieselAve <- mean(MMSEA_AgriRent4$DieselPriceDfl[MMSEA_AgriRent4$Year %in% (startYr-1):endYr] )
priceGasAve <- mean(MMSEA_AgriRent4$GasPriceDfl[MMSEA_AgriRent4$Year %in% (startYr-1):endYr] )
priceTimberAve <- mean(MMSEA_AgriRent4$TimbPriceHaDfl[MMSEA_AgriRent4$Year %in% (startYr-1):endYr] )

avePrices <- c(meanYieldTemp = meanYieldAve,
               wageTemp = wageAve,
               priceFertTemp = priceFertAve,
               priceDieselTemp = priceDieselAve,
               priceTimberTemp = priceTimberAve)
rm(meanYieldAve, wageAve, priceFertAve, priceDieselAve, priceGasAve, priceTimberAve)

# a bunch of variables in vonThunen fxn here will be defined later, in CropExpansion fxn
head(inputcells)


#### -------NPV + EAC calculation  ####

discountRate <- 0.10 #Warren-Thomas 2018
YrCycle <- 25 #25year cycle assumed for OP
YrMature <- 7 # say rubber only produces fruit after 7 years

# create df (array to multiply NPV across different years)
NPV_df <- data.frame(
  NPV_df.year = 1:YrCycle, # include 0 as the year cleared. USE 1 if clear in same yr!
  NPV_df.timber = c(1, rep(0, YrCycle-1)), # timber is only in year 0.
  NPV_df.yield = c(rep(0, YrMature), rep(1, YrCycle - YrMature)),
  NPV_df.fert = rep(1, YrCycle),
  NPV_df.labour = rep(1, YrCycle)
)

## function calculates NPV, adapted from Maria's NPVcalc function.
# applies to each row of unconverted cells
# this function is independent of the vonThunen fxn: i included calculations based on other prices
# uses the NPV_df created above

NPVcalc3 <- function(forest, acc_min, suitability, meanYield, meanSuit, priceCropTemp, wageTemp, priceFertTemp, priceDieselTemp, priceTimberTemp, DeflatorRateTemp, NPV_df = NPV_df){
  require(dplyr)

  # # #define values from MMSEA_cells (spatial variables: varies across cells/space)
  forest = forest # tableRentsRow$oilPalmSuiCorr
  acc_min = acc_min  # tableRentsRow$acc_min
  suitability = suitability
  meanYield = meanYield
  meanSuit = meanSuit
  priceCropTemp = priceCropTemp
  wageTemp = wageTemp
  priceFertTemp = priceFertTemp
  priceDieselTemp = priceDieselTemp
  priceTimberTemp = priceTimberTemp
  DeflatorRateTemp = DeflatorRateTemp

  # calculate von Thunen ag rent (yearly variables:vary across years) to feed into NPV calc

  # scale Yield with suitability #### 23/08/2018
  yieldSuit = meanYield*(suitability/meanSuit)

  grossBenef = yieldSuit*priceCropTemp #profit for OP sales (USD/ha y) = OP Yield (ton/ha y) * OP price (USD/ton)

  labInput = wageTemp*manDayPerHaPerYear # daily min wage * manDay per ha per year. so we can get USD per ha per year
  fertInput = priceFertTemp*fertInputTonPerHa #atm use fixed fert input
  numTrips = yieldSuit/tonsPerTruck #this is per ha.y to be consistent
  driverCost = 2*numTrips*(wageTemp/8)*(acc_min/60)
  fuelCost = 2*numTrips*avgSpeedTruck*(acc_min/60)*priceDieselTemp*fuelConsumption
  timberBenef = priceTimberTemp*forest*0.8 #20% wastage, only when lcc is forest
  #rubberwoodBenef = (rubberwoodPrice.tree / plantingDensity ) / tHoriz

  NPVcalc_df <- NPV_df %>%
    ## first, total returns over a cycle, split up by years
    mutate(costsbyyear =
             grossBenef * NPV_df.yield +  # returns from OP, excluding years before mature
             timberBenef * NPV_df.timber - #timber only for the 0th year, other years 0 gain.
             fertInput * NPV_df.fert - #fert+labour same throughout
             labInput * NPV_df.labour - #fert+labour same throughout
             (driverCost+fuelCost) * NPV_df.yield, #driver+fuel cost dpend on yield, after maturity
           div = (1 + discountRate )^(NPV_df.year-1), # divide yearly rent by (1+ discountrate)^year, since start from Year 1 instead of Year 0, the first year should be '0' aka there shouldn't be a denominator for first year.
           NPVsub = costsbyyear/div )
  return(sum(NPVcalc_df$NPVsub)) #NPV is summation over all years in cycle

}


#### BUFFER FXN: The next fxn returns the number of plantation cells within a circular buffer
# note that here that 'converted_cells' is inbuilt. I've written so it should update itself each iteration
#within the crop expansion function
# 'converted_cells' are the cells that are OP plantations/have been converted to OP. This will be created in the Crop Expansion fxn.
# hint: the code makes much more sense if you draw a diagram

amt_crop_buffer  <- function(x) #x = cell position
{
  buffer <- 0.1   #adjust the buffer size. 1 deg was too big. .1 seemed alright
  xcoord <- as.numeric(x$POINT_X)
  ycoord <- as.numeric(x$POINT_Y)

  # filter out cells that are plantations, and within 1 unit either side of the cell (square buffer ok??)
  sq_buffer_posi <- dplyr::filter(converted_cells, POINT_X >= (xcoord-buffer) & POINT_X <= (xcoord+buffer),
                                  POINT_Y >= (ycoord-buffer) & POINT_Y <= (ycoord+buffer) )

  #calculate euclidian distance within square.makes circle
  Circ_buffer_posi<- mutate(sq_buffer_posi,
                            eucDisttocell = sqrt( (xcoord - POINT_X)^2 + (ycoord - POINT_Y)^2 ) )  %>%
    dplyr::filter(., eucDisttocell <= buffer)#then filter out those outside the buffer distance
  pr_length <- nrow(Circ_buffer_posi)
  return(pr_length)
} #fxn end

#### calculate no. of cells within buffer. DONE in separate script ####
# The script 06-NCells_buffer.R calculates the total number of cells in a 0.1deg buffer (buffer size can be changed), This column will be used later for calculating proportion/percentage of of cells within 0.1deg buffer that is existing rubber, i.e. the spatial contagion effect S

print(range(inputcells$NCells_buffer)) #check there are no 0s: can't divide 0.






#######  Crop Expansion Model ######################

##### + Mark cells that should be excld from conversion (filter out water, other crops, urban, suitability0 ####

inputcells <- inputcells %>% mutate(excld.from.unconverted = ifelse(
  (is.na(.$suitability)==TRUE |
     suitability==0 |
     lcc2000 %in% c(1,11,8,9,10,12,14,16,17) |
     (is.na(.$urban)==FALSE & urban == 1 & lcc2000==5) # | #15 urban cells
   #pa==1
  ), 1, 0) ) #1=exclude, 0=allow conversion


CropExpansionYr <- function(converted_cells, unconverted_cells, startPAR, cells, agRent, contagionfx){
  # startPAR is a vector of starting parameter values. the weights for K (national threshold calibration), S (spatial contagion effect, also referred to as buffer), and other params if used.

  starttime <-  Sys.time()
  print(startPAR); print(contagionfx)

  ##  Define our parameters ('coefficients', or 'weights').
  #FL note: I multiply weights by 100 (make sure starting values are adjusted before putting into fxn), to speed mle
  Threshold_calib <- as.numeric(startPAR[,1] *1000) #K (1000*100)
  Crop_buffer_weight <- as.numeric(startPAR[,2] *100) #Spatial contagion, S (50*100)

  #creates dataset with predicted converted or not (binary) each year. Allows to track yearly expansion later
  ConvertedbyYear <- cells %>% dplyr::select(., countryGID, uniqueCell_ID, POINT_X, POINT_Y, excld.from.unconverted, outcome_bin, convtd.in.2000)


  ## ~~START crop expansion loop ####

  #  for(yr in 2001:2014)    {
  for(yr in startYr:endYr)    {
    otherPricesYEAR <- dplyr::filter(agRent, Year == yr)

    YearlyRents <- unconverted_cells
    YearlyRents <- left_join(YearlyRents, otherPricesYEAR, by = "countryGID")

    assign('converted_cells',converted_cells, envir = .GlobalEnv)
    assign('YearlyRents',YearlyRents, envir = .GlobalEnv) # assign into global env

    ## Calculate NPV at each cell based on prices. adds column called NPV
    NPV <- mclapply(1:nrow(YearlyRents), function(i, ...) {
      y=YearlyRents[i,]
      NPVcalc3(forest = y[,'forest'],
               acc_min = y[,'acc_min'],
               suitability = y[,'suitability'],
               meanYield = y[,'meanYield'],
               meanSuit = y[,'meanSuit'],
               priceCropTemp = y[,'PriceDfl'],
               wageTemp = y[,'USD_day_wageDfl'],
               priceFertTemp = y[,'FertPriceDfl'],
               priceDieselTemp = y[,'DieselPriceDfl'],
               priceTimberTemp = y[,'TimbPriceHaDfl'],
               DeflatorRateTemp = y[,'DeflatorRate'],
               NPV_df = NPV_df)} ,
      mc.cores=ncores, mc.set.seed=TRUE)

    NPV <- unlist(NPV, use.names=FALSE)
    YearlyRents <- cbind(YearlyRents, NPV=as.numeric(NPV) )

    ## then calculate Equivalent Annual Cost (so that comparisons are made in terms of $/ha/year instead)
    YearlyRents <- mutate(YearlyRents, EAC=(NPV*discountRate)/(1 - (1+discountRate)^-25))


    #### ~additCost2: PROP_crop_Buffer #######
    # calculate percentage of OP cells (=converted) within buffer

    if(Crop_buffer_weight != 0){

      N_crop_inBuffer <- mclapply(1:nrow(YearlyRents), function(i, ...) {amt_crop_buffer(x=YearlyRents[i, c('POINT_X', 'POINT_Y')])} , mc.cores=ncores, mc.set.seed=TRUE)

      N_crop_inBuffer <- unlist(N_crop_inBuffer, use.names=FALSE)

      YearlyRents <- cbind(YearlyRents, N_crop_inBuffer=as.numeric(N_crop_inBuffer) )
      YearlyRents <- mutate(YearlyRents, prop_crop_Buffer=N_crop_inBuffer/NCells_buffer*100)

      ## calculate crop_pres_wt, the additional costs/weight of the n_cells in Buffer
      if(contagionfx =='linear'){ ##linear, (propCrop) * (negative weight)
        YearlyRents <- mutate(YearlyRents, crop_pres_wt = (prop_crop_Buffer) * (-Crop_buffer_weight))} else

          if(contagionfx =='log'){ ##log, log(propCrop+1) * (negative weight);
            # more cells less cost needed, but the relationship is less strong with more?
            YearlyRents <- mutate(YearlyRents, crop_pres_wt = log(prop_crop_Buffer+1) * (-Crop_buffer_weight))} else

              if(contagionfx =='sqrt'){ ##sqrt (allows for 0s and 1s?): sqrt(propCrop) * (negative weight)
                YearlyRents <- mutate(YearlyRents, crop_pres_wt = sqrt(prop_crop_Buffer) * (- Crop_buffer_weight))}

    }

    ## else if OP weight is 0, treat as no effect
    if(Crop_buffer_weight == 0)
    {YearlyRents <- mutate(YearlyRents, N_crop_inBuffer=0, crop_pres_wt=0)}

    #print(range(YearlyRents$N_crop_inBuffer))
    #print(range(YearlyRents$crop_pres_wt))

    #### ~Adjust EAC rents by Threshold and Contagion ####
    YearlyRents <- mutate(YearlyRents,
                          Thresh_dfl = as.numeric(Threshold_calib)*DeflatorRate/100, #24/08/2018 added deflator
                          adjEAC = EAC - Thresh_dfl - crop_pres_wt, #- cost_suit, #using EAC

                          Crop_threshold = ifelse(adjEAC > 0, yes=meanYield, no=0), #should be yield value but i don't have that
                          Converted = ifelse(adjEAC > 0, yes=1, no=0) ) # make binary: yes 1 no 0


    # Select columns to return (EAC components)
    YearlyRents_toreturn <- YearlyRents %>% dplyr::select(uniqueCell_ID, POINT_X, POINT_Y, EAC, N_crop_inBuffer, adjEAC)

    # Add yr as suffix to EAC components
    names(YearlyRents_toreturn)[4:6] <- paste(names(YearlyRents_toreturn)[4:6], yr, sep='_')

    # Join to df to be returned
    ConvertedbyYear <- ConvertedbyYear %>% left_join(YearlyRents_toreturn, by = c('uniqueCell_ID', 'POINT_X', 'POINT_Y')) #make sure EAC and prop_crop_Buffer values get returned

    # converted cells join with last iteration
    newlyconverted <- YearlyRents %>% filter(Converted==1) %>% mutate(yearConverted = yr)
    newlyconverted <- newlyconverted %>% dplyr::select(names(converted_cells)) #select only columns that are in converted_cells so can rbind
    converted_cells <- rbind(converted_cells, newlyconverted)


    # join to df to be returned (ConvertedbyYear)
    ConvertedbyYear <- ConvertedbyYear %>% left_join(dplyr::select(converted_cells, uniqueCell_ID, POINT_X, POINT_Y, yearConverted), by = c('uniqueCell_ID', 'POINT_X', 'POINT_Y'))

    # change yearConverted to binary (converted=1, unconverted=0)
    ConvertedbyYear <- ConvertedbyYear %>% mutate(yearConverted=ifelse(is.na(yearConverted)==TRUE, 0, 1) )

    # change col.name to indicate yr converted
    ConvertedbyYear <- ConvertedbyYear %>% rename(!!paste('convtd.in.', yr, sep='') := yearConverted)

    # unconverted cells: overwrite previous year's unconverted cells, moves to next year in the loop
    unconverted_cells_year <- dplyr::filter(YearlyRents, Converted==0) #filter only remaining unconverted cells
    unconverted_cells <- dplyr::select(unconverted_cells_year, names(unconverted_cells)) # select only column names needed as input for unconverted_cells for next loop iteration of yr in startYr:endYr

  } # ~ end Crop expansion loop ####

  print(Sys.time()- starttime)
  return(ConvertedbyYear)
} #function end


###### Load EvalMetrics Code #####
# source ConfusionMatrix and EvaluateMet code from a separate script
source('09-classification.eval.metrics-notidyr.R')



###-------- ++++ Parameters for mclapply ++++ -------- #####
ncores = 8 #max4 on interactive hpc node
set.seed(1234)



######## -------- OPTIMISER -------- ########
# Likelihood fxn calculates the -ve sum binomial between observed and expected (from model)
# i.e. the error between observed and the model (neg error squared and summed)

if(startYr == 2001) # IF start from 2001 ##
{ converted_cells_Start <- inputcells %>% dplyr::filter(lcc2000 == 8) %>% mutate(yearConverted = 2000)
unconverted_cells_Start <- inputcells %>% dplyr::filter(excld.from.unconverted==0)
}

### Likelihood fxn calculates the -ve sum binomial (negative sum of squares, logged) between obseved and expected (from model)

likelihood <- function(a)
{ # run crop expansion model fxn first.
  cat('\n')
  cat('\n')

  CE_2014 <- CropExpansionYr(converted_cells = converted_cells_Start, unconverted_cells = unconverted_cells_Start, startPAR = a, cells = inputcells, agRent = MMSEA_AgriRent4, contagionfx=contagionfxmle)

  # ~~ remove starting plantations & exclded lu types from model eval #
  CE_2014 <- dplyr::filter(CE_2014, excld.from.unconverted==0) #only the cells allowed to be converted

  #predicted <- CE_2014$convtd.in.2013 #pred results for year 2014 WITH 1-YR-TIMELAG so its 2013
  predicted <- CE_2014 %>% dplyr::select(!!paste0('convtd.in.', finalyearoutput))
  predicted <- as.numeric(unlist(predicted))

  p.pred <- ifelse(predicted == 1, yes=0.9999, no=0.0001) #necessary for dbinom to work
  observed <- as.numeric(CE_2014$outcome_bin)

  likel <- -sum(dbinom(observed, size= 1, prob=p.pred,log=TRUE)) ## -ve sum sq (logged)
  print(likel) #print likelihood for ref

  #save model results info
  cm <- ConfusionMatrix(actual=observed, predicted=predicted)
  metricstbl <- EvaluateMet(actual=observed, predicted=predicted, cm=cm)

  para_iter <- data.frame(Threshold_calib_param = a[1], Crop_buffer_param = a[2], contagionfx = contagionfxmle, likelihood = likel, row.names = NULL) %>% cbind(metricstbl)

  parametersave <- rbind(parametersave, para_iter )
  assign('parametersave', parametersave, envir = .GlobalEnv) #assign the dataset into global envt to then save
  #save as txt file. overwrites each time!
  write.csv(parametersave, file=parametersaveFilename, sep = "\t", row.names = FALSE)

  ##print for reference while running.
  cat('\n')
  print(cm) ## print confusion matrix for ref
  print(para_iter[,c(1:6)]) ## print likelihood, accuracy and recall for easy ref
  cat('\n')

  -sum(dbinom(observed, size = 1, prob = p.pred, log = TRUE)) ## -ve sum sq

} #function end
parnames(likelihood) <- c('Threshold_calib_param', 'Crop_buffer_param') #, "suit_param"


likelihood_noContagion <- function(x) {
  paras_noContagion<- c(x[1], 0) ##keeps contagion at 0
  likelihood(a = paras_noContagion)}
parnames(likelihood_noContagion) <- c('Threshold_calib_param')



#### + Running the optimiser ! #####
startYr <- 2001
endYr <- 2005
timelag <- 1
finalyearoutput <- endYr-timelag

traincountry <- "KHM_train10" ###change this manually, it's used in saving filename



# + + optim for K + linear S ####
model <- "3-K+linearS" #1-EAConly, "2-Konly", 3-K+linearS, 4-K+sqrtS
parametersave <- NULL
parametersaveFilename <- paste0(OutFolder, 'parmetbl_mle.', Sys.Date(),'.', traincountry, '.model', model, '.csv') #

startPARmle <- c(Threshold_calib_param=7.5, Crop_buffer_param=0.1) #best starting param from manual grid search
contagionfxmle <- "linear"
OPTI_coefs <- mle2(likelihood, start = startPARmle, vecpar = TRUE, default.start = TRUE, method="SANN", trace=TRUE) #go on forever, manually stop
