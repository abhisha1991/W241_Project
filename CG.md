Experiments and Causality: W241 Final Project Coast Guards
================
Hanyu, Brendan and Abhi
4/2021

``` r
library(data.table)
library(sandwich)
library(lmtest)
library(knitr)
library(stargazer)
library(dplyr)
```

## Feature Engineering

``` r
d <- fread("data.csv",  na.strings=c("","NA"))

# head(d)
```

``` r
# display number of columns
length(d[0])
```

    ## [1] 124

``` r
# ResponseID is our primary key for the table as it is unique for each row and is guaranteed to be present
nrow(d[, .(count=.N), by = list(ResponseID)]) == nrow(d)
```

    ## [1] TRUE

``` r
d[, .(count=.N), by = list(ResponseType)]
```

    ##      ResponseType count
    ## 1: Survey Preview     8
    ## 2:     IP Address   304
    ## 3:           Spam     2

``` r
d[, .(count=.N), by = list(Finished)]
```

    ##    Finished count
    ## 1:     TRUE   284
    ## 2:    FALSE    30

``` r
d[, .(count=.N), by = list(Progress)]
```

    ##     Progress count
    ##  1:      100   285
    ##  2:       26     1
    ##  3:        1     5
    ##  4:       16     1
    ##  5:       13     1
    ##  6:        0     4
    ##  7:       40     2
    ##  8:       12     1
    ##  9:       89     1
    ## 10:       53     1
    ## 11:       14     1
    ## 12:       67     1
    ## 13:       63     1
    ## 14:       27     1
    ## 15:       99     1
    ## 16:       10     1
    ## 17:       15     2
    ## 18:       21     1
    ## 19:       49     1
    ## 20:       78     1
    ## 21:       69     1
    ##     Progress count

``` r
# gets a row by the field "Name" in dataframe "data"
getRowByName <- function(name, data) {
  return (data[data$Name == name, ]) 
}

# gets a row by the field "TreatmentCol" in dataframe "data"
getRowByTreatmentCol <- function(num, data) {
  return (data[data$TreatmentCol == paste("TreatmentQ", num, sep=""), ]) 
}

# gets a row by the field "ControlCol" in dataframe "data"
getRowByControlCol <- function(num, data) {
  return (data[data$ControlCol == paste("ControlQ", num, sep=""), ]) 
}

# gets a row by the field "ResponseID" in dataframe "data"
getRowByResponseID <- function(responseId, data) {
  return (data[data$ResponseID == responseId, ]) 
}

# are all treatment questions NA (condition for participant being in control group)
areAllTreatmentQNA <- function(data, rowNum) {
  return (is.na(data[rowNum]$TreatmentQ1) & is.na(d[rowNum]$TreatmentQ2) & is.na(d[rowNum]$TreatmentQ3) &   is.na(d[rowNum]$TreatmentQ4) & is.na(d[rowNum]$TreatmentQ5) & is.na(d[rowNum]$TreatmentQ6))
}

# are all control questions NA (condition for participant being in treatment group)
areAllControlQNA <- function(data, rowNum) {
  return (is.na(data[rowNum]$ControlQ1) & is.na(d[rowNum]$ControlQ2) & is.na(d[rowNum]$ControlQ3) &   is.na(d[rowNum]$ControlQ4) & is.na(d[rowNum]$ControlQ5) & is.na(d[rowNum]$ControlQ6))
}

# gets number of treatment columns that are NA 
getSumTreatmentQNA <- function(data, rowNum) {
  
  someTreatmentNA = c(is.na(data[rowNum]$TreatmentQ1), is.na(d[rowNum]$TreatmentQ2),  is.na(d[rowNum]$TreatmentQ3), is.na(d[rowNum]$TreatmentQ4), is.na(d[rowNum]$TreatmentQ5), is.na(d[rowNum]$TreatmentQ6))
  
  return (sum(someTreatmentNA))
}

# gets number of control columns that are NA 
getSumControlQNA <- function(data, rowNum) {
  
  someControlNA = c(is.na(data[rowNum]$ControlQ1), is.na(d[rowNum]$ControlQ2), is.na(d[rowNum]$ControlQ3),   is.na(d[rowNum]$ControlQ4), is.na(d[rowNum]$ControlQ5), is.na(d[rowNum]$ControlQ6))
  
  return (sum(someControlNA))
}

# if all 12 columns of treatment and control are NA, then the row is invalid
# represents a likely non-complier (never taker)
isInvalidRow <- function(data, rowNum) {
  return(getSumControlQNA(data, rowNum) + getSumTreatmentQNA(data, rowNum) == 12)
}

# if person was assigned to either treatment or control, but didn't end up answering all questions
# if person didn't answer any of the questions, then also they are considered an attritor
isAttritedRow <- function(data, rowNum) {
  cols = getSumControlQNA(data, rowNum) 
  result = (cols < 6 & cols > 0)
  if (result) {
    return (result)
  }
  
  cols = getSumTreatmentQNA(data, rowNum)
  result = (cols < 6 & cols > 0)
  return (result | isInvalidRow(data, rowNum))
}

# we cannot have some NA in both treatment and control
isInvalidTreatmentAssignment <- function(data, rowNum) {
  controlNa = getSumControlQNA(data, rowNum) 
  treatmentNa = getSumTreatmentQNA(data, rowNum)
  return ((controlNa > 0 & controlNa < 6) & (treatmentNa > 0 & treatmentNa < 6))
}

# generating random numbers from low to high for fake treatment/control responses
generateRandomIntNums <- function(n, mean, sd, low=1, high=7) {
  
  nums = mean + sd * scale(rnorm(n))
  # numbers must be 1-5
  for (i in 1:nrow(nums)) {
    
    # arbitrarily choose floor or ceiling for that number (since response must be integer)
    if(sample(c(0,1), size =1))  {
      nums[i] = ceiling(nums[i])
    }
    else {
      nums[i] = floor(nums[i]) 
    }
    
    # bound number between 1-7
    if(nums[i] < low) {
      nums[i] = low
    }
    
    if(nums[i] > high) {
      nums[i] = high
    }
  }
  
  return(nums[1:n])
}
```

``` r
nameResumeBinding <- data.table(
  Name  = c("Bradley Meyer", "Reginald Washington", "Kirsten Schmidt", "Gwendolyn Jackson"),
  Race = c("White", "Black", "White", "Black"),
  Gender = c("Male", "Male", "Female", "Female"),
  RaceGender = c("WM", "BM", "WF", "BF"),
  ResumeId   = c(1, 2, 3, 4)
)
```

``` r
treatmentBinding <- data.table(
  LeftPersonName  = c("Bradley Meyer", "Bradley Meyer", "Bradley Meyer", "Kirsten Schmidt", "Reginald Washington", "Kirsten Schmidt"),
  RightPersonName = c("Reginald Washington", "Kirsten Schmidt", "Gwendolyn Jackson", "Reginald Washington", "Gwendolyn Jackson", "Gwendolyn Jackson"),
  TreatmentCol = c("TreatmentQ1", "TreatmentQ2", "TreatmentQ3", "TreatmentQ4", "TreatmentQ5", "TreatmentQ6")
)
```

``` r
treatmentMetadata <- data.table(treatmentBinding)
n <- nrow(treatmentMetadata)

# fill dummy values to create column names
for (i in 1:n) {
  treatmentMetadata$LeftPersonRace = "test"
  treatmentMetadata$LeftPersonGender = "test"
  treatmentMetadata$LeftPersonRaceGender = "test"
  treatmentMetadata$LeftPersonResumeId = "test"
   
  treatmentMetadata$RightPersonRace = "test"
  treatmentMetadata$RightPersonGender = "test"
  treatmentMetadata$RightPersonRaceGender = "test"
  treatmentMetadata$RightPersonResumeId = "test"
}

# iterate through left name and fill left person metadata
for (i in 1:n) {
  row <- getRowByName(treatmentMetadata[i]$LeftPersonName, nameResumeBinding)
   
  treatmentMetadata[i]$LeftPersonRace = row$Race
  treatmentMetadata[i]$LeftPersonGender = row$Gender
  treatmentMetadata[i]$LeftPersonRaceGender = row$RaceGender
  treatmentMetadata[i]$LeftPersonResumeId = row$ResumeId
}

# iterate through right name and fill right person metadata
for (i in 1:n) {
  row <- getRowByName(treatmentMetadata[i]$RightPersonName, nameResumeBinding)
   
  treatmentMetadata[i]$RightPersonRace = row$Race
  treatmentMetadata[i]$RightPersonGender = row$Gender
  treatmentMetadata[i]$RightPersonRaceGender = row$RaceGender
  treatmentMetadata[i]$RightPersonResumeId = row$ResumeId
}
```

``` r
controlBinding <- data.table(
  LeftPersonResumeId  = c(1, 1, 1, 3, 2, 3),
  RightPersonResumeId = c(2, 3, 4, 2, 4, 4),
  ControlCol = c("ControlQ1", "ControlQ2", "ControlQ3", "ControlQ4", "ControlQ5", "ControlQ6")
)
```

``` r
# treatment and control assignment
n <- nrow(d)

# have valid columns for treatment assignment and attrition
d$TreatmentAssignment = NA
d$IsAttrited = NA
d$IsNeverTaker = NA

for (i in 1:n) {
  # row <- getRowByResponseID(d[i]$ResponseID, d)
  
  control <- areAllTreatmentQNA(d, i)
  treatment <- areAllControlQNA(d, i)
  isInvalid <- isInvalidRow(d, i)
  isAttrited <- isAttritedRow(d, i)
  isInvalidTreatmentAssigned <- isInvalidTreatmentAssignment(d, i)
  
  # print(c(control, treatment, isInvalid, isAttrited, isInvalidTreatmentAssigned))
  if (isInvalid | isInvalidTreatmentAssigned) {
    
    if (isInvalidTreatmentAssigned) {
      e = sprintf("Row %s has invalid treatment assignment", i)  
      stop(e)
    }
    
    if (isInvalid) {
      sprintf("Row %s has no responses in treatment and control columns", i)  
    }
    
    d[i]$TreatmentAssignment = NA
    
  }
  else {
    
    if (treatment & control) {
      e = sprintf("Both treatment and control are true! This should never happen! Error found in row %s", i)
      stop(e)
    }
    
    d[i]$TreatmentAssignment = treatment  
  }
  
  d[i]$IsAttrited = isAttrited
  d[i]$IsNeverTaker = isInvalid
  
  # special case, if we find attrition only, then we need to re-assign treatment  
  if (isAttrited & !(isInvalid | isInvalidTreatmentAssigned)) {
    
    numNa = getSumTreatmentQNA(d, i)
    treatment = numNa > 0 & numNa < 6    
    d[i]$TreatmentAssignment = treatment      
  
  }
}
```

``` r
d[, .(count=.N), by = list(TreatmentAssignment)]
```

    ##    TreatmentAssignment count
    ## 1:                  NA    32
    ## 2:               FALSE   143
    ## 3:                TRUE   139

``` r
d[, .(count=.N), by = list(IsAttrited)]
```

    ##    IsAttrited count
    ## 1:       TRUE    33
    ## 2:      FALSE   281

``` r
# do a sanity check of who we think are "attritors" vs what qualtrics thinks are "attritors"
responseIDsAttrited = d[IsAttrited == TRUE, ResponseID]
responseIDsBad = d[Finished == FALSE | ResponseType != "IP Address", ResponseID]
responseIDsUnFinishedOnly = d[Finished == FALSE, ResponseID]

# there seem to be a mismatch in what we consider attrited vs what Qualtrics thinks is attrited
length(responseIDsAttrited)
```

    ## [1] 33

``` r
length(responseIDsBad)
```

    ## [1] 40

``` r
length(responseIDsUnFinishedOnly)
```

    ## [1] 30

``` r
# Number of common elements between these sets
length(intersect(responseIDsAttrited, responseIDsBad))
```

    ## [1] 29

``` r
length(intersect(responseIDsAttrited, responseIDsUnFinishedOnly))
```

    ## [1] 26

``` r
# setdiff tells us the response IDs that exist in the first list, but dont exist in the 2nd list

print("These are the respondents we think are attritors but Qualtrics thinks are not attritors")
```

    ## [1] "These are the respondents we think are attritors but Qualtrics thinks are not attritors"

``` r
setdiff(responseIDsAttrited, responseIDsBad)
```

    ## [1] "R_1mquxpzqO154HvP" "R_sbzFZuUqtaBaCR3" "R_3PhFt6wfKWlz3LM"
    ## [4] "R_22SmK7qgpdwWIDV"

``` r
print("These are the respondents Qualtrics think are attritors but we think are not")
```

    ## [1] "These are the respondents Qualtrics think are attritors but we think are not"

``` r
setdiff(responseIDsBad, responseIDsAttrited)
```

    ##  [1] "R_2rORoun447CrzQB" "R_2QSquguoMN5Dj7A" "R_2Yn2e6TZ2jGGDgh"
    ##  [4] "R_V3IqeSnvscmn2Mx" "R_scHLCigTnnJHMVb" "R_31vJynnHBul11qv"
    ##  [7] "R_3fOa4QVxUnOCVUD" "R_zTKeFpVhVNAZPjj" "R_1E6Y207cj3ZVUY0"
    ## [10] "R_2BlcjDxd8DaWZio" "R_3rJpZq7KiJ8aUqE"

``` r
setdiff(responseIDsAttrited, responseIDsUnFinishedOnly)
```

    ## [1] "R_3kpkeubEVJMXVeY" "R_1mquxpzqO154HvP" "R_sbzFZuUqtaBaCR3"
    ## [4] "R_3Db9GpUuKV9bATZ" "R_2zo1aIOrEzj31Mt" "R_3PhFt6wfKWlz3LM"
    ## [7] "R_22SmK7qgpdwWIDV"

``` r
setdiff(responseIDsUnFinishedOnly, responseIDsAttrited)
```

    ## [1] "R_zTKeFpVhVNAZPjj" "R_1E6Y207cj3ZVUY0" "R_2BlcjDxd8DaWZio"
    ## [4] "R_3rJpZq7KiJ8aUqE"

``` r
# Examining the list that qualtrics thinks are attritors, we see in fact they are not attritors, so we'll stick to our definition
# example: d[ResponseID == "R_1E6Y207cj3ZVUY0"]
```

## Create Datasets to be used

``` r
cols <- colnames(d)
coreColumns <- cols[!startsWith(cols, "TimeFC_") & !startsWith(cols, "TimeLC_") & !startsWith(cols, "TimePS_") & !startsWith(cols, "TimeCC_") & !startsWith(cols, "TreatmentQ") & !startsWith(cols, "ControlQ")]

getCoreColumnsDataset <- function() {
  return(d %>% select(one_of(coreColumns)))
}

getTreatmentDataset <- function(qNum) {
  treatmentqNum = paste("TreatmentQ", qNum, sep ="")
  treatmentSet = (d %>% select(one_of(coreColumns) | ends_with(treatmentqNum)))
  return (treatmentSet[TreatmentAssignment == TRUE, ])
}

getControlDataset <- function(qNum) {
  controlqNum = paste("ControlQ", qNum, sep ="")
  controlSet = (d %>% select(one_of(coreColumns) | ends_with(controlqNum)))
  return (controlSet[TreatmentAssignment == FALSE, ])
}
```

``` r
dCore = getCoreColumnsDataset()

dTQ1 = getTreatmentDataset(1)
dTQ2 = getTreatmentDataset(2)
dTQ3 = getTreatmentDataset(3)
dTQ4 = getTreatmentDataset(4)
dTQ5 = getTreatmentDataset(5)
dTQ6 = getTreatmentDataset(6)

# sanity check to see all data sets are of the same size
nrow(dTQ1) == nrow(dTQ2)
```

    ## [1] TRUE

``` r
nrow(dTQ2) == nrow(dTQ3) 
```

    ## [1] TRUE

``` r
nrow(dTQ3) == nrow(dTQ4) 
```

    ## [1] TRUE

``` r
nrow(dTQ4) == nrow(dTQ5) 
```

    ## [1] TRUE

``` r
nrow(dTQ5) == nrow(dTQ6)
```

    ## [1] TRUE

``` r
dCQ1 = getControlDataset(1)
dCQ2 = getControlDataset(2)
dCQ3 = getControlDataset(3)
dCQ4 = getControlDataset(4)
dCQ5 = getControlDataset(5)
dCQ6 = getControlDataset(6)

# sanity check to see all data sets are of the same size
nrow(dCQ1) == nrow(dCQ2)
```

    ## [1] TRUE

``` r
nrow(dCQ2) == nrow(dCQ3) 
```

    ## [1] TRUE

``` r
nrow(dCQ3) == nrow(dCQ4) 
```

    ## [1] TRUE

``` r
nrow(dCQ4) == nrow(dCQ5) 
```

    ## [1] TRUE

``` r
nrow(dCQ5) == nrow(dCQ6)
```

    ## [1] TRUE

``` r
# head(dTQ1)
```

### Assign Fake Treatment and Control Reponses (Optional)

> As we are aware, we had an error in the procedure around collecting
> data for our participants. Thus we will overwrite the data with fake
> responses so as to prove out our analysis methodology. Below we choose
> a `random number generator to generate responses from 1-7`. The rubric
> is as follows:

> `1: Strongly prefer left resume`

> `2: Prefer left resume`

> `3: Slightly prefer left resume`

> `4: Neutral (no preference on left or right resume)`

> `5: Slightly prefer right resume`

> `6: Prefer right resume`

> `7: Strongly prefer right resume`

``` r
# We deliberately give fake treatment effects
dTQ1$TreatmentQ1 = generateRandomIntNums(nrow(dTQ1), 1, 2) # WM BM
dTQ2$TreatmentQ2 = generateRandomIntNums(nrow(dTQ2), 2, 1) # WM WF
dTQ3$TreatmentQ3 = generateRandomIntNums(nrow(dTQ3), 5, 1) # WM BF
dTQ4$TreatmentQ4 = generateRandomIntNums(nrow(dTQ4), 4, 1) # WF BM
dTQ5$TreatmentQ5 = generateRandomIntNums(nrow(dTQ5), 4, 1) # BM BF
dTQ6$TreatmentQ6 = generateRandomIntNums(nrow(dTQ6), 2, 1) # WF BF

# Note: means are typically underestimated
print("Treatment Means")
```

    ## [1] "Treatment Means"

``` r
mean(dTQ1$TreatmentQ1)
```

    ## [1] 1.841727

``` r
mean(dTQ2$TreatmentQ2)
```

    ## [1] 2.079137

``` r
mean(dTQ3$TreatmentQ3)
```

    ## [1] 5.05036

``` r
mean(dTQ4$TreatmentQ4)
```

    ## [1] 3.892086

``` r
mean(dTQ5$TreatmentQ5)
```

    ## [1] 4.064748

``` r
mean(dTQ6$TreatmentQ6)
```

    ## [1] 2.143885

``` r
# We expect the control group to have no difference in left / right resumes (on average)
dCQ1$ControlQ1 = generateRandomIntNums(nrow(dCQ1), 3, 2)
dCQ2$ControlQ2 = generateRandomIntNums(nrow(dCQ2), 5, 2)
dCQ3$ControlQ3 = generateRandomIntNums(nrow(dCQ3), 6, 2)
dCQ4$ControlQ4 = generateRandomIntNums(nrow(dCQ4), 4, 1)
dCQ5$ControlQ5 = generateRandomIntNums(nrow(dCQ5), 4, 1)
dCQ6$ControlQ6 = generateRandomIntNums(nrow(dCQ6), 5, 1)

# Note: means are typically underestimated
print("Control Means")
```

    ## [1] "Control Means"

``` r
mean(dCQ1$ControlQ1)
```

    ## [1] 3.097902

``` r
mean(dCQ2$ControlQ2)
```

    ## [1] 4.895105

``` r
mean(dCQ3$ControlQ3)
```

    ## [1] 5.552448

``` r
mean(dCQ4$ControlQ4)
```

    ## [1] 3.958042

``` r
mean(dCQ5$ControlQ5)
```

    ## [1] 4.041958

``` r
mean(dCQ6$ControlQ6)
```

    ## [1] 4.944056

``` r
addTreatmentMetadataToDataset <- function(dt, treatmentNum) {
  
  row = getRowByTreatmentCol(treatmentNum, treatmentMetadata)
  
  dt$LeftPersonName = row$LeftPersonName
  dt$LeftPersonRace = row$LeftPersonRace
  dt$LeftPersonGender = row$LeftPersonGender
  dt$LeftPersonRaceGender = row$LeftPersonRaceGender
  dt$LeftPersonResumeId = row$LeftPersonResumeId
    
  dt$RightPersonName = row$RightPersonName
  dt$RightPersonRace = row$RightPersonRace
  dt$RightPersonGender = row$RightPersonGender
  dt$RightPersonRaceGender = row$RightPersonRaceGender
  dt$RightPersonResumeId = row$RightPersonResumeId
  
  return(dt)
}

addControlMetadataToDataset <- function(dt, controlNum) {
  
  row = getRowByControlCol(controlNum, controlBinding)

  # only resume ID is available in the control group (by design)
  dt$LeftPersonName = NA
  dt$LeftPersonRace = NA
  dt$LeftPersonGender = NA
  dt$LeftPersonRaceGender = NA
  dt$LeftPersonResumeId = row$LeftPersonResumeId
  
  # only resume ID is available in the control group (by design)
  dt$RightPersonName = NA
  dt$RightPersonRace = NA
  dt$RightPersonGender = NA
  dt$RightPersonRaceGender = NA
  dt$RightPersonResumeId = row$RightPersonResumeId
  
  return(dt)
}

dTQ1 = addTreatmentMetadataToDataset(dTQ1, 1)
dTQ2 = addTreatmentMetadataToDataset(dTQ2, 2)
dTQ3 = addTreatmentMetadataToDataset(dTQ3, 3)
dTQ4 = addTreatmentMetadataToDataset(dTQ4, 4)
dTQ5 = addTreatmentMetadataToDataset(dTQ5, 5)
dTQ6 = addTreatmentMetadataToDataset(dTQ6, 6)

dCQ1 = addControlMetadataToDataset(dCQ1, 1)
dCQ2 = addControlMetadataToDataset(dCQ2, 2)
dCQ3 = addControlMetadataToDataset(dCQ3, 3)
dCQ4 = addControlMetadataToDataset(dCQ4, 4)
dCQ5 = addControlMetadataToDataset(dCQ5, 5)
dCQ6 = addControlMetadataToDataset(dCQ6, 6)
```

``` r
# First, we notice that the 2 datasets dont have the same columns
print("Before convergence")
```

    ## [1] "Before convergence"

``` r
identical(colnames(dTQ1), colnames(dCQ1))
```

    ## [1] FALSE

``` r
# Since the "Question columns" are named like "TimeFC_TreatmentQ1" in the treatment set and "TimeFC_ControlQ1" in the control set
# Thus we snap to naming these as "TimeFCQ" in both treatment and control. We add a dedicated column for Question Number as well to differentiate.
# The same naming convention goes for other columns - TimePS, TimeLS, TimeCC etc.

convergeTreatmentColumns <- function(dt, qNum) {
  names(dt)[names(dt) == paste("TimeFC_TreatmentQ", qNum, sep = "")] <- paste("TimeFCQ")
  names(dt)[names(dt) == paste("TimeLC_TreatmentQ", qNum, sep = "")] <- paste("TimeLCQ")
  names(dt)[names(dt) == paste("TimePS_TreatmentQ", qNum, sep = "")] <- paste("TimePSQ")
  names(dt)[names(dt) == paste("TimeCC_TreatmentQ", qNum, sep = "")] <- paste("TimeCCQ")
  names(dt)[names(dt) == paste("TreatmentQ", qNum, sep = "")] <- paste("ResponseQ")
  dt$QNum = qNum
  return(dt)
}

convergeControlColumns <- function(dt, qNum) {
  names(dt)[names(dt) == paste("TimeFC_ControlQ", qNum, sep = "")] <- paste("TimeFCQ")
  names(dt)[names(dt) == paste("TimeLC_ControlQ", qNum, sep = "")] <- paste("TimeLCQ")
  names(dt)[names(dt) == paste("TimePS_ControlQ", qNum, sep = "")] <- paste("TimePSQ")
  names(dt)[names(dt) == paste("TimeCC_ControlQ", qNum, sep = "")] <- paste("TimeCCQ")
  names(dt)[names(dt) == paste("ControlQ", qNum, sep = "")] <- paste("ResponseQ")
  dt$QNum = qNum
  return(dt)
}

dTQ1 = convergeTreatmentColumns(dTQ1, 1)
dCQ1 = convergeControlColumns(dCQ1, 1)

# now the test should pass 
print("After convergence")
```

    ## [1] "After convergence"

``` r
identical(colnames(dTQ1), colnames(dCQ1))
```

    ## [1] TRUE

``` r
# do the same for other questions

dTQ2 = convergeTreatmentColumns(dTQ2, 2)
dCQ2 = convergeControlColumns(dCQ2, 2)

dTQ3 = convergeTreatmentColumns(dTQ3, 3)
dCQ3 = convergeControlColumns(dCQ3, 3)

dTQ4 = convergeTreatmentColumns(dTQ4, 4)
dCQ4 = convergeControlColumns(dCQ4, 4)

dTQ5 = convergeTreatmentColumns(dTQ5, 5)
dCQ5 = convergeControlColumns(dCQ5, 5)

dTQ6 = convergeTreatmentColumns(dTQ6, 6)
dCQ6 = convergeControlColumns(dCQ6, 6)

# sanity check on column names for all datasets
identical(colnames(dTQ2), colnames(dCQ2))
```

    ## [1] TRUE

``` r
identical(colnames(dTQ3), colnames(dCQ3))
```

    ## [1] TRUE

``` r
identical(colnames(dTQ4), colnames(dCQ4))
```

    ## [1] TRUE

``` r
identical(colnames(dTQ5), colnames(dCQ5))
```

    ## [1] TRUE

``` r
identical(colnames(dTQ6), colnames(dCQ6))
```

    ## [1] TRUE

``` r
# now that we have converged on column names, we should be able to merge all data sets in row major format
appendAllDataTables <- function(tableList) {
  obj <- deparse(substitute(tableList[1]))
  assign(obj, value = data.table::rbindlist(tableList), envir = .GlobalEnv)
}
  
dataset = appendAllDataTables(list(dTQ1, dTQ2, dTQ3, dTQ4, dTQ5, dTQ6, dCQ1, dCQ2, dCQ3, dCQ4, dCQ5, dCQ6))
```

    ## Warning in assign(obj, value = data.table::rbindlist(tableList), envir
    ## = .GlobalEnv): only the first element is used as variable name

## Analysis

``` r
tryCatch(
  {
    nrow(dataset)
  },
  error = function(e) {
    print("dataset variable not found, cannot proceed further!")
    stop(e)
  }
)
```

    ## [1] 1692

``` r
# Lets perform the difference in differences manually for Q1 and then verify it with a regression model
A = mean(dataset[QNum == 1 & IsAttrited == FALSE & IsNeverTaker == FALSE & TreatmentAssignment == TRUE, ResponseQ])
B = mean(dataset[QNum == 1 & IsAttrited == FALSE & IsNeverTaker == FALSE & TreatmentAssignment == FALSE, ResponseQ])
```

``` r
mod_q1 = lm(ResponseQ ~ TreatmentAssignment, data = dataset[QNum == 1 & IsAttrited == FALSE & IsNeverTaker == FALSE, ])
summary(mod_q1)
```

    ## 
    ## Call:
    ## lm(formula = ResponseQ ~ TreatmentAssignment, data = dataset[QNum == 
    ##     1 & IsAttrited == FALSE & IsNeverTaker == FALSE, ])
    ## 
    ## Residuals:
    ##     Min      1Q  Median      3Q     Max 
    ## -2.0845 -0.8417 -0.8417  0.9155  5.1583 
    ## 
    ## Coefficients:
    ##                         Estimate Std. Error t value Pr(>|t|)    
    ## (Intercept)               3.0845     0.1333  23.147  < 2e-16 ***
    ## TreatmentAssignmentTRUE  -1.2428     0.1895  -6.559 2.61e-10 ***
    ## ---
    ## Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1
    ## 
    ## Residual standard error: 1.588 on 279 degrees of freedom
    ## Multiple R-squared:  0.1336, Adjusted R-squared:  0.1305 
    ## F-statistic: 43.03 on 1 and 279 DF,  p-value: 2.615e-10

> > > From the above, we notice that the treatment coefficient value
> > > from the regression `-1.2427804` exactly matches what we obtained
> > > from our manual differences in differences model. `A - B =`
> > > `-1.2427804`
