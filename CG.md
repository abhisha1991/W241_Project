Experiments and Causality: W241 Final Project Coast Guards
================
Hanyu, Brendan and Abhi
4/2021

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
dTQ1$TreatmentQ1 = generateRandomIntNums(nrow(dTQ1), 1, 1) # WM BM
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

    ## [1] 1.446043

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
dCQ1$ControlQ1 = generateRandomIntNums(nrow(dCQ1), 3.5, 1)
dCQ2$ControlQ2 = generateRandomIntNums(nrow(dCQ2), 5, 2)
dCQ3$ControlQ3 = generateRandomIntNums(nrow(dCQ3), 6.5, 2)
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

    ## [1] 3.538462

``` r
mean(dCQ2$ControlQ2)
```

    ## [1] 4.895105

``` r
mean(dCQ3$ControlQ3)
```

    ## [1] 5.853147

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

``` r
# AgeBin is a feature of interest for us, which tells us which age group the respondent belongs to
# We will infer this from the YearOfBirth variable

# First convert the variable to numeric
dataset$YearOfBirth = as.numeric(dataset$YearOfBirth)
```

    ## Warning: NAs introduced by coercion

``` r
# Get current age
dataset$Age = as.integer(format(Sys.Date(), "%Y")) - dataset$YearOfBirth
# Add a column called AgeBin
dataset$AgeBin = "NA"

# TODO: clean up - probably a better way to implement this
AssignAgeBin <- function(dt) {
  
  for (i in 1:nrow(dt)) {
    
          # we were able to parse the year as an int
          if (!is.na(dt[i]$Age) & is.numeric(dt[i]$Age) & dt[i]$Age > 0 & (floor(log10(dt[i]$YearOfBirth)) + 1) == 4) {
              # valid age, assign to category
              # we will keep things simple and have 0-12, 12-25, 25-40, 40-60, 60+
              if (dt[i]$Age <= 12) {
                dt[i]$AgeBin = "Child"
              } else if (dt[i]$Age <= 25) {
                dt[i]$AgeBin = "Student"
              } else if (dt[i]$Age <= 40) {
                dt[i]$AgeBin = "Adult"
              } else if (dt[i]$Age <= 60) {
                dt[i]$AgeBin = "MiddleAge"
              } else if (dt[i]$Age > 60) {
                dt[i]$AgeBin = "Senior"
              } else {
                dt[i]$AgeBin = NA
              }
          
          }
          else {
            # assign to NA
            dt[i]$AgeBin = NA
          }
    
  } # end of for loop
  
  return(dt)
}


dataset = AssignAgeBin(dataset)
```

## Feature Analysis

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
# load Q1 data - WM vs BM
Q1 = dataset[QNum == 1 & IsAttrited == FALSE & IsNeverTaker == FALSE, ]
```

``` r
# Lets perform the difference in differences manually for Q1 and then verify it with a regression model
A = mean(Q1[TreatmentAssignment == TRUE, ResponseQ])
B = mean(Q1[TreatmentAssignment == FALSE, ResponseQ])
```

``` r
mod_q1 = lm(ResponseQ ~ TreatmentAssignment, data = Q1)
stargazer(mod_q1, type = "text", title = "Differences in Differences via Regression")
```

    ## 
    ## Differences in Differences via Regression
    ## ===============================================
    ##                         Dependent variable:    
    ##                     ---------------------------
    ##                              ResponseQ         
    ## -----------------------------------------------
    ## TreatmentAssignment          -2.089***         
    ##                               (0.112)          
    ##                                                
    ## Constant                     3.535***          
    ##                               (0.079)          
    ##                                                
    ## -----------------------------------------------
    ## Observations                    281            
    ## R2                             0.555           
    ## Adjusted R2                    0.554           
    ## Residual Std. Error      0.938 (df = 279)      
    ## F Statistic          348.175*** (df = 1; 279)  
    ## ===============================================
    ## Note:               *p<0.1; **p<0.05; ***p<0.01

> From the above, we notice that the treatment coefficient value from
> the regression `-2.0891681` exactly matches what we obtained from our
> manual differences in differences model. `A - B =` `-2.0891681`. There
> seems to be a favorable significant effect for white males since the
> DiD result is negative.

> Next, we attempt to add covariates into the analysis. We can think of
> our covariates belonging to different categories - `demographic`,
> `profession`, and `behaviorial`.

> We are interested in a few `demographic` covariates - namely, `Race`,
> `GenderCGSurvey`, `PoliticalAffiliation`, `ReligiousAffliation`,
> `ClassLevel`, `AgeBin`, `IsEnglishFirstLanguage`,
> `CountryOfResidence`, `StateOfResidence`

> Next we are also interested in a few `profession` related covariates -
> namely `HasServedInCG`, `HasServedInAnyOtherUniformedService`,
> `HighestDegreeReceived`, `EmploymentStatus`, `DidEarnMoneyLastYear`,
> `RoleAtBerkeley`, `YearsAtBerkeley`, `IncomeIn2020`

> Lastly, we may be interested in few `behavior` related covariates -
> namely `FrequencySocialMediaAccess`, `FrequencySocialMediaPosting`,
> `BookFormatPurchasedMostOften`. There is weak interest in this
> category as we suspect there is little effect of these behavioral
> forces that may be affecting resume comparison. We will explore them
> nevertheless.

``` r
Q_WMBM = dataset[QNum == 1 & IsAttrited == FALSE & IsNeverTaker == FALSE, ] # WM BM
Q_WMWF = dataset[QNum == 2 & IsAttrited == FALSE & IsNeverTaker == FALSE, ] # WM WF
Q_WMBF = dataset[QNum == 3 & IsAttrited == FALSE & IsNeverTaker == FALSE, ] # WM BF
Q_WFBM = dataset[QNum == 4 & IsAttrited == FALSE & IsNeverTaker == FALSE, ] # WF BM
Q_BMBF = dataset[QNum == 5 & IsAttrited == FALSE & IsNeverTaker == FALSE, ] # BM BF
Q_WFBF = dataset[QNum == 6 & IsAttrited == FALSE & IsNeverTaker == FALSE, ] # WF BF

data_list = list(Q_WMBM, Q_WMWF, Q_WMBF, Q_WFBM, Q_BMBF, Q_WFBF)
```

### Covariate Analysis (Demographic Variables)

``` r
mod_dmg = list()

for (i in 1:length(data_list)) {
  mod_dmg[[i]] = lm(ResponseQ ~ TreatmentAssignment +  Race  +  GenderCGSurvey +  PoliticalAffiliation + ReligiousAffliation + AgeBin, data = data_list[[i]])
}

stargazer(mod_dmg, type = "text", title = "Differences in Differences via Regression (Demographic Covariates)")
```

    ## 
    ## Differences in Differences via Regression (Demographic Covariates)
    ## =================================================================================================
    ##                                                            Dependent variable:                   
    ##                                         ---------------------------------------------------------
    ##                                                                 ResponseQ                        
    ##                                            (1)       (2)       (3)      (4)      (5)       (6)   
    ## -------------------------------------------------------------------------------------------------
    ## TreatmentAssignment                     -2.083*** -2.665*** -0.796***  -0.035   0.022   -2.781***
    ##                                          (0.119)   (0.184)   (0.158)  (0.140)  (0.148)   (0.137) 
    ##                                                                                                  
    ## RaceBlack or African American             0.307    -0.317     0.180    0.393    0.396     0.378  
    ##                                          (0.344)   (0.532)   (0.456)  (0.404)  (0.427)   (0.396) 
    ##                                                                                                  
    ## RaceHispanic or Latino                   -0.220    -0.216    -0.008    -0.285   0.030   0.773*** 
    ##                                          (0.232)   (0.359)   (0.308)  (0.273)  (0.288)   (0.267) 
    ##                                                                                                  
    ## RaceNative American                      -0.029    -0.450    -0.480    0.461    -0.557   -0.299  
    ##                                          (0.542)   (0.839)   (0.718)  (0.636)  (0.673)   (0.623) 
    ##                                                                                                  
    ## RaceNative Hawaiian or Pacific Islander  -1.085     0.320     1.141    1.099    -1.244    0.713  
    ##                                          (0.991)   (1.535)   (1.313)  (1.165)  (1.231)   (1.141) 
    ##                                                                                                  
    ## RaceNon-Hispanic White                    0.071    -0.005    -0.419*   0.324   -0.376*    0.185  
    ##                                          (0.172)   (0.267)   (0.229)  (0.203)  (0.214)   (0.198) 
    ##                                                                                                  
    ## RaceOther:                                0.133     0.067    -0.088    0.484    -0.351   -0.191  
    ##                                          (0.325)   (0.504)   (0.431)  (0.382)  (0.404)   (0.374) 
    ##                                                                                                  
    ## RacePrefer not to answer                 -0.656    -0.101    -0.264    -0.065   0.135     0.058  
    ##                                          (0.789)   (1.222)   (1.045)  (0.927)  (0.980)   (0.908) 
    ##                                                                                                  
    ## GenderCGSurveyMale                        0.033    -0.180    -0.169    0.051    -0.162   -0.116  
    ##                                          (0.138)   (0.214)   (0.183)  (0.162)  (0.171)   (0.159) 
    ##                                                                                                  
    ## GenderCGSurveyNon-binary / third gender  0.631**  -1.091**    0.225    0.331    0.225    -0.054  
    ##                                          (0.304)   (0.470)   (0.402)  (0.357)  (0.377)   (0.349) 
    ##                                                                                                  
    ## GenderCGSurveyPrefer not to answer        0.919    -1.305    -0.047    0.277    0.453     0.502  
    ##                                          (0.774)   (1.199)   (1.025)  (0.909)  (0.961)   (0.891) 
    ##                                                                                                  
    ## PoliticalAffiliationIndependent          -0.133    -0.078     0.238    -0.007   -0.044    0.024  
    ##                                          (0.172)   (0.267)   (0.228)  (0.202)  (0.214)   (0.198) 
    ##                                                                                                  
    ## PoliticalAffiliationOther                -0.298     0.147    -0.278    0.041    0.096    -0.214  
    ##                                          (0.250)   (0.387)   (0.331)  (0.294)  (0.310)   (0.288) 
    ##                                                                                                  
    ## PoliticalAffiliationPrefer not to say     0.109     0.298     0.158    0.032    -0.057    0.226  
    ##                                          (0.205)   (0.317)   (0.272)  (0.241)  (0.255)   (0.236) 
    ##                                                                                                  
    ## PoliticalAffiliationRepublican           0.692**   -0.165     0.572    0.071    0.170    -0.114  
    ##                                          (0.273)   (0.423)   (0.362)  (0.321)  (0.340)   (0.315) 
    ##                                                                                                  
    ## PoliticalAffiliationSocialist             0.164    0.781**   -0.284    0.244    0.244    -0.142  
    ##                                          (0.255)   (0.395)   (0.338)  (0.299)  (0.316)   (0.293) 
    ##                                                                                                  
    ## ReligiousAffliationBuddhist               0.260    -0.544    -0.814*   -0.163   -0.369    0.125  
    ##                                          (0.341)   (0.527)   (0.451)  (0.400)  (0.423)   (0.392) 
    ##                                                                                                  
    ## ReligiousAffliationHindu                  0.330    -0.664    -0.086    -0.007   -0.124   -0.035  
    ##                                          (0.291)   (0.450)   (0.385)  (0.341)  (0.361)   (0.334) 
    ##                                                                                                  
    ## ReligiousAffliationJewish                 0.102     0.098     0.433   -0.675*   0.377    -0.040  
    ##                                          (0.314)   (0.486)   (0.415)  (0.368)  (0.389)   (0.361) 
    ##                                                                                                  
    ## ReligiousAffliationMuslim                 0.586   -1.487**    0.028    -0.312   -0.127    0.051  
    ##                                          (0.437)   (0.676)   (0.579)  (0.513)  (0.543)   (0.503) 
    ##                                                                                                  
    ## ReligiousAffliationOther Christian:      -0.297   -1.002**   -0.079    -0.089   -0.304    0.033  
    ##                                          (0.279)   (0.432)   (0.370)  (0.328)  (0.347)   (0.321) 
    ##                                                                                                  
    ## ReligiousAffliationOther:                -0.033    -0.846    0.890*    0.320    0.880*    0.029  
    ##                                          (0.397)   (0.614)   (0.526)  (0.466)  (0.493)   (0.456) 
    ##                                                                                                  
    ## ReligiousAffliationPrefer not to say      0.166    -0.187    -0.290    0.243    -0.053    0.151  
    ##                                          (0.219)   (0.339)   (0.290)  (0.257)  (0.272)   (0.252) 
    ##                                                                                                  
    ## ReligiousAffliationProtestant             0.104     0.142     0.498    0.328    -0.465   -0.109  
    ##                                          (0.233)   (0.360)   (0.308)  (0.273)  (0.289)   (0.268) 
    ##                                                                                                  
    ## ReligiousAffliationRoman Catholic        -0.016     0.107   -0.625**   -0.023   0.094    -0.336  
    ##                                          (0.189)   (0.293)   (0.251)  (0.223)  (0.235)   (0.218) 
    ##                                                                                                  
    ## AgeBinChild                               0.295     1.440    -1.291    1.683    0.012   -2.302** 
    ##                                          (1.013)   (1.568)   (1.342)  (1.190)  (1.258)   (1.165) 
    ##                                                                                                  
    ## AgeBinMiddleAge                          -0.201     0.131    -0.235   -0.943*   1.013*   -0.807  
    ##                                          (0.435)   (0.673)   (0.576)  (0.511)  (0.540)   (0.500) 
    ##                                                                                                  
    ## AgeBinStudent                            -0.061   -0.604**    0.144    0.068    -0.143   -0.401* 
    ##                                          (0.181)   (0.281)   (0.240)  (0.213)  (0.225)   (0.209) 
    ##                                                                                                  
    ## Constant                                3.437***  5.521***  5.937***  3.734*** 4.284*** 5.253*** 
    ##                                          (0.219)   (0.340)   (0.291)  (0.258)  (0.272)   (0.252) 
    ##                                                                                                  
    ## -------------------------------------------------------------------------------------------------
    ## Observations                               266       266       266      266      266       266   
    ## R2                                        0.597     0.543     0.225    0.091    0.074     0.670  
    ## Adjusted R2                               0.550     0.489     0.134    -0.016   -0.036    0.631  
    ## Residual Std. Error (df = 237)            0.933     1.445     1.236    1.097    1.159     1.074  
    ## F Statistic (df = 28; 237)              12.545*** 10.048*** 2.464***   0.852    0.672   17.162***
    ## =================================================================================================
    ## Note:                                                                 *p<0.1; **p<0.05; ***p<0.01
