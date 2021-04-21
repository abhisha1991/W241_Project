Experiments and Causality: W241 Final Project Coast Guards
================
Hanyu, Brendan and Abhi
4/2021

## Feature Engineering

### Load Data

``` r
d <- fread("data.csv",  na.strings=c("","NA"))

# head(d)
```

### Basic Distribution Checks

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

### Utility Tools

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
  # numbers must be low-high
  for (i in 1:nrow(nums)) {
    
    # arbitrarily choose floor or ceiling for that number (since response must be integer)
    if(sample(c(0,1), size =1))  {
      nums[i] = ceiling(nums[i])
    }
    else {
      nums[i] = floor(nums[i]) 
    }
    
    # bound number between low-high
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

### Identifying Attrition and Non Compliance

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

## Create Datasets Per Question

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

### Get Final Dataset

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
stargazer(mod_q1, type = "text", title = "Differences in Differences via Regression (WM vs BM)")
```

    ## 
    ## Differences in Differences via Regression (WM vs BM)
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

### Treatment Effect (Vanilla Model)

``` r
mod_vanilla = list()

for (i in 1:length(data_list)) {
  mod_vanilla[[i]] = lm(ResponseQ ~ TreatmentAssignment, data = data_list[[i]])
}

stargazer(mod_vanilla, type = "text", title = "Treatment Effects (Vanilla - No Covariates)")
```

    ## 
    ## Treatment Effects (Vanilla - No Covariates)
    ## ===========================================================================================
    ##                                                    Dependent variable:                     
    ##                                ------------------------------------------------------------
    ##                                                         ResponseQ                          
    ##                                   (1)        (2)        (3)      (4)      (5)       (6)    
    ## -------------------------------------------------------------------------------------------
    ## TreatmentAssignment            -2.089***  -2.808***  -0.795***  -0.059   0.037   -2.821*** 
    ##                                 (0.112)    (0.174)    (0.151)  (0.132)  (0.138)   (0.125)  
    ##                                                                                            
    ## Constant                        3.535***   4.887***  5.845***  3.951*** 4.028***  4.965*** 
    ##                                 (0.079)    (0.122)    (0.106)  (0.093)  (0.097)   (0.088)  
    ##                                                                                            
    ## -------------------------------------------------------------------------------------------
    ## Observations                      281        281        281      281      281       281    
    ## R2                               0.555      0.482      0.090    0.001    0.0003    0.645   
    ## Adjusted R2                      0.554      0.481      0.087    -0.003   -0.003    0.643   
    ## Residual Std. Error (df = 279)   0.938      1.460      1.269    1.104    1.158     1.051   
    ## F Statistic (df = 1; 279)      348.175*** 260.033*** 27.551***  0.198    0.070   506.410***
    ## ===========================================================================================
    ## Note:                                                           *p<0.1; **p<0.05; ***p<0.01

### Covariate Analysis (Core Variables)

> In our actual run of the survey against the coast guard audience, we
> expect to only have access to some core covariates - `Race`,
> `GenderCGSurvey` and `AgeBin`. This is due to the restrictive nature
> of information sharing protocols followed at organizations like the
> U.S. Coast Guard. These are also the covariates which we are
> explicitly interested in because the preference of the resumes can be
> hypothesized to depend on the above covariates (as compared to other
> covariates, say `HighestDegreeReceived` - that probably won’t
> influence the treatment as much).

``` r
mod_core = list()

for (i in 1:length(data_list)) {
  mod_core[[i]] = lm(ResponseQ ~ TreatmentAssignment + Race + GenderCGSurvey + AgeBin, data = data_list[[i]])
}

stargazer(mod_core, type = "text", title = "Treatment Effects (Core Covariates)")
```

    ## 
    ## Treatment Effects (Core Covariates)
    ## =================================================================================================
    ##                                                            Dependent variable:                   
    ##                                         ---------------------------------------------------------
    ##                                                                 ResponseQ                        
    ##                                            (1)       (2)       (3)      (4)      (5)       (6)   
    ## -------------------------------------------------------------------------------------------------
    ## TreatmentAssignment                     -2.087*** -2.722*** -0.840***  -0.062   0.042   -2.798***
    ##                                          (0.118)   (0.184)   (0.159)  (0.136)  (0.145)   (0.133) 
    ##                                                                                                  
    ## RaceBlack or African American             0.156    -0.449     0.090    0.325    0.364     0.270  
    ##                                          (0.329)   (0.512)   (0.443)  (0.379)  (0.402)   (0.369) 
    ##                                                                                                  
    ## RaceHispanic or Latino                   -0.282    -0.022    -0.253    -0.375   0.196    0.577** 
    ##                                          (0.213)   (0.332)   (0.287)  (0.246)  (0.261)   (0.240) 
    ##                                                                                                  
    ## RaceNative American                       0.185    -0.292    -0.910    0.356    -0.298   -0.632  
    ##                                          (0.509)   (0.793)   (0.686)  (0.588)  (0.623)   (0.572) 
    ##                                                                                                  
    ## RaceNative Hawaiian or Pacific Islander  -0.555     0.364     1.013    1.077    -0.912    0.251  
    ##                                          (0.951)   (1.481)   (1.281)  (1.098)  (1.164)   (1.068) 
    ##                                                                                                  
    ## RaceNon-Hispanic White                   -0.002     0.208   -0.500**   0.197    -0.158    0.040  
    ##                                          (0.146)   (0.227)   (0.196)  (0.168)  (0.178)   (0.163) 
    ##                                                                                                  
    ## RaceOther:                                0.225     0.003    -0.081    0.521    -0.387   -0.170  
    ##                                          (0.311)   (0.485)   (0.419)  (0.359)  (0.381)   (0.349) 
    ##                                                                                                  
    ## RacePrefer not to answer                 -0.317    -0.845    -0.386    -0.321   0.161     0.270  
    ##                                          (0.737)   (1.149)   (0.993)  (0.851)  (0.903)   (0.828) 
    ##                                                                                                  
    ## GenderCGSurveyMale                        0.098    -0.164    -0.096    0.048    -0.140   -0.153  
    ##                                          (0.131)   (0.203)   (0.176)  (0.151)  (0.160)   (0.147) 
    ##                                                                                                  
    ## GenderCGSurveyNon-binary / third gender  0.516*   -0.933**    0.221    0.416    0.189    -0.149  
    ##                                          (0.284)   (0.443)   (0.383)  (0.328)  (0.348)   (0.319) 
    ##                                                                                                  
    ## GenderCGSurveyPrefer not to answer        0.812    -0.422     0.401    0.713    0.147     0.450  
    ##                                          (0.737)   (1.147)   (0.992)  (0.850)  (0.902)   (0.827) 
    ##                                                                                                  
    ## AgeBinChild                              -0.120     0.358    -0.873    1.575    -0.414   -2.014* 
    ##                                          (0.976)   (1.520)   (1.315)  (1.127)  (1.195)   (1.096) 
    ##                                                                                                  
    ## AgeBinMiddleAge                          -0.341    -0.124     0.018   -0.893*   0.720    -0.607  
    ##                                          (0.420)   (0.654)   (0.565)  (0.484)  (0.514)   (0.471) 
    ##                                                                                                  
    ## AgeBinStudent                            -0.031   -0.587**    0.117    0.014    -0.126   -0.333* 
    ##                                          (0.176)   (0.274)   (0.237)  (0.203)  (0.216)   (0.198) 
    ##                                                                                                  
    ## Constant                                3.488***  5.387***  5.966***  3.862*** 4.177*** 5.235*** 
    ##                                          (0.198)   (0.309)   (0.267)  (0.229)  (0.243)   (0.223) 
    ##                                                                                                  
    ## -------------------------------------------------------------------------------------------------
    ## Observations                               266       266       266      266      266       266   
    ## R2                                        0.567     0.503     0.140    0.058    0.032     0.662  
    ## Adjusted R2                               0.543     0.475     0.092    0.005    -0.022    0.643  
    ## Residual Std. Error (df = 251)            0.940     1.464     1.266    1.085    1.151     1.056  
    ## F Statistic (df = 14; 251)              23.464*** 18.127*** 2.912***   1.098    0.602   35.098***
    ## =================================================================================================
    ## Note:                                                                 *p<0.1; **p<0.05; ***p<0.01

### Covariate Analysis (Demographic Variables)

``` r
mod_dmg = list()

for (i in 1:length(data_list)) {
  mod_dmg[[i]] = lm(ResponseQ ~ TreatmentAssignment +  Race  +  GenderCGSurvey +  PoliticalAffiliation + ReligiousAffliation + AgeBin + ClassLevel, data = data_list[[i]])
}

stargazer(mod_dmg, type = "text", title = "Treatment Effects (Demographic Covariates)")
```

    ## 
    ## Treatment Effects (Demographic Covariates)
    ## =================================================================================================
    ##                                                            Dependent variable:                   
    ##                                         ---------------------------------------------------------
    ##                                                                 ResponseQ                        
    ##                                            (1)       (2)       (3)      (4)      (5)       (6)   
    ## -------------------------------------------------------------------------------------------------
    ## TreatmentAssignment                     -2.081*** -2.666*** -0.795***  -0.041   0.017   -2.790***
    ##                                          (0.119)   (0.184)   (0.158)  (0.139)  (0.149)   (0.136) 
    ##                                                                                                  
    ## RaceBlack or African American             0.327    -0.338     0.153    0.363    0.387     0.459  
    ##                                          (0.346)   (0.535)   (0.459)  (0.402)  (0.431)   (0.393) 
    ##                                                                                                  
    ## RaceHispanic or Latino                   -0.203    -0.269    -0.032    -0.376   -0.017  0.798*** 
    ##                                          (0.235)   (0.364)   (0.312)  (0.274)  (0.293)   (0.267) 
    ##                                                                                                  
    ## RaceNative American                      -0.049    -0.424    -0.487    0.536    -0.501   -0.236  
    ##                                          (0.544)   (0.841)   (0.722)  (0.633)  (0.677)   (0.618) 
    ##                                                                                                  
    ## RaceNative Hawaiian or Pacific Islander  -1.014     0.332     1.114    1.087    -1.261    0.840  
    ##                                          (0.995)   (1.538)   (1.321)  (1.158)  (1.239)   (1.131) 
    ##                                                                                                  
    ## RaceNon-Hispanic White                    0.055    -0.058    -0.445*   0.263   -0.392*    0.213  
    ##                                          (0.174)   (0.269)   (0.231)  (0.202)  (0.216)   (0.198) 
    ##                                                                                                  
    ## RaceOther:                                0.089     0.033    -0.118    0.489    -0.316   -0.119  
    ##                                          (0.327)   (0.506)   (0.435)  (0.381)  (0.408)   (0.372) 
    ##                                                                                                  
    ## RacePrefer not to answer                 -0.727    -0.643    -0.548    -0.751   -0.096    0.433  
    ##                                          (0.822)   (1.271)   (1.092)  (0.956)  (1.024)   (0.935) 
    ##                                                                                                  
    ## GenderCGSurveyMale                        0.020    -0.211    -0.168    0.001    -0.188   -0.164  
    ##                                          (0.141)   (0.218)   (0.187)  (0.164)  (0.176)   (0.160) 
    ##                                                                                                  
    ## GenderCGSurveyNon-binary / third gender  0.681**  -0.990**    0.283    0.419    0.228    -0.143  
    ##                                          (0.306)   (0.474)   (0.407)  (0.356)  (0.381)   (0.348) 
    ##                                                                                                  
    ## GenderCGSurveyPrefer not to answer        0.871    -1.401    -0.073    0.162    0.415     0.470  
    ##                                          (0.776)   (1.200)   (1.031)  (0.903)  (0.967)   (0.883) 
    ##                                                                                                  
    ## PoliticalAffiliationIndependent          -0.108    -0.054     0.250    0.006    -0.052    0.010  
    ##                                          (0.173)   (0.268)   (0.230)  (0.202)  (0.216)   (0.197) 
    ##                                                                                                  
    ## PoliticalAffiliationOther                -0.309     0.095    -0.299    -0.026   0.072    -0.201  
    ##                                          (0.251)   (0.388)   (0.333)  (0.292)  (0.313)   (0.286) 
    ##                                                                                                  
    ## PoliticalAffiliationPrefer not to say     0.113     0.287     0.166    0.0004   -0.082    0.180  
    ##                                          (0.206)   (0.319)   (0.274)  (0.240)  (0.257)   (0.235) 
    ##                                                                                                  
    ## PoliticalAffiliationRepublican          0.714***   -0.116     0.597    0.120    0.178    -0.145  
    ##                                          (0.274)   (0.424)   (0.364)  (0.319)  (0.342)   (0.312) 
    ##                                                                                                  
    ## PoliticalAffiliationSocialist             0.179    0.800**   -0.282    0.264    0.250    -0.123  
    ##                                          (0.255)   (0.395)   (0.339)  (0.297)  (0.318)   (0.290) 
    ##                                                                                                  
    ## ReligiousAffliationBuddhist               0.263    -0.546    -0.860*   -0.123   -0.320    0.306  
    ##                                          (0.347)   (0.537)   (0.461)  (0.404)  (0.433)   (0.395) 
    ##                                                                                                  
    ## ReligiousAffliationHindu                  0.325    -0.713    -0.122    -0.060   -0.135    0.041  
    ##                                          (0.292)   (0.452)   (0.388)  (0.340)  (0.364)   (0.332) 
    ##                                                                                                  
    ## ReligiousAffliationJewish                 0.089     0.125     0.466   -0.647*   0.379    -0.137  
    ##                                          (0.316)   (0.488)   (0.419)  (0.367)  (0.393)   (0.359) 
    ##                                                                                                  
    ## ReligiousAffliationMuslim                 0.662   -1.424**    0.047    -0.280   -0.145    0.065  
    ##                                          (0.440)   (0.681)   (0.585)  (0.512)  (0.548)   (0.501) 
    ##                                                                                                  
    ## ReligiousAffliationOther Christian:      -0.340   -1.060**   -0.105    -0.136   -0.303    0.053  
    ##                                          (0.281)   (0.434)   (0.373)  (0.327)  (0.350)   (0.319) 
    ##                                                                                                  
    ## ReligiousAffliationOther:                -0.068    -0.943     0.811    0.246    0.892*    0.206  
    ##                                          (0.401)   (0.620)   (0.532)  (0.466)  (0.499)   (0.456) 
    ##                                                                                                  
    ## ReligiousAffliationPrefer not to say      0.174    -0.165    -0.281    0.267    -0.047    0.145  
    ##                                          (0.219)   (0.339)   (0.291)  (0.255)  (0.273)   (0.249) 
    ##                                                                                                  
    ## ReligiousAffliationProtestant             0.111     0.168    0.513*    0.355    -0.460   -0.135  
    ##                                          (0.233)   (0.360)   (0.309)  (0.271)  (0.290)   (0.265) 
    ##                                                                                                  
    ## ReligiousAffliationRoman Catholic        -0.032     0.123   -0.626**   0.023    0.128    -0.310  
    ##                                          (0.191)   (0.296)   (0.254)  (0.223)  (0.238)   (0.217) 
    ##                                                                                                  
    ## AgeBinChild                               0.183     1.482    -1.294    1.865    0.163    -2.238* 
    ##                                          (1.020)   (1.578)   (1.355)  (1.187)  (1.271)   (1.160) 
    ##                                                                                                  
    ## AgeBinMiddleAge                          -0.176     0.120    -0.234   -0.988*   0.976*   -0.825* 
    ##                                          (0.436)   (0.674)   (0.579)  (0.507)  (0.543)   (0.496) 
    ##                                                                                                  
    ## AgeBinStudent                             0.060    -0.374     0.285    0.257    -0.150  -0.638***
    ##                                          (0.204)   (0.315)   (0.271)  (0.237)  (0.254)   (0.232) 
    ##                                                                                                  
    ## ClassLevelGrad/Post-Grad                  0.206     0.281     0.247    0.102    -0.150  -0.586** 
    ##                                          (0.203)   (0.314)   (0.269)  (0.236)  (0.253)   (0.231) 
    ##                                                                                                  
    ## ClassLevelJr/Sr                          -0.091    -0.040     0.070    -0.067   -0.049  -0.336** 
    ##                                          (0.145)   (0.224)   (0.192)  (0.169)  (0.180)   (0.165) 
    ##                                                                                                  
    ## ClassLevelPrefer not to answer            0.087     0.671     0.394   0.804**   0.234    -0.637* 
    ##                                          (0.286)   (0.443)   (0.380)  (0.333)  (0.357)   (0.326) 
    ##                                                                                                  
    ## Constant                                3.343***  5.285***  5.728***  3.580*** 4.342*** 5.753*** 
    ##                                          (0.268)   (0.414)   (0.355)  (0.311)  (0.333)   (0.304) 
    ##                                                                                                  
    ## -------------------------------------------------------------------------------------------------
    ## Observations                               266       266       266      266      266       266   
    ## R2                                        0.602     0.549     0.230    0.119    0.078     0.681  
    ## Adjusted R2                               0.549     0.489     0.128    0.002    -0.044    0.639  
    ## Residual Std. Error (df = 234)            0.934     1.444     1.240    1.087    1.163     1.062  
    ## F Statistic (df = 31; 234)              11.395*** 9.187***  2.258***   1.015    0.642   16.120***
    ## =================================================================================================
    ## Note:                                                                 *p<0.1; **p<0.05; ***p<0.01

### Covariate Analysis (Profession Variables)

``` r
mod_pf = list()

for (i in 1:length(data_list)) {
  mod_pf[[i]] = lm(ResponseQ ~ TreatmentAssignment + HasServedInCG + HasServedInAnyOtherUniformedService + HighestDegreeReceived + EmploymentStatus  + RoleAtBerkeley + YearsAtBerkeley + as.factor(IncomeIn2020), data = data_list[[i]])
}

stargazer(mod_pf, type = "text", title = "Treatment Effects (Profession Covariates)")
```

    ## 
    ## Treatment Effects (Profession Covariates)
    ## =================================================================================================================
    ##                                                                            Dependent variable:                   
    ##                                                         ---------------------------------------------------------
    ##                                                                                 ResponseQ                        
    ##                                                            (1)       (2)       (3)      (4)      (5)       (6)   
    ## -----------------------------------------------------------------------------------------------------------------
    ## TreatmentAssignment                                     -1.961*** -3.028*** -0.796***  0.054    -0.028  -2.801***
    ##                                                          (0.163)   (0.241)   (0.227)  (0.206)  (0.217)   (0.179) 
    ##                                                                                                                  
    ## HasServedInCGPrefer not to answer                         0.212     0.738    -0.775    1.948   3.334**    1.738  
    ##                                                          (1.148)   (1.703)   (1.607)  (1.456)  (1.532)   (1.266) 
    ##                                                                                                                  
    ## HasServedInCGYes                                          1.136    -1.085     0.293    -1.044   0.334    -1.330  
    ##                                                          (0.781)   (1.158)   (1.092)  (0.990)  (1.041)   (0.861) 
    ##                                                                                                                  
    ## HasServedInAnyOtherUniformedServicePrefer not to answer   0.789     0.074     1.210    -0.160   -1.963   -1.234  
    ##                                                          (0.932)   (1.383)   (1.304)  (1.182)  (1.244)   (1.028) 
    ##                                                                                                                  
    ## HasServedInAnyOtherUniformedServiceYes                    1.133     0.470    -0.770    0.874    -0.571    0.220  
    ##                                                          (0.700)   (1.038)   (0.980)  (0.888)  (0.934)   (0.772) 
    ##                                                                                                                  
    ## HighestDegreeReceivedBachelor's degree                    0.463     0.233    -0.309    0.082    0.028   -1.531***
    ##                                                          (0.438)   (0.650)   (0.613)  (0.555)  (0.584)   (0.483) 
    ##                                                                                                                  
    ## HighestDegreeReceivedNo college                           0.399     0.204    -0.562    0.189    0.315   -1.287** 
    ##                                                          (0.529)   (0.785)   (0.741)  (0.671)  (0.706)   (0.584) 
    ##                                                                                                                  
    ## HighestDegreeReceivedSome college                         0.580     0.198    -0.587    0.366    0.112   -1.231** 
    ##                                                          (0.505)   (0.749)   (0.706)  (0.640)  (0.674)   (0.557) 
    ##                                                                                                                  
    ## EmploymentStatusEmployed part time                        0.467     0.944     0.469    -0.626   -0.472    0.358  
    ##                                                          (0.408)   (0.605)   (0.571)  (0.518)  (0.545)   (0.450) 
    ##                                                                                                                  
    ## EmploymentStatusStudent                                   0.273    1.117*     0.639    -0.472   -0.308    0.235  
    ##                                                          (0.398)   (0.591)   (0.557)  (0.505)  (0.532)   (0.439) 
    ##                                                                                                                  
    ## EmploymentStatusUnemployed looking for work               0.394     0.378     0.752    -0.836   -0.231   -0.088  
    ##                                                          (0.511)   (0.758)   (0.715)  (0.648)  (0.682)   (0.563) 
    ##                                                                                                                  
    ## RoleAtBerkeleyUndergraduate Student                       0.515   -1.454**   -0.059    -0.150   0.141     0.591  
    ##                                                          (0.410)   (0.607)   (0.573)  (0.519)  (0.546)   (0.451) 
    ##                                                                                                                  
    ## YearsAtBerkeley2nd Year                                  -0.115    -0.473     0.282    -0.173   -0.197    0.308  
    ##                                                          (0.271)   (0.402)   (0.379)  (0.344)  (0.361)   (0.299) 
    ##                                                                                                                  
    ## YearsAtBerkeley3rd Year                                  -0.034    -0.520     0.173    -0.270   -0.058   -0.399  
    ##                                                          (0.277)   (0.411)   (0.388)  (0.352)  (0.370)   (0.306) 
    ##                                                                                                                  
    ## YearsAtBerkeley4th Year                                  -0.085    -0.523     0.486    -0.294   -0.477   0.0002  
    ##                                                          (0.275)   (0.407)   (0.384)  (0.348)  (0.366)   (0.303) 
    ##                                                                                                                  
    ## YearsAtBerkeley5th Year or beyond                        0.685*   -1.784***  -0.298    -0.272   0.294     0.202  
    ##                                                          (0.401)   (0.595)   (0.561)  (0.509)  (0.535)   (0.442) 
    ##                                                                                                                  
    ## 29,999                                                   -0.063     0.551     0.197    -0.540   0.098    -0.372  
    ##                                                          (0.351)   (0.520)   (0.491)  (0.445)  (0.468)   (0.387) 
    ##                                                                                                                  
    ## 39,999                                                    0.753    -0.991    -0.501    -0.278   0.316    -0.533  
    ##                                                          (0.502)   (0.744)   (0.702)  (0.636)  (0.670)   (0.553) 
    ##                                                                                                                  
    ## 49,999                                                   1.955**    0.513     0.865    -0.446   -0.005    1.002  
    ##                                                          (0.869)   (1.289)   (1.216)  (1.102)  (1.160)   (0.958) 
    ##                                                                                                                  
    ## 59,999                                                    0.347     1.321     0.769    -0.834   -0.214    0.949  
    ##                                                          (0.626)   (0.928)   (0.876)  (0.794)  (0.835)   (0.690) 
    ##                                                                                                                  
    ## 69,999                                                    0.239    -0.952    -0.121    -1.509   -0.969   3.584** 
    ##                                                          (1.361)   (2.018)   (1.904)  (1.726)  (1.816)   (1.501) 
    ##                                                                                                                  
    ## 79,999                                                    1.518    -2.545    -0.599    1.496    1.046    1.981*  
    ##                                                          (1.076)   (1.595)   (1.505)  (1.364)  (1.435)   (1.186) 
    ##                                                                                                                  
    ## 89,999                                                   -0.840    -0.186     0.248    -0.326   -0.123    1.746  
    ##                                                          (1.042)   (1.545)   (1.458)  (1.321)  (1.390)   (1.149) 
    ##                                                                                                                  
    ## 99,999                                                    0.981     1.687    -2.908*   -0.422  -2.925**  -0.549  
    ##                                                          (1.106)   (1.641)   (1.548)  (1.403)  (1.476)   (1.220) 
    ##                                                                                                                  
    ## 10,000                                                   -0.147    -0.299     0.093    -0.127   -0.188   -0.219  
    ##                                                          (0.258)   (0.383)   (0.361)  (0.327)  (0.344)   (0.285) 
    ##                                                                                                                  
    ## Constant                                                2.104***  5.836***  5.422***  4.716*** 4.403*** 5.549*** 
    ##                                                          (0.670)   (0.993)   (0.937)  (0.849)  (0.893)   (0.738) 
    ##                                                                                                                  
    ## -----------------------------------------------------------------------------------------------------------------
    ## Observations                                               152       152       152      152      152       152   
    ## R2                                                        0.637     0.633     0.173    0.132    0.137     0.727  
    ## Adjusted R2                                               0.565     0.561     0.009    -0.040   -0.034    0.673  
    ## Residual Std. Error (df = 126)                            0.912     1.352     1.275    1.156    1.216     1.005  
    ## F Statistic (df = 25; 126)                              8.839***  8.705***    1.056    0.768    0.801   13.414***
    ## =================================================================================================================
    ## Note:                                                                                 *p<0.1; **p<0.05; ***p<0.01

> Note that here, the coefficient with numbers in thousands is referring
> to the `IncomeIn2020` covariate. For example, a value like `99,999` in
> the above table refers to the interval `$90,000 - $99,999`. So we are
> referring to the upper bound income in the coefficient above.

### Covariate Analysis (Behavioral Variables)

``` r
mod_bhv = list()

for (i in 1:length(data_list)) {
  mod_bhv[[i]] = lm(ResponseQ ~ TreatmentAssignment +  FrequencySocialMediaAccess + FrequencySocialMediaPosting + BookFormatPurchasedMostOften, data = data_list[[i]])
}

stargazer(mod_bhv, type = "text", title = "Treatment Effects (Behavioral Covariates)")
```

    ## 
    ## Treatment Effects (Behavioral Covariates)
    ## ============================================================================================================
    ##                                                                       Dependent variable:                   
    ##                                                    ---------------------------------------------------------
    ##                                                                            ResponseQ                        
    ##                                                       (1)       (2)       (3)      (4)      (5)       (6)   
    ## ------------------------------------------------------------------------------------------------------------
    ## TreatmentAssignment                                -2.070*** -2.790*** -0.822***  -0.030   0.036   -2.835***
    ##                                                     (0.114)   (0.180)   (0.153)  (0.135)  (0.140)   (0.127) 
    ##                                                                                                             
    ## FrequencySocialMediaAccessLess than Weekly           0.256    -0.186   -1.048***  0.447    -0.202    0.113  
    ##                                                     (0.270)   (0.427)   (0.363)  (0.320)  (0.332)   (0.302) 
    ##                                                                                                             
    ## FrequencySocialMediaAccessMore than once a day      -0.026    -0.028    -0.089    0.027    -0.119   -0.161  
    ##                                                     (0.138)   (0.219)   (0.186)  (0.164)  (0.170)   (0.155) 
    ##                                                                                                             
    ## FrequencySocialMediaAccessPrefer not to say          0.700     0.354    -2.491*   0.155    -1.409    1.071  
    ##                                                     (0.952)   (1.506)   (1.279)  (1.128)  (1.170)   (1.064) 
    ##                                                                                                             
    ## FrequencySocialMediaAccessWeekly                   0.613***   -0.418    -0.469    -0.029   0.420     0.353  
    ##                                                     (0.234)   (0.370)   (0.314)  (0.277)  (0.288)   (0.261) 
    ##                                                                                                             
    ## FrequencySocialMediaPostingLess than Weekly         -0.155    -0.131     0.423    -0.071   -0.137    0.101  
    ##                                                     (0.212)   (0.335)   (0.284)  (0.251)  (0.260)   (0.237) 
    ##                                                                                                             
    ## FrequencySocialMediaPostingMore than once a day     -0.170    -0.158     0.529    -0.468   -0.227    0.647  
    ##                                                     (0.391)   (0.618)   (0.525)  (0.463)  (0.480)   (0.437) 
    ##                                                                                                             
    ## FrequencySocialMediaPostingPrefer not to say         0.223    -0.036    1.794*    -0.158   0.722    -1.078  
    ##                                                     (0.706)   (1.117)   (0.949)  (0.837)  (0.868)   (0.789) 
    ##                                                                                                             
    ## FrequencySocialMediaPostingWeekly                   -0.232    -0.267     0.277    -0.280   -0.301    0.077  
    ##                                                     (0.245)   (0.387)   (0.329)  (0.290)  (0.301)   (0.274) 
    ##                                                                                                             
    ## BookFormatPurchasedMostOftenI buy ebooks             0.127     0.037     0.065    -0.132  0.517**    0.342  
    ##                                                     (0.192)   (0.304)   (0.258)  (0.228)  (0.236)   (0.215) 
    ##                                                                                                             
    ## BookFormatPurchasedMostOftenI buy physical books    -0.012    -0.334    -0.226    0.240   0.479***  -0.066  
    ##                                                     (0.135)   (0.214)   (0.182)  (0.160)  (0.166)   (0.151) 
    ##                                                                                                             
    ## BookFormatPurchasedMostOftenI don't purchase books  -0.065    -0.268    -0.047    0.205    0.381*   -0.213  
    ##                                                     (0.166)   (0.262)   (0.223)  (0.197)  (0.204)   (0.185) 
    ##                                                                                                             
    ## Constant                                           3.618***  5.244***  5.720***  3.898*** 3.930*** 4.957*** 
    ##                                                     (0.240)   (0.380)   (0.323)  (0.285)  (0.295)   (0.269) 
    ##                                                                                                             
    ## ------------------------------------------------------------------------------------------------------------
    ## Observations                                          279       279       279      279      279       279   
    ## R2                                                   0.579     0.490     0.152    0.030    0.060     0.664  
    ## Adjusted R2                                          0.560     0.467     0.114    -0.013   0.017     0.649  
    ## Residual Std. Error (df = 266)                       0.933     1.475     1.254    1.106    1.147     1.043  
    ## F Statistic (df = 12; 266)                         30.474*** 21.318*** 3.976***   0.693    1.412   43.822***
    ## ============================================================================================================
    ## Note:                                                                            *p<0.1; **p<0.05; ***p<0.01

### Placebo Test (Pre and Post Within Subject Response Change)

> In order to test that the audience had not been influenced by the
> survey itself (in terms of their affinity to judge others’ resumes),
> we conducted a pre-post placebo test where we asked a related question
> whose responses should NOT have changed per participant (within
> subject). The question asked was `"How much do you like your
> coworkers?"`. This question was posed to the participant `before they
> started answering` the resume comparison questions and `after they had
> finished answering` the resume comparison questions. We expect that
> the participants (on average) should not have had a change in response
> to this question throughout the survey.

``` r
getNumericAffinityChange <- function(data) {
  Change = c()
  for (i in 1:nrow(data)) {
    pre = -1000
    post = 1000
    
    # pre survey response
    if (!is.na(data[i]$PlaceboPreTestColleagueAffinity) & data[i]$PlaceboPreTestColleagueAffinity == "Dislike a great deal") {
      pre = 0 
    } else if (!is.na(data[i]$PlaceboPreTestColleagueAffinity) & data[i]$PlaceboPreTestColleagueAffinity == "Dislike somewhat") {
      pre = 1
    } else if (!is.na(data[i]$PlaceboPreTestColleagueAffinity) & data[i]$PlaceboPreTestColleagueAffinity == "Neither like nor dislike") {
      pre = 2
    } else if (!is.na(data[i]$PlaceboPreTestColleagueAffinity) & data[i]$PlaceboPreTestColleagueAffinity == "Like somewhat") {
      pre = 3
    } else if (!is.na(data[i]$PlaceboPreTestColleagueAffinity) & data[i]$PlaceboPreTestColleagueAffinity == "Like a great deal") {
      pre = 4
    }
    
    # post survey response
    if (!is.na(data[i]$PlaceboPostTestColleagueAffinity) & data[i]$PlaceboPostTestColleagueAffinity == "Dislike a great deal") {
      post = 0 
    } else if (!is.na(data[i]$PlaceboPostTestColleagueAffinity) & data[i]$PlaceboPostTestColleagueAffinity == "Dislike somewhat") {
      post = 1
    } else if (!is.na(data[i]$PlaceboPostTestColleagueAffinity) & data[i]$PlaceboPostTestColleagueAffinity == "Neither like nor dislike") {
      post = 2
    } else if (!is.na(data[i]$PlaceboPostTestColleagueAffinity) & data[i]$PlaceboPostTestColleagueAffinity == "Like somewhat") {
      post = 3
    } else if (!is.na(data[i]$PlaceboPostTestColleagueAffinity) & data[i]$PlaceboPostTestColleagueAffinity == "Like a great deal") {
      post = 4
    }
    
    Change = c(Change, post-pre)
  }
  
  return(data.table(Change))
}

controlPlaceboTestChange = getNumericAffinityChange(Q_WMBM[TreatmentAssignment == FALSE, ] %>% select(PlaceboPreTestColleagueAffinity, PlaceboPostTestColleagueAffinity))

treatmentlPlaceboTestChange = getNumericAffinityChange(Q_WMBM[TreatmentAssignment == TRUE, ] %>% select(PlaceboPreTestColleagueAffinity, PlaceboPostTestColleagueAffinity))
```

``` r
controlPlaceboTestChange[, .(count=.N), by = list(Change)]
```

    ##    Change count
    ## 1:      0   130
    ## 2:      1     7
    ## 3:     -1     4
    ## 4:      2     1

``` r
treatmentlPlaceboTestChange[, .(count=.N), by = list(Change)]
```

    ##    Change count
    ## 1:      0   130
    ## 2:     -1     6
    ## 3:     -2     1
    ## 4:   2000     1
    ## 5:      1     1

> We notice in both treatment and control groups, most values are 0,
> which means that there was no change in the placebo test response pre
> and post survey. We notice a very large value when one of the
> responses were NA. Those can be ignored. From the above, we conclude
> that the placebo test worked\!
