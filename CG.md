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
library(data.table)
library(lmtest)
library(sandwich)
library(stargazer)
library(dplyr)
```

``` r
d <- fread("data.csv")

head(d)
```

    ##          StartDate         EndDate   ResponseType     IPAddress Progress
    ## 1: 3/29/2021 10:59 3/29/2021 11:05 Survey Preview                    100
    ## 2: 3/30/2021 11:40 3/30/2021 12:30 Survey Preview                    100
    ## 3: 3/30/2021 13:11 3/30/2021 13:21 Survey Preview                    100
    ## 4: 3/30/2021 14:58 3/30/2021 15:01     IP Address 136.50.29.235      100
    ## 5: 3/30/2021 15:40 3/30/2021 16:31 Survey Preview                    100
    ## 6: 3/30/2021 18:03 3/30/2021 19:22 Survey Preview                    100
    ##    DurationSeconds Finished    RecordedDate        ResponseID LocationLatitude
    ## 1:             361     TRUE 3/29/2021 11:05 R_3kpkeubEVJMXVeY         37.89220
    ## 2:            2956     TRUE 3/30/2021 12:30 R_2rORoun447CrzQB         37.89220
    ## 3:             621     TRUE 3/30/2021 13:21 R_2QSquguoMN5Dj7A         37.49670
    ## 4:             193     TRUE 3/30/2021 15:01 R_u1yNIpPNbXY8GXv         29.45509
    ## 5:            3098     TRUE 3/30/2021 16:31 R_2Yn2e6TZ2jGGDgh         25.73489
    ## 6:            4711     TRUE 3/30/2021 19:22 R_V3IqeSnvscmn2Mx         47.55791
    ##    LocationLongitude DistributionChannel UserLanguage YearOfBirth
    ## 1:         -122.2729             preview           EN            
    ## 2:         -122.2729             preview           EN            
    ## 3:         -122.2665             preview           EN        1986
    ## 4:          -98.6498           anonymous           EN        1985
    ## 5:          -80.2228             preview           EN        1991
    ## 6:         -122.1633             preview           EN       India
    ##    GenderCategory Gender IsEnglishFirstLanguage               Race RaceOther
    ## 1:                                                 Native American          
    ## 2:                                                                          
    ## 3:  Cisgender Man                           Yes              Asian          
    ## 4:  Cisgender Man                           Yes Non-Hispanic White          
    ## 5:  Cisgender Man                           Yes Non-Hispanic White          
    ## 6:  Cisgender Man                            No              Asian          
    ##    AbleToAnswerCountryOfResidence       CountryOfResidence StateOfResidence
    ## 1:                                                                         
    ## 2:                                                                         
    ## 3:                            Yes United States of America       California
    ## 4:                            Yes United States of America    Massachusetts
    ## 5:                            Yes United States of America          Florida
    ## 6:                            Yes United States of America       Washington
    ##    RoleAtBerkeley IsTransferStudent YearsAtBerkeley DidEarnMoneyLastYear
    ## 1:                                                                      
    ## 2:                                                                      
    ## 3:                                                                   Yes
    ## 4:                                                                    No
    ## 5:                                                                   Yes
    ## 6:                                                                   Yes
    ##      EmploymentStatus       IncomeIn2020                 HighestDegreeReceived
    ## 1:                                                                            
    ## 2:                                                                            
    ## 3: Employed full time  $60,000 - $69,999 Advanced degree (Master's, Doctorate)
    ## 4:            Student                                             Some college
    ## 5: Employed full time  $70,000 - $79,999                     Bachelor's degree
    ## 6: Employed full time More than $150,000 Advanced degree (Master's, Doctorate)
    ##    PoliticalAffiliation ReligiousAffliation ReligiousAffliationOther
    ## 1:                           Roman Catholic                         
    ## 2:                                                                  
    ## 3:                Other Atheist or agnostic                         
    ## 4:                Other Atheist or agnostic                         
    ## 5:             Democrat              Jewish                         
    ## 6:                Other               Hindu                         
    ##    ReligiousAffliationOther2 BookFormatPurchasedMostOften
    ## 1:                                                       
    ## 2:                                                       
    ## 3:                                 I don't purchase books
    ## 4:                                           I buy ebooks
    ## 5:                                             I buy both
    ## 6:                                   I buy physical books
    ##    FrequencySocialMediaAccess FrequencySocialMediaPosting
    ## 1:                                                       
    ## 2:                                                       
    ## 3:                     Weekly                      Weekly
    ## 4:       More than once a day                       Daily
    ## 5:       More than once a day            Less than Weekly
    ## 6:                     Weekly            Less than Weekly
    ##    TimeFC_FrequencySocialMediaPosting TimeLC_FrequencySocialMediaPosting
    ## 1:                                 NA                                 NA
    ## 2:                             20.318                             27.299
    ## 3:                              1.650                              2.569
    ## 4:                                 NA                                 NA
    ## 5:                              4.690                             10.335
    ## 6:                             12.111                             20.623
    ##    TimePS_FrequencySocialMediaPosting TimeCC_FrequencySocialMediaPosting
    ## 1:                                 NA                                 NA
    ## 2:                             29.076                                  2
    ## 3:                              4.129                                  2
    ## 4:                                 NA                                 NA
    ## 5:                             15.422                                  2
    ## 6:                             22.330                                  2
    ##              ClassLevel            GenderCGSurvey              RaceCGSurvey
    ## 1:                                                                         
    ## 2:           Fresh/Soph      Prefer not to answer                     White
    ## 3:                Jr/Sr Non-binary / third gender Black or African American
    ## 4: Prefer not to answer                      Male               White,Asian
    ## 5:       Grad/Post-Grad                      Male                     White
    ## 6:       Grad/Post-Grad                      Male                     Asian
    ##           HasServedInCG HasServedInAnyOtherUniformedService
    ## 1:                                                         
    ## 2:                   No                                  No
    ## 3: Prefer not to answer                                 Yes
    ## 4:                  Yes                Prefer not to answer
    ## 5:                   No                                  No
    ## 6:                   No                                  No
    ##    PlaceboPreTestColleagueAffinity TimeFC_Intro TimeLC_Intro TimePS_Intro
    ## 1:                                           NA           NA           NA
    ## 2:               Like a great deal       24.082       51.220       52.363
    ## 3:                   Like somewhat        1.657        8.757       11.707
    ## 4:                   Like somewhat        1.200        9.544       10.684
    ## 5:                   Like somewhat        1.514       17.319       22.490
    ## 6:               Like a great deal        2.352       49.812       51.767
    ##    TimeCC_Intro TimeFC_Intro2 TimeLC_Intro2 TimePS_Intro2 TimeCC_Intro2
    ## 1:           NA            NA            NA            NA            NA
    ## 2:            6        47.122        58.574        68.850             3
    ## 3:            7         1.561         1.911         2.533             2
    ## 4:            7         0.000         0.000         1.405             0
    ## 5:            7         0.000         0.000        26.883             0
    ## 6:           11         0.000         0.000        54.712             0
    ##    PracticeQCGComparison TimeFC_PracticeQCGComparison
    ## 1:                    NA                           NA
    ## 2:                     0                       39.974
    ## 3:                     0                        2.960
    ## 4:                     0                        0.000
    ## 5:                     0                        0.000
    ## 6:                     0                       52.950
    ##    TimeLC_PracticeQCGComparison TimePS_PracticeQCGComparison
    ## 1:                           NA                           NA
    ## 2:                       61.745                       62.569
    ## 3:                        4.103                        7.411
    ## 4:                        0.000                        1.105
    ## 5:                        0.000                       29.675
    ## 6:                       52.950                       54.334
    ##    TimeCC_PracticeQCGComparison TreatmentQ1 TimeFC_TreatmentQ1
    ## 1:                           NA          NA                 NA
    ## 2:                            3          NA                 NA
    ## 3:                            3           0                  0
    ## 4:                            0           0                  0
    ## 5:                            0          NA                 NA
    ## 6:                            1          NA                 NA
    ##    TimeLC_TreatmentQ1 TimePS_TreatmentQ1 TimeCC_TreatmentQ1 TreatmentQ2
    ## 1:                 NA                 NA                 NA          NA
    ## 2:                 NA                 NA                 NA          NA
    ## 3:                  0              1.495                  0           0
    ## 4:                  0              0.605                  0           0
    ## 5:                 NA                 NA                 NA          NA
    ## 6:                 NA                 NA                 NA          NA
    ##    TimeFC_TreatmentQ2 TimeLC_TreatmentQ2 TimePS_TreatmentQ2 TimeCC_TreatmentQ2
    ## 1:                 NA                 NA                 NA                 NA
    ## 2:                 NA                 NA                 NA                 NA
    ## 3:              2.554              3.165              4.306                  2
    ## 4:              0.000              0.000              0.699                  0
    ## 5:                 NA                 NA                 NA                 NA
    ## 6:                 NA                 NA                 NA                 NA
    ##    TreatmentQ3 TimeFC_TreatmentQ3 TimeLC_TreatmentQ3 TimePS_TreatmentQ3
    ## 1:          NA                 NA                 NA                 NA
    ## 2:          NA                 NA                 NA                 NA
    ## 3:           0                  0                  0              1.607
    ## 4:           0                  0                  0              0.714
    ## 5:          NA                 NA                 NA                 NA
    ## 6:          NA                 NA                 NA                 NA
    ##    TimeCC_TreatmentQ3 TreatmentQ4 TimeFC_TreatmentQ4 TimeLC_TreatmentQ4
    ## 1:                 NA          NA                 NA                 NA
    ## 2:                 NA          NA                 NA                 NA
    ## 3:                  0           0              2.247              2.953
    ## 4:                  0           0              0.000              0.000
    ## 5:                 NA          NA                 NA                 NA
    ## 6:                 NA          NA                 NA                 NA
    ##    TimePS_TreatmentQ4 TimeCC_TreatmentQ4 TreatmentQ5 TimeFC_TreatmentQ5
    ## 1:                 NA                 NA          NA                 NA
    ## 2:                 NA                 NA          NA                 NA
    ## 3:              5.046                  2           0                  0
    ## 4:              0.768                  0           0                  0
    ## 5:                 NA                 NA          NA                 NA
    ## 6:                 NA                 NA          NA                 NA
    ##    TimeLC_TreatmentQ5 TimePS_TreatmentQ5 TimeCC_TreatmentQ5 TreatmentQ6
    ## 1:                 NA                 NA                 NA          NA
    ## 2:                 NA                 NA                 NA          NA
    ## 3:                  0              0.851                  0           0
    ## 4:                  0              0.966                  0           0
    ## 5:                 NA                 NA                 NA          NA
    ## 6:                 NA                 NA                 NA          NA
    ##    TimeFC_TreatmentQ6 TimeLC_TreatmentQ6 TimePS_TreatmentQ6 TimeCC_TreatmentQ6
    ## 1:                 NA                 NA                 NA                 NA
    ## 2:                 NA                 NA                 NA                 NA
    ## 3:                  0                  0              1.355                  0
    ## 4:                  0                  0              0.799                  0
    ## 5:                 NA                 NA                 NA                 NA
    ## 6:                 NA                 NA                 NA                 NA
    ##    ControlQ1 TimeFC_ControlQ1 TimeLC_ControlQ1 TimePS_ControlQ1
    ## 1:        NA               NA               NA               NA
    ## 2:         0           38.169           40.097           46.181
    ## 3:        NA               NA               NA               NA
    ## 4:        NA               NA               NA               NA
    ## 5:         0           23.005           26.580           28.616
    ## 6:         0            1.936            1.936            3.178
    ##    TimeCC_ControlQ1 ControlQ2 TimeFC_ControlQ2 TimeLC_ControlQ2
    ## 1:               NA        NA               NA               NA
    ## 2:                4         0           10.362           11.260
    ## 3:               NA        NA               NA               NA
    ## 4:               NA        NA               NA               NA
    ## 5:                2         0           13.129           13.129
    ## 6:                1         0            2.320            2.320
    ##    TimePS_ControlQ2 TimeCC_ControlQ2 ControlQ3 TimeFC_ControlQ3
    ## 1:               NA               NA        NA               NA
    ## 2:           12.850                2         0            4.348
    ## 3:               NA               NA        NA               NA
    ## 4:               NA               NA        NA               NA
    ## 5:           29.820                1         0           18.009
    ## 6:            3.909                1         0           15.285
    ##    TimeLC_ControlQ3 TimePS_ControlQ3 TimeCC_ControlQ3 ControlQ4
    ## 1:               NA               NA               NA        NA
    ## 2:            5.581            7.236                3         0
    ## 3:               NA               NA               NA        NA
    ## 4:               NA               NA               NA        NA
    ## 5:           21.203           23.268                2         0
    ## 6:           16.160           16.961                2         0
    ##    TimeFC_ControlQ4 TimeLC_ControlQ4 TimePS_ControlQ4 TimeCC_ControlQ4
    ## 1:               NA               NA               NA               NA
    ## 2:            5.515            7.273            8.893                3
    ## 3:               NA               NA               NA               NA
    ## 4:               NA               NA               NA               NA
    ## 5:           78.911           83.224           85.278                5
    ## 6:            2.219            2.219            3.812                1
    ##    ControlQ5 TimeFC_ControlQ5 TimeLC_ControlQ5 TimePS_ControlQ5
    ## 1:        NA               NA               NA               NA
    ## 2:         0           37.240           41.553           44.113
    ## 3:        NA               NA               NA               NA
    ## 4:        NA               NA               NA               NA
    ## 5:         0           17.811           17.811           44.156
    ## 6:         0           37.230           37.230           38.977
    ##    TimeCC_ControlQ5 ControlQ6 TimeFC_ControlQ6 TimeLC_ControlQ6
    ## 1:               NA        NA               NA               NA
    ## 2:                4         0           19.124           20.431
    ## 3:               NA        NA               NA               NA
    ## 4:               NA        NA               NA               NA
    ## 5:                1         0           24.197           29.429
    ## 6:                1         0           77.822           86.594
    ##    TimePS_ControlQ6 TimeCC_ControlQ6 PlaceboPostTestColleagueAffinity
    ## 1:               NA               NA                                 
    ## 2:           22.586                3                Like a great deal
    ## 3:               NA               NA                    Like somewhat
    ## 4:               NA               NA         Neither like nor dislike
    ## 5:           31.440                3                    Like somewhat
    ## 6:           90.565                2                Like a great deal
    ##    TimeFC_PlaceboPostTestColleagueAffinity
    ## 1:                                      NA
    ## 2:                                      NA
    ## 3:                                      NA
    ## 4:                                      NA
    ## 5:                                      NA
    ## 6:                                       0
    ##    TimeLC_PlaceboPostTestColleagueAffinity
    ## 1:                                      NA
    ## 2:                                      NA
    ## 3:                                      NA
    ## 4:                                      NA
    ## 5:                                      NA
    ## 6:                                       0
    ##    TimePS_PlaceboPostTestColleagueAffinity
    ## 1:                                      NA
    ## 2:                                      NA
    ## 3:                                      NA
    ## 4:                                      NA
    ## 5:                                      NA
    ## 6:                                  20.466
    ##    TimeCC_PlaceboPostTestColleagueAffinity
    ## 1:                                      NA
    ## 2:                                      NA
    ## 3:                                      NA
    ## 4:                                      NA
    ## 5:                                      NA
    ## 6:                                       0

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
  LeftName  = c("Bradley Meyer", "Bradley Meyer", "Bradley Meyer", "Kirsten Schmidt", "Reginald Washington", "Kirsten Schmidt"),
  RightName = c("Reginald Washington", "Kirsten Schmidt", "Gwendolyn Jackson", "Reginald Washington", "Gwendolyn Jackson", "Gwendolyn Jackson"),
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
  row <- getRowByName(treatmentMetadata[i]$LeftName, nameResumeBinding)
   
  treatmentMetadata[i]$LeftPersonRace = row$Race
  treatmentMetadata[i]$LeftPersonGender = row$Gender
  treatmentMetadata[i]$LeftPersonRaceGender = row$RaceGender
  treatmentMetadata[i]$LeftPersonResumeId = row$ResumeId
}

# iterate through right name and fill right person metadata
for (i in 1:n) {
  row <- getRowByName(treatmentMetadata[i]$RightName, nameResumeBinding)
   
  treatmentMetadata[i]$RightPersonRace = row$Race
  treatmentMetadata[i]$RightPersonGender = row$Gender
  treatmentMetadata[i]$RightPersonRaceGender = row$RaceGender
  treatmentMetadata[i]$RightPersonResumeId = row$ResumeId
}
```

``` r
controlBinding <- data.table(
  LeftResumeId  = c(1, 1, 1, 3, 2, 3),
  RightResumeId = c(2, 3, 4, 2, 4, 4),
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
head(dTQ1)
```

    ##          StartDate         EndDate   ResponseType      IPAddress Progress
    ## 1: 3/30/2021 13:11 3/30/2021 13:21 Survey Preview                     100
    ## 2: 3/30/2021 14:58 3/30/2021 15:01     IP Address  136.50.29.235      100
    ## 3:  3/31/2021 0:27  3/31/2021 0:42 Survey Preview                     100
    ## 4: 3/31/2021 20:37 3/31/2021 21:34     IP Address   96.250.83.61      100
    ## 5: 3/31/2021 20:33 3/31/2021 21:41     IP Address 97.114.213.149      100
    ## 6: 3/31/2021 20:42 3/31/2021 21:58     IP Address   12.169.99.98      100
    ##    DurationSeconds Finished    RecordedDate        ResponseID LocationLatitude
    ## 1:             621     TRUE 3/30/2021 13:21 R_2QSquguoMN5Dj7A         37.49670
    ## 2:             193     TRUE 3/30/2021 15:01 R_u1yNIpPNbXY8GXv         29.45509
    ## 3:             900     TRUE  3/31/2021 0:42 R_31vJynnHBul11qv         25.04781
    ## 4:            3383     TRUE 3/31/2021 21:34 R_2WSDtOHMY0iEfyB         40.70329
    ## 5:            4068     TRUE 3/31/2021 21:41 R_31cuvH38jf2jrSO         35.49080
    ## 6:            4520     TRUE 3/31/2021 21:58 R_QghTi29Xndqrr6F         36.91020
    ##    LocationLongitude DistributionChannel UserLanguage YearOfBirth
    ## 1:         -122.2665             preview           EN        1986
    ## 2:          -98.6498           anonymous           EN        1985
    ## 3:          121.5318             preview           EN      Taiwan
    ## 4:          -74.0039           anonymous           EN        1995
    ## 5:          -93.4911           anonymous           EN        1978
    ## 6:         -121.7564           anonymous           EN        1997
    ##     GenderCategory Gender IsEnglishFirstLanguage               Race RaceOther
    ## 1:   Cisgender Man                           Yes              Asian          
    ## 2:   Cisgender Man                           Yes Non-Hispanic White          
    ## 3:   Cisgender Man                            No              Asian          
    ## 4:   Cisgender Man                           Yes Non-Hispanic White          
    ## 5:   Cisgender Man                           Yes Non-Hispanic White          
    ## 6: Cisgender Woman                           Yes Hispanic or Latino          
    ##    AbleToAnswerCountryOfResidence       CountryOfResidence StateOfResidence
    ## 1:                            Yes United States of America       California
    ## 2:                            Yes United States of America    Massachusetts
    ## 3:                            Yes United States of America       California
    ## 4:                            Yes United States of America         New York
    ## 5:                            Yes United States of America         Arkansas
    ## 6:                            Yes United States of America       California
    ##           RoleAtBerkeley IsTransferStudent YearsAtBerkeley DidEarnMoneyLastYear
    ## 1:                                                                          Yes
    ## 2:                                                                           No
    ## 3:      Graduate Student                                                     No
    ## 4:                 Other                                                    Yes
    ## 5: Undergraduate Student                No        2nd Year                  Yes
    ## 6:                 Other                                                    Yes
    ##      EmploymentStatus      IncomeIn2020                 HighestDegreeReceived
    ## 1: Employed full time $60,000 - $69,999 Advanced degree (Master's, Doctorate)
    ## 2:            Student                                            Some college
    ## 3:            Student                   Advanced degree (Master's, Doctorate)
    ## 4: Employed full time $50,000 - $59,999                     Bachelor's degree
    ## 5: Employed part time $20,000 - $29,999                          Some college
    ## 6: Employed part time $10,000 - $19,999                          Some college
    ##    PoliticalAffiliation ReligiousAffliation ReligiousAffliationOther
    ## 1:                Other Atheist or agnostic                         
    ## 2:                Other Atheist or agnostic                         
    ## 3:             Democrat Atheist or agnostic                         
    ## 4:           Republican              Jewish                         
    ## 5:          Independent          Protestant                         
    ## 6:             Democrat      Roman Catholic                         
    ##    ReligiousAffliationOther2 BookFormatPurchasedMostOften
    ## 1:                                 I don't purchase books
    ## 2:                                           I buy ebooks
    ## 3:                                   I buy physical books
    ## 4:                                   I buy physical books
    ## 5:                                             I buy both
    ## 6:                                 I don't purchase books
    ##    FrequencySocialMediaAccess FrequencySocialMediaPosting           ClassLevel
    ## 1:                     Weekly                      Weekly                Jr/Sr
    ## 2:       More than once a day                       Daily Prefer not to answer
    ## 3:                      Daily            Less than Weekly       Grad/Post-Grad
    ## 4:                      Daily            Less than Weekly       Grad/Post-Grad
    ## 5:                      Daily            Less than Weekly           Fresh/Soph
    ## 6:       More than once a day                       Daily Prefer not to answer
    ##               GenderCGSurvey                         RaceCGSurvey
    ## 1: Non-binary / third gender            Black or African American
    ## 2:                      Male                          White,Asian
    ## 3:                      Male                                Asian
    ## 4:                      Male                                White
    ## 5:                      Male                                White
    ## 6:                    Female Hispanic, Latinex, or Spanish Origin
    ##           HasServedInCG HasServedInAnyOtherUniformedService
    ## 1: Prefer not to answer                                 Yes
    ## 2:                  Yes                Prefer not to answer
    ## 3:                   No                                  No
    ## 4:                   No                                  No
    ## 5:                   No                                  No
    ## 6:                   No                                  No
    ##    PlaceboPreTestColleagueAffinity PracticeQCGComparison
    ## 1:                   Like somewhat                     0
    ## 2:                   Like somewhat                     0
    ## 3:                   Like somewhat                     0
    ## 4:                   Like somewhat                     0
    ## 5:                   Like somewhat                     0
    ## 6:               Like a great deal                     0
    ##    PlaceboPostTestColleagueAffinity TreatmentAssignment IsAttrited IsNeverTaker
    ## 1:                    Like somewhat                TRUE      FALSE        FALSE
    ## 2:         Neither like nor dislike                TRUE      FALSE        FALSE
    ## 3:                    Like somewhat                TRUE      FALSE        FALSE
    ## 4:                    Like somewhat                TRUE      FALSE        FALSE
    ## 5:                    Like somewhat                TRUE      FALSE        FALSE
    ## 6:         Neither like nor dislike                TRUE      FALSE        FALSE
    ##    TreatmentQ1 TimeFC_TreatmentQ1 TimeLC_TreatmentQ1 TimePS_TreatmentQ1
    ## 1:           0              0.000              0.000              1.495
    ## 2:           0              0.000              0.000              0.605
    ## 3:           0              0.000              0.000              1.502
    ## 4:           0             68.865             68.865             70.682
    ## 5:           0             30.063             30.063             34.215
    ## 6:           0              4.052              4.052              6.397
    ##    TimeCC_TreatmentQ1
    ## 1:                  0
    ## 2:                  0
    ## 3:                  0
    ## 4:                  1
    ## 5:                  1
    ## 6:                  1

### Assign Fake Treatment and Control Reponses (Optional)

> As we are aware, we had an error in the procedure around collecting
> data for our participants. Thus we will overwrite the data with fake
> responses so as to prove out our analysis methodology. Below we choose
> a `random number generator to generate responses from 1-5`. The rubric
> is as follows:

> `1: Strongly prefer left resume`

> `2: Slightly prefer left resume`

> `3: Neutral (no preference on left or right resume)`

> `4: Slightly prefer right resume`

> `5: Strongly prefer right resume`

## Analysis
