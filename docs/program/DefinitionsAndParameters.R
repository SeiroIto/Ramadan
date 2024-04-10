#### DefinitionsAndParameters.R

#### agHH is defined in CreateVariablesIn2RoundPanel.rnw
#### agHH0: Most inclusive definition = union of isagHH, hdagHH, ocagHH
#### isagHH: Income source based definition
#### hdagHH: Head's reply
#### ocagHH: Occupation based definition
aghh.defs <- c("agHH0", "isagHH", "hdagHH", "ocagHH")
aghh.defs.regexpr <- paste0(aghh.defs, "\\.")
agd <- paste0(toupper(substr(aghh.defs, 1, 1)), substr(aghh.defs, 2, 2))
agd[1] <- ""

#### Age range
#### According to this, AgeInRange AgeInRangeX AgeInRangeInRX are defined.
minAge <- 6
maxAge <- 18

#### Use sd2 = sons/daughters with extended family
Usesd2 <- F

#### Demean at individual level?
DemeanAtIndividualLevel <- F

#### Clustering methods
ClusteringMethod <- c("LiangZeger", "satterthwaite", "wildclusterboot")

#### Gender subsamples
genderitems <- c("boys", "girls", "boys+girls")
genderregexpr <- c("ys$", "^g", "\\+")

#### Age group
agitems <- c("pri", "sec", "coll")
AgeGroup2 <- c("primary", "secondary", "college")

#### Placebo group
placeboitems <- c("1999 cohort", "2002 cohort")

##### Other outcomes
gdfiles <- c("grade initial enr", "grade enr", "absent enr")

