#### DefinitionsAndParameters.R

#### agHH is defined in CreateVariablesIn2RoundPanel.rnw
#### agHH0: Most inclusive definition = union of isagHH, hdagHH, ocagHH
#### isagHH: Income source based definition
#### hdagHH: Head's reply
#### ocagHH: Occupation based definition
aghh.defs <- c("agHH0", "isagHH", "hdagHH", "ocagHH")
Aghh.defs <- c("Combined", "Income source", "Head's reply", "Occupation")
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
gdfiles <- c("grade initial enr", "grade enr", "absent enr", "absent cross sec")

#### Add stars to table
ADDStar <- T

#### ggplot theme
library(ggplot2)
ThisTheme <- ggplot2::theme(
   axis.text.x = element_text(size = 12, angle = 0, vjust = 1, hjust = .5), 
   axis.text.y = element_text(size = 12), 
   axis.title = element_text(size = 10), 
   strip.text.x = element_text(color = "blue", size = 8, 
     margin = margin(0, 1.25, 0, 1.25, "cm")), 
   strip.text.y = element_text(color = "blue", size = 8, 
     margin = margin(1.5, 0, 1.5, 0, "cm")),
   panel.spacing.x = unit(c(.1), units = "cm"),
   panel.spacing.y = unit(.1, units = "cm"), 
   legend.position = "none")
ThisThemeEnd <- ThisTheme + ggplot2::theme(legend.position="bottom")
