CovariatesMemo <- 
  " \\textsf{Agricultural households * year 2002} is an interaction term of agricultural household dummy and year 2002 dummy. All the interaction terms are demeaned."
CovariatesMemo2 <- 
  paste(CovariatesMemo, "\\textsf{$\\underline{\\phantom{mm}}$ * Older sisters} and \\textsf{$\\underline{\\phantom{mm}}$ * Older brothers} are triple interaction terms of agricultural household dummy, year 2002 dummy, and number of older sisters/brother in a household for each child.")
AllLowerInteractions <- 
  "Regressions include all the linear terms of each interactions and all the double interaction terms of each triple interactions."
CovariatesMemoWithInteractions <- 
  paste(CovariatesMemo2, AllLowerInteractions)
ThreeSpecs <- 
  "For each panel, first columns are raw DID. Second columns add time-varying thana level characteristics (yield, mean rainfall, mean high temperature, mean low temperature), time-varying individual level characteristics (age squared, recipient of a poverty program), and \\textsf{Thana trends} that are interactions of year 2002 dummy with Thana fixed effects. Third columns add interactions of year 2002 dummy and baseline individual level characterstics (sex of individual, household head's and spouse's education, number of older male/female siblings, per member land holding, per member non-land asset holding, piped water access, structured toilet access) $\\bfx_{i}r_{t}$, and triple interactions of year 2002 dummy, individual characteristics, and agricultural household dummy $\\bfx_{i}r_{t}D_{i}$. "
SpecMemo1 <- 
  paste(CovariatesMemo, ThreeSpecs, 
  "Rows of $\\underline{\\phantom{mm}}*x$ show estimates of the triple interaction term of $x_{i}$, or $x_{i}r_{t}D_{i}$.", AllLowerInteractions, "Parental education variables are strongly collinear with agricultural household dummy and are used only in year 2002 interaction terms to avoid multicollinearity.")
SpecMemo0 <- gsub("first columns", "\\\\textsf\\{Specification 1\\}", SpecMemo1)
SpecMemo0 <- gsub("Second columns", "\\\\textsf\\{Specification 2\\}", SpecMemo0)
SpecMemo0 <- gsub("Third columns", "\\\\textsf\\{Specification 3\\}", SpecMemo0)
SpecMemoAgdef <- 
  paste(CovariatesMemo, "Panels A. - D. give estimates under different agricultural household definitions. A: Head's reply base, B: Income source base, C: Occupation base, D: All combined (with ``or'' operations).",  ThreeSpecs, AllLowerInteractions, "Parental education variables are strongly collinear with agricultural household dummy and are used only in year 2002 interaction terms to avoid multicollinearity.")
SpecMemoAgewise <- 
  paste(CovariatesMemo, "", 
  ThreeSpecs, AllLowerInteractions, "Parental education variables are strongly collinear with agricultural household dummy and are used only in year 2002 interaction terms to avoid multicollinearity.")
SpecMemoAgewise <- gsub("first columns", "first, fourth, and seventh columns", SpecMemoAgewise)
SpecMemoAgewise <- gsub("Second columns", "Second, fifth, and eith columns", SpecMemoAgewise)
SpecMemoAgewise <- gsub("Third columns", "Third, sixth, and ninth columns", SpecMemoAgewise)
SpecMemoAgeGroupwise <- 
  paste(CovariatesMemo, "Primary and Secondary panels show results for primary and secondary school aged children, respectively.", 
  ThreeSpecs, AllLowerInteractions, "Parental education variables are strongly collinear with agricultural household dummy and are used only in year 2002 interaction terms to avoid multicollinearity.")
TabFnNonMuslimFlood <- 
  "Panel headed by Non Muslims shows impacts with non-Muslim dummy and its interactions. Panel headed by Flooded shows impacts with Flooded dummy and its interactions."
SpecMemoNonMuslimFlood <- paste(CovariatesMemo, TabFnNonMuslimFlood)
TabFnNumGradesDaysAbsent <- 
  "Panel under Number of grades shows impacts on grade progression. Panel under Days absent shows impacts monthly absent days from school in April - June (boro planting season) of respective years."
SpecMemoNumGradesDaysAbsent <- paste(CovariatesMemo, TabFnNumGradesDaysAbsent)
SEMemoForSelectedResultsNoStar <- 
  " Standard errors in the parentheses are clusterd at thana level with a correction for small number of clusters using bias reduced linearization (Satterthwaite correction). $P$ values in percentages are shown in braces. 95\\% confidence intervals are shown in square brackets. "
SEMemoForSelectedResults <- 
  paste(SEMemoForSelectedResultsNoStar, " $*$, $**$, $***$ indicate significance levels at 10\\%, 5\\%, 1\\% under BRL cluster robust standard errors, respectively.")
SpecMemo1Placebo <- gsub("2002", "2006", SpecMemo1)
SpecMemo1html <- gsub("\\\\textsf\\{(.*?)\\}", "\\1", SpecMemo1Placebo)
SpecMemo1Placebohtml <- paste(
  "Panels under 2002 cohort estimate placebo impacts on the cohort of 10-18 years old in 2002. Panels under 1999 cohort estimate placebo impacts on the cohort of 10-18 years old in 1999 (13-21 in 2002).", SpecMemo1Placebohtml
  )
SpecMemoAgewisehtml <- gsub("\\\\textsf\\{(.*?)\\}", "\\1", SpecMemoAgewise)
SpecMemoAgeGroupwisehtml <- gsub("\\\\textsf\\{(.*?)\\}", "\\1", SpecMemoAgeGroupwise)
SpecMemoNonMuslimFloodhtml <- gsub("\\\\textsf\\{(.*?)\\}", "\\1", SpecMemoNonMuslimFlood)
SpecMemoNumGradesDaysAbsenthtml <- 
  gsub("\\\\textsf\\{(.*?)\\}", "\\1", SpecMemoNumGradesDaysAbsent)
SEMemoForSelectedResultshtml <- gsub("\\\\textsf\\{(.*?)\\}", "\\1", SEMemoForSelectedResults)
SEMemoForSelectedResultsNoStarhtml <- 
  gsub("\\\\textsf\\{(.*?)\\}", "\\1", SEMemoForSelectedResultsNoStar)
