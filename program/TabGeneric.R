TabHead1 <- 1
TabTitle <- 2
TabHead2 <- 3
TabFoot1 <- 4
TabFNTHeader <- 5
AddToTabFNT <- 6
TabFNLine1 <- 7
EnrNote1 <- 8
EnrNote2 <- 9
EnrNote3 <- 10
TabFNLine1Continue <- 11
AddedTabFN <- 12
TabFNTFooter <- 13
TabFNLine2 <- 14

TabHead1 <- "\\begin{table}\\hfil\\textsc{\\footnotesize Table \\refstepcounter{table}\\thetable: " 
TabTitle <- "1999-2002, 10 years and older in 1999, exist sample, same sample size" 
TabHeadLabelPre <- "\\label{"
TabHeadLabel <- "zEm.1999.10.sameN"
TabHeadLabelPost <- "}}"
TabHead2 <- "\\\\\\setlength{\\tabcolsep}{.5pt}\\renewcommand{\\arraystretch}{.675}\\hspace{-2em}\\hfil\\begin{tikzpicture}\\node (tbl) {\\input{"
TabFoot1 <- "}};\\input{c:/data/ramadan/save/tablecolortemplate.tex}\\end{tikzpicture}\\\\\\renewcommand{\\arraystretch}{1}"
TabHead2NoTIKZ <- "\\\\\\setlength{\\tabcolsep}{.5pt}\\renewcommand{\\arraystretch}{.675}\\hspace{-2em}\\hfil\\input{"
TabFoot1NoTIKZ <- "}\\\\\\renewcommand{\\arraystretch}{1}"
TabFNTHeader <- "\\hfil\\begin{tabular}{>{\\hfill\\scriptsize}p{1cm}<{}>{\\scriptsize}p{12cm}<{\\hfill}} Source:& Compiled from IFPRI data. "
#AddToTabFNT <- "Cohort of 10 - 18 year olds in 1999. Only direct offspring of household head are used." 
AddToTabFNT <- "Cohort of 10 - 18 year olds in 1999." 
TabFNLine1 <- "\\\\[-1ex] Notes:& 1. A first-difference estimator with standard errors clustered at \\textit{thana} level. Standard errors are clusterd at thana level with Satterthwaite degrees of freedom adjusted for a small number of clusters. $P$ values are shown in the parentheses. Satterthwaite degrees of freedom adjusted for a small number of clusters are shown in square brackets. $*$, $**$, $***$ indicate significance levels at 10\\%, 5\\%, 1\\%, respectively. Enrollment rates in 1999, 2002 are"

# EnrNote1 <- Enr.Base[grepl("zEm.1999", sample) & grepl("10", age)  & grepl("all", HHtype) & grepl("def", agHHdef) & grepl("1", agHH), rate]
# EnrAg = "for agricultural households,"
# EnrNote2 <- Enr.Base[grepl("zEm.1999", sample) & grepl("10", age) & grepl("all", HHtype) & grepl("def", agHHdef) & grepl("0", agHH), rate]
# EnrNonAg = "for non-agricultural households, respectively, with a difference-in-differences of"
# EnrNote3 <- unlist(Enrchg.Base[grepl("zEm.1999", sample) & grepl("10", age) & grepl("all", HHtype) & grepl("def", agHHdef), "AgNonag", with = F])
#AddedTabFN <- paste(memocovariates, memo4, collapse = "")
EnrNote1 <- EnrAg <- EnrNote2 <- EnrNonAg <- EnrNote3 <- AddedTabFN <- NULL
TabFNLine1Continue <- "Location (\\textit{thana}) dummies are omitted from the table for brevity. \\\\"
TabFNTFooter <- "\\end{tabular}"
TabFNLine2 <- "& 2. "
TabFoot2 <- "\\end{table}"

TabFilePathF <- function(
  FolderPath = pathsave, 
  Sample = "zEm1999",
  Estimator = "",
  AgeCutoff = "Older10",
  HHType = "",
  AgHHDef = "",
  CRSEMethod = "Satterthwaite", ...
  ) {
paste0(FolderPath, Sample, Estimator, AgeCutoff, HHType, AgHHDef, CRSEMethod, ".tex")
}
DisplayEstTable0Old <- function(
OmitFootnote = F, Position = NULL,
TABHead1=TabHead1, TABTitle=TabTitle,  
TABHeadLabelPre=TabHeadLabelPre, TABHeadLabel=TabHeadLabel,
TABHeadLabelPost = TabHeadLabelPost, TABHead2=TabHead2, 
TABFilePath=TabFilePath, TABFoot1=TabFoot1, 
TABFNTHeader=TabFNTHeader, AddToTABFNT=AddToTabFNT,  
TABFNLine1=TabFNLine1, ENRNote1=EnrNote1, ENRNote2=EnrNote2, 
ENRNote3=EnrNote3, ENRAg = EnrAg, ENRNonAg = EnrNonAg,
TABFNLine1Continue=TabFNLine1Continue, 
TABFNLine2=TabFNLine2, AddedTABFN = AddedTabFN,
TABFNTFooter=TabFNTFooter, TABFoot2=TabFoot2, 
ARRAYStretch = NULL, ...)
{
  if (!is.null(ARRAYStretch))
    TABHead2 <- gsub(".5", ARRAYStretch, TABHead2)
# By using a list, one can have a space between elements.
# By dividing elements one by one, R syntax like c(...) as in c(1, 2) is not displayed.
  if (OmitFootnote) 
    list(paste(
      c(TABHead1, TABTitle, TABHeadLabelPre, TABHeadLabel, 
      TABHeadLabelPost, TABHead2, TABFilePath, 
      TABFoot1, TABFoot2), collapse = "")) else
    list(paste(
      c(TABHead1, TABTitle, 
      TABHeadLabelPre, TABHeadLabel, TABHeadLabelPost,
      TABHead2, TABFilePath, TABFoot1,
      TABFNTHeader, AddToTABFNT, TABFNLine1), collapse = ""), 
      ENRNote1[1], 
      ENRNote1[2], 
      ENRAg,
      ENRNote2[1], 
      ENRNote2[2], 
      ENRNonAg, 
      ENRNote3, 
      paste(TABFNLine1Continue, 
      TABFNLine2, AddedTABFN, TABFNTFooter, TABFoot2, collapse = ""))
}
DisplayEstTable0 <- function(
OmitFootnote = F, Position = NULL,
TABHead1=TabHead1, TABTitle=TabTitle,  
TABHeadLabelPre=TabHeadLabelPre, TABHeadLabel=TabHeadLabel,
TABHeadLabelPost = TabHeadLabelPost, TABHead2=TabHead2, 
TABFilePath=TabFilePath, TABFoot1=TabFoot1, 
TABFNTHeader=TabFNTHeader, AddToTABFNT=AddToTabFNT,  
TABFNLine1=TabFNLine1, ENRNote1=EnrNote1, ENRNote2=EnrNote2, 
ENRNote3=EnrNote3, ENRAg = EnrAg, ENRNonAg = EnrNonAg,
TABFNLine1Continue=TabFNLine1Continue, 
TABFNLine2=TabFNLine2, AddedTABFN = AddedTabFN,
TABFNTFooter=TabFNTFooter, TABFoot2=TabFoot2, 
ARRAYStretch = NULL, ...)
{
  if (!is.null(ARRAYStretch))
    TABHead2 <- gsub(".5", ARRAYStretch, TABHead2)
# By using a list, one can have a space between elements.
# By dividing elements one by one, R syntax like c(...) as in c(1, 2) is not displayed.
  if (OmitFootnote) 
    list(paste(
      c(TABHead1, TABTitle, TABHeadLabelPre, TABHeadLabel, 
      TABHeadLabelPost, TABHead2, TABFilePath, 
      TABFoot1, TABFoot2), collapse = "")) else
    list(paste(
      c(TABHead1, TABTitle, 
      TABHeadLabelPre, TABHeadLabel, TABHeadLabelPost,
      TABHead2, TABFilePath, TABFoot1,
      TABFNTHeader, AddToTABFNT, TABFNLine1), collapse = ""), 
      paste(TABFNLine1Continue, 
      TABFNLine2, AddedTABFN, TABFNTFooter, TABFoot2, collapse = ""))
}

DisplayEstTable <- function(
TwoContinuedTables = F, Position = NULL,
TABHead1=TabHead1, TABTitle=TabTitle,  
TABHeadLabelPre=TabHeadLabelPre, TABHeadLabel=TabHeadLabel,
TABHeadLabelPost = TabHeadLabelPost, TABHead2=TabHead2, 
TABFilePath1=TabFilePath1, TABFilePath2=TabFilePath2, 
TABFoot1=TabFoot1, 
TABFNTHeader=TabFNTHeader, AddToTABFNT=AddToTabFNT,  
TABFNLine1=TabFNLine1, ENRNote1=EnrNote1, ENRNote2=EnrNote2, 
ENRNote3=EnrNote3, ENRAg = EnrAg, ENRNonAg = EnrNonAg,
TABFNLine1Continue=TabFNLine1Continue, 
TABFNLine2=TabFNLine2, AddedTABFN = AddedTabFN,
TABFNTFooter=TabFNTFooter, TABFoot2=TabFoot2, 
ArrayStrech = NULL, ...)
{
# By using a list, one can have a space between elements.
# By dividing elements one by one, R syntax like c(...) as in c(1, 2) is not displayed.
  if (!is.null(Position) && grepl("[Htbp]", Position)) 
    TABHead1 <- gsub("\\\\begin\\{table\\}", paste0("\\\\begin{table}[", Position, "]"), TabHead1) else
    TABHead1 <- TabHead1
  if (TwoContinuedTables) 
    c(
      DisplayEstTable0(OmitFootnote = T, 
      TABHead1 = TABHead1, TABTitle=TABTitle, TABHead2 = TABHead2, 
      TABFilePath=TABFilePath1, TABFoot1=TABFoot1, ARRAYStrech=ArrayStrech),
      "\\addtocounter{table}{-1}",
      DisplayEstTable0(OmitFootnote = F, TABFilePath=TABFilePath2, 
        TABTitle=paste0(TABTitle, " (continued)"), ARRAYStrech=ArrayStrech)
    ) else 
    DisplayEstTable0(
      TABHead1 = TABHead1, TABTitle=TABTitle,
      TABHead2 = TABHead2, TABFilePath=TABFilePath1, 
      TABFoot1=TABFoot1, ARRAYStrech=ArrayStrech)
}

disp <- function(A="a", B="b")
{
paste(A, B, collapse = "")
}
