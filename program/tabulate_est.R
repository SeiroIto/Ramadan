## For a summary table with est [LZ ci] [Satterthwaite CI]: Use TabulatedMainResults.qs (tabulated results) where "inference" column gives LZ and Satterthwaite information. Construct a table by stacking up est, LZ-CI, LZ-Satterthwaite. 
## For a show-all-results table only with one statistic line per estimate (est, se; ets, p; est, ci), use results in a list (DID2024_MainResults.qs) and construct a table.

## Dependency between functions ##
## 1. Tabulate list of reg tables in latex
## TabEstSEPvalCI ==> (is used in) tabs2latex4.
## tabs2latex4, reordertab ==> Tab.Est
## 2. Tabulate list of reg tables in (r)markdown
## TabEstSEPvalCIHTML ==> latextab4
## tabs2latex4, reordertab ==> Tab.Est.Html 
## 3. Tabulate a table (produced by Tab.Est without a variable column) of reg lists in latex
## latextab ==> SaveEstTable 

reordertab <- function(tb, ord, deletethese = "^se\\$|^p\\$|^DoF\\$|^CI\\$", RowsPerCovariate = 3)
#  reorder table according to specified order by rownames of a table
#  ver3: Accommodates CI and DoF rows
#  tb: table with with rownames in either row order:
#     est.A                     est.A
#     se_{A}                   p_{A}
#     CI_{A}/DoF_{A}      CI_{A}/DoF_{A} <= this row may be missing
#  ord: "1st|2nd|3rd" like regexp of ordering
#  deletethese: a vector of regexp indicating rownames to be deleted
#     default is rows starting with se$_{...}, CI$_{}$, DoF$_{}$
#  RowsPerCovariate: 3 if there is CI or DoF row, 2 if est and p-value/SE.
{
  rn <- rownames(tb)
  if (is.null(rn)) stop("no rowname in the table.")
  for (d in 1:length(deletethese)) rn[grep(deletethese[d], rn)] <- ""
  ord <- asc(strsplit(ord, "\\|"))
  roword <- NULL
  for (i in 1:length(ord)) roword <- c(roword, grep(ord[i], rn))
  if (RowsPerCovariate == 2) 
    roword <- c(t(cbind(roword, roword+1))) else
    roword <- c(t(cbind(roword, roword+1, roword+2)))
  return(tb[roword, ])
}
map2bigmat <- function(tabl,bigmatrownames,perl=TRUE,startwithslash=FALSE)
{
  ii <- NULL;
  if (perl==FALSE)
  {
  for (i in 1:nrow(tabl))
    ii <- c(ii,
        grep(rownames(tabl)[i],bigmatrownames, ignore.case=F, fixed=T));
  } else 
  {
    str1 <- "^";
    if (startwithslash==TRUE) str1 <- paste(str1,rep("\\",1),sep=",");
    for (i in 1:nrow(tabl))
      ii <- c(ii,
      #  since each entries are considered as a line
      #  we need ^XXX$ for shortest matching
              grep(paste(paste(str1,rownames(tabl)[i],sep=""),"$",sep=""),
              perl=T,bigmatrownames,ignore.case=F));
}
return(ii);
}
tabs2latex <- function(est, digits = 3, useperl = T, ...)
#  produces estimated results table with asterisks, se,
#  from est,  a list of estimated tables.
#  each est[[i]] is a table of estimate, std error, p value
#  e.g., est <- list(e1=est1,e2=est2,e3=est3,e4=est4)
#  requires "map2bigmat"
#  useperl: if F, not use perl in map2bigmat 
#                (perl creates problems with rownames with escape characters)
{
  n <- length(est); rn <- rownames(est[[1]])
  for (i in 2:n) rn <- union(rn, rownames(est[[i]]))
  rn <- c(t(cbind(rn, paste("se$_{", rn, "}$", sep=""))))
  bigmat <- array("",  dim=c(length(rn), n))
  for (i in 1:n)
  {
    tab <- matrix(as.numeric(as.matrix(est[[i]])), byrow=F, ncol=3)
    dimnames(tab) <- dimnames(est[[i]])
    tab <- tabstar1(tab, digits=digits)
    if (length(grep("\\(?Intercept\\)?$|^Const\\.$", rn, perl=T))>0)
    {
      bigmat[c(1, map2bigmat(tab, rn)), i] <- 
        as.character(tab[seq(1, nrow(tab)-1, 2), 1])
      bigmat[c(1, map2bigmat(tab, rn))+1, i] <- 
        as.character(tab[seq(2, nrow(tab), 2), 1])
    } else
    {
      bigmat[map2bigmat(tab, rn), i] <- 
        as.character(tab[seq(1, nrow(tab)-1, 2), 1])
      bigmat[map2bigmat(tab, rn)+1, i] <- 
        as.character(tab[seq(2, nrow(tab), 2), 1])
    }
  }
  bigmat <- data.frame(bigmat)
  rownames(bigmat) <- rn
  colnames(bigmat) <- paste("(", 1:n, ")", sep="")
  return(bigmat)
}
addaseparatingline <- function(tb, nextto, add = NULL, message = NULL, 
  UseMultiCol = F, HorPosition = "l")
#  Add a "message" line next to "nextto+add" row in a table tb.
#    nextto: regexp
#    add: Number of rows to be added to nextto row
#    UseMultiCol: T/F. Use multicolumn to place the message?
#    HorPosition: Horizontal orientation. Either {l, c, r}.
{
  numcol <- ncol(tb)
  inserthere <- grep(nextto, rownames(tb))
  # If inserting below/above nextto row
  if (!is.null(add)) inserthere <- inserthere + add
  tb <- as.matrix(tb)
  if (UseMultiCol) {
    tb <- rbind(tb[1:inserthere, , drop = F], 
        message = paste0("\\multicolumn{", numcol, "}{", HorPosition, "}{", message, "}"),
        tb[-(1:inserthere), , drop = F])
  } else {
    tb <- rbind(tb[1:inserthere, , drop = F], 
        message = rep("", numcol),
        tb[-(1:inserthere), , drop = F])
    rownames(tb)[grep("message", rownames(tb))] <- message
  }
  return(tb)
}

tabulate.est <- function(est, reorder = NULL, output.in.list = F,
  drop.dots = F, 
  lastLevelVariable = NULL, inter.with = NULL, 
  addbottom = NULL, subst.table = NULL)
# Tabulate est (list of estimated results) in a single table,
# and output in cbind(rn, table) or list(rn, table).
# Uses tabs2latex, reordertab, addaseparatingline functions.
#   reorder: ordering of variable names in regexp (to be used in reordertab)
#   output.in.list: if T, output is list(rn, table), if F, cbind(rn, table)
#   drop.dots: if T, drop "." from variable names
#   lastLevelVariable: regexp for the last level variable 
#     so I can insert a separating line using addaseparatingline
#   inter.with: name of interaction variables (main of main*cross)
#   addbottom: Lines (such as "n", "R2", etc.) to be added at the bottom
#   subst.table: variable name substitution table 
#    (see: c:/dropbox/data/ramadan/program/substitution_table.R)
# 
{
  tb <- tabs2latex(est)
  rn <- rownames(tb)
  if (drop.dots) rownames(tb) <- rn <- gsub("\\.", "", rn)
  #  order according to regexp order
  if (!is.null(reorder)) tb <- reordertab(tb, reorder)
  # set a separating line for interaction terms using addaseparatingline
  sepline.text <- paste0("\\hspace{-.1em}\\textit{\\footnotesize interaction with ", inter.with, "}")
  if (!is.null(lastLevelVariable)) 
    tb <- addaseparatingline(tb, lastLevelVariable, add = 1, message = sepline.text) else
    tb <- addaseparatingline(tb, "^any", add = 1, message = sepline.text)
  # further adding an empty line to separate from remaining rows
  tb <- addaseparatingline(tb, "interaction", message = "")
  if (!is.null(addbottom)) tb <- rbind(as.matrix(tb), addbottom)
  # Get rownames again after reordering/addition of rows
  rn <- rownames(tb)
  #  replace variable names, deleting std error names
  if (!is.null(subst.table)) {
    for (k in 1:nrow(subst.table)){
      if (any(grepl(subst.table[k, "org"], rn))) 
        rn[grep(subst.table[k, "org"], rn)] <- subst.table[k, "changedto"]
    }
  }
  if (output.in.list) list(rn, tb) else return(cbind(rn, tb))
}
TabEstSEPvalCI <- function(tab, digits=4, 
  InFormat = "satt", OutFormat = "epc", Star = F, 
  XX.YYY = F, Pdigits = 1, CIdigits = 2, ciintinysize = F, 
  pparenthesis = NULL, Satterthwaitedigits = 2)
#  summary regression tables ==> estimation table fit for html
#     "epc"       "esc"      "ep"         "satt"     "esDoF"  "es"     "espc"
#      est         est        est          est         est     est      est
#   (p value) or  (se)   or (p value) or (p value) or (se)  or (se) or  (se)
#    [lb, ub]    [lb, ub]                 [dof]       [dof]           ((p value))
#                                                                     [lb, ub]
#  tab: a 5-column table of estimates, SE, p, lb, ub, or,
#      a 4-column table of estimates, SE, t/dof, p
#  InFormat: "LZ" (est, se, [t], p), "satt" (est, se, [t], dof, p), 
#    "wcb" (est, se, [t], p, lb, ub), "sattCI" (est, se, [t], dof, p, lb, ub)
#  OutFormat = "epc", or other formats in the above
#  Star: if T, add * to estimates
#  XX.YYY: If T, display in 12.345 (%) format, if F, .123 format.
#  Pdigits: digits fo p values in XX.YYY format.
#  CIdigits: YY digits fo CI in XX.YY format.
#  ciintinysize: Set font size of CI to tiny.
# 
#  Satterthwaitedigits: digits for [degree of freedom] in XX.YY format
{
  ## if digits = 0, above formatC rounds pval to 0 or 1, so sub back
  ii <- sign(tab[, 1]) >= 0
  # satt: est, se, dof, p => est, se, p, dof
  if (grepl("satt", InFormat)) tab <- tab[, c(1:2, 4, 3)]
  cntab <- colnames(tab)
  # Magnify p value
  if (XX.YYY) 
    MagFactor <- 100 else
    MagFactor <- 1
  tab[, 3] <- tab[, 3] * MagFactor
  tab00 = copy(tab)
  if (digits == Pdigits & Satterthwaitedigits != digits) {
    tab <- round(tab00, digits) 
    tab0 <- formatC(tab00, digits = digits, format = "f")
  } else  {
    # estimates, SE, pvalue, [lb, ub] or [DoF]
    tab <- round(tab00, digits) 
    tab0 <- formatC(tab00, digits = digits, format = "f")
    # pvalue digits
    tab[, 3] <- round(tab00[, 3], Pdigits) 
    tab0[, 3] <- formatC(tab00[, 3], digits = Pdigits, format = "f")
  }
  if (grepl("satt", InFormat)) {
    # DoF digits
    tab[, 4] <- round(tab00[, 4], Satterthwaitedigits) 
    tab0[, 4] <-  formatC(tab00[, 4], digits = Satterthwaitedigits, format = "f")
  }
  # construct CI/DoF column
  # Column order: Estimate, Std. Error, P(>|t|), CI/DoF
  if (grepl("satt$|esDoF", OutFormat)) {
    tab <- cbind(tab[, -4], paste0("[", tab[, 4], "]")) 
    tab0 <- cbind(tab0[, -4], paste0("[", tab0[, 4], "]"))
    colnames(tab) <- colnames(tab0) <- c(cntab[1:3], "DoF")
  } else if (grepl("epc|esc", OutFormat)) {
  # If with CI
    # If setting font size to tiny
    if (ciintinysize) {
      tab <- cbind(tab[, -(4:5)], paste0("\\mbox{\\tiny [", round(tab[, 4], CIdigits), ", ", round(tab[, 5],, CIdigits),  "]}"))
      tab0 <- cbind(tab0[, -(4:5)], 
        paste0("\\mbox{\\tiny [", 
        formatC(as.numeric(tab0[, 4]), digits = CIdigits, format = "f"), 
        ", ", 
        formatC(as.numeric(tab0[, 5]), digits = CIdigits, format = "f"), 
        "]}"))
    } else { 
      tab <- cbind(tab[, -(4:5)], paste0("[", round(tab[, 4], CIdigits), ", ", round(tab[, 5],, CIdigits),  "]"))
      tab0 <- cbind(tab0[, -(4:5)], 
        paste0("[", 
        formatC(as.numeric(tab0[, 4]), digits = CIdigits, format = "f"), 
        ", ", 
        formatC(as.numeric(tab0[, 5]), digits = CIdigits, format = "f"), 
        "]"))
    }
    colnames(tab) <- c(cntab[1:3], "CI")
  } else if (grepl("^e[ps]$", OutFormat)) {
    if (ncol(tab) == 4) {
      tab <- tab[, -4]
      tab0 <- tab0[, -4]
      colnames(tab) <- cntab[-4]
    } # tabulation for LZ (est, se, p) remains unchanged
  }
  # add minus phantom if positive
  tab0[ii, 1] <- paste0("\\phantom{-}", tab0[ii, 1])
  # add *
  if (Star) {
    pstars <- paste0("^{", pvalstar(as.numeric(tab00[, 3]/MagFactor)), "}")
    #if (grepl("^es$", OutFormat))
    #  pstars <- paste0("^{", pvalstar(as.numeric(tabforp[, 4])/MagFactor), "}")
    eststars <- paste(tab0[, 1], pstars, sep = "")
    # number of maximum stars
    maxs <- 2
    separenthesis <- paste0("(", tab0[, 2], ")^{\\phantom{", 
                                   paste(rep("*", maxs), collapse = ""), "}}")
    pvalparenthesis <- paste0("(", tab0[, 3], ")^{\\phantom{", 
                                   paste(rep("*", maxs), collapse = ""), "}}")
    if (grepl("satt|esDoF", OutFormat))
      # Satterthwaite needs to show degree of freedom correction with stars
      DoFparenthesis <- paste0(tab0[, 4], "^{\\phantom{", 
         paste(rep("*", maxs), collapse = ""), "}}") else if (grepl("epc", OutFormat)) 
      DoFparenthesis <- tab0[, 4, drop = F] else
      DoFparenthesis <- NULL
    tab0 <- cbind(eststars, separenthesis, pvalparenthesis, DoFparenthesis)
  } else tab0[, 2:3] <- paste0("(", tab0[, 2:3], ")")
  if (grepl("epc|satt|^ep$", OutFormat)) tab1 <- tab0[, -2] else 
  if (grepl("esc|^es$|esDoF", OutFormat)) tab1 <- tab0[, -3]
  tab2 <- matrix(c(t(tab1)))
  rn <- rownames(tab)
  if (grepl("epc", OutFormat)) 
    rn2 <- c(t(cbind(rn, paste("$p_{", rn, "}$", sep = ""), paste("$CI_{", rn, "}$", sep = "")))) else
  if (grepl("esc", OutFormat)) 
    rn2 <- c(t(cbind(rn, paste("$se_{", rn, "}$", sep = ""), paste("$CI_{", rn, "}$", sep = "")))) else
  if (grepl("satt", OutFormat)) 
    rn2 <- c(t(cbind(rn, paste("$p_{", rn, "}$", sep = ""), paste("$DoF_{", rn, "}$", sep = "")))) else
  if (grepl("esDoF", OutFormat)) 
    rn2 <- c(t(cbind(rn, paste("$se_{", rn, "}$", sep = ""), paste("$DoF_{", rn, "}$", sep = "")))) else
  if (grepl("e[ps]$", OutFormat))
    rn2 <- c(t(cbind(rn, paste("$p_{", rn, "}$", sep = ""))))
  rownames(tab2) <- rn2
  return(data.frame(tab2))
}
TabEstSEPvalCIHTMLOld <- function(tab, digits=4, 
  InFormat = "satt", OutFormat = "epc", Star = F, 
  XX.YYY = F, Pdigits = 1, CIdigits = 2, ciintinysize = F, 
  Satterthwaitedigits = 2)
#  summary regression tables ==> estimation table fit for html
#      "epc"              "esc"            "ep"           "satt"       "esDoF"  "es"
#       est                est              est              est           est        est
#    (p value)   or       (se)      or   (p value)  or (p value)  (se)  or  (se)
#    [lb, ub]            [lb, ub]                          [dof]        [dof]
#  tab: a 5-column data.frame of  estimates, SE, p, lb, ub, or,
#      a 4-column data.frame of estimates, SE, t/dof, p
#  InFormat: "LZ" (est, se, [t], p), "satt" (est, se, [t], dof, p), 
#    "wcb" (est, se, [t], p, lb, ub), "sattCI" (est, se, [t], dof, p, lb, ub)
#  OutFormat = "epc", or other formats in the above
#  Star: if T, add * to estimates
#  XX.YYY: If T, display in 12.345 (%) format, if F, .123 format.
#  Pdigits: digits fo p values in XX.YYY format.
#  CIdigits: YY digits fo CI in XX.YY format.
#  ciintinysize: Set font size of CI to tiny.
#  Satterthwaitedigits: digits for [degree of freedom] in XX.YY format
#  RN: Can provide a vector of rownames(tab)
{
  ## if digits = 0, above formatC rounds pval to 0 or 1, so sub back
  ii <- sign(tab[, 1]) >= 0
  # satt: est, se, dof, p => est, se, p, dof
  if (grepl("satt$", InFormat)) tab <- tab[, c(1:2, 4, 3)]
  # sattCI: est, se, dof, p => est, se, p, dof
  if (grepl("sattCI$", InFormat)) tab <- tab[, c(1:2, 5, 3:4)]
  cntab <- colnames(tab)
  # Magnify p value
  if (XX.YYY) 
    MagFactor <- 100 else
    MagFactor <- 1
  tab[, 3] <- tab[, 3] * MagFactor
  tab00 = as.matrix(copy(tab))
  if (digits == Pdigits & Satterthwaitedigits != digits) {
    tab <- round(tab00, digits) 
    tab0 <- formatC(tab00, digits = digits, format = "f")
  } else  {
    # estimates, SE, pvalue, [lb, ub] or [DoF]
    tab <- round(tab00, digits) 
    tab0 <- formatC(tab00, digits = digits, format = "f")
    # pvalue digits
    tab[, 3] <- round(tab00[, 3], Pdigits) 
    tab0[, 3] <- formatC(tab00[, 3], digits = Pdigits, format = "f")
  }
  if (grepl("satt$", InFormat)) {
    # DoF digits
    tab[, 4] <- round(tab00[, 4], Satterthwaitedigits) 
    tab0[, 4] <-  formatC(tab00[, 4], digits = Satterthwaitedigits, format = "f")
  }
  # construct CI/DoF column
  # Column order: Estimate, Std. Error, P(>|t|), CI/DoF
  if (grepl("satt$|esDoF", OutFormat)) {
    tab <- cbind(tab[, -4], paste0("[", tab[, 4], "]")) 
    tab0 <- cbind(tab0[, -4], paste0("[", tab0[, 4], "]"))
    colnames(tab) <- colnames(tab0) <- c(cntab[1:3], "DoF")
  } else if (grepl("epc|esc", OutFormat)) {
  # If with CI
    # If setting font size to tiny
    if (ciintinysize) {
      tab <- cbind(tab[, -(4:5)], paste0("\\mbox{\\tiny [", round(tab[, 4], CIdigits), ", ", round(tab[, 5], CIdigits),  "]}"))
      tab0 <- cbind(tab0[, -(4:5)], 
        paste0("\\mbox{\\tiny [", 
        formatC(as.numeric(tab0[, 4]), digits = CIdigits, format = "f"), 
        ", ", 
        formatC(as.numeric(tab0[, 5]), digits = CIdigits, format = "f"), 
        "]}"))
    } else { 
      tab <- cbind(tab[, -(4:5)], paste0("[", round(tab[, 4], CIdigits), ", ", round(tab[, 5], CIdigits),  "]"))
      tab0 <- cbind(tab0[, -(4:5)], 
        paste0("[", 
        formatC(as.numeric(tab0[, 4]), digits = CIdigits, format = "f"), 
        ", ", 
        formatC(as.numeric(tab0[, 5]), digits = CIdigits, format = "f"), 
        "]"))
    }
    colnames(tab) <- c(cntab[1:3], "CI")
  } else if (grepl("^e[ps]$", OutFormat)) {
    if (ncol(tab) == 4) {
      tab <- tab[, -4]
      tab0 <- tab0[, -4]
      colnames(tab) <- cntab[-4]
    } # tabulation for LZ (est, se, p) remains unchanged
  }
  # add minus phantom if positive
  #tab0[ii, 1] <- paste0("\\phantom{-}", tab0[ii, 1])
  # add *
  if (Star) {
    pstars <- paste0("^{", pvalstar(as.numeric(tab00[, 3]/MagFactor)), "}")
    #if (grepl("^es$", OutFormat))
    #  pstars <- paste0("^{", pvalstar(as.numeric(tabforp[, 4])/MagFactor), "}")
    eststars <- paste(tab0[, 1], pstars, sep = "")
    # number of maximum stars
    maxs <- 2
    separenthesis <- paste0("(", tab0[, 2], ")^{\\phantom{", 
                                   paste(rep("*", maxs), collapse = ""), "}}")
    pvalparenthesis <- paste0("(", tab0[, 3], ")^{\\phantom{", 
                                   paste(rep("*", maxs), collapse = ""), "}}")
    if (grepl("satt|esDoF", OutFormat))
      # Satterthwaite needs to show degree of freedom correction with stars
      DoFparenthesis <- paste0(tab0[, 4], "^{\\phantom{", 
         paste(rep("*", maxs), collapse = ""), "}}") else if (grepl("epc", OutFormat)) 
      DoFparenthesis <- tab0[, 4, drop = F] else
      DoFparenthesis <- NULL
    tab0 <- cbind(eststars, separenthesis, pvalparenthesis, DoFparenthesis)
  } else tab0[, 2:3] <- paste0("(", tab0[, 2:3], ")")
  if (grepl("epc|satt|^ep$", OutFormat)) tab1 <- tab0[, -2] else 
  if (grepl("esc|^es$|esDoF", OutFormat)) tab1 <- tab0[, -3]
  tab2 <- matrix(c(t(tab1)))
  rn <- rownames(tab00) 
  if (grepl("epc", OutFormat)) 
    rn2 <- c(t(cbind(rn, paste("$p_{", rn, "}$", sep = ""), paste("$CI_{", rn, "}$", sep = "")))) else
  if (grepl("esc", OutFormat)) 
    rn2 <- c(t(cbind(rn, paste("$se_{", rn, "}$", sep = ""), paste("$CI_{", rn, "}$", sep = "")))) else
  if (grepl("satt", OutFormat)) 
    rn2 <- c(t(cbind(rn, paste("$p_{", rn, "}$", sep = ""), paste("$DoF_{", rn, "}$", sep = "")))) else
  if (grepl("esDoF", OutFormat)) 
    rn2 <- c(t(cbind(rn, paste("$se_{", rn, "}$", sep = ""), paste("$DoF_{", rn, "}$", sep = "")))) else
  if (grepl("e[ps]$", OutFormat))
    rn2 <- c(t(cbind(rn, paste("$p_{", rn, "}$", sep = ""))))
  rownames(tab2) <- rn2
  return(data.frame(tab2))
}

TabEstSEPvalCIHTML <- function(tab, digits=4, 
  InFormat = "satt", OutFormat = "epc", Star = F, 
  XX.YYY = F, Pdigits = 1, CIdigits = 2, ciintinysize = F, 
  pparenthesis = NULL, Satterthwaitedigits = 2)
#  summary regression tables ==> estimation table fit for html
#     "epc"       "esc"      "ep"         "satt"     "esDoF"  "es"     "espc"
#      est         est        est          est         est     est      est
#   (p value) or  (se)   or (p value) or (p value) or (se)  or (se) or  (se)
#    [lb, ub]    [lb, ub]                 [dof]       [dof]           ((p value))
#                                                                     [lb, ub]
#  tab: a 5-column data.frame of  estimates, SE, p, lb, ub, or,
#      a 4-column data.frame of estimates, SE, t/dof, p
#  InFormat: "LZ" (est, se, [t], p), "satt" (est, se, [t], dof, p), 
#    "wcb" (est, se, [t], p, lb, ub), "sattCI" (est, se, [t], dof, p, lb, ub)
#  OutFormat = "epc", or other formats in the above
#  Star: if T, add * to estimates
#  XX.YYY: If T, display in 12.345 (%) format, if F, .123 format.
#  Pdigits: digits fo p values in XX.YYY format.
#  CIdigits: YY digits fo CI in XX.YY format.
#  ciintinysize: Set font size of CI to tiny.
#  pparenthesis: In espc, Double=((p)), brace={p}, angled=<p>, NULL=(p)
#  Satterthwaitedigits: digits for [degree of freedom] in XX.YY format
#  RN: Can provide a vector of rownames(tab)
{
  ## if digits = 0, above formatC rounds pval to 0 or 1, so sub back
  ii <- sign(tab[, 1]) >= 0
  # satt: est, se, dof, p => est, se, p, dof
  if (grepl("satt$", InFormat)) tab <- tab[, c(1:2, 4, 3)]
  # sattCI: est, se, CI_L, CI_U, p => est, se, p, ci_l, ci_u
  if (grepl("sattCI$", InFormat)) tab <- tab[, c(1:2, 5, 3:4)]
  cntab <- colnames(tab)
  # Magnify p value
  if (XX.YYY) 
    MagFactor <- 100 else
    MagFactor <- 1
  tab[, 3] <- tab[, 3] * MagFactor
  tab00 = as.matrix(copy(tab))
  if (digits == Pdigits & Satterthwaitedigits != digits) {
    tab <- round(tab00, digits) 
    tab0 <- formatC(tab00, digits = digits, format = "f")
  } else  {
    # estimates, SE, pvalue, [lb, ub] or [DoF]
    tab <- round(tab00, digits) 
    tab0 <- formatC(tab00, digits = digits, format = "f")
    # For pvalue digits, use Pdigits 
    tab[, 3] <- round(tab00[, 3], Pdigits) 
    tab0[, 3] <- formatC(tab00[, 3], digits = Pdigits, format = "f")
  }
  if (grepl("satt$", InFormat)) {
    # DoF digits
    tab[, 4] <- round(tab00[, 4], Satterthwaitedigits) 
    tab0[, 4] <-  formatC(tab00[, 4], digits = Satterthwaitedigits, format = "f")
  }
  # construct CI/DoF column
  # Column order: Estimate, Std. Error, P(>|t|), CI/DoF
  if (grepl("satt$|esDoF", OutFormat)) {
    tab <- cbind(tab[, -4], paste0("[", tab[, 4], "]")) 
    tab0 <- cbind(tab0[, -4], paste0("[", tab0[, 4], "]"))
    colnames(tab) <- colnames(tab0) <- c(cntab[1:3], "DoF")
  } else if (grepl("epc|esc|espc", OutFormat)) {
  # If with CI
    # If setting font size to tiny
    if (ciintinysize) {
      tab <- cbind(tab[, -(4:5)], paste0("\\mbox{\\tiny [", round(tab[, 4], CIdigits), ", ", round(tab[, 5], CIdigits),  "]}"))
      tab0 <- cbind(tab0[, -(4:5)], 
        paste0("\\mbox{\\tiny [", 
        formatC(as.numeric(tab0[, 4]), digits = CIdigits, format = "f"), 
        ", ", 
        formatC(as.numeric(tab0[, 5]), digits = CIdigits, format = "f"), 
        "]}"))
    } else { 
      tab <- cbind(tab[, -(4:5)], paste0("[", round(tab[, 4], CIdigits), ", ", round(tab[, 5], CIdigits),  "]"))
      tab0 <- cbind(tab0[, -(4:5)], 
        paste0("[", 
        formatC(as.numeric(tab0[, 4]), digits = CIdigits, format = "f"), 
        ", ", 
        formatC(as.numeric(tab0[, 5]), digits = CIdigits, format = "f"), 
        "]"))
    }
    colnames(tab) <- c(cntab[1:3], "CI")
  } else if (grepl("^e[ps]$", OutFormat)) {
    if (ncol(tab) == 4) {
      tab <- tab[, -4]
      tab0 <- tab0[, -4]
      colnames(tab) <- cntab[-4]
    } # tabulation for LZ (est, se, p) remains unchanged
  }
  # add minus phantom if positive
  #tab0[ii, 1] <- paste0("\\phantom{-}", tab0[ii, 1])
  # add *
  if (Star) {
    pstars <- paste0("^{", pvalstar(as.numeric(tab00[, 3]/MagFactor)), "}")
    #if (grepl("^es$", OutFormat))
    #  pstars <- paste0("^{", pvalstar(as.numeric(tabforp[, 4])/MagFactor), "}")
    eststars <- paste(tab0[, 1], pstars, sep = "")
    # number of maximum stars
    maxs <- 2
    separenthesis <- paste0("(", tab0[, 2], ")^{\\phantom{", 
                                   paste(rep("*", maxs), collapse = ""), "}}")
    if (grepl("^D$|oub", pparenthesis)) 
      pparen <- c("((", "))") else
    if (grepl(".race", pparenthesis)) 
      pparen <- c("{", "}") else
    if (grepl("angle", pparenthesis)) 
      pparen <- c("<", ">") else
      pparen <- c("(", ")")
    pvalparenthesis <- paste0(pparen[1], tab0[, 3], pparen[2], "^{\\phantom{", 
                                   paste(rep("*", maxs), collapse = ""), "}}")
    if (grepl("satt|esDoF", OutFormat))
      # Satterthwaite needs to show degree of freedom correction with stars
      DoFparenthesis <- paste0(tab0[, 4], "^{\\phantom{", 
         paste(rep("*", maxs), collapse = ""), "}}") else 
    if (grepl("pc", OutFormat)) 
      DoFparenthesis <- tab0[, 4, drop = F] else
      DoFparenthesis <- NULL
    tab0 <- cbind(eststars, separenthesis, pvalparenthesis, DoFparenthesis)
  } else tab0[, 2:3] <- paste0("(", tab0[, 2:3], ")")
  if (grepl("epc|satt|^ep$", OutFormat)) 
    tab1 <- tab0[, -2] else 
  if (grepl("esc|^es$|esDoF", OutFormat)) 
    tab1 <- tab0[, -3] else
  if (grepl("espc", OutFormat)) 
    tab1 <- tab0
  tab2 <- matrix(c(t(tab1)))
  rn <- rownames(tab00) 
  if (grepl("epc", OutFormat)) 
    rn2 <- c(t(cbind(rn, paste("p$_{", rn, "}$", sep = ""), paste("CI$_{", rn, "}$", sep = "")))) else
  if (grepl("esc", OutFormat)) 
    rn2 <- c(t(cbind(rn, paste("se$_{", rn, "}$", sep = ""), paste("CI$_{", rn, "}$", sep = "")))) else
  if (grepl("espc", OutFormat)) 
    rn2 <- c(t(cbind(rn, paste("se$_{", rn, "}$", sep = ""), paste("p$_{", rn, "}$", sep = ""), paste("CI$_{", rn, "}$", sep = "")))) else
  if (grepl("satt", OutFormat)) 
    rn2 <- c(t(cbind(rn, paste("p$_{", rn, "}$", sep = ""), paste("DoF$_{", rn, "}$", sep = "")))) else
  if (grepl("esDoF", OutFormat)) 
    rn2 <- c(t(cbind(rn, paste("se$_{", rn, "}$", sep = ""), paste("DoF$_{", rn, "}$", sep = "")))) else
  if (grepl("e[ps]$", OutFormat))
    rn2 <- c(t(cbind(rn, paste("p$_{", rn, "}$", sep = ""))))
  rownames(tab2) <- rn2
  return(data.frame(tab2))
}


tabs2latex4 <- function(est, digits = 3, roworder = NULL, 
  suppress.message = T, informat = "satt",
  outformat = "epc", AddStarIntabstarP = F, 
  UseHTML = F,
  xx.yyy = F, pdigits = 1, CIDigits = digits, CIIntinysize = F,
  Pparenthesis = NULL, SatterthwaiteDigits = digits,
  startingnumber = 1, ...)
#  Produces estimated results table of columns
#     "epc"       "esc"      "ep"         "satt"     "esDoF"  "es"     "espc"
#      est         est        est          est         est     est      est
#   (p value) or  (se)   or (p value) or (p value) or (se)  or (se) or  (se)
#    [lb, ub]    [lb, ub]                 [dof]       [dof]           ((p value))
#                                                                     [lb, ub]
#  from est, a list of estimated tables with CI or Satterthwaite degree of freedom correction.
#  Each est[[i]] is a table of 
#    estimate, std error, p value, lb, ub Or
#    estimate, std error, p value, DoF Or
#    estimate, std error, p value
#  E.g., est <- list(e1=est1, e2=est2, e3=est3, e4=est4)
#  roworder: A regexp vector. If given, reorder rows. It will take all matches except se rows. 
#  suppress.message: If F, message("Length given by roworder is not the same as length of rn. 
#     Use it anyway.") 
#  informat: "satt" (est, se, [t], dof, p), "sattCI" (est, se, [t], dof, lb, ub), 
#    "wcb" (est, se, [t], lb, ub, p), "LZ" (est, se, [t], p)
#  outformat: "epc" (default), or other formats in the above
#  AddStarIntabstarP: If T, add stars to estimates when outformat = "epc" or "ep". Defaults to F.
#    Uses TabEstSEPvalCI function.
#  UseHTML: If T, exports in a html like format. Uses TabEstSEPvalCIHTML function.
#  xx.yyy: If T, display in 12.345 (%) format, if F, .123 format.
#  pdigits: digits for p values in xx.yyy format
#  CIDigits: under decimal digits for [lb, ub] in xx.yyy
#  CIIntinysize: If T, set CI's font size to tiny.
#  Pparenthesis: Double use ((pval))
#  SatterthwaiteDigits: under decimal digits for [degree of freedom]
#  startingnumber: specify the column number to start, (5), (6), ..., not (1), (2), ...
{
  n <- length(est); rn <- rownames(est[[n]])
  for (i in 2:n) rn <- union(rn, rownames(est[[i]]))
  if (grepl("epc", outformat)) 
    rn <- c(t(cbind(rn, paste0("p$_{", rn, "}$"), paste0("CI$_{", rn, "}$")))) else 
  if (grepl("esc", outformat))
    rn <- c(t(cbind(rn, paste0("se$_{", rn, "}$"), paste0("CI$_{", rn, "}$")))) else
  if (grepl("espc", outformat)) 
    rn <- c(t(cbind(rn, paste("se$_{", rn, "}$", sep = ""), paste("p$_{", rn, "}$", sep = ""), paste("CI$_{", rn, "}$", sep = "")))) else
  if (grepl("satt", outformat))
    rn <- c(t(cbind(rn, paste0("p$_{", rn, "}$"), paste0("DoF$_{", rn, "}$")))) else
  if (grepl("esDoF", outformat))
    rn <- c(t(cbind(rn, paste0("se$_{", rn, "}$"), paste0("DoF$_{", rn, "}$")))) else
  if (grepl("ep$", outformat))
    rn <- c(t(cbind(rn, paste0("p$_{", rn, "}$"))))
  if (grepl("es$", outformat))
    rn <- c(t(cbind(rn, paste0("se$_{", rn, "}$"))))
  if (!is.null(roworder)) {
    ro <- as.list(unlist(strsplit(roworder, "\\|")))
    ro <- unlist(lapply(ro, grep, rn))
    # drop anything with se$_{.}$ or p$_{.}$ or DoF
    ro <- ro[!(ro %in% grep("^se\\$|^p\\$|DoF", rn))]
    if (length(ro) != length(rn)) {
      if (!suppress.message) 
        message("Length given by roworder is not the same as length of rn. Use it anyway.") 
      if (grepl("satt|^..c$", outformat))
      # satt has est, pval, DoF
      #  est.x1, pval.x1, DoF.x1, ... for all ordered covariates 
      #  and others in rn[-c(rbind(ro, ro+1, ro+2))]
      # epc/esc has est, pval, [CI.lb, CI.ub]
      #  est.x1, pval/se.x1, [lb.x1, ub.x1], ... for all ordered covariates 
      # espc has est, se, pval, [CI.lb, CI.ub]
      #  est.x1, se.x1, pval/se.x1, [lb.x1, ub.x1], ... for all ordered covariates 
      #  and others in rn[-c(rbind(ro, ro+1, ro+2))]
        rn <- c(rn[c(rbind(ro, ro+1, ro+2))], rn[-c(rbind(ro, ro+1, ro+2))]) else 
      if (grepl("espc", outformat))
        rn <- c(rn[c(rbind(ro, ro+1, ro+2, ro+3))], rn[-c(rbind(ro, ro+1, ro+2, ro+3))]) else
      # ep|es has est, pval|se
        rn <- c(rn[c(rbind(ro, ro+1))], rn[-c(rbind(ro, ro+1))])
    } else rn <- rn[c(rbind(ro, ro+1))]
  }
  bigmat <- array("",  dim=c(length(rn), n))
  for (i in 1:n)
  {
    if (grepl("wcb|sattCI", informat))
      tab <- matrix(as.numeric(as.matrix(est[[i]])), byrow=F, ncol=5) else
    if (grepl("satt$", informat))
      tab <- matrix(as.numeric(as.matrix(est[[i]])), byrow=F, ncol=4) else
    if (grepl("LZ", informat))
      tab <- matrix(as.numeric(as.matrix(est[[i]])), byrow=F, ncol=3)
    dimnames(tab) <- dimnames(est[[i]])
    if (!UseHTML) {
      if (grepl("pc|ep$", outformat))
        tab <- TabEstSEPvalCI(tab, digits = digits, Star = AddStarIntabstarP, 
          XX.YYY = xx.yyy, OutFormat = outformat, InFormat = informat, 
          Pdigits = pdigits, ciintinysize = CIIntinysize, 
          pparenthesis = Pparenthesis, CIdigits = CIDigits) else 
        tab <- TabEstSEPvalCI(tab, digits = digits, Star = AddStarIntabstarP,
          OutFormat = outformat, InFormat = informat, Satterthwaitedigits = 2, 
          ciintinysize = CIIntinysize, pparenthesis = Pparenthesis, 
          CIdigits = CIDigits)
    } else {
      if (grepl("pc|ep$", outformat))
        tab <- TabEstSEPvalCIHTML(tab, digits = digits, Star = AddStarIntabstarP, 
          XX.YYY = xx.yyy, OutFormat = outformat, InFormat = informat, 
          Pdigits = pdigits, ciintinysize = CIIntinysize, 
          pparenthesis = Pparenthesis, CIdigits = CIDigits) else 
        tab <- TabEstSEPvalCIHTML(tab, digits = digits, Star = AddStarIntabstarP,
          OutFormat = outformat, InFormat = informat, Satterthwaitedigits = 2, 
          ciintinysize = CIIntinysize, pparenthesis = Pparenthesis, 
          CIdigits = CIDigits)
    }
    if (grepl("epc|esc|sat|esDoF", outformat)) {
      this.rn <- rownames(tab)[seq(1, nrow(tab), 3)]
      ii <- unlist(lapply(1:length(this.rn), function(i) which(this.rn[i]==rn)))
      bigmat[ii  , i] <- as.character(tab[seq(1, nrow(tab)-1, 3), 1])
      bigmat[ii+1, i] <- as.character(tab[seq(2, nrow(tab)-1, 3), 1])
      bigmat[ii+2, i] <- as.character(tab[seq(3, nrow(tab)  , 3), 1])
    } else 
    if (grepl("espc", outformat)) {
      this.rn <- rownames(tab)[seq(1, nrow(tab), 4)]
      ii <- unlist(lapply(1:length(this.rn), function(i) which(this.rn[i]==rn)))
      bigmat[ii  , i] <- as.character(tab[seq(1, nrow(tab)-1, 4), 1])
      bigmat[ii+1, i] <- as.character(tab[seq(2, nrow(tab)-1, 4), 1])
      bigmat[ii+2, i] <- as.character(tab[seq(3, nrow(tab)  , 4), 1])
      bigmat[ii+3, i] <- as.character(tab[seq(4, nrow(tab)  , 4), 1])
    } else {
      this.rn <- rownames(tab)[seq(1, nrow(tab), 2)]
      ii <- unlist(lapply(1:length(this.rn), function(i) which(this.rn[i]==rn)))
      bigmat[ii  , i] <- as.character(tab[seq(1, nrow(tab)-1, 2), 1]) # estimate
      bigmat[ii+1, i] <- as.character(tab[seq(2, nrow(tab)  , 2), 1]) # p value
    }
  }
  bigmat <- data.frame(bigmat)
  rownames(bigmat) <- rn
  colnames(bigmat) <- paste("(", startingnumber - 1 + (1:n), ")", sep="")
  return(bigmat)
}
Tab.Est <- function(est, reorder = NULL, output.in.list = F,
  drop.dots = F, 
  Informat = "satt",
  Outformat = "epc", # Also, "esc" or "espc" or "ep" or "satt" are allowed
  AddStars = F,
  CIInTinySize = F, 
  PParenthesis = NULL, 
  lastLevelVariable = NULL, inter.with = NULL, # keep these for backward compatibility
  LastLineVariables = NULL,  # use these for a multiple part table, inter with A, inter with B
  InterWithTexts = NULL,      # A and B in "int with A", "int with B", ...
  DeleteRowStrings = NULL,
  AddTopStripSpace = F, # Add space between (1), (2), ... line and 1st row e.g., (Intercept)
  addbottom = NULL, subst.table = NULL)
# Outformat: est (list of estimated results) in a single table, in a format of
#     "epc"       "esc"      "ep"         "satt"     "esDoF"  "es"     "espc"
#      est         est        est          est         est     est      est
#   (p value) or  (se)   or (p value) or (p value) or (se)  or (se) or  (se)
#    [lb, ub]    [lb, ub]                 [dof]       [dof]           ((p value))
#                                                                     [lb, ub]
# and output in cbind(rn, table) or list(rn, table). est is in the following format
#   estimate, std error, p value, lb, ub Or
#   estimate, std error, p value, DoF Or
#   estimate, std error, p value Or
#   estimate, std error, Or
#   estimate, p value
# Uses reordertab, tabs2latex4 (uses TabEstSEPvalCI) function stored in functions.R
# Uses addaseparatingline function stored in path_function_rnw.R
#   reorder: ordering of variable names in regexp (to be used in reordertab)
#   output.in.list: if T, output is list(rn, table), if F, cbind(rn, table)
#   drop.dots: if T, drop "." from variable names
#   Outformat: epc, esc, ep, satt
#   Informat: satt, sattCI, wcb, LZ
#   AddStars: If T, add stars of statistical significance
#   CIInTinySize: If T, put CIs in tiny size irrespective of common font size setting.
#   PParenthesis: Double use ((pval))
#   lastLevelVariable: Obsolete. regexp for the last level variable 
#     so I can insert a separating line using addaseparatingline
#   inter.with: Obsolete. name of interaction variables (main of main*cross)
#   LastLineVariables: regexp for the last variable names of part A, B, C, ...
#     This is applied after having reordered the rows with "reorder" option.
#   InterWithTexts: A, B, ... in "interaction with A", "interaction with B", ...
#   DeleteRowStrings: A vector of regexp of rownames to be dropped. 
#     E.g.,  "^p\\$", "^CI\\$", ...
#   AddTopStripSpace: Add space ["AddTopStripSpace"] after change line "\\" 
#     of TopStrip [(1), (2), ...] row.
#   addbottom: Lines (such as "n", "R2", etc.) to be added at the bottom
#   subst.table: variable name substitution table 
#    (see: c:/data/ramadan/program/program/substitution_table.R)
# 
{
  # XX.YYY: If T, display in 12.345 (%) format, if F, .123 format.
  # Uses tabs2latex4 (using TabEstSEPvalCI) function stored in functions.R
  # source("c:/seiro/settings/Rsetting/functions.R")
  tb <- tabs2latex4(est, outformat = Outformat, informat = Informat, xx.yyy = T, 
    AddStarIntabstarP = AddStars, Pparenthesis = PParenthesis,
    CIIntinysize = CIInTinySize) # default: digits = 3, CIDigits = digits
  rn <- rownames(tb)
  if (drop.dots) rownames(tb) <- rn <- gsub("\\.", "", rn)
  #  order according to regexp order
  if (grepl("e[ps]$", Outformat)) AddNum <- 1 else AddNum <- 2
  if (!is.null(reorder)) 
    tb <- reordertab(tb, reorder, deletethese = DeleteRowStrings, RowsPerCovariate = AddNum+1)
  if (is.null(LastLineVariables)) {
    # === Backward compatibility begins ===
    # set a separating line for interaction terms using addaseparatingline
    sepline.text <- paste0("\\hspace{-.1em}\\textit{\\footnotesize Interaction with ", inter.with, "}")
    if (!is.null(lastLevelVariable))
      tb <- addaseparatingline(tb, lastLevelVariable, add = 1, message = sepline.text) else
      tb <- addaseparatingline(tb, "^any", add = 1, message = sepline.text)
    # further adding an empty line to separate from remaining rows
    tb <- addaseparatingline(tb, "interaction", message = "")
    # === Backward compatibility ends ===
  } else {
    if (length(LastLineVariables) != length(InterWithTexts)) 
      stop("Length of LastLineVariables and InterWithTexts differ. These must be same.")
    sepline.text.vec <- paste0("\\hspace{-.1em}\\textit{\\footnotesize interaction with ", 
        InterWithTexts, "}")
    for (m in 1:length(LastLineVariables)) {
      # insert a message line
      tb <- addaseparatingline(tb, LastLineVariables[m], add = AddNum, message = sepline.text.vec[m])
      # further adding an empty line to separate from remaining rows
      tb <- addaseparatingline(tb, LastLineVariables[m], add = AddNum, message = "")
      # if 3 lines for one covariate, add another line to make table look more balanced
      if (!grepl("ep$", Outformat)) 
        tb <- addaseparatingline(tb, LastLineVariables[m], add = 2, message = "")
    }
  }
  if (!is.null(addbottom)) tb <- rbind(as.matrix(tb), addbottom)
  # Get rownames again after reordering/addition of rows
  rn <- rownames(tb)
  #  replace variable names
  if (!is.null(subst.table)) {
    for (k in 1:nrow(subst.table)){
      if (any(grepl(subst.table[k, "org"], rn))) 
        rn[grep(subst.table[k, "org"], rn)] <- subst.table[k, "changedto"]
    }
  }
  # AddTopStripSpace: Add space between TopStripRow and "(Intercept)"
  if (!is.logical(AddTopStripSpace) & is.character(AddTopStripSpace)) {
    TopStripRowNum <- grep("\\(1\\).*\\(2\\)", tb)
    tb[TopStripRowNum, ] <- paste0(tb[TopStripRowNum, ], "[", AddTopStripSpace, "]")
  }
  # Delete std error/pval/CI/DoF rownames
  rn <- gsub("^..?.?\\$.*", "", rn)
  if (output.in.list) list(rn, tb) else return(cbind(rn, tb))
}
latextab <- function(tab, hleft = NULL, hcenter = NULL, unit = "cm", 
                               hright = NULL, hline = NULL, cline = F, 
                               addseparatingcols = NULL,
                               separatingcolwidth = NULL,
                               separatingcoltitle = NULL,
                               addsubcoltitlehere = T,
                               NoVariablenameColumn = F,
                               # When using addheaderAbove or addheaderBelow, they do not take addseparatingcols into consideration, need to correct
                               LastDiffVariable = NULL, 
                               SepLineText = "interaction with",
                               inter.with = NULL,
                               AdjustInterWith = NULL,
                               InterWithLength = ".5em",
                               adjustlineskip = NULL, adjlskiprows = 2,
                               delimiterline = NULL, nohorizontalline = T,
                               addheaderAbove = NULL, addheaderBelow = NULL,
                               addtopstripspace = F, 
                               headercolor = NULL, alternatecolor = NULL,
                               alternatecolor2 = NULL, alternatestart2 = 3, 
                               alternatecolor3 = NULL, alternatestart3 = 4, 
                               alternatecolor4 = NULL, alternatestart4 = 5, 
                               alternatecolorManual = NULL,
                               alternatecolorManualColor = NULL,
                               estimationspace = NULL, estimationspacelast = 0, 
                               altestspace = NULL,
                               yesnospace = "-.5ex")
#  produces a LaTeX table.
#    hleft = header left: >{ "here" }
#    hcenter = header center length: p{ "here" lengthunit}, need for 
#    unit = header center length unit: p{ length "here" }
#    hright = header right: <{ "here" }
#    hline = T: inserts "\hline" after each line
#             = numeric vector: inserts "\hline" at specified rows
#    cline = matrix(row, from, to) that gives "\cline{from-to}" at specified rows
#    addseparatingcols: insert thin column(s) specified by column number. 
#      E.g., c(2, 4) give:
#                       (1) (2) [HERE] (3) (4) [HERE] (5)
#    separatingcolwidth: Thin column widths in cm.
#    separatingcoltitle: Titles of separat*ed* columns.
#    addsubcoltitlehere: If T, add titles of separat*ed* columns. 
#    NoVariablenameColumn: if T, shift one column to right
#    LastDiffVariable: regexp for the last differenced variable (last row of panel A)
#      so I can insert a separating line "paste(SepLineText, inter.with)"
#    SepLineText: Default is "'interation with'".
#    inter.with: name of interaction variables (main of main*cross)
#    AdjustInterWith: number of rows to be added to LastDiffVariable. (1st row of panel B)
#      If I want to place separting row 1 below from deafault, set to 1.
#    InterWithLength: length to be shifted right, \hspace{HERE}, default is .5em
#    adjustlineskip: insert specified string to every second ""\\[here]""
#    adjlskiprows: If a numeric vector, ""\\[adjustlineskip]"" is added to adjlskiprows rows, 
#      not every second row. If a scalar of 2, adds after every second rows. If a scalar of 3, 
#      adds a vertical space every after second and third rows, so 
#      estimate, p value, confidence interval can be grouped together.
#      Default is 2. If adjustlineskip is NULL, nothing is added.
#    delimiterline: if NULL, "|" will be eliminated
#    nohorizontalline: if T, drop all horizontal lines
#    addheaderAbove: if not NULL, additional header row attached above the header 
#      If "^num" ("numeric"), add (1), (2), ... or c("str1", "str2", "str3", ...) is added to header
#    addheaderBelow: if not NULL, additional header row attached below the header 
#    addtopstripspace: Add space by ["addtopstripspace"] between 
#       TopStripRow [(1), (2), ... line] and "(Intercept)".
#    headercolor: if not NULL, specify color of the header row
#    alternatecolor: if not NULL, specify color of the alternating rows (1, 3, ...)
#    alternatecolor2: if not NULL, specify color of the alternating 2 rows (3:4, 7:8, ...)
#    alternatecolor3: if not NULL, specify color of the alternating 3 rows (4:6, 10:12, ...)
#    alternatecolor4: if not NULL, specify color of the alternating 4 rows (5:8, 13:16, ...)
#    alternatecolorManualColor: if not NULL, specify color of the alternating rows specified
#    alternatecolorManual: if not NULL, specify rows of the alternating colour
#      These are rows before SepLineText is inserted. Need to specify rows anticipating SepLineText.
#      If SepLineText is inserted after line 3 (=2nd+1), alternatecolorManual may be 
#        2, [3], [+3], 4, ...
#      If SepLineText is inserted after line 4 (=3rd+1), alternatecolorManual may be
#        2, [3], 4, [+4], [5], 6,..
#      If SepLineText is inserted after line k (=kth+1), alternatecolorManual may be 
#        3:4, [5:6], 7:8, ..., (k-1):k, [+(k+1)], [(k+1):(k+2)], (k+3):(k+4), ...
#        3:4, [5:6], 7:8, ..., (k-3):(k-2), [(k-1):k], [+(k+1)], (k+1):(k+2), ...
#    estimationspace: if not NULL, insert empty half line space in 3, 5, 7, ... rows
#    estimationspacelast: if not NULL, do not put empty space of last estimationspacelast rows
#    altestspace: if not NULL, specify color of the alternating 2+1 rows (4:5, 10:11, 16:17...)
#    yesnospace: if not NULL, insert ["here"] line space in rows only with "yes"/"no"
{
  tab0 = copy(tab)
  ResultNCol <- ncol(tab0)+length(addseparatingcols)
  if (is.null(hcenter)) hcenter <- rep(2, ResultNCol)
  if (length(hleft) == 1) 
    hleftForTab <- rep(hleft, ResultNCol) else
    hleftForTab <- c(hleft[1], rep(hleft[2], ResultNCol-1))
  if (length(hcenter) == 1) 
    hcenterForTab <- rep(hcenter, ResultNCol) else
    hcenterForTab <- c(hcenter[1], rep(hcenter[2], ResultNCol-1))
  if (length(hright) == 1) 
    hrightForTab <- rep(hright, ResultNCol) else
    hrightForTab <- c(hright[1], rep(hright[2], ResultNCol-1))
  if (!is.null(delimiterline)) dlm <- "|" else dlm <- ""
  if (nohorizontalline) holine <- "" else holine <- "\\hline"
  # column additions as separator columns
  if (!is.null(addseparatingcols)) {
    if (max(addseparatingcols) > ncol(tab)) 
      message("max(addseparatingcols) must be smaller than ncol(tab).")
    if (length(addseparatingcols) > length(separatingcolwidth))
      message("length(addseparatingcols) must be smaller than length(separatingcolwidth).")
    ## Shift table contents
    # add 1 to take the variable name column into account
    if (!NoVariablenameColumn) addseparatingcols <- addseparatingcols + 1
    # add spaces in a table
    tab <- tab0[, 1:addseparatingcols[1]]
    if (length(addseparatingcols) > 1) 
      for (i in 2:length(addseparatingcols))
         tab <- cbind(tab, "", tab0[, (addseparatingcols[i-1]+1):addseparatingcols[i]])
    tab <- cbind(tab, "", tab0[, (addseparatingcols[length(separatingcolwidth)]+1):ncol(tab0)])
    # add separating columns to hleft, hcenter, hright
    # do I need this if (!is.null(hleft)) loop?
    if (!is.null(hleft)) {
      if (any(grepl("\\$", hright))) space.hleft.char <- "$" else space.hleft.char <- ""
      # first block
      hleft0 <- hleftForTab[1:addseparatingcols[1]]
      hcenter0 <- hcenterForTab[1:addseparatingcols[1]]
      hright0 <- hrightForTab[1:addseparatingcols[1]]
      # second - (n-1) blocks
      if (length(addseparatingcols) > 1) {
        for (i in 2:length(addseparatingcols)) {
           hleft0 <- c(hleft0, space.hleft.char, 
              hleftForTab[(addseparatingcols[i-1]+1):addseparatingcols[i]])
           hcenter0 <- c(hcenter0, separatingcolwidth[i], 
              hcenterForTab[(addseparatingcols[i-1]+1):addseparatingcols[i]])
           hright0 <- c(hright0, space.hleft.char,
              hrightForTab[(addseparatingcols[i-1]+1):addseparatingcols[i]])
        }
      }
      # final block after the last separating column
      hleft0 <- c(hleft0, space.hleft.char, 
         hleftForTab[(addseparatingcols[length(separatingcolwidth)]+1):ncol(tab0)])
      hcenter0 <- c(hcenter0, separatingcolwidth[length(separatingcolwidth)], 
         hcenterForTab[(addseparatingcols[length(separatingcolwidth)]+1):ncol(tab0)])
      hright0 <- c(hright0, space.hleft.char, 
         hrightForTab[(addseparatingcols[length(separatingcolwidth)]+1):ncol(tab0)])
      hleft <- hleft0; hcenter <- hcenter0; hright <- hright0
      ## Title rows
      SubColTitle <- 
        AddSubTitleInTable(tab0, addseparatingcols-1, separatingcoltitle, hcenter)
    } 
  } else SubColTitle <- NULL
  if (!addsubcoltitlehere) SubColTitle <- NULL
  # insert "&" in table. ltab is table body, coln is header
  ltab <- coln <- NULL
  for (i in 1:ncol(tab)) 
  # tab has embeded separating columns
  {
    ltab <- cbind(ltab, tab[, i], " & ")
    if (!is.null(headercolor)) 
      coln <- c(coln, 
          paste("\\makebox[", hcenter[i], unit, "]{", 
          "\\cellcolor{", headercolor, "}", 
          colnames(tab)[i], "}", 
            sep = "", collapse = ""),
          " & ") else 
      coln <- c(coln, 
          paste("\\makebox[", hcenter[i], unit, "]{", 
          colnames(tab)[i], "}", 
            sep = "", collapse = ""),
          " & ")
  } # end for i loop
  dim(coln) <- c(1, length(coln))
  # add additional header rows
  if (!is.null(addheaderAbove)) {
    if (all(grepl("^num", addheaderAbove))) {
      if (!NoVariablenameColumn)
        coln <- 
        rbind(
          c(c("", "&"), rbind(
            paste0(
              "\\makebox[", hcenter[-1], unit, "]{",
              paste0("\\cellcolor{", headercolor, "}"),
              "(", 1:(ncol(tab)-1), ")}" 
            )
          , rep("&", ncol(tab)-1)))
          , coln) else
        coln <- rbind(
          c("", "&", rbind(
            paste0(
              "\\makebox[", hcenter[-1], unit, "]{",
              paste0("\\cellcolor{", headercolor, "}"),
              "(", 1:(ncol(tab)-1), ")}"
            )
          , rep("&", ncol(tab)-1)))
          , coln)
    } else {
      if (length(addheaderAbove) != ncol(tab)) 
        message("length(addheaderAbove) is not equal to ncol(tab).")
      if (!NoVariablenameColumn)
        coln <- rbind(
          c("", "&", rbind(
            paste0(
              "\\makebox[", hcenter, unit, "]{", 
              paste0("\\cellcolor{", headercolor, "}"),
              addheaderAbove, "}"
            )
          , rep("&", ncol(tab))))
          , coln) else 
        coln <- rbind(
          c(rbind(
            paste0(
              "\\makebox[", hcenter, unit, "]{", 
              paste0("\\cellcolor{", headercolor, "}"),
              addheaderAbove, "}"
            )
          , rep("&", ncol(tab))))
          , coln) 
     }
  } # end if: addheaderAbove
  if (!is.null(addheaderBelow)) {
    if (all(grepl("^num", addheaderBelow))) {
      coln <- rbind(coln,
        c("", "&", rbind(
          paste(
           "\\makebox[", hcenter[-1], unit, "]{", 
            paste0("\\cellcolor{", headercolor, "}"),
            "(", 1:(ncol(tab)-1), ")}", 
          sep = "")
        , rep("&", ncol(tab)-1)))
      ) 
    } else {
      if (length(addheaderBelow) != ncol(tab)) 
        message("length(addheaderBelow) is not equal to ncol(tab).")
      coln <- rbind(coln,
        c(rbind(
          paste(
            "\\makebox[", hcenter, unit, "]{", 
            paste0("\\cellcolor{", headercolor, "}"),
            addheaderBelow, "}", sep = "")
        , rep("&", ncol(tab))))
      ) 
    }
  } # end if: addheaderBelow
  if (!is.null(estimationspace))
  {
    ltab0 <- ltab[1:2, ]
    for (j in seq(3, nrow(ltab) - estimationspacelast, 2))
      ltab0 <- rbind(ltab0, rep(c("", "&"), ncol(ltab)/2), ltab[j + 0:1, ])
    # add skipped last X rows
    if (estimationspacelast > 0) ltab0 <- rbind(ltab0, ltab[nrow(ltab) - (estimationspacelast-1):0, ])
    ltab <- ltab0
    ltab <- rbind(coln, ltab)
    espace <- rep("", nrow(ltab))
    espace[seq(3, nrow(ltab) - estimationspacelast - 1, 3)] <- paste("[", estimationspace, "]", sep = "")
    linebreak <- paste(rep("\\\\", nrow(ltab)), espace, sep = "")
  } else
  {
    ltab <- rbind(coln, ltab)
    linebreak <- rep("\\\\", nrow(ltab))
  }
  # AddTopStripSpace
  if (!is.logical(addtopstripspace) & is.character(addtopstripspace)) 
    linebreak[1] <- paste0(linebreak[1], "[", addtopstripspace, "]")
  # Adjust line skip space. 
  if (is.null(adjustlineskip) & (!is.null(adjlskiprows) & length(adjlskiprows) > 1)) 
    message("Please set adjustlineskip if you set adjlskiprows.")
  if (!is.null(adjustlineskip)) {
    if (max(adjlskiprows)+1 == nrow(ltab)) 
      stop("Row space change cannot applied to the last row of table. Drop the last row from adjlskiprows.")
    if (length(adjlskiprows) == 1) {
      if (grepl(2, adjlskiprows)) {
        linebreak[seq(2, nrow(ltab), 2)] <- 
          paste0(linebreak[seq(2, nrow(ltab), 2)], "[", adjustlineskip, "]")
      } else if (grepl(3, adjlskiprows)) {
        linebreak[c(seq(2, nrow(ltab), 2), seq(3, nrow(ltab), 3))] <- 
          c(paste0(linebreak[seq(2, nrow(ltab), 2)], "[", adjustlineskip, "]"), 
             paste0(linebreak[seq(3, nrow(ltab), 3)], "[", adjustlineskip, "]")
            )
      }
    } else if (length(adjlskiprows)>1) {
      linebreak[adjlskiprows+1] <- 
        paste(linebreak[adjlskiprows+1], "[", adjustlineskip, "]", sep = "")
    }
  }
  if (!is.null(yesnospace))
  {
    iiyesnospace <- apply(apply(ltab[, seq(3, ncol(ltab), 2), drop = F], 1, 
                                                  function(x) whichgrep("yes|^no$", x)), 2, all)
    linebreak[iiyesnospace] <- paste("\\\\[", yesnospace, "]", sep = "")
  }
  if (!is.null(hline)) linebreak <- paste(linebreak, holine, sep ="") 
  if (!is.null(alternatecolorManual) & is.null(alternatecolorManualColor)) 
    message("You need to specify alternatecolorManualColor.")
  if (is.null(alternatecolorManual) & !is.null(alternatecolorManualColor))
    message("You need to specify alternatecolorManual.")
  if (!is.null(alternatecolorManual) & !is.null(alternatecolorManualColor)) {
    linebreak[alternatecolorManual] <- 
      paste(linebreak[alternatecolorManual], "\\rowcolor{", alternatecolorManualColor, "}", sep = "")
  } else {
    if (!is.null(alternatecolor))
        linebreak[seq(2, nrow(ltab), 2)] <- 
          paste(linebreak[seq(2, nrow(ltab), 2)], "\\rowcolor{", alternatecolor, "}", sep = "")
    if (!is.null(alternatecolor2))
    {
      #  block sequence:  3:4, 7:8, ..., nrow(ltab)
      seqby2 <- rep(seq(alternatestart2, nrow(ltab), 4), each=2)
      seqby2[seq(2, length(seqby2), 2)] <- seqby2[seq(1, length(seqby2), 2)] + 1
      if (seqby2[length(seqby2)] > nrow(ltab)) seqby2 <- seqby2[-length(seqby2)] 
        linebreak[seqby2] <- 
          paste(linebreak[seqby2], "\\rowcolor{", alternatecolor2, "}", sep = "")
     }
    if (!is.null(alternatecolor3))
    {
      #  block sequence:  4:6, 10:12, ..., nrow(ltab)
      seqby3 <- rep(seq(alternatestart3, nrow(ltab), 6), each=3)
      for (k in 2:3)
        seqby3[seq(k, length(seqby3), 3)] <- seqby3[seq(1, length(seqby3), 3)] + k-1
      while (seqby3[length(seqby3)] > nrow(ltab)) seqby3 <- seqby3[-length(seqby3)] 
        linebreak[seqby3] <- 
          paste(linebreak[seqby3], "\\rowcolor{", alternatecolor3, "}", sep = "")
     }
    if (!is.null(alternatecolor4))
    {
      #  block sequence:  5:8, 13:16, ..., nrow(ltab)
      seqby4 <- rep(seq(alternatestart4, nrow(ltab), 8), each = 4)
      for (k in 2:4)
        seqby4[seq(k, length(seqby4), 4)] <- seqby4[seq(1, length(seqby4), 4)] + k-1
      while (seqby4[length(seqby4)] > nrow(ltab)) seqby4 <- seqby4[-length(seqby4)] 
        linebreak[seqby4] <- 
          paste(linebreak[seqby4], "\\rowcolor{", alternatecolor4, "}", sep = "")
    }
    if (!is.null(altestspace))
    {
      #  block sequence:  4, 5, 10, 11, 16, 17,..., nrow(ltab) - estimationspacelast
      seqby2 <- rep(seq(4, nrow(ltab) - estimationspacelast, 6), each=2)
      seqby2[seq(2, length(seqby2), 2)] <- seqby2[seq(1, length(seqby2), 2)] + 1
      if (seqby2[length(seqby2)] > nrow(ltab) - estimationspacelast) seqby2 <- seqby2[-length(seqby2)] 
        linebreak[seqby2] <- 
          paste(linebreak[seqby2], "\\rowcolor{", altestspace, "}", sep = "")
    }
  } # end if: !is.null(alternatecolorManual) & !is.null(alternatecolorManualColor)
  ltab[, ncol(ltab)] <- linebreak
      # cline[, 1] + 1 because of the header line
  if (!is.null(dim(cline)))
  {
    if (nrow(ltab) == cline[nrow(cline), 1])
    {
      cat("\\cline cannot be added at the bottom row...ignored.")
      cline <- cline[-nrow(cline), , drop = FALSE]
    }
    clineadd <- rep("", nrow(ltab))
    clineadd[cline[, 1] + 1] <- paste("\\cline{", cline[, 2], "-", cline[, 3], "}", sep = "")
      # cline[, 1] + 1 because of the header line
    ltab[, ncol(ltab)] <- paste(ltab[, ncol(ltab)], clineadd, sep = "")
  }
  # Make sure 1st and last rows are only "\\\holine"
  ltab[c(1, nrow(ltab)), ncol(ltab)] <- paste("\\\\", holine, sep = "")
  # Collapse table into a single column table.
  ltab2 <- NULL
  for (i in 1:nrow(ltab)) ltab2 <- rbind(ltab2, paste(ltab[i, ], collapse = ""))
  if (!is.null(hleft))
    head <- paste(
      c("\\begin{tabular}{", 
         t(
            cbind(
              paste(dlm, ">{", hleft, "}", sep = ""), 
              paste("p{", hcenter, unit, "}", sep = ""),
              paste("<{", hright, "}", sep = "")
            )
          ), dlm, "}")
        , collapse = "") else
    head <- paste(
      c("\\begin{tabular}{", 
         t(
            cbind(
              paste(dlm, ">{\\scriptsize\\hfil", rep("", ncol(tab)), "}", sep = ""), 
              paste("p{", hcenter, unit, "}", sep = ""),
              paste("<{", rep("", ncol(tab)), "}", sep = "")
            )
          ), dlm, "}")
      , collapse = "")
  foot <- "\\end{tabular}"
  if (!is.null(headercolor)) head <- paste0(head, "\\rowcolor{", headercolor, "}")
  ltab3 <- rbind(head, holine, SubColTitle, ltab2, foot)
  # Drop any empty rows.
  if (any(iidrop <- which(ltab3 == ""))) ltab3 <- ltab3[-iidrop, , drop = F]
  rownames(ltab3) <- NULL
  # set a separating line for interaction terms using addaseparatingline
  if (!is.null(LastDiffVariable) & !is.null(inter.with)) {
    sepline.text <- paste0("\\multicolumn{", ncol(ltab)/2, "}{l}{\\textit{\\footnotesize ", 
      SepLineText, " ", inter.with, "}}\\\\")
    inserthere <- grep(LastDiffVariable, ltab[, 1])
    # shift separating line by AdjustInterWith rows
    if (!is.null(AdjustInterWith)) inserthere <- inserthere + AdjustInterWith
    if (length(inserthere) == 0) 
      stop("LastDiffVariable (line starting inter.with) is not found in ltab[,1].")
    # shift first column InterWithLength (default: .5em) to right
    ltab2[(inserthere+2):nrow(ltab2), 1] <- 
      paste0("\\hspace{", InterWithLength, "}", 
        gsub(paste0(hcenter[1], unit), paste0(hcenter[1]-.5, unit), 
          ltab2[(inserthere+2):nrow(ltab2), 1]))
    ltab2 <- matrix(c(ltab2[1:(inserthere+1), ], sepline.text, 
      ltab2[(inserthere+2):nrow(ltab2), ]))
    ltab3 <- rbind(head, holine, SubColTitle, ltab2, foot)
    rownames(ltab3) <- NULL
  }
  return(ltab3)
}
saveEstTable <- function(tb, varnames, width, hleft = NULL, hcenter = NULL, 
  unit = "cm", hright = NULL, hline = NULL, cline = F, 
  delimiterline = NULL, nohorizontalline = T,
  Addseparatingcols = NULL,
  Separatingcolwidth = NULL,
  Separatingcoltitle = NULL,
  LastDiffVariable = NULL, 
  SepLineText = "", 
  inter.with = NULL,
  AdjustInterWith = NULL,
  adjustlineskip = NULL, adjlskiprows = 2,
  addheaderAbove = NULL, addheaderBelow = NULL,
  AddTopStripSpace = F,
  headercolor = NULL,
  alternatecolor = NULL,
  alternatecolor2 = NULL, alternatestart2 = 3, 
  alternatecolor3 = NULL, alternatestart3 = 4, 
  alternatecolor4 = NULL, alternatestart4 = 5, 
  alternatecolorManual = NULL,
  alternatecolorManualColor = NULL,
  estimationspace = NULL,
  estimationspacelast = NULL,
  pathtosavedtable = NULL, 
  altestspace = NULL, yesnospace = "-.5ex",
  ...)
#  To be used for the outputs of tabulate.est (uses tabs2latex3) or Tab.Est (uses tabs2latex4).
#  tb: latex-formatted regression table without the variable name column
#  varnames: variable names to appear in latex table
#  description: description of variables
#  width: width of makebox table for variable name column
#  requires latextab
{
  rn <- paste("\\makebox[", width, unit, "]{\\footnotesize ", varnames, "\\hfill}", sep = "")
  ltb <-   latextab(as.matrix(cbind(variables = rn, tb)),
            hleft = hleft, 
            hcenter = hcenter, 
            unit = unit, hright = hright, hline = hline, cline = cline, 
            delimiterline = delimiterline, nohorizontalline = T,
            addseparatingcols = Addseparatingcols,
            separatingcolwidth = Separatingcolwidth,
            separatingcoltitle = Separatingcoltitle,
            addsubcoltitlehere = T,
            LastDiffVariable = LastDiffVariable, 
            SepLineText = SepLineText,
            inter.with = inter.with,
            AdjustInterWith = AdjustInterWith,
            adjustlineskip = adjustlineskip, adjlskiprows = adjlskiprows,
            # When using addheaderAbove or addheaderBelow, they do not take addseparatingcols into consideration, need to correct
            addheaderAbove = addheaderAbove, addheaderBelow = addheaderBelow,
            addtopstripspace = AddTopStripSpace,
            alternatecolorManual = alternatecolorManual,
            alternatecolorManualColor = alternatecolorManualColor,
            headercolor = headercolor,
            alternatecolor = alternatecolor,
            alternatecolor2 = alternatecolor2, alternatestart2 = 3, 
            alternatecolor3 = alternatecolor3, alternatestart3 = 4, 
            alternatecolor4 = alternatecolor4, alternatestart4 = 5, 
            estimationspace = estimationspace, 
            estimationspacelast = estimationspacelast, 
            altestspace = altestspace, yesnospace = yesnospace)
   if (!is.null(Addseparatingcols)) {
     # cbind(1, tb) to adjust ncol(tb) as tb has no variable name column
     SubColTitle <- AddSubTitleInTable(tb, Addseparatingcols, Separatingcoltitle, hcenter)
     ltb <- rbind(
         ltb[1, , drop = F], 
         SubColTitle,
         ltb[-(1:3), , drop = F]
       )
   }
  if (!is.null(pathtosavedtable))
  {
    write.tablev(ltb, pathtosavedtable, colnamestrue = F, rownamestrue = F, nastrings = " ") 
    cat("Table saved as", pathtosavedtable, "\n")
  } else return(ltb)
}



Tab.Est.Html <- function(est, reorder = NULL, output.in.list = F,
  drop.dots = F, 
  Informat = "satt",
  Outformat = "epc", # Also, "esc" or "ep" or "satt" are allowed
  AddStars = F,
  CIInTinySize = F, 
  lastLevelVariable = NULL, inter.with = NULL, # keep these for backward compatibility
  LastLineVariables = NULL,  # use these for a multiple part table, inter with A, inter with B
  InterWithTexts = NULL,      # A and B in "int with A", "int with B", ...
  DeleteRowStrings = NULL,
  AddTopStripSpace = F, # Add space between (1), (2), ... line and 1st row e.g., (Intercept)
  addbottom = NULL, subst.table = NULL)
# Outformat: est (list of estimated results) in a single table, in a format of
#      "epc"              "esc"            "ep"            "satt"        "es"
#       est                est              est                est         est 
#    (p value)   or       (se)      or   (p value)  or (p value) or  (se)
#    [lb, ub]            [lb, ub]                            [dof]
# and output in cbind(rn, table) or list(rn, table). est is in the following format
#   estimate, std error, p value, lb, ub Or
#   estimate, std error, p value, DoF Or
#   estimate, std error, p value Or
#   estimate, std error, Or
#   estimate, p value
# Uses reordertab, tabs2latex4 (uses TabEstSEPvalCI) function stored in functions.R
# Uses addaseparatingline function stored in path_function_rnw.R
#   reorder: ordering of variable names in regexp (to be used in reordertab)
#   output.in.list: if T, output is list(rn, table), if F, cbind(rn, table)
#   drop.dots: if T, drop "." from variable names
#   Outformat: epc, esc, ep, satt
#   Informat: satt, sattCI, wcb, LZ
#   AddStars: If T, add stars of statistical significance
#   CIInTinySize: If T, put CIs in tiny size irrespective of common font size setting.
#   lastLevelVariable: Obsolete. regexp for the last level variable 
#     so I can insert a separating line using addaseparatingline
#   inter.with: Obsolete. name of interaction variables (main of main*cross)
#   LastLineVariables: regexp for the last variable names of part A, B, C, ...
#     This is applied after having reordered the rows with "reorder" option.
#   InterWithTexts: A, B, ... in "interaction with A", "interaction with B", ...
#   DeleteRowStrings: A vector of regexp of rownames to be dropped. 
#     E.g.,  "^p\\$", "^CI\\$", ...
#   AddTopStripSpace: Add space ["AddTopStripSpace"] after change line "\\" 
#     of TopStrip [(1), (2), ...] row.
#   addbottom: Lines (such as "n", "R2", etc.) to be added at the bottom
#   subst.table: variable name substitution table 
#    (see: c:/data/ramadan/program/program/substitution_table.R)
# 
{
  # XX.YYY: If T, display in 12.345 (%) format, if F, .123 format.
  # Uses tabs2latex4 (using TabEstSEPvalCI) function stored in functions.R
  # source("c:/seiro/settings/Rsetting/functions.R")
  tb <- tabs2latex4(est, outformat = Outformat, informat = Informat, xx.yyy = T, 
    AddStarIntabstarP = AddStars, CIIntinysize = CIInTinySize) # default: digits = 3, CIDigits = digits
  rn <- rownames(tb)
  if (drop.dots) rownames(tb) <- rn <- gsub("\\.", "", rn)
  #  order according to regexp order
  if (grepl("e[ps]$", Outformat)) AddNum <- 1 else AddNum <- 2
  if (!is.null(reorder)) 
    tb <- reordertab(tb, reorder, deletethese = DeleteRowStrings, RowsPerCovariate = AddNum+1)
  if (is.null(LastLineVariables)) {
    # === Backward compatibility begins ===
    # set a separating line for interaction terms using addaseparatingline
    sepline.text <- paste0("\\hspace{-.1em}\\textit{\\footnotesize Interaction with ", inter.with, "}")
    if (!is.null(lastLevelVariable))
      tb <- addaseparatingline(tb, lastLevelVariable, add = 1, message = sepline.text) else
      tb <- addaseparatingline(tb, "^any", add = 1, message = sepline.text)
    # further adding an empty line to separate from remaining rows
    tb <- addaseparatingline(tb, "interaction", message = "")
    # === Backward compatibility ends ===
  } else {
    if (length(LastLineVariables) != length(InterWithTexts)) 
      stop("Length of LastLineVariables and InterWithTexts differ. These must be same.")
    sepline.text.vec <- paste0("\\hspace{-.1em}\\textit{\\footnotesize interaction with ", 
        InterWithTexts, "}")
    for (m in 1:length(LastLineVariables)) {
      # insert a message line
      tb <- addaseparatingline(tb, LastLineVariables[m], add = AddNum, message = sepline.text.vec[m])
      # further adding an empty line to separate from remaining rows
      tb <- addaseparatingline(tb, LastLineVariables[m], add = AddNum, message = "")
      # if 3 lines for one covariate, add another line to make table look more balanced
      if (!grepl("ep$", Outformat)) 
        tb <- addaseparatingline(tb, LastLineVariables[m], add = 2, message = "")
    }
  }
  if (!is.null(addbottom)) tb <- rbind(as.matrix(tb), addbottom)
  # Get rownames again after reordering/addition of rows
  rn <- rownames(tb)
  #  replace variable names
  if (!is.null(subst.table)) {
    for (k in 1:nrow(subst.table)){
      if (any(grepl(subst.table[k, "org"], rn))) 
        rn[grep(subst.table[k, "org"], rn)] <- subst.table[k, "changedto"]
    }
  }
  # AddTopStripSpace: Add space between TopStripRow and "(Intercept)"
  if (!is.logical(AddTopStripSpace) & is.character(AddTopStripSpace)) {
    TopStripRowNum <- grep("\\(1\\).*\\(2\\)", tb)
    tb[TopStripRowNum, ] <- paste0(tb[TopStripRowNum, ], "[", AddTopStripSpace, "]")
  }
  # Delete std error/pval/CI/DoF rownames
  rn <- gsub("^..?.?\\$.*", "", rn)
  if (output.in.list) list(rn, tb) else return(cbind(rn, tb))
}



dropunbalanced <- function(Z, idcol = "uniquid", returnDT = F) {
####	drop unbalanced obs (fast operation using data.table)
  require(data.table)
  dZ <- data.table(Z)
  setkeyv(dZ, idcol)
  dZ[, period := .N, by = idcol]
  #### keep only individuals with largest round numbers
  dZ <- dZ[period == max(period), ]
  dZ <- dZ[, period := NULL]
  if (returnDT) dZ else data.frame(dZ)
}


