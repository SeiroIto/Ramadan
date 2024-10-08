library(clubSandwich)
source("TabGeneric.R")
clusterlevel <- "thana"
DivInto2Tables <- T
zEm.1999 <- qread("../save/zEm1999.qs")
zEm.1999[, agHH0 :=  as.numeric(agHH0 > 0)]
#### DaysAbsent is created in Create2RoundPanel.rnw
#### There is no equivalent information in 2007 data.
samples <- "main"
z23 <- c("z2", "z3")
zsobj <- c("zmobjDays", "zpobjDays")
zmobjDays <- c("zEm.1999", "zSm.1999")[1]
zpobjDays <- c("zEp.2002", "zSp.2002", "zEp.1999", "zSp.1999")[c(1, 3)]
cohort.years.list <- list(# year age is defined
  main = rep(1999, 2), 
  placebo = c(rep(2002, 1), rep(1999, 1))
  )
cutout.years<- c(rep(2006, 1), rep(1999, 1), rep(2006, 1)) # year to drop in data, main = 2006, placebo 1999
#### Used in "interaction with year InterYears" in results table
InterYearsList <- list(main = rep(2002, 1), placebo = rep(c(2006, 2002), each = 2))
regressors.list <- list(
  main = regressorsM2024,
  placebo = regressorsP2024
)
variables.always.use <- "^DaysAbsent$|^agHH.yr2|^agHH$|^thana$|uniqu"
yrXs <- c("yr2", "yr3")
mix.reorder <- function(x, y=main.reorder.JHR) 
  paste0(c(y[1], x, y[3], y[4]), collapse = "")
sub.reorder <- function(x, z, y=main.reorder.JHR) 
  paste0(c(y[1], gsub(x, z, y[2]), y[3], y[4]), collapse = "")
reorder.list <- list(
    main = main.reorder.JHR
  , placebo = main.reorder.JHR
)
boxWidth <- 4
centerWidth <- 1.3
DaysAbsent <- DaysAbsentchg <- NULL
results <- resultsN <- vector("list", length = length(samples)) # ii
####for (ii in 1:length(samples)) {
for (ii in 1) {
  zSobj <- get(zsobj[ii])
  regressorsS <-  regressors.list[[ii]]
  cohort.years <- cohort.years.list[[ii]]
  cutout.year <- cutout.years[ii]
  InterYears <- InterYearsList[[ii]]
  var.always.use <- gsub("yr2", yrXs[ii], variables.always.use)
  reorder <- reorder.list[[ii]]
  regsnd <- rep("DaysAbsent", length(regressorsS))
  est <- res <- vector("list", length = length(regressorsS)) # k, specification
  # Use satterthwaite only.
  est <- res <- vector("list", length = length(regressorsS)) # k, specification
  res <- list("LiangZeger" = res, "Satterthwaite" = res, "WildClusterBoot" = res) # cl, clustering correction choice
  res <- list(res, res, res, res) # m, agHH definition
  names(res) <- aghh.defs
  res <- list(boys = res, girls = res, "boys+girls" = res) # ge, gender
  res <- list("extended" = res, "nuclear" = res, "exonly" = res) # j, nuclear, extended, extended only HHs
  res <- list("LB10" = res, "LB11" = res, "LB12" = res) # s, age lowerbound
  # res[[s]][[j]][[ge]][[m]][[clnum]][[k]] is same for each jj in zSobj: An element of results0[[jj]]
  results0 <- resultsN0 <- vector("list", length = length(zSobj)) # jj, zE/zS sample selection
  for (jj in 1:length(zSobj)) {
    resultsN0[[jj]] <- results0[[jj]] <- res
    cat("\n\n")
    print0(zSobj[jj])
    cat("\n")
    z01 <- changehyphen(get(zSobj[jj]))
    # Restrict sample to ss == 2: Enrolled in both 1999 and 2002. 
    # Justification: If enrolled not in both years, difficult to interpret the "changes in days of absent"
    z01[, ss := cumsum(schoolp), by = uniquid]
    z01[, ss := ss[2], by = uniquid]
    z02 <- z01[ss==2, ]
      z1 = copy(z02)
      z1[, grepout("dummy[A-Z].*HH0?.yr.$", colnames(z1)) := NULL]
      # keep UDOldSib, UDhdsex, UDnonmuslim, UDflooded as undemeaned levels
      setnames(z1, 
        grepout("UDOldSib|UDhds|UDnon|UDfl", colnames(z1)),
        gsub("UD", "ud", grepout("UDOldSib|UDhds|UDnon|UDfl", colnames(z1))))
      z1[, grepout("^UD", colnames(z1)) := NULL]
      setnames(z1, 
        grepout("^ud", colnames(z1)),
        gsub("ud", "UD", grepout("^ud", colnames(z1))))
      tabextend <- c("yes", "", "yes", "")
      tabcohortdemeaned <- c("", "yes", "", "yes")
      for (s in 1:3)
      #  choice of age cutoff
      {
        s0 <- (10:12)[s]
        if (ii == 2 & jj == 5) {
          s0 <- 6
          MaxAge <- 9
        } else {
          MaxAge <- 18
        }
        i <- paste0("older", s0)
        # latter panel: 5 <= age < s0 in 1999/2002
        iiid <- unique(z1[
          s0 <= eval(parse(text = paste0("AgeIn", cohort.years[jj]))) & 
          eval(parse(text = paste0("AgeIn", cohort.years[jj]))) <= MaxAge 
          #maxAge
          , uniquid])
        # Keep only former complete panel and respective years.
        z2 <- z1[uniquid %in% iiid & survey != cutout.year, ]
        z2[, grepout("exist|In", colnames(z2)) := NULL]
        z2 <- dropunbalanced(z2, returnDT = T)
        # z3: nuclear family
        z3 <- z2[sd == 1, ]
        z3 <- dropunbalanced(z3, returnDT = T)
        cat("\n\nage cutoff:", i, "\n\n")
        print(table0(z1[, .(survey, agegroup = (uniquid %in% iiid))]))
        cat("dimension of original z1:", dim(z1), "\n")
        cat("dimension of z2 after keeping only", s0, "-", maxAge, "year olds:", 
        dim(z1)[1], "==>", dim(z1[uniquid %in% iiid & survey != cutout.year, ])[1], "\n")
        cat("dimension of z2 after keeping only balanced portion:", 
        dim(z1[uniquid %in% iiid & survey != cutout.year, ])[1], "==>", dim(z2)[1], "\n")
        cat("number of individuals in the panel:")
        print(table(table(z2[, uniquid])))
        cat("dimension of z3 after keeping only nuclear members:", dim(z3), "\n\n")
        cat("first-diffference estimator\n")
        for (j in 1:length(z23))
        {
          zz00 = copy(get(z23[j]))
          for (ge in 1:3)
          {
            if (ge == 1) {
              zz0 = copy(zz00[sex <= 0, ]) 
              zz0[, grepout("^sex", colnames(zz0)) := NULL]
            } else  if (ge == 2){
              zz0 = copy(zz00[sex > 0, ])
              zz0[, grepout("^sex", colnames(zz0)) := NULL]
            } else zz0 = copy(zz00)
            if (nrow(zz0) < SkipLowerBound) {
              cat("Skipped due to small number of obs:", nrow(zz0), "\n")
              next
            }
            setkey(zz0, uniquid, survey)
            zz0[, survey := NULL]
            for (m in 1:length(aghh.defs))
            {
              zz = copy(zz0)
              # Use a particular agHH definition.
              # change the name of current ag HH (agHH0, isagHH, ocagHH) to "agHH"
              setnames(zz, 
                grepout(aghh.defs[m], colnames(zz))
                ,
                gsub(aghh.defs[m], "agHH", grepout(aghh.defs[m], colnames(zz)))
              )
              # drop other ag HH definition
              zz[, grepout(paste0(aghh.defs.regexpr[-m], collapse = "|"), colnames(zz)) := NULL]
              zz[, grepout(paste0("^", aghh.defs[-m], "$", collapse = "|"), colnames(zz)) := NULL]
              ns <- NULL
              resul <- vector("list", length = length(regressorsS))
              # First run: Estimation loop for getting N (number of obs) and first-differenced data.
              for (k in 1:length(regressorsS))
              {
                if (s0 == 10 & j == 1 & m == 1) {
                  cat(paste0("(", k, ")\n"))
                  print0(paste0("+ ", 
                    grepout(paste(regressorsS[k], sep = "", collapse = "|"), colnames(zz))))
                }
                regrsr <- paste(regressorsS[1:k], sep = "", collapse = "|")
                covariates <- grepout(
                  paste(var.always.use, regrsr, sep = "|", collapse = "|")
                  , colnames(zz))
                zr <- zz[, covariates, with = F]
                rs <- DID1(data.frame(zr), regressand = regsnd[k], 
                   clusterstring = clusterlevel, group = "^uniquid$", 
                   NotToBeDifferenced = "^agHH$",
                   intercept = T, 
                   TimeVariant = "program|age2|meanY",
                   PeriodToDropForLC = 2, 
                   opposite.time.order = F,
                   TurnFactorToNumeric = T, returnV = T, print.messages = F)
                resul[[k]] <- list(level.data = rs$level, diff.data = rs$diff, est = rs$est)
                est[[k]] <- round(rs$est[, -3], 5)
                ns <- c(ns, rs$N)
              }
              # resultsN0: raw results (not under same obs)
              resultsN0[[jj]][[s]][[j]][[ge]][[m]] <- resul
                source(paste0(pathprogram0, "ReconstructCovariatesForDemeanedInteractions.tex")) 
              zidd[, tee := 1]
              zid[, tee := 1:.N, by = uniquid]
              if (any(grepl("DaysAbsent", colnames(zidd)))) setnames(zidd, "DaysAbsent", "LHS")
              if (any(grepl("DaysAbsent", colnames(zid)))) setnames(zid, "DaysAbsent", "LHS")
              # Save mean days of absence
              enrr <- zid[, .(MeanDaysAbsent = mean(LHS), Num = .N), by = .(agHH, tee)]
              DaysAbsent <- rbind(DaysAbsent, 
                cbind(zSobj[jj], c("all", "nuclear")[j], c("default", aghh.defs[-1])[m], 
                  c("boys", "girls", "boys+girls")[ge],
                  s0, enrr),
                use.names = F
              )
              # Save mean progression rate changes
              # x: agHH, y: nonagHH
              ttestE <- t.test(zidd[agHH == 1, LHS], zidd[agHH == 0, LHS])
              DaysAbsentchg <- rbind(DaysAbsentchg, 
                cbind(
                    zSobj[jj], c("all", "nuclear")[j], c("default", aghh.defs[-1])[m], 
                    c("boys", "girls", "boys+girls")[ge],
                    s0, round(-diff(unlist(ttestE["estimate"])), 3), # -diff = -(y - x) = AgHH - nonagHH
                    t(as.numeric(unlist(lapply(ttestE[c("estimate", "conf.int", "p.value")], round, 4))))
                    )
                  )
              if (any(grepl("LHS", colnames(zidd)))) setnames(zidd, "LHS", "DaysAbsent")
              zidd[, tee := 1]
              for (cl in c("LiangZeger", "satterthwaite", "wildclusterboot")[-3]) 
              {
                Rs <- ns <- NULL
                est <- vector("list", length(regressorsS))
                UseSmallClusterCorrection <- cl 
                cat("\n\n###", cl, "###\n\n")
                if (grepl("Yp|S", zSobj[jj]) & grepl("wild", cl)) {
                  cat("fwildclusterboot fails in Julia for zSm.1999, zYp.1999 because Sib", 
                    "covariates are near zero. Skip to next.\n\n" )
                  next
                }
                for (k in 1:length(regressorsS))
                {
                  regrsr <- paste(regressorsS[1:k], sep = "", collapse = "|")
                  covariates <- grepout(paste(var.always.use, regrsr, sep = "|"), 
                    colnames(zidd))
                  zr <- zidd[, c(covariates, "tee"), with = F]
                  source("EstimatorFunctions.R")
                  rsl <- DID2(dX0 = zr, Regressand = "DaysAbsent", 
                           Group = "^uniquid$", TimeVar = "tee", Cluster = "thana", 
                           TimeVariant = "program|age2|meanY|yield",
                           opposite.time.order = F, Exclude = "^agHH$", intercept = T, 
                           SmallClusterCorrection = UseSmallClusterCorrection,
                           WCBType = "webb",
                           return.V = T, print.messages = T)
                  if (!is.logical(UseSmallClusterCorrection) && grepl("satter", UseSmallClusterCorrection)) {
                    # Correct format of estimation results for clubSandwich outputs
                    rsl$est <- as.data.frame(rsl$est)
                    rsl$est <- rsl$est[, -1]
                    colnames(rsl$est)[c(1:2, 4:5)] <- c("Estimate", "Std. Error", "Satt. DoF", "Pr(>|t|)")
                  } else if (!is.logical(UseSmallClusterCorrection) && grepl("wild", UseSmallClusterCorrection)) {
                    # Correct format of estimation results for wildclusterboot outputs
                    rsl$est <- as.data.frame(rsl$est)
                    colnames(rsl$est)[c(1:2, 4)] <- c("Estimate", "Std. Error", "Pr(>|t|)")
                  } else {
                    # Correct format of estimation results for Liang-Zeger outputs
                    rsl$est <- as.matrix(rsl$est)
                    colnames(rsl$est)[c(1:2, 4)] <- c("Estimate", "Std. Error", "Pr(>|t|)")
                  }
                  # results0: results under same obs
                  clnum <- 1
                  if (cl == "satterthwaite") clnum <- 2 else if (cl == "wildclusterboot") clnum <- 3
                  results0[[jj]][[s]][[j]][[ge]][[m]][[clnum]][[k]] <- 
                    list(est = rsl$est, ci = rsl$CI,
                      df = rsl$reg$df, reg = rsl$reg,
                      level.data = zid2[, covariates, with = F], 
                      diff.data = rsl$data)
                  est[[k]] <- round(rsl$est[, -3], 5)
                  Rs <- c(Rs, summary(rsl$nonrobust)$adj.r)
                  ns <- c(ns, rsl$N)
                } # k: reg specification
                assign(paste0("addthis", j),
                   rbind("\\hspace{.5em}thana dummies" = 
                      paste0("\\mbox{", c(rep("", length(regressorsS)-1), rep("yes", 1)), "}"),
                     "$\\bar{R}^{2}$" = gsub("^0", "", formatC(Rs, digits = 4, format = "f")),
                     "n" = ns,
                     "control mean at baseline" = 
                       rep(formatC(enrr[tee == 1 & agHH == 0, MeanDaysAbsent], 
                         digits = 2, format = "f"), length(regressorsS)),
                     "control mean at follow up" = 
                       rep(formatC(enrr[tee == 2 & agHH == 0, MeanDaysAbsent], 
                         digits = 2, format = "f"), length(regressorsS)),
                     "treated mean at baseline" =
                       rep(formatC(enrr[tee == 1 & agHH == 1, MeanDaysAbsent], 
                         digits = 2, format = "f"), length(regressorsS)),
                     "treated mean at follow up" =
                       rep(formatC(enrr[tee == 2 & agHH == 1, MeanDaysAbsent], 
                         digits = 2, format = "f"), length(regressorsS)),
                     "raw DID" =
                       rep(formatC(
                       enrr[tee == 2 & agHH == 1, MeanDaysAbsent] - enrr[tee == 1 & agHH == 1, MeanDaysAbsent] 
                       -(enrr[tee == 2 & agHH == 0, MeanDaysAbsent] - enrr[tee == 1 & agHH == 0, MeanDaysAbsent]), 
                         digits = 2, format = "f"), length(regressorsS))
                   )
                )
                INformat <- "LZ"
                OUTformat <- "ep"
                if (cl == "wildclusterboot") {
                  INformat <- "wcb"
                  OUTformat <- "epc"
                } else if (cl == "satterthwaite") {
                  INformat <- "satt"
                  OUTformat <- "satt"
                  OUTformat <- "esDoF"
                }
                # Incorporate CI/DoF in table
                # reorder needs to be corrected
                # Tab.Est is in tabulate_est.R
                # source(paste0(pathprogram, "tabulate_est.R"))
                tbest <- Tab.Est(est, reorder, output.in.list = T,
                  Informat = INformat, Outformat = OUTformat, 
                  AddStars = T, #TableFormat = tabformat,
                  LastLineVariables = c("lowMeanY$", "kut.*e.yr.$"),
                  InterWithTexts = paste0(InterYears[jj], c("", "*agricultural household")),
                  DeleteRowStrings = "^p\\$|^se\\$|^CI\\$|^DoF\\$",
                  CIInTinySize = T, 
                  addbottom = get(paste0("addthis", j)), subst.table = sbt)
                clCap <- paste0(toupper(substr(cl, 1, 1)), substr(cl, 2, 100))
                if (DivInto2Tables) {
                  # Split a table in to 2 tables
                  if (grepl("Lian", cl)) 
                    NumRowsAfterEst <- 2 else 
                    NumRowsAfterEst <- 3
                  tbest11 <- tbest[[1]][1:(grep("inter.*200..*ag", tbest[[1]])-NumRowsAfterEst)]
                  tbest12 <- tbest[[2]][1:(grep("inter.*200..*ag", tbest[[1]])-NumRowsAfterEst), ]
                  tbest21 <- tbest[[1]][grep("inter.*200..*ag", tbest[[1]]):length(tbest[[1]])]
                  tbest22 <- tbest[[2]][grep("inter.*200..*ag", tbest[[1]]):length(tbest[[1]]), ]
                  iispace11 <- which(
                    grepl(".", tbest11) & 
                    !grepl("interaction with|^n$|bar.R|thana dum|mean at|raw DID", tbest11)
                    )
                  iispace12 <- iispace11[seq(2, length(iispace11), 2)]
                  iispace21 <- which(
                    grepl(".", tbest21) & 
                    !grepl("interaction with|^n$|bar.R|thana dum|mean at|raw DID", tbest21)
                    )
                  # drop last rows of tbest2 to shrink row space
                  iispace21 <- iispace21[iispace21 < max(grep("toilet|water|nonla", tbest21))]
                  iispace22 <- iispace21[seq(2, length(iispace21), 2)]
                  if (grepl("Lian", cl)) {
                  # ep: 2 rows per estimate
                    AdjustLineSkipRows1 <- iispace11
                    AltColorRows1 <- c(iispace12, iispace12+1)
                    AdjustLineSkipRows2 <- iispace21
                    AltColorRows2 <- c(iispace22, iispace22+1)
                  } else {
                  # epc, satt: 3 rows per estimate
                    AdjustLineSkipRows1 <- c(iispace11, iispace11+1)
                    AltColorRows1 <- c(iispace12, iispace12+1, iispace12+2)
                    AdjustLineSkipRows2 <- c(iispace21, iispace21+1)
                    AltColorRows2 <- c(iispace22, iispace22+1, iispace22+2)
                  }
                  tbl1 <- saveEstTable(tbest12, tbest11, boxWidth, 
                    hleft = "\\hfil\\scriptsize$", hright = "$", 
                    hcenter = c(boxWidth, rep(centerWidth+.15, ncol(tbest[[2]]))), 
                    delimiterline = NULL, adjustlineskip = "-0.7ex", 
                    adjlskiprows = AdjustLineSkipRows1,
                    alternatecolorManual = AltColorRows1,
                    alternatecolorManualColor = "gray80")
                  tbl2 <- saveEstTable(tbest22, tbest21, boxWidth, 
                    estimationspacelast = grep("thana dummi", tbest21),
                    hleft = "\\hfil\\scriptsize$", hright = "$", 
                    hcenter = c(boxWidth, rep(centerWidth+.15, ncol(tbest[[2]]))), 
                    delimiterline = NULL, adjustlineskip = "-0.7ex", 
                    adjlskiprows = AdjustLineSkipRows2,
                    alternatecolorManual = AltColorRows2,
                    alternatecolorManualColor = "gray80")
                  # Modify "interaction with ..." lines to use multicolumn
                  InterRows1 <- grep("nteract.*\\d", tbl1)
                  InterRows2 <- grep("nteract.*\\d", tbl2)
                  for (ir in InterRows1) {
                    if (any(grepl("rowcolor", tbl1[ir])))
                      tbl1[ir] <- 
                        # \makbox[]{inter with A} &&&& \\[-1ex] => \multicolumn{5}{l}{\makebox[]{inter with A}} \\[-1ex]
                        # For rows with rowcolor command at the end
                        paste0("\\multicolumn{", ncol(tbest[[2]]), "}{l}{", 
                          gsub("(\\\\\\\\.*ex.*?rowcolor.*?)$", "}\\1", 
                          #gsub("\\\\hfill", "", gsub("\\&", "", tbl[ir]))
                          gsub("\\\\hfill", "}", gsub("\\&", "", tbl1[ir]))
                          )
                          ) else
                        # For rows without rowcolor command at the end
                      tbl1[ir] <- 
                        paste0("\\multicolumn{", ncol(tbest[[2]]), "}{l}{", 
                          gsub("(\\\\\\\\.*ex.$)", "}\\1", 
                          #gsub("\\\\hfill", "", gsub("\\&", "", tbl[ir]))
                          gsub("\\\\hfill", "}", gsub("\\&", "", tbl1[ir]))
                          )
                          )
                      # \multicolumn{5}{l}{\makebox[Xcm]{inter with A}} \\\rowcolor{}
                      # => \multicolumn{5}{l}{\makebox[10cm]{\textit{inter with A}\hfill}}\\[.5ex]\rowcolor{}
                      tbl1[ir] <- gsub("makebox\\[.cm\\]", "makebox[10cm]", tbl1[ir])
                      tbl1[ir] <- gsub("(\\\\textit\\{.*?\\})", "\\1\\\\hfill", tbl1[ir])
                      tbl1[ir] <- gsub("\\\\rowcolor", "[.5ex]\\\\rowcolor", tbl1[ir])
                  }
                  for (ir in InterRows2) {
                    if (any(grepl("rowcolor", tbl2[ir])))
                      tbl2[ir] <- 
                        # \makbox[]{inter with A} &&&& \\[-1ex] => \multicolumn{5}{l}{\makebox[]{inter with A}} \\[-1ex]
                        # For rows with rowcolor command at the end
                        paste0("\\multicolumn{", ncol(tbest[[2]]), "}{l}{", 
                          gsub("(\\\\\\\\.*ex.*?rowcolor.*?)$", "}\\1", 
                          #gsub("\\\\hfill", "", gsub("\\&", "", tbl[ir]))
                          gsub("\\\\hfill", "}", gsub("\\&", "", tbl2[ir]))
                          )
                          ) else
                        # For rows without rowcolor command at the end
                      tbl2[ir] <- 
                        paste0("\\multicolumn{", ncol(tbest[[2]]), "}{l}{", 
                          gsub("(\\\\\\\\.*ex.$)", "}\\1", 
                          #gsub("\\\\hfill", "", gsub("\\&", "", tbl[ir]))
                          gsub("\\\\hfill", "}", gsub("\\&", "", tbl2[ir]))
                          )
                          )
                      tbl2[ir] <- gsub("makebox\\[.cm\\]", "makebox[10cm]", tbl2[ir])
                      tbl2[ir] <- gsub("(\\\\textit\\{.*?\\})", "\\1\\\\hfill", tbl2[ir])
                      tbl2[ir] <- gsub("\\\\rowcolor", "[.5ex]\\\\rowcolor", tbl2[ir])
                  }
                  # file path to saved table
                  pathtosavedtable1 <- TabFilePathF(
                    FolderPath = pathsave, 
                    Sample = gsub("\\.", "", zSobj[jj]), 
                    AgeCutoff = paste0("DaysAbsent2024EnrollersOlder", (10:12)[s]),
                    HHType = paste0(c("", "Nuclear")[j], c("Boys", "Girls", "")[ge]),
                    AgHHDef = c("", "Is", "Hd", "Occ")[m],
                    CRSEMethod = paste0(clCap, 1)
                    )
                  pathtosavedtable2 <- TabFilePathF(
                    FolderPath = pathsave, 
                    Sample = gsub("\\.", "", zSobj[jj]), 
                    AgeCutoff = paste0("DaysAbsent2024EnrollersOlder", (10:12)[s]),
                    HHType = paste0(c("", "Nuclear")[j], c("Boys", "Girls", "")[ge]),
                    AgHHDef = c("", "Is", "Hd", "Occ")[m],
                    CRSEMethod = paste0(clCap, 2)
                    )
                  write.tablev(tbl1, pathtosavedtable1, colnamestrue = F, rownamestrue = F, nastrings = "") 
                  write.tablev(tbl2, pathtosavedtable2, colnamestrue = F, rownamestrue = F, nastrings = "") 
                  cat("Table saved as", pathtosavedtable1, "\n")
                  cat("Table saved as", pathtosavedtable2, "\n")
                } else {
                  # iispace, iispace+1: rows i to shrink rowspace between row i+1 to group together
                  # iispace2, iispace2+1, iispace2+2: (group of) rows to be coloured
                  iispace <- which(
                    # rows with \hspace{.5em} and "non-estimate" rows (R2, n, ...)
                    grepl(".", tbest[[1]]) & 
                    !grepl("interaction with|^n$|bar.R|thana dum|mean at|raw DID", tbest[[1]])
                    )
                  iispace2 <- iispace[seq(2, length(iispace), 2)]
                  # adjlskiprows = c(iispace, iispace+1)
                  # saveEstTable is in functions.R
                  # source("C:/seiro/settings/Rsetting/functions.R", echo=F)
                  tbl <- saveEstTable(tbest[[2]], tbest[[1]], boxWidth, 
                    estimationspacelast = grep("thana dummi", tbest[[1]]),
                    hleft = "\\hfil\\scriptsize$", hright = "$", 
                    hcenter = c(boxWidth, rep(centerWidth-.15, ncol(tbest[[2]]))), 
                    delimiterline = NULL, adjustlineskip = "-0.7ex", 
                    adjlskiprows = c(iispace, iispace+1),
                    alternatecolorManual = c(iispace2, iispace2+1, iispace2+2),
                    alternatecolorManualColor = "gray80")
                  # Modify "interaction with ..." lines to use multicolumn
                  InterRows <- grep("nteract.*\\d", tbl)
                  for (ir in InterRows)
                    if (any(grepl("rowcolor", tbl[ir])))
                      tbl[ir] <- 
                        # \makbox[]{inter with A} &&&& \\[-1ex] => \multicolumn{5}{l}{\makebox[]{inter with A}} \\[-1ex]
                        # rows with rowcolor command at the end
                        paste0("\\multicolumn{", ncol(tbest[[2]]), "}{l}{", 
                          gsub("(\\\\\\\\.*ex.*?rowcolor.*?)$", "}\\1", 
                          gsub("\\\\hfill", "}", gsub("\\&", "", tbl[ir]))
                          )
                          ) else
                        # rows without rowcolor command at the end
                        paste0("\\multicolumn{", ncol(tbest[[2]]), "}{l}{", 
                          gsub("(\\\\\\\\.*ex.$)", "}\\1", 
                          gsub("\\\\hfill", "}", gsub("\\&", "", tbl[ir]))
                          )
                          )
                  # file path to saved table
                  pathtosavedtable <- TabFilePathF(FolderPath = pathsave, 
                    Sample = gsub("\\.", "", zSobj[jj]), 
                    AgeCutoff = paste0("DaysAbsent2024EnrollersOlder", (10:12)[s]),
                    HHType = paste0(c("", "Nuclear")[j], c("Boys", "Girls", "")[ge]),
                    AgHHDef = c("", "Is", "Hd", "Occ")[m],
                    CRSEMethod = paste0(toupper(substr(cl, 1, 1)), substr(cl, 2, 100)))
                  write.tablev(tbl, pathtosavedtable, colnamestrue = F, rownamestrue = F, nastrings = "") 
                  cat("Table saved as", pathtosavedtable, "\n")
                } # If: DivInto2Tables ends
              } # cl: SE clustering choice ends
            }  # m: ag HH definitions
          } # ge: gender
        }  # j: z2 (incl. extended) or z3 (nuclear)
      }  # s: upperbound age cutoffs (10, 15)
  }  # jj: zE / zS sample selection
  results[[ii]] <- results0
  resultsN[[ii]] <- resultsN0
}  # ii: main / placebo samples
#### results: Results under same obs with BRL (satterthwaith dof) information
#### resultsN: Results under varying number of obs between specifications
#### https://cran.r-project.org/web/packages/qs/vignettes/vignette.html
library(qs)
qsave(results, "../save/DID2024_DaysAbsentEnrollersGenderResults.qs")
qsave(resultsN, "../save/DID2024_N_DaysAbsentEnrollersGenderResults.qs")
DaysAbsent <- data.table(DaysAbsent)
DaysAbsentchg <- data.table(DaysAbsentchg)
setnames(DaysAbsentchg, c("sample", "HHtype", "agHHdef", "demean", "gender", "agelb", "AgNonag", 
  "agHH", "nonagHH", "lb95", "ub95", "pvalue")[-4])
setnames(DaysAbsent, c("sample", "HHtype", "agHHdef", "demean", "gender", "agelb",
   "agHH", "tee", "rate", "Obs")[-4])
qsave(DaysAbsent, "../save/DaysAbsent2024EnrollersGender.qs")
qsave(DaysAbsentchg, "../save/DaysAbsentchg2024EnrollersGender.qs")
