#### Age groups tried. Broader grouping is more efficient.
#### AgeGroup1 <- list(pri=6:10, jsec=11:13, sec=14:15, hsec=16:17, coll=18)
#### AgeGroup2 <- list(pri=6:10, sec=11:17, coll=18)
#### AgeGroup3 <- list(young=6:9, junior=10:15, senior=16:18)
library(clubSandwich)
library(fwildclusterboot)
SkipLowerBound <- 40
clusterlevel <- "thana"
source("../program/TabGeneric.R")
zEm.1999 <- qread("../save/zEm1999.qs")
zEm.1999[, agHH0 :=  as.numeric(agHH0 > 0)]
zEm.1999[, FFWProgram := as.integer(grepl("FFW", anyprog))]
zEm.1999[, NonFFWProgram := as.integer(!is.na(anyprog) & !grepl("FFW|non-m", anyprog))]
zEp.1999 <- qread("../save/zEp1999.qs")
zEp.2002 <- qread("../save/zEp2002.qs")
zEp.1999[, AgeIn1999 := Age[survey == 2002] - 3, by = uniquid]
zf <- unique(zEm.1999[, .(uniquid, AgeGroup1999, Agegroup1999)])
zf[, AgeGroup1999 := droplevels(AgeGroup1999)]
zf[, Agegroup1999 := droplevels(Agegroup1999)]
setkey(zf, uniquid)
ze <- unique(zEm.1999[, .(uniquid, Edu1999)])
ze <- ze[!grepl("other", Edu1999), ]
ze[, Edu1999 := droplevels(Edu1999)]
setkey(ze, uniquid)
z23 <- c("z2", "z3")
#### Edu1999 is actual class in 1999 which is not defined for outofschool children.
####    6-10: Primary (primary0103, primary0405)
####  11-13: Junior secondary (secondary 0608)
####  14-15: Secondary (secondary0912)
####  16-17: Higher secondary (secondary0912)
#### Supposed school age: Class to be enrolled under official school system schedule
#### But this is just another labeling of age.
#### In class wise regressions, default category becomes Edu1999 = NA and schoolp = 0.
#### This is a group of children who are out of school for long period (so they do not
#### provide class information). This is not a natural reference category, 
#### so we may drop the class wise estimation.
#### We can leave class wise regressions in the code for now.
##### Age wise agHH.yrX was created in Construct3RoundPanel_JHR.rnw
####  zEm.1999: Main, exist sample.
####  zSm.1999: Main, schooling sample.
####  zEp.1999: Placebo, 2002 shocks on 1999 cohort.
####  zEp.2002: Placebo, 2002 shocks on 2002 cohort.
####  zYp.1999: Placebo, 2002 shocks on young 1999 cohort.
for (zob in c("zEm.1999", "zEp.1999", "zEp.2002")) {
  zdata <- get(zob)
  for (agstring in aghh.defs) {
    for (aa in 6:maxAge) {
      if (grepl("m", zob)) # zEm.1999, zSm.1999
      {
        zdata[, dumage := as.numeric(eval(parse(text=paste0("Age==", aa, ""))))]
        zdata[, dumage := dumage - mean(dumage, na.rm = T)]
        zdata[, (paste0("DummyAgeIn1999.", putzeroontop(aa, F, 2), ".yr2")) :=
          eval(parse(text="(survey == 1999)*dumage"))] 
        # agHH.yr2 * dumage
        zdata[, (paste0("DummyAgeIn1999.", putzeroontop(aa, F, 2), ".", agstring, ".yr2")) :=
          eval(parse(text=paste0(agstring, ".yr2*dumage")))] 
      } else if (grepl("p.1", zob)) # zEp.1999, zYp.1999
      {
        zdata[, dumage := as.numeric(eval(parse(text=paste0("Age==", aa, ""))))]
        zdata[, dumage := dumage - mean(dumage, na.rm = T)]
        zdata[, (paste0("DummyAgeIn1999.", putzeroontop(aa, F, 2), ".yr3")) :=
          eval(parse(text="(survey == 2002)*dumage"))] 
        zdata[, (paste0("DummyAgeIn1999.", putzeroontop(aa, F, 2), ".", agstring, ".yr3")) :=
          eval(parse(text=paste0(agstring, ".yr3*dumage")))] 
      } else { # zEp.2002
        zdata[, dumage := as.numeric(eval(parse(text=paste0("Age==", aa, ""))))]
        zdata[, dumage := dumage - mean(dumage, na.rm = T)]
        zdata[, (paste0("DummyAgeIn2002.", putzeroontop(aa, F, 2), ".yr3")) :=
          eval(parse(text="(survey == 2002)*dumage"))] 
        zdata[, (paste0("DummyAgeIn2002.", putzeroontop(aa, F, 2), ".", agstring, ".yr3")) :=
          eval(parse(text=paste0(agstring, ".yr3*dumage")))] 
      }
    }
  }
  assign(zob, zdata)
}
zsobj <- c("zEm.1999", "zEp.2002", "zEp.1999", "zYp.1999")[-4]
names(zsobj) <- c("main", "placebo2", "placebo9", "placeboY")[-4]
#### Data vector elements. 
#### Except for Y, each has 3 elements because there are 3 age groupings.
#### zEm.1999: 1999 shocks on 1999 cohorts (agewise, AgeGroup, Agegroup)
#### zEp.2002: 2002 shocks on 2002 cohorts (agewise, AgeGroup, Agegroup)
#### zEp.1999: 2002 shocks on 1999 cohorts (agewise, AgeGroup, Agegroup)
#### zYp.1999: 2002 shocks on young 1999 cohorts
cohort.years.list <- list(
    main = 1999
  , placebo2 = 2002
  , placebo9 = 1999
)
#### year to drop from data
cutout.years <- c(2006, 1999, 1999, 1999)[-4]
InterYearsList <- list(
    main = 2002
  , placebo2 = 2006
  , placebo9 = 2002
)
yrXs <- c("yr2", "yr3", "yr3", "yr3")[-4]
ShockYears <- c(1999, 2002, 2002, 2002)[-4]
AGEgrouping <- c("agewise", "AgeGroup1", "AgeGroup2", "AgeGroup3")[-c(1, 2)]
agewise <- as.list(6:18); names(agewise) <- putzeroontop(6:18)
AgeGroup1 <- list(pri=6:10, jsec=11:13, sec=14:15, hsec=16:17, coll=18)
AgeGroup2 <- list(pri=6:10, sec=11:17, coll=18)
AgeGroup3 <- list(young=6:9, junior=10:15, senior=16:18)
  # AgeGroup2 = Agegroup: 6-10, 11-17, above17
  # AgeGroup3 = Agegroup: 6-10, 11-15, above16
variables.always.use <- "schoolp|Enrolled|^agHH.yr2|^agHH$|^thana$|uniqu|^UDO|UDhds"
#### year to interact with agHH. yr2 is yr1-yr2 diff, yr3 is yr2-yr3 diff.
reorder.list <- list(
    main = main.reorder.2024
  , placebo = main.reorder.2024
  , placebo2 = main.reorder.2024
)
regressors.list <- list(
  main = regressorsM2024,
  placebo2 = regressorsP2024,
  placebo9 = regressorsP2024
)
#### EnrCV.age: enrollment rates by agHH*agegroup*period
#### EnrCVchg.age: enrollment rate changes by agHH*agegroup*period
Enr.Agegroup <- Enrchg.Agegroup <- NULL
boxWidth <- 4
centerWidth <- 1.2
old <- F
ii <- jj <- dd <- m <- 1
j <- 2; ge <- 3
results <- vector("list", length(zsobj)) # ii
names(results) <- names(zsobj)
#for (ii in 1:length(zsobj)) {
for (ii in 1) {
  z001 <- changehyphen(get(zsobj[ii]))
  regressorsS <-  regressors.list[[ii]]
  cohort.years <- cohort.years.list[[ii]] # tested on cohort 1999 or cohort 2002
  cutout.year <- cutout.years[ii]
  InterYears <- InterYearsList[[ii]]
  reorder <- reorder.list[[ii]]
  yrxYear <- ShockYears[ii] # Supposed shock year: 1999 or 2002
  yrX <- yrXs[ii]  # Supposed shock year in yrY description: yr2 or yr3
  resge <- list(boys = NULL, girls = NULL, "boys+girls" = NULL) # j
  for (ge in 1:3) {
    if (ge == 1) z01 <- z001[sex <= 0, ] else if (ge == 2) z01 <- z001[sex > 0, ] else z01 <- z001
    resdd <- list(demeaned = NULL, undemeaned = NULL)
      z1 = copy(z01)
      # keep UDOldSib, UDhdsex, UDnonmuslim, UDflooded as undemeaned levels
      setnames(z1, 
        grepout("UDOldSib|UDhds|UDnon|UDfl", colnames(z1)),
        gsub("UD", "ud", grepout("UDOldSib|UDhds|UDnon|UDfl", colnames(z1))))
      z1[, grepout("^UD", colnames(z1)) := NULL]
      setnames(z1, 
        grepout("^ud", colnames(z1)),
        gsub("ud", "UD", grepout("^ud", colnames(z1))))
      z1[, grepout("^UD", colnames(z1)) := NULL]
      z1[, village := factor(gsub(" ", "", village))]
      z1[, grepout("exist|\\-\\d$|DummyAge", colnames(z1)) := NULL]
      resm <- vector("list", length = length(aghh.defs)) # m
      names(resm) <- aghh.defs
      for (m in 1:4) {
        z11 = copy(z1)
        # change the name of current ag HH (agHH0, isagHH, ocagHH) to "agHH"
        setnames(z11, 
          grepout(aghh.defs[m], colnames(z1))
          ,
          gsub(aghh.defs[m], "agHH", grepout(aghh.defs[m], colnames(z11)))
        )
        # drop other ag HH definition
        z11[, grepout(paste0(aghh.defs.regexpr[-m], collapse = "|"), colnames(z11)) := NULL]
        # change yr2 => yr3 if placebo, yr3 => yr2 if main
        var.always.use <- variables.always.use
        if (grepl("yr2", yrX)) 
          var.always.use <- gsub("yr3", "yr2", var.always.use) else
          var.always.use <- gsub("yr2", "yr3", var.always.use)
        z2 <- z11[survey != cutout.year, ]
        # Drop yrX other than yrx
        if (any(grepl(unique(yrXs[yrX != yrXs]), colnames(z2))))
          z2[, grepout(unique(yrXs[yrX != yrXs]), colnames(z2)) := NULL]
        # Do not drop agHH.yrX becase we use it as the regressor of reference category
        z2 <- dropunbalanced(z2, returnDT = T)
        z3 <- z2[sd == 1, ]
        z3 <- dropunbalanced(z3, returnDT = T)
        resj <- vector("list", length = 2) # j
        names(resj) <- c("all", "direct")
        regsnd <- rep("schoolp", length(regressorsS))
        for (j in 1:2) {
          zz0 <- get(z23[j])
          setkey(zz0, uniquid, survey)
          zz = copy(zz0)
          resgg <- vector("list", length = length(AGEgrouping)) # gg
          names(resgg) <- AGEgrouping
          for (gg in 1:length(AGEgrouping)) {
            AGEgroup <- get(AGEgrouping[gg])  # agewise, AgeGroup1, AgeGroup2
            aghhvar <- aghh.defs[1]
            ns <- NULL
            cat("\n\n")
            print0(zSobj[gg])
            cat("\n")
            cat(AGEgrouping[gg])
            cat("\n\n")
            print(grepout("agHH\\.|Age", grepout(var.always.use, colnames(z3))))
            resag <- vector("list", length = length(AGEgroup)) # ag
            names(resag) <-  names(AGEgroup)
            for (ag in 1:length(AGEgroup))
            {
              # target ages: minAge - maxAge in cohort.years
              iiid <- unique(z2[eval(parse(text = paste0("AgeIn", cohort.years))) >= min(AGEgroup[[ag]]) 
                & eval(parse(text = paste0("AgeIn", cohort.years))) <= max(AGEgroup[[ag]]), uniquid]) 
              zzg <- zz[uniquid %in% iiid, ]
              if (nrow(zzg) < SkipLowerBound) {
                cat("Skipped due to small number of obs:", nrow(zzg), "\n")
                next
              }
              ns <- NULL
              resul <- est <- vector("list", length = length(regressorsS))
              for (k in 1:length(regressorsS))
              {
                regrsr <- paste(regressorsS[1:k], sep = "", collapse = "|")
                covariates <- grepout(paste(var.always.use, regrsr, sep = "|"), colnames(zzg))
                print0(paste0("+ ", grepout(regressorsS[k], colnames(z3))))
                zr <- zzg[, covariates, with = F]
                # source("EstimatorFunctions.R")
                rs <- DID1(data.frame(zr), regressand = regsnd[k], 
                    clusterstring = clusterlevel, group = "^uniquid$", 
                    NotToBeDifferenced = "^agHH$|^UD|^pc.*[dt]$",
                    intercept = T, PeriodToDropForLC = 2, 
                    TimeVariant = "program|age2|meanY",
                    opposite.time.order = F,
                    TurnFactorToNumeric = T, returnV = T, print.messages = F)
                resul[[k]] <- list(level.data = rs$level, diff.data = rs$diff, est = rs$est)
                est[[k]] <- round(rs$est[, -3], 5)
                ns <- c(ns, rs$N)
              }
                # Reconstruct covariates and take demeaned interactions are done in the file below.
              source(paste0(pathprogram0, "ReconstructCovariatesForDemeanedInteractions.tex")) 
              zt = copy(zz) # zz is z2/z3
              if (any(grepl("schoolp", colnames(zt)))) setnames(zt, "schoolp", "Enrolled")
              # save mean enrollment rate changes
              zt[, aghh := 1L]
              zt[agHH <= 0, aghh := 0L]
              zt[, tee := 1]
              zt[survey == max(survey), tee := 2]
              enrr <- zt[, .(EnRate = mean(Enrolled), Obs = .N), 
                by = .(aghh, tee)]
              d0 <- zt[aghh == 0L, .(diff = diff(Enrolled)), by = uniquid][, diff]
              d1 <- zt[aghh == 1L, .(diff = diff(Enrolled)), by = uniquid][, diff]
              ttestE <- t.test(d1, d0)
              Enr.Agegroup <- rbind(Enr.Agegroup, 
                cbind(zSobj[jj], aghh.defs[m],  
                  names(resj)[j], AGEgrouping[gg], names(AGEgroup)[ag], names(resge)[ge],
                  enrr)
                , use.names = T, fill = T)
              Enrchg.Agegroup <- rbind(Enrchg.Agegroup, 
                t(c(zSobj[jj], aghh.defs[m], 
                  names(resj)[j], AGEgrouping[gg], names(AGEgroup)[ag], names(resge)[ge],
                  round(-diff(unlist(ttestE["estimate"])), 3),
                  unlist(lapply(ttestE[c("estimate", "conf.int", "p.value")], round, 4))))
                , use.names = F)
              if (any(grepl("LHS", colnames(zidd)))) setnames(zidd, "LHS", "Enrolled")
              # Drop covariates not used in 1st run for zYp.1999 data, because they are all zero's.
              # Covariates of: AgeIn1999.06, AgeIn1999.07.
              iiAllZero <- sapply(zidd, function(x) all(x == 0))
              zidd <- zidd[, !iiAllZero, with = F]
              zidd[, tee := 1] # redundant but needed in FDestimation.
              res <- vector("list", length(ClusteringMethod)) # ii
              names(res) <- names(ClusteringMethod)
              for (cl in ClusteringMethod[-3]) {
                Rs <- ns <- NULL
                est <- vector("list", length(regressorsS))
                UseSmallClusterCorrection <- cl 
                cat("\n\n###", cl, "###\n\n")
                est <- res <- resul <- vector("list", length = length(regressorsS)) # k
                res <- rep(list(res), length(ClusteringMethod)) # cl: 
                names(res) <- ClusteringMethod
                clnum <- 1 
                if (grepl("satt", cl)) clnum <- 2
                # res[[cl]][[k]]: this is stored for each cl in resge[[ag]]
                # resultsN: raw results (not under same obs)
                for (k in 1:length(regressorsS)) {
                  regrsr <- paste(regressorsS[1:k], sep = "", collapse = "|")
                  covariates <- grepout(paste(var.always.use, regrsr, sep = "|"), 
                    colnames(zidd))
                  # var.always.use has level variables used only for destat purpose, so drop them
                  covariates <- covariates[!grepl("^UD|^pc.*[dt]$", covariates)]
                  zr <- zidd[, c(covariates, "tee"), with = F]
                  # zidd took t - (t-1) difference, so schoolp is usually 0 or -1 (1 in 1999, 0 in 2002).
                  # In our estimation, we take (t-1) - t difference.
                  # source("EstimatorFunctions.R")
                  rsl <- DID2(dX0 = zr, Regressand = "Enrolled", 
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
                  }
                  # results0: results under same obs
                  res[[clnum]][[k]] <- list(
                    est = rsl$est, ci = rsl$CI,
                    df = rsl$reg$df, reg = rsl$reg,
                    level.data = z2[uniquid %in% zidd[, uniquid], gsub("Enrolled", "schoolp", covariates), with = F], 
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
                       rep(formatC(enrr[tee == 1 & aghh == 0, EnRate], 
                         digits = 2, format = "f"), length(regressorsS)),
                     "control mean at follow up" = 
                       rep(formatC(enrr[tee == 2 & aghh == 0, EnRate], 
                         digits = 2, format = "f"), length(regressorsS)),
                     "treated mean at baseline" =
                       rep(formatC(enrr[tee == 1 & aghh == 1, EnRate], 
                         digits = 2, format = "f"), length(regressorsS)),
                     "treated mean at follow up" =
                       rep(formatC(enrr[tee == 2 & aghh == 1, EnRate], 
                         digits = 2, format = "f"), length(regressorsS)),
                     "raw DID" =
                       rep(formatC(
                       enrr[tee == 2 & aghh == 1, EnRate] - enrr[tee == 1 & aghh == 1, EnRate] 
                       -(enrr[tee == 2 & aghh == 0, EnRate] - enrr[tee == 1 & aghh == 0, EnRate]), 
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
                # source("tabulate_est.R")
                tbest <- Tab.Est(est, reorder, output.in.list = T,
                  Informat = INformat, Outformat = OUTformat, 
                  AddStars = T, #TableFormat = tabformat,
                  LastLineVariables = c("lowMeanY$", "kut.*e.yr.$"),
                  InterWithTexts = paste0(InterYears, c("", "*agricultural household")),
                  DeleteRowStrings = "^p\\$|^se\\$|^CI\\$|^DoF\\$",
                  addbottom = get(paste0("addthis", j)), subst.table = sbt)
                  # If base::":"(from, to) error, check reorder.
                # Split a table in to 2 tables
                tbest11 <- tbest[[1]][1:(grep(paste0("inter.*", InterYears, ".*ag"), tbest[[1]])-3)]
                tbest12 <- tbest[[2]][1:(grep(paste0("inter.*", InterYears, ".*ag"), tbest[[1]])-3), ]
                tbest21 <- tbest[[1]][grep(paste0("inter.*", InterYears, ".*ag"), tbest[[1]]):length(tbest[[1]])]
                tbest22 <- tbest[[2]][grep(paste0("inter.*", InterYears, ".*ag"), tbest[[1]]):length(tbest[[1]]), ]
                iispace11 <- which(
                  grepl(".", tbest11) & 
                  !grepl("interaction with|^n$|bar.R|thana dum|mean at|raw DID", tbest11)
                  )
                if (nrow(tbest12) == max(iispace11)) iispace11 <- iispace11[iispace11 != max(iispace11)]
                iispace12 <- iispace11[seq(2, length(iispace11), 2)]
                iispace21 <- which(
                  grepl(".", tbest21) & 
                  !grepl("interaction with|^n$|bar.R|thana dum|mean at|raw DID", tbest21)
                  )
                if (nrow(tbest22) == max(iispace21)) iispace21 <- iispace21[iispace21 != max(iispace21)]
                # drop last rows of tbest2 to shrink row space
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
                  hleft = "\\hfil\\tiny$", hright = "$", 
                  hcenter = c(boxWidth, rep(centerWidth+.15, ncol(tbest[[2]]))), 
                  delimiterline = NULL, adjustlineskip = "-0.7ex", 
                  adjlskiprows = AdjustLineSkipRows1,
                  alternatecolorManual = AltColorRows1,
                  alternatecolorManualColor = "gray80")
                tbl2 <- saveEstTable(tbest22, tbest21, boxWidth, 
                  estimationspacelast = grep("thana dummi", tbest21),
                  hleft = "\\hfil\\tiny$", hright = "$", 
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
                    # \multicolumn{5}{l}{\makebox[Xcm]{inter with A}} \\\rowcolor{}
                    # => \multicolumn{5}{l}{\makebox[10cm]{\textit{inter with A}\hfill}}\\[.5ex]\rowcolor{}
                    tbl2[ir] <- gsub("makebox\\[.cm\\]", "makebox[10cm]", tbl2[ir])
                    tbl2[ir] <- gsub("(\\\\textit\\{.*?\\})", "\\1\\\\hfill", tbl2[ir])
                    tbl2[ir] <- gsub("\\\\rowcolor", "[.5ex]\\\\rowcolor", tbl2[ir])
                }
                clCap <- paste0(toupper(substr(cl, 1, 1)), substr(cl, 2, 100))
                pathtosavedtable1 <- TabFilePathF(
                  FolderPath = pathsave, 
                  Sample = paste0(gsub("\\.", "", zSobj[jj]), 2024),
                  AgeCutoff = paste0(AGEgrouping[gg], names(AGEgroup)[ag]),
                  HHType = paste0(c("", "Nuclear")[j], c("Boys", "Girls", "")[ge]),
                  AgHHDef = "",
                  CRSEMethod = paste0(clCap, 1)
                )
                pathtosavedtable2 <- TabFilePathF(
                  FolderPath = pathsave, 
                  Sample = paste0(gsub("\\.", "", zSobj[jj]), 2024),
                  AgeCutoff = paste0(AGEgrouping[gg], names(AGEgroup)[ag]),
                  HHType = paste0(c("", "Nuclear")[j], c("Boys", "Girls", "")[ge]),
                  AgHHDef = "",
                  CRSEMethod = paste0(clCap, 2)
                )
                write.tablev(tbl1, pathtosavedtable1, colnamestrue = F, rownamestrue = F, nastrings = "") 
                write.tablev(tbl2, pathtosavedtable2, colnamestrue = F, rownamestrue = F, nastrings = "") 
                cat("Table saved as", pathtosavedtable1, "\n")
                cat("Table saved as", pathtosavedtable2, "\n")
              } # cl: SE clustering option
              # res has [[clnum]][[k]] levels for each ag.
              # resag has [[ag]][[clnum]][[k]] levels.
              resag[[ag]] <- res
            } # ag: age group
            # resgg has [[gg]][[ag]][[clnum]][[k]] levels.
            resgg[[gg]] <- resag
          } # gg: AGEgrouping
          # resj has [[j]][[gg]][[ag]][[clnum]][[k]] levels.
          resj[[j]] <- resgg
        } # j: household type
        # resm has [[m]][[j]][[gg]][[ag]][[clnum]][[k]] levels.
        resm[[m]] <- resj
      } # m: agHH def
    # resge has [[ge]][[j]][[gg]][[ag]][[clnum]][[k]] levels.
    resge[[ge]] <- resm
  } # ge: gender: 1 = boys, 2 = girls, 3 = boys+girls
  # results has [[ii]][[ge]][[m]][[j]][[gg]][[ag]][[clnum]][[k]] levels.
  results[[ii]] <- resge
} # ii: main/placebo
library(qs)
qsave(results, "../save/DID2024_SubsampleAgeGroupGenderResults.qs")
Enr.Agegroup <- data.table(Enr.Agegroup)
Enrchg.Agegroup <- data.table(Enrchg.Agegroup)
setnames(Enrchg.Agegroup, c("sample", "agdef", "demean", "HHtype", 
  "AgeGrouping", "ages", "gender",
  "AgNonag", "agHH", "nonagHH", "lb95", "ub95", "pvalue")[-3])
setnames(Enr.Agegroup, c("sample", "agdef", "demean", "HHtype", "AgeGrouping", 
  "ages", "gender", "agHH", "survey", "rate", "Obs")[-3])
Enr.Agegroup[, gender := factor(gender, levels = genderitems)]
Enr.Agegroup[, ages := factor(ages)]
Enr.Agegroup[, AgeGrouping := factor(AgeGrouping)]
Enr.Agegroup[, HHtype := factor(HHtype)]
Enr.Agegroup[, agdef := factor(agdef)]
Enr.Agegroup[, sample := factor(sample)]
qsave(Enr.Agegroup, "../save/Enr2024.AgegroupGender.qs")
qsave(Enrchg.Agegroup, "../save/Enrchg2024.AgegroupGender.qs")
Enr.Agegroup <- qread("../save/Enr2024.AgegroupGender.qs")
