#### Estimation by main/placebo * aghh.defs * age lb 
####    * gender * demeaned/level interaction * HHtype
#### with LiangZeger or Satterthwaite CRSEs.
#### source(paste0(pathprogram, "PartialFile.R"))
library(clubSandwich)
clusterlevel <- "thana"
DivInto2Tables <- T
source("../program/TabGeneric.R")
regressors.list <- list(
    main = regressorsM2024
  , placebo = regressorsP2024
)
zEm.1999 <- qread("../save/zEm1999.qs") 
zEm.1999[, agHH0 :=  as.integer(agHH0 > 0)]
####zEm.1999[, FFWProgram := as.integer(grepl("FFW", anyprog))]
#### In JHR paper, BRAC and Grameen members => program := 0
#### Currently, !grepl("FFW|non-m", anyprog): BRAC, Grameen members => program := 1
#### To get JHR paper: 
#### Comment out below (results in DID2024_MainResults_JHR.qs)
#### zEm.1999[, NonFFWProgram := 
####  as.integer(!is.na(anyprog) & !grepl("FFW|non-m", anyprog))]
#### To get current version:
#### Keeping the below line gives (results in DID2024_MainResults.qs)
####zEm.1999[, NonFFWProgram := 
####  as.integer(!is.na(anyprog) & !grepl("FFW|non-m|BRA|Gram", anyprog))]
#### non ag HHs have no siblings in agriculture...
#### this is how we defined ag HHs.
zEm.1999[survey==1999 & age>=10 & age<=18, 
  .(AgSibM=mean(UDOldAgSibM, na.rm= T), SibM=mean(UDOldSibM)), by = agHH0]
zEp.1999 <- qread("../save/zEp1999.qs")
zEp.2002 <- qread("../save/zEp2002.qs")
zEp.1999[, AgeIn1999 := Age[survey == 2002] - 3, by = uniquid]
table0(zEp.1999[,.(survey, FFEProgram)])
table0(zEp.1999[,.(survey, NonFFEProgram)])
table0(zEp.1999[survey == 2006,.(FFEProgram, NonFFEProgram)])
table0(zEp.1999[survey == 2006,.(program, bothprograms=FFEProgram+NonFFEProgram)])
#### FFEProgram in 2006: FFE program exists & grepl("did get", d7[, X04_q18])
#### NonFFEProgram in 2006: Stipend program exists & grepl("gets", d7[, X04_q17]
#### A quick fix (this should be ideally done in Construct3RoundPanelAndClean.rnw)
#### So: StipendProgram = NonFFE, NonStipendProgram = FFE
zEp.1999[survey == 2006, StipendProgram := NonFFEProgram]
zEp.1999[survey == 2006, NonStipendProgram := FFEProgram]
zEp.2002[survey == 2006, StipendProgram := NonFFEProgram]
zEp.2002[survey == 2006, NonStipendProgram := FFEProgram]

cohort.years.list <- list(# year age is defined
  main = rep(1999, 2), # main: use 1999 age to set age range
  placebo = c(rep(2002, 1), rep(1999, 1))
  # placebo: use 1999 and 2002 age to set age range
  # placebo: cohorts 10-18 in 1999, 10-18 in 2002 are 
  #   tested for impacts between 2002-2006
  )
cutout.years<- c(2006, 1999) # year to drop in data, main = 2006, placebo 1999
#### Used in "interaction with year InterYears" in results table
InterYearsList <- list(main = rep(2002, 2), placebo = rep(2006, 2))
variables.always.use <- paste0("schoolp|Enrolled|^agHH.yr2|^agHH$|", 
  "^thana$|uniqu|^UDnon|^UDfl|^UD.*Sib|^UDhds|^pcland$|^pcnlasset$")
yrXs <- c("yr2", "yr3")
reorder.list <- list(
    main = main.reorder.2024
  , placebo = main.reorder.2024
)
boxWidth <- 4
centerWidth <- 1.3
Enr.Base <- Enrchg.Base <- NULL
samples <- c("main", "placebo")
results <- resultsN <- vector("list", length = length(samples)) # ii
names(results) <- names(resultsN) <- samples
ii <- jj <-  m <- s <- 1
j <- 2; ge <- 3
z234 <- c("z2", "z3", "z4")
zsobj <- c("zmobj", "zpobj")
zmobj <- c("zEm.1999", "zSm.1999")[1]
#### jj: 1,2 = 10-18 in 2002, 3,4 = 10-18 in 1999, 5 = 6-9 in 1999
zpobj <- c("zEp.2002", "zEp.1999")
SkipLowerBound <- 40
for (ii in 1:length(samples)) {
####for (ii in 1) {
  zSobj <- get(zsobj[ii])
  regressorsS <-  regressors.list[[ii]]
  cohort.years <- cohort.years.list[[ii]]
  cutout.year <- cutout.years[ii]
  InterYears <- InterYearsList[[ii]]
  yrX <- yrXs[ii]
  var.always.use <- gsub("yr2", yrX, variables.always.use)
  reorder <- reorder.list[[ii]]
  regsnd <- rep("schoolp", length(regressorsS))
  # k, specification
  est <- res <- vector("list", length = length(regressorsS)) 
  # cl, clustering correction choice
  res <- list("LiangZeger" = res, "Satterthwaite" = res, "WildClusterBoot" = res)
  # m, agHH definition
  res <- list(res, res, res, res)
  names(res) <- aghh.defs
  # ge, gender
  res <- list(boys = res, girls = res, "boys+girls" = res)
  # j, nuclear, extended, extended only HHs
  res <- list("extended" = res, "nuclear" = res) 
  # s, age lowerbound
  res <- list("LB10" = res, "LB11" = res, "LB12" = res)
  # res[[s]][[j]][[ge]][[m]][[clnum]][[k]] is same for each jj in zSobj: 
  #   An element of results0[[jj]]
  results0 <- resultsN0 <- vector("list", length = length(zSobj)) 
  # jj, zE/zS sample selection
  names(results0) <- names(resultsN0) <- zSobj
  for (jj in 1:length(zSobj)) {
    resultsN0[[jj]] <- results0[[jj]] <- res
    cat("\n\n")
    print0(zSobj[jj])
    cat("\n")
    z01 <- changehyphen(get(zSobj[jj]))
    ## dd loop is dropped
      z1 = copy(z01)
      z1[, grepout("dummy[A-Z].*HH0?.yr.$", colnames(z1)) := NULL]
      # keep UDOldSib, UDhdsex, UDnonmuslim, UDflooded as undemeaned levels
      setnames(z1, 
        grepout("UDOldSib|UDhds|UDnon|UDfl", colnames(z1)),
        gsub("UD", "ud", grepout("UDOldSib|UDhds|UDnon|UDfl", colnames(z1))))
      z1[, grepout("^UD", colnames(z1)) := NULL]
      setnames(z1, 
        grepout("^ud", colnames(z1)),
        gsub("ud", "UD", grepout("^ud", colnames(z1))))
      for (s in 1:3)
      #  choice of age cutoff
      {
        s0 <- (10:12)[s]
        i <- paste0("older", s0)
        # latter panel: s <= age < maxAge in 1999/2002
        iiid <- unique(z1[
          s0 <= eval(parse(text = paste0("AgeIn", cohort.years[jj]))) & 
          eval(parse(text = paste0("AgeIn", cohort.years[jj]))) <= maxAge 
          , uniquid])
        # Keep only former complete panel and respective years.
        z2 <- z1[uniquid %in% iiid & survey != cutout.year, ]
        z2[, grepout("exist|In", colnames(z2)) := NULL]
        z2 <- dropunbalanced(z2, returnDT = T)
        # z3: nuclear family
        z3 <- z2[sd == 1, ]
        z3 <- dropunbalanced(z3, returnDT = T)
        z4 <- z2[sd != 1, ]
        z4 <- dropunbalanced(z4, returnDT = T)
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
        for (j in 1:2) {
          zz00 <- get(z234[j])
          setkey(zz00, uniquid, survey)
          zz00[, survey := NULL]
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
            for (m in 1:length(aghh.defs))
            {
              zz = copy(zz0)
              # Use a particular agHH definition.
              # change the name of current ag HH (agHH0, isagHH, ocagHH)
              #   to "agHH"
              setnames(zz, 
                grepout(aghh.defs[m], colnames(zz))
                ,
                gsub(aghh.defs[m], "agHH", grepout(aghh.defs[m], colnames(zz)))
              )
              # drop other ag HH definition
              zz[, grepout(paste0(aghh.defs.regexpr[-m], collapse = "|"), colnames(zz)) := NULL]
              zz[, grepout(paste0("^", aghh.defs[-m], "$", collapse = "|"), colnames(zz)) := NULL]
              ns <- NULL
              resul <- est <- vector("list", length = length(regressorsS))
              # First run: Estimation loop for getting N (number of obs) 
              #   and first-differenced data.
              for (k in 1:length(regressorsS))
              {
                if (s0 == 10 & j == 1 & m == 1) {
                  cat(paste0("(", k, ")\n"))
                  print0(paste0("+ ", 
                    grepout(paste(regressorsS[k], sep = "", collapse = "|"), colnames(zz))))
                }
                regrsr <- paste(regressorsS[1:k], sep = "", collapse = "|")
                # pick covariates for k-th regression: 
                #  paste " ..|.." & "..|.." with collapse = "|" then use it in grepout
                covariates <- grepout(
                  paste(var.always.use, regrsr, sep = "|", collapse = "|")
                  , colnames(zz))
                #if (grepl("zEp.2|zSp", zSobj[jj]))
                # zEp.2002: UDOldSibF is all 0, UDOldSibM is all 0 but 2 obs, so drop them (and interactions).
                #  covariates <- covariates[!grepl("OldSib", covariates)]
                covariates <- covariates[!grepl("^UD|^pc.*[dt]$", covariates)] # drop real valued level variables
                zr <- zz[, covariates, with = F]
                # source("../program/EstimatorFunctions.R")
                rs <- DID1(data.frame(zr), regressand = regsnd[k], 
                   clusterstring = clusterlevel, group = "^uniquid$", 
                   NotToBeDifferenced = "^agHH$|^UD|^pc.*[dt]$",
                   intercept = T, 
                   TimeVariant = "program|age2|meanY",
                   PeriodToDropForLC = 2, 
                   # opposite.time.order: set to F to get t-(t-1) difference. 
                   # (to be used in FDestimation in the later chunk)
                   # Under F, diff(LHS) = -1 if schoolp 1 (1999) -> 0 (2002).
                   # agHH.yr2 is demeaned interaction (of agHH and yr2=1999 dummies), 
                   #   275 of obs (agHH==0) =  .59 because   -.39  (1999)  ->  .20 (2002)
                   #   407 of obs (agHH==1) = -.401 because  .266 (1999) -> -.135 (2002)
                   # In diff data, ag HH who dropped out: 
                   #   LHS = -1, agHH.yr2 = -.4 => OLS estimate > 0.
                   # A larger drop in LHS (more negative) for agHH == 1 dummy 
                   # needs agHH.yr2 to be defined as a positive value.
                   # Similarly, sex (female) == 1 gives diff(sex.yr2) < 0 
                   #   for females in t-(t-1) diff. 
                   # X.yr2 needs to be defind as positive. To do so, in FDestimation, 
                   # one needs to set opposite.time.order = F & all time variant covariates
                   # to be X.yr2 := -1 * X.yr2 [so diff(agHH.yr2) > 0].
                   opposite.time.order = F, # Use t - (t-1) diff
                   TurnFactorToNumeric = T, returnV = T, print.messages = F)
                resul[[k]] <- list(level.data = rs$level, diff.data = rs$diff, 
                  est = rs$est, org.data = zz)
                est[[k]] <- round(rs$est[, -3], 5)
                ns <- c(ns, rs$N)
              }
              if (!any(grepl("latrine.agHH.yr|water.agHH.yr", rownames(est[[k]])))) {
                cat(zSobj[jj], "agelb", s0, z234[j], c("boys", "girls", "boys+girls")[ge], 
                  aghh.defs[m], "\n")
                cat("Skipped, some covariates cannot be used due to too small number of obs:", 
                  nrow(zr), "\n\n")
                next
              }
              # resultsN0: raw results (not under same obs)
              resultsN0[[jj]][[s]][[j]][[ge]][[m]] <- resul
              # First run estimation data is stored in resul.
              # Pick the last item of data list which has the least num of obs. 
              # (This is data to use for all specifications.)
              # zid: Condition: Original data set & regression obs.
              # zid = copy(yzw[uniquid %in% zidd[, uniquid] & survey != cutout.year, ])
              # zidd: Differenced data of the last item in resul.
              # zid2: Level data to reconstruct and demean interaction terms of covariates.
                # Reconstruct covariates and take demeaned interactions are done in the file below.
              source(paste0(pathprogram0, "ReconstructCovariatesForDemeanedInteractions.tex"))
              zidd[, tee := 1]
              enrr <- zid[, .(EnRate = mean(Enrolled), Num = .N), by = .(agHH, tee)]
              Enr.Base <- rbind(Enr.Base, 
                cbind(zSobj[jj], c("all", "direct", "exonly")[j], c("default", aghh.defs[-1])[m], 
                  s0, enrr),
                use.names = F
              )
              # Save mean enrollment rate changes
              # x: agHH, y: nonagHH
              if (any(grepl("LHS", colnames(zidd)))) setnames(zidd, "LHS", "Enrolled")
              ttestE <- t.test(zidd[agHH == 1, Enrolled], zidd[agHH == 0, Enrolled])
              Enrchg.Base <- rbind(Enrchg.Base, 
                cbind(
                    zSobj[jj], c("all", "direct", "exonly")[j], c("default", aghh.defs[-1])[m], 
                    s0, -diff(unlist(ttestE["estimate"])), # -diff = -(y - x) = AgHH - nonagHH
                    t(as.numeric(unlist(lapply(ttestE[c("estimate", "conf.int", "p.value")], round, 4))))
                    )
                  )
              #for (cl in c("LiangZeger", "satterthwaite", "wildclusterboot")) 
              for (cl in c("LiangZeger", "satterthwaite")) 
              {
                Rs <- ns <- NULL
                est <- vector("list", length(regressorsS))
                UseSmallClusterCorrection <- cl
                cat("\n\n###", cl, "###\n\n")
                #if (grepl("Yp|S", zSobj[jj]) & grepl("wild", cl) & any(grepl("Sib", colnames(zidd)))) {
                if (grepl("Yp|S", zSobj[jj]) & grepl("wild", cl)) {
                  cat("fwildclusterboot fails in Julia for zSm.1999, zYp.1999 because Sib", 
                    "covariates are near zero. Skip to next.\n\n" )
                  next
                }
                for (k in 1:length(regressorsS))
                {
                  # Julia fails for specification 6 in zEm.1999, zEp.1999, zEp.2002
                  if (grepl("wild", cl) & k == 6) next
                  #if (ii == 1 & grepl("S", zSobj[jj]) & s >= 1 & m == 4 & k >= 5 & grepl("wild", cl)) 
                  #zSm1999FDOlder10Occ
                  #  next
                  regrsr <- paste(regressorsS[1:k], sep = "", collapse = "|")
                  covariates <- grepout(paste(var.always.use, regrsr, sep = "|"), 
                    colnames(zidd))
                  # var.always.use has level variables used only for destat purpose, so drop them
                  covariates <- covariates[!grepl("^UD|^pc.*[dt]$", covariates)]
                  # Commented out: Aug 2, 2023 Start
                  #if (grepl("zEp|zSp", zSobj[jj]))
                  #  covariates <- covariates[!grepl("OldSib", covariates)]
                  # Commented out: Aug 2, 2023 End
                  zr <- zidd[, c(covariates, "tee"), with = F]
                  # source("../program/EstimatorFunctions.R")
                  rsl <- DID2(dX0 = zr, Regressand = "Enrolled", 
                           Group = "^uniquid$", TimeVar = "tee", Cluster = "thana", 
                           TimeVariant = "program|age2|meanY|yield",
                           opposite.time.order = F, Exclude = "^agHH$", intercept = T, 
                           SmallClusterCorrection = UseSmallClusterCorrection,
                           WCBType = "webb",
                           return.V = T, print.messages = T)
                  if (grepl("satter", UseSmallClusterCorrection)) {
                    # Correct format of estimation results for clubSandwich outputs
                    rsl$est <- as.data.frame(rsl$est)
                    rsl$est <- rsl$est[, -1]
                    colnames(rsl$est)[c(1:2, 4:5)] <- c("Estimate", "Std. Error", "Satt. DoF", "Pr(>|t|)")
                  } else if (grepl("wild", UseSmallClusterCorrection)) {
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
                  if (cl == "satterthwaite") clnum <- 2 else 
                  if (cl == "wildclusterboot") clnum <- 3
                  results0[[jj]][[s]][[j]][[ge]][[m]][[clnum]][[k]] <- 
                    list(est = rsl$est, ci = rsl$CI,
                      df = rsl$reg$df, reg = rsl$reg,
                      #level.data = leveldata[, gsub("Enrolled", "LHS", covariates), with = F], 
                      level.data = zid, 
                      diff.data = rsl$data)
                  est[[k]] <- round(rsl$est[, -3], 5)
                  # Sign reversion is done before FDestimation. Below is redundant.
                  # Take19992002Diff is set to F in "read data chunk" at the beginning
                  # If (t-1) - t difference (opposite time order), 
                  # signs of yrX cross terms are inverted.
                  # if (Take19992002Diff) est[[k]][
                  #   grepout("Inter|yr.$", rownames(est[[k]])), c(1, 3)] <- 
                  #    -1 * est[[k]][grepout("Inter|yr.$", rownames(est[[k]])), c(1, 3)]
                  Rs <- c(Rs, summary(rsl$nonrobust)$adj.r)
                  ns <- c(ns, rsl$N)
                } # k: reg specification
                assign(paste0("addthis", j),
                   rbind("\\hspace{.5em}thana dummies" = 
                      paste0("\\mbox{", c(rep("", length(regressorsS)-1), rep("yes", 1)), "}"),
                     "$\\bar{R}^{2}$" = gsub("^0", "", formatC(Rs, digits = 4, format = "f")),
                     "n" = ns,
                     "control mean at baseline" = 
                       rep(formatC(enrr[tee == 1 & agHH == 0, EnRate], 
                         digits = 2, format = "f"), length(regressorsS)),
                     "control mean at follow up" = 
                       rep(formatC(enrr[tee == 2 & agHH == 0, EnRate], 
                         digits = 2, format = "f"), length(regressorsS)),
                     "treated mean at baseline" =
                       rep(formatC(enrr[tee == 1 & agHH == 1, EnRate], 
                         digits = 2, format = "f"), length(regressorsS)),
                     "treated mean at follow up" =
                       rep(formatC(enrr[tee == 2 & agHH == 1, EnRate], 
                         digits = 2, format = "f"), length(regressorsS)),
                     "raw DID" =
                       rep(formatC(
                       enrr[tee == 2 & agHH == 1, EnRate] - enrr[tee == 1 & agHH == 1, EnRate] 
                       -(enrr[tee == 2 & agHH == 0, EnRate] - enrr[tee == 1 & agHH == 0, EnRate]), 
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
                  OUTformat <- "epc"
                  OUTformat <- "esDoF"
                }
                # Incorporate CI/DoF in table
                # reorder needs to be corrected
                # Tab.Est is in tabulate_est.R
                # source("tabulate_est.R")
                tbest <- Tab.Est(est, reorder, output.in.list = T,
                  Informat = INformat, Outformat = OUTformat, 
                  AddStars = T, 
                  CIInTinySize = T, 
                  LastLineVariables = c("lowMeanY$", "kut.*e.yr.$"),
                  InterWithTexts = paste0(InterYears[jj], c("", "*agricultural household")),
                  DeleteRowStrings = "^p\\$|^se\\$|^CI\\$|^DoF\\$",
                  addbottom = get(paste0("addthis", j)), subst.table = sbt)
                  # Split a table in to 2 tables
                if (DivInto2Tables) {
                  # Split a table in to 2 tables
                  if (grepl("e[ps]$", OUTformat)) 
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
                  iispace21 <- iispace21[iispace21 < max(grep("toilet|water", tbest21))]
                  iispace22 <- iispace21[seq(2, length(iispace21), 2)]
                  if (grepl("e[ps]$", OUTformat)) {
                  # ep, es: 2 rows per estimate
                    AdjustLineSkipRows1 <- iispace11
                    AltColorRows1 <- c(iispace12, iispace12+1)
                    AdjustLineSkipRows2 <- iispace21
                    AltColorRows2 <- c(iispace22, iispace22+1)
                  } else {
                  # epc, esc, satt: 3 rows per estimate
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
                  clCap <- paste0(toupper(substr(cl, 1, 1)), substr(cl, 2, 100))
                  # file path to saved table
                  pathtosavedtable1 <- TabFilePathF(
                    FolderPath = pathsave, 
                    Sample = paste0(gsub("\\.", "", zSobj[jj]), 2024),
                    AgeCutoff = paste0("Older", (10:12)[s]),
                    HHType = paste0(c("Boys", "Girls", "")[ge],
                      c("", "Nuclear", "ExOnly")[j]),
                    AgHHDef = c("", "Is", "Hd", "Occ")[m],
                    CRSEMethod = paste0(clCap, 1)
                    )
                  pathtosavedtable2 <- TabFilePathF(
                    FolderPath = pathsave, 
                    Sample = paste0(gsub("\\.", "", zSobj[jj]), 2024),
                    AgeCutoff = paste0("Older", (10:12)[s]),
                    HHType = paste0(c("Boys", "Girls", "")[ge],
                      c("", "Nuclear", "ExOnly")[j]),
                    AgHHDef = c("", "Is", "Hd", "Occ")[m],
                    CRSEMethod = paste0(clCap, 2)
                    )
                  write.tablev(tbl1, pathtosavedtable1, colnamestrue = F, rownamestrue = F, nastrings = "") 
                  write.tablev(tbl2, pathtosavedtable2, colnamestrue = F, rownamestrue = F, nastrings = "") 
                  cat("Table saved as", pathtosavedtable1, "\n")
                  cat("Table saved as", pathtosavedtable2, "\n")
                } else {
                  # iispace2, iispace2+1, iispace2+2: (group of) rows to be coloured
                  iispace <- which(
                    # rows with \hspace{.5em} and "non-estimate" rows (R2, n, ...)
                    grepl(".", tbest[[1]]) & 
                    !grepl("interaction with|^n$|bar.R|thana dum|mean at|raw DID", tbest[[1]])
                    )
                  iispace2 <- iispace[seq(2, length(iispace), 2)]
                  # iispace, iispace+1: rows i to shrink rowspace between row i+1 to group together
                  # adjlskiprows = c(iispace, iispace+1)
                  tbl <- saveEstTable(tbest[[2]], tbest[[1]], boxWidth, 
                    estimationspacelast = grep("thana dummi", tbest[[1]]),
                    hleft = "\\hfil\\tiny$", hright = "$", 
                    hcenter = c(boxWidth, rep(centerWidth+.15, ncol(tbest[[2]]))), 
                    delimiterline = NULL, adjustlineskip = "-0.5ex", 
                    adjlskiprows = c(iispace, iispace+1),
                    alternatecolorManual = c(iispace2, iispace2+1, iispace2+2),
                    alternatecolorManualColor = "gray80")
                  if (grepl("Liang", cl))
                    tbl <- saveEstTable(tbest[[2]], tbest[[1]], boxWidth, 
                      estimationspacelast = grep("thana dummi", tbest[[1]]),
                      hleft = "\\hfil\\tiny$", hright = "$", 
                      hcenter = c(boxWidth, rep(centerWidth+.15, ncol(tbest[[2]]))), 
                      delimiterline = NULL, adjustlineskip = "-0.5ex", 
                      adjlskiprows = c(iispace),
                      alternatecolorManual = c(iispace2, iispace2+1),
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
                  pathtosavedtable <- TabFilePathF(
                    FolderPath = pathsave, 
                    Sample = paste0(gsub("\\.", "", zSobj[jj]), 2024),
                    AgeCutoff = paste0("Older", (10:12)[s]),
                    HHType = paste0(c("Boys", "Girls", "")[ge],
                      c("", "Nuclear", "ExOnly")[j]),
                    AgHHDef = c("", "Is", "Hd", "Occ")[m],
                    CRSEMethod = paste0(toupper(substr(cl, 1, 1)), substr(cl, 2, 100)))
                  write.tablev(tbl, pathtosavedtable, colnamestrue = F, rownamestrue = F, nastrings = "") 
                  cat("Table saved as", pathtosavedtable, "\n")
                } # if end: DivInto2Tables	
              } # cl: SE clustering option
            }  # m: ag HH definitions
          } # ge: gendered or both gender
        } # j: all children or direct offspring
      } # s: lowerbound age cutoffs (10, 11, 12)
     # dd layer is dropped
  }  # jj: zE / zS sample selection
  results[[ii]] <- results0
  resultsN[[ii]] <- resultsN0
}  # ii: main / placebo samples
#### results: Results under same obs with BRL (satterthwaith dof) information
####    [[ii]][[jj]][[s]][[j]][[ge]][[m]][[clnum]][[k]]
#### resultsN: Results under varying number of obs between specifications
####    [[ii]][[jj]][[s]][[j]][[ge]][[m]][[k]]
#### https://cran.r-project.org/web/packages/qs/vignettes/vignette.html
library(qs)
qsave(results, "../save/DID2024_MainResults.qs")
qsave(resultsN, "../save/DID2024_N_MainResults.qs")
Enr.Base <- data.table(Enr.Base)
Enrchg.Base <- data.table(Enrchg.Base)
setnames(Enrchg.Base, c("sample", "HHtype", "agHHdef", "demean", "age", "AgNonag", 
  "agHH", "nonagHH", "lb95", "ub95", "pvalue")[-4])
setnames(Enr.Base, c("sample", "HHtype", "agHHdef", "demean", "age", "agHH", "tee", "rate", "Obs")[-4])
qsave(Enr.Base, "../save/Enr2024.Base.qs")
qsave(Enrchg.Base, "../save/Enrchg2024.Base.qs")
