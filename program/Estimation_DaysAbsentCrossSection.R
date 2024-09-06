library(clubSandwich)
source("TabGeneric.R")
clusterlevel <- "thana"
DivInto2Tables <- T
SkipLowerBound <- 40
zEm.1999 <- qread("../save/zEm1999.qs")
samples <- c("main", "placebo")
z23 <- c("z2", "z3")
zsobj <- c("zmobjDays", "zpobjDays")
zmobjDays <- c("zEm.1999", "zSm.1999")[1]
zpobjDays <- c("zEp.2002", "zSp.2002", "zEp.1999", "zSp.1999")[1]
cohort.years.list <- list(# year age is defined
  main = rep(1999, 2), 
  placebo = c(rep(2002, 1), rep(1999, 1))
  )
cutout.years<- c(rep(2006, 1), rep(1999, 1), rep(2006, 1)) # year to drop in data, main = 2006, placebo 1999
#### Used in "interaction with year InterYears" in results table
InterYearsList <- list(main = rep(2002, 1), placebo = rep(c(2006, 2002), each = 2))
regressors.list <- list(
  main = regressorsM2024Apr10,
  placebo = regressorsP2024Apr10
)
variables.always.use <- "^DaysAbsent$|^agHH$|^thana$|uniqu"
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
DaysAbsent <- DaysAbsentchg <- CIs <- NULL
results <- resultsN <- vector("list", length = length(samples)) # ii
ii <- jj <- rr <- yy <- m <- s <- ge <- 1
j <- 2
for (ii in 1) {
  zSobj <- get(zsobj[ii])
  # take away all .yr2$ and .*2 from regressorsS
  regressorsS <-  regressors.list[[ii]]
  regressorsS <- gsub("\\.yr.|\\.\\*yr[23]", "", regressorsS)
  regressorsS <- gsub("\\.\\*[23]", "", regressorsS)
  regressorsS <- gsub("\\$", "", regressorsS)
  cohort.years <- cohort.years.list[[ii]]
  cutout.year <- cutout.years[ii]
  InterYears <- InterYearsList[[ii]]
  var.always.use <- gsub("yr[23]", yrXs[ii], variables.always.use)
  reorder <- reorder.list[[ii]]
  sampleyears <- list(c(1999, 2002), c(2002, 2006))[[ii]]
  regsnd <- rep("DaysAbsent", length(regressorsS))
  # Use satterthwaite only.
  est <- res <- vector("list", length = length(regressorsS)) # k, specification
  res <- list(res, res, res, res) # m, agHH definition
  names(res) <- aghh.defs
  res <- list(boys = res, girls = res, "boys+girls" = res) # ge, gender
  res <- list("extended" = res, "nuclear" = res, "exonly" = res) # j, nuclear, extended, extended only HHs
  res <- list("LB10" = res, "LB11" = res, "LB12" = res) # s, age lowerbound
  res <- list("AtLeastOnce" = res, "BothPeriods" = res) # rr, sample by enrollment status
  res <- list("1999" = res, "2002" = res) # yy, cross section year
  # res[[s]][[j]][[ge]][[m]][[k]] is same for each jj in zSobj: An element of results0[[jj]]
  results0 <- vector("list", length = length(zSobj)) # jj, zE/zS sample selection
  for (jj in 1) {
    results0[[jj]] <- res
    cat("\n\n")
    print0(zSobj[jj])
    cat("\n")
    z01 <- changehyphen(get(zSobj[jj]))
    # ss == 2: enrolled in 1999 and 2002. Number of grade progression?
    z01[, ss := cumsum(schoolp), by = uniquid]
    z01[, ss := ss[2], by = uniquid]
    # Choose enrollment status rr: 1=at least once, 2=both rounds
    for (rr in 1:2) {
      # Choose year yy: 1=1999, 2=2002
      for (yy in 1:2) {
        z02 = copy(z01[survey == sampleyears[yy] & schoolp == 1 & ss>=rr & 
          !is.na(sp.edulevel.primary) & !is.na(sp.edulevel.secondary) & !is.na(pcland), ])
        z1 = copy(z02)
        z1[, grepout("dummy[A-Z].*HH0?.yr.$", colnames(z1)) := NULL]
    #       # keep UDOldSib, UDhdsex, UDnonmuslim, UDflooded as undemeaned levels
    #       setnames(z1, 
    #         grepout("UDOldSib|UDhds|UDnon|UDfl", colnames(z1)),
    #         gsub("UD", "ud", grepout("UDOldSib|UDhds|UDnon|UDfl", colnames(z1))))
    #       z1[, grepout("^UD", colnames(z1)) := NULL]
    #       setnames(z1, 
    #         grepout("^ud", colnames(z1)),
    #         gsub("ud", "UD", grepout("^ud", colnames(z1))))
        tabextend <- c("yes", "", "yes", "")
        tabcohortdemeaned <- c("", "yes", "", "yes")
        for (s in 1:3) {
          #  choice of age cutoff
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
          for (j in 2) {
            zz00 = copy(get(z23[j]))
            for (ge in 1:3) {
              if (ge == 1) {
                zz0 = copy(zz00[UDsex <= 0, ]) 
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
              for (m in 1:length(aghh.defs)) {
                zz = copy(zz0)
                # Use a particular agHH definition.
                # change the name of current ag HH (agHH0, isagHH, ocagHH) to "agHH"
                setnames(zz, 
                  grepout(aghh.defs[m], colnames(zz))
                  ,
                  gsub(aghh.defs[m], "agHH", grepout(aghh.defs[m], colnames(zz)))
                )
                # drop other ag HH definitions
                zz[, grepout(paste0(aghh.defs.regexpr[-m], collapse = "|"), colnames(zz)) := NULL]
                zz[, grepout(paste0("^", aghh.defs[-m], "$", collapse = "|"), colnames(zz)) := NULL]
                zz[, grepout(paste0("^UD", aghh.defs[-m], "$", collapse = "|"), colnames(zz)) := NULL]
                # Create X.agHH terms
                # add sex as covariate for UDTerms to have corresponding DemeanedTerms
                zz[, sex := UDsex - mean(UDsex)]
                # 1. Extract interaction terms
                yrXTerms <- grepout("\\.yr.$", colnames(zz))
                agIntTerms <- gsub("\\.yr.", "", grepout("\\.ag.*?\\.yr", yrXTerms))
                agIntHead <- gsub("\\.agHH", "", agIntTerms)
                UDagIntHead <- paste0("UD", agIntHead)
                UDagIntTerms <- paste0("UD", agIntTerms)
                # 2. Create UDagIntTerms (undemeaned interaction) UDX.agHH0 terms
                zz[, (UDagIntTerms) := NA]
                # agHH def is already demeaned
                zz[, agHH := as.integer(agHH>0)]
                for (v in 1:length(UDagIntTerms))
                  set(zz, j = UDagIntTerms[v], value = zz[[UDagIntHead[v]]]*zz[["agHH"]])
                # 4. Create demeaned X terms
                # Restore demeaned terms to form demeaned interactions
                # (Also demean agHH def which is included in DemeanedTerms)
                for (v in 1:length(UDagIntHead))
                  set(zz, j = agIntHead[v], 
                    value = zz[[UDagIntHead[v]]]-mean(zz[[UDagIntHead[v]]]))
                # 4. Create agIntTerms (demeaned X.agHH terms)
                zz[, (agIntTerms) := NA]
                for (v in 1:length(agIntTerms))
                  set(zz, j = agIntTerms[v], value = zz[[agIntHead[v]]]*zz[["agHH"]])
                # drop yrX terms
                zz[, grepout("\\.yr.$", colnames(zz)) := NULL]
                ns <- Rs <- NULL
                resul <- est <- vector("list", length = length(regressorsS))
                for (k in 1:length(regressorsS)) {
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
                  RHS <- covariates[!grepl("^uniquid$|^thana$|^DaysAbsent$|^UD", covariates)]
                  form <- as.formula(paste0(regsnd[k], "~", paste(RHS, collapse = "+")))
                  rsl <- lm(data=data.frame(zr), form)
                  estres <- clubSandwich::coef_test(rsl, vcov = "CR2", 
                    cluster = as.numeric(zr[, thana]), test = "Satterthwaite")
                  estres <- as.data.frame(estres)
                  estres <- estres[, -1]
                  estci <- clubSandwich::conf_int(rsl, vcov = "CR2", level = 0.95, 
                    test = "Satterthwaite", cluster = as.numeric(zr[, thana]), coefs = "All", p_values = T)
                  estci <- as.data.frame(estci)
                  colnames(estres)[c(1:2, 4:5)] <- c("Estimate", "Std. Error", "Satt. DoF", "Pr(>|t|)")
                  estresLZ <- clx(rsl, cluster = matrix(as.numeric(zr[, thana])), 
                    returnV = T, deviation = F)
                  enrr <- zr[, .(MeanDaysAbsent = mean(DaysAbsent), Num = .N), by = agHH]
                  resul[[k]] <- 
                    list(est = summary(rsl)$coeff, estBRL = estres, ciBRL = estci,
                      estLZ = estresLZ$est, ciLZ = estresLZ$ci,
                      df = rsl$df.residual, reg = rsl, 
                      R = summary(rsl)$adj.r, n = nrow(rsl$model),
                      rate = enrr, level.data = zr)
                  est[[k]] <- round(estres[, -3], 5)
                  Rs <- c(Rs, summary(rsl)$adj.r)
                  ns <- c(ns, nrow(rsl$model))
                }
                results0[[jj]][[yy]][[rr]][[s]][[j]][[ge]][[m]] <- resul
                ciBRL <- lapply(resul, "[[", "ciBRL")
                ciBRL <- lapply(ciBRL, as.matrix)
                ciBRL <- lapply(ciBRL, function(x) as.data.table(x[, ]))
                ciBRL <- lapply(1:length(ciBRL), function(i) ciBRL[[i]][, reg := i])
                dfs <- lapply(resul, "[[", "df")
                ciBRL <- lapply(1:length(ciBRL), function(i) ciBRL[[i]][, df := dfs[[i]]])
                ciBRL <- lapply(1:length(ciBRL), function(i) ciBRL[[i]][, n := ns[i]])
                ciBRL <- lapply(1:length(ciBRL), function(i) ciBRL[[i]][, R2 := Rs[i]])
                ciBRL <- rbindlist(ciBRL, use.names = T, fill = T)
                ciBRL[, inference := "BRL"]
                setcolorder(ciBRL,  c("reg", "n", "df", "R2", "inference", "Coef", "beta", "SE", "p_val", "CI_L", "CI_U"))
                esp <- lapply(resul, "[[", "ciLZ")
                esp <- lapply(esp, as.matrix)
                esprn <- unlist(lapply(esp, rownames))
                esp <- lapply(esp, function(x) as.data.table(x[, ]))
                esp <- lapply(1:length(esp), function(i) esp[[i]][, reg := i])
                dfs <- lapply(resul, "[[", "df")
                esp <- lapply(1:length(esp), function(i) esp[[i]][, df := dfs[[i]]])
                esp <- lapply(1:length(esp), function(i) esp[[i]][, n := ns[i]])
                esp <- lapply(1:length(esp), function(i) esp[[i]][, R2 := Rs[i]])
                esp <- rbindlist(esp, use.names = T, fill = T)
                if (any(grepl("z value", colnames(esp)))) esp[, "z value" := NULL]
                estlz <- lapply(resul, "[[", "estLZ")
                estlz <- lapply(estlz, as.matrix)
                estrn <- unlist(lapply(estlz, rownames))
                estlz <- lapply(estlz, function(x) as.data.table(x[, ]))
                estlz <- rbindlist(estlz, use.names = T, fill = T)
                ciLZ <- cbind(Coef = estrn, estlz, esp)
                setnames(ciLZ,  c("Coef", "beta", "SE", "t", "p_val", "CI_L", "CI_U", "reg", "df", "n", "R2"))
                ciLZ[, t := NULL]
                ciLZ[, inference := "LZ"]
                cis <- rbindlist(list(ciBRL, ciLZ), use.names = T)
                cis[, data := zSobj[jj]]
                cis[, HHtype := c("all", "direct")[j]]
                cis[, agdef := aghh.defs[m]]
                cis[, agelb := s0]
                cis[, year := sampleyears[yy]]
                cis[, sample := c("contemporaneous", "all time")[rr]]
                cis[, gender := genderitems[ge]]
                CIs <- rbind(CIs, cis)
                enrr <- zr[, .(MeanDaysAbsent = mean(DaysAbsent), Num = .N), by = agHH]
                DaysAbsent <- rbind(DaysAbsent, 
                  cbind(zSobj[jj], c("all", "nuclear")[j], 
                    sampleyears[yy], c("contemporaneous", "all time")[rr],
                    c("default", aghh.defs[-1])[m], 
                    c("boys", "girls", "boys+girls")[ge],
                    s0, enrr),
                  use.names = F
                )
                # Save mean progression rate changes
                # x: agHH, y: nonagHH
                ttestE <- t.test(zr[agHH == 1L, DaysAbsent], zr[agHH == 0L, DaysAbsent])
                DaysAbsentchg <- rbind(DaysAbsentchg, 
                  cbind(
                      zSobj[jj], c("all", "nuclear")[j], 
                      sampleyears[yy], c("contemporaneous", "all time")[rr],
                      c("default", aghh.defs[-1])[m], 
                      c("boys", "girls", "boys+girls")[ge],
                      s0, round(-diff(unlist(ttestE["estimate"])), 3), # -diff = -(y - x) = AgHH - nonagHH
                      t(as.numeric(unlist(lapply(ttestE[c("estimate", "conf.int", "p.value")], round, 4))))
                      )
                    )
              }  # m: ag HH definitions
            } # ge: gender
          }  # j: z2 (incl. extended) or z3 (nuclear)
        }  # s: upperbound age cutoffs (10, 15)
      } # yy: 1999 or 2002
    } # rr: 1 contemporanesou enroller or 2 all time enroller
  }  # jj: zE / zS sample selection
  results[[ii]] <- results0
}  # ii: main / placebo samples
#### results: Results under same obs with BRL (satterthwaith dof) information
#### inference is not given unique level clnum. Each k spec has ciLZ and ciBRL.
#### results[[ii]][[jj]][[yy]][[rr]][[s]][[j]][[ge]][[m]]
#### resultsN: Results under varying number of obs between specifications
#### https://cran.r-project.org/web/packages/qs/vignettes/vignette.html
setcolorder(CIs,  c("data", "sample", "year", "HHtype", "agdef", "agelb", "gender", "reg", "n", "df", "R2", "inference", 
   "Coef", "beta", "SE", "p_val", "CI_L", "CI_U"))
thesecols <- c("n", "df", "R2", "beta", "SE", "p_val", "CI_L", "CI_U")
CIs[, (thesecols) := lapply(.SD, as.numeric), .SDcols = thesecols]
thesecols <- colnames(CIs)[!colnames(CIs) %in% thesecols]
CIs[, (thesecols) := lapply(.SD, as.factor), .SDcols = thesecols]
library(qs)
qsave(results, "../save/DID2024_DaysAbsentCrossSectionResults.qs")
qsave(CIs, "../save/DID2024_DaysAbsentCrossSectionCIs.qs")

DaysAbsent <- data.table(DaysAbsent)
DaysAbsentchg <- data.table(DaysAbsentchg)
setnames(DaysAbsentchg, c("data", "HHtype", "year", "sample", "agHHdef", 
  "gender", "agelb", "AgNonag", "agHH", "nonagHH", "lb95", "ub95", "pvalue"))
setnames(DaysAbsent, c("data", "HHtype", "year", "sample", "agHHdef", "gender", "agelb",
   "agHH",  "rate", "Obs"))
DaysAbsent[, gender := factor(gender, levels=genderitems)]
DaysAbsent[, agHHdef := factor(agHHdef, levels=aghh.defs)]
qsave(DaysAbsent, "../save/DaysAbsent2024CrossSection.qs")
qsave(DaysAbsentchg, "../save/DaysAbsentchg2024CrossSection.qs")
