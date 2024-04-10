changehyphen <- function(z)
{
cn <- colnames(z)
cn <- strsplit(cn, "_")
for (i in 1:length(cn))
  if (length(cn[[i]]) == 2) cn[[i]] <- paste(cn[[i]][1], cn[[i]][2], sep = ".") else
  if (length(cn[[i]]) == 3) cn[[i]] <- paste(cn[[i]][1], cn[[i]][2], cn[[i]][3], sep = ".")
colnames(z) <- asc(cn)
return(z)
}
asn <- function(x) as.numeric(asc(x))
asl <- function(x) as.logical(asc(x))
tonumeric <- function(x) {
#  change factor or logical variables to numeric
    if (is.factor(x)) x <- asn(x)
    if (is.logical(x)) x <- asl(x) + 0
    return(x)
}
DID1 <- function(X, group, regressand, clusterstring = NULL, intercept = T, 
  exclude = NULL, NotToBeDifferenced = NULL, LevelCovariates = NULL, 
  TimeVariant = NULL, PeriodToDropForLC = NULL, opposite.time.order = F,  
  TurnFactorToNumeric = T, returnV = F, 
  returnLevelData = F, returnDiffData = F, print.messages = T)
# Data preparation:
#   NotToBeDifferenced is set to isolate ID columns.
#   LevelCovariates is set to add as covariates. 
#     Since FD reduces time dimension from T to T-1, 
#     need to merge x[-T, ] or x[-1, ] matrix.
#   PeriodToDropForLC (which of level covariates) specifies 
#     which period (1st or Tth) to be dropped.
#   TurnFactorToNumeric = T allows binary factor to 0/1 numeric.
#   clusterstring: Variable is converted to a numeric vector.
# Estimation:
#   data.table is used for first-differencing. Faster than data.frame.
#   "exclude" vector selects covariate specification. K vector => K regressions.
#   (plm does not directly accept different level of clustering than panel grouping, so not used.)
# Covariance estimation:
#   a wrapper for clx of Arai 2000 after first differencing using data.table
#   ** for the time being, we assume T = 2.
#   ** can extend for T > 2 but need to examine contiguous time periods to be observed.
#  -- Arguments --
#  X: data object
#  group: regexp for the grouping variable (must be a single numeric column)
#  regressand: LHS of regression (internally, column name is changed to "LHS")
#  clusterstring: column name for clustering variable, if NULL no clustring of SE
#  exclude: a vector of regexp for columns to be excluded from regressor
#  NotToBeDifferenced: regexp for covariates to be excluded from first-differencing 
#    (to avoid an error in lapply(.SD, diff) in data.table)
#  LevelCovariates: reg exp for covariates in X to be added as a level (x_{t-1} level for Delta y_{t})
#  TimeVariant: reg exp for time-varying variables (not cross term with time dummies)
#  PeriodToDropForLC: an integer in [1, T] to be dropped in level covariates, x[-PeriodToDropForLC, ].
#  opposite.time.order: if T, take diff t-1 - (t), so signs will be reversed. If F, take t - (t-1) diff.
#  TurnFactorToNumeric: if T, turn the binary varables to numeric according to levels(x) - 1
#  returnV: if T, return covariance matrix
#  returnLevelData: if returnV==T, return undifferenced data matrix (y and X)
#  returnDiffData: if returnV==T, return differenced data matrix (y and X). May have less obs than LevelData because we drop NAs only after differencing.
#  print.messages: if T, "dropped X obs due to NA" is printed.
#  -- Outputs --
#  est: regression estimation object (lm)
#  V: covariance matrix
#  level.data: Data matrix before differencing
#  diff.data: Data matrix after differencing and NA dropping
#  reg: Estimation results
#  groupname: Group string.
#  omitgroupid: IDs dropped due to NA.
#  dropped.rows: Dropped row numbers in level.data due to NA.
#  FD.time.order: t - (t-1) or (t-1) - t
#  N: Number of obs
#  nonrobust: Results without robustifying standard errors
{
  #  Keep exclude covariates.
  if (!is.null(exclude)) X2 <- X[, -grep(exclude, colnames(X))] else X2 <- X
  #  logical => numeric
  iil <- sapply(X2, is.logical)
  if (any(iil) & TurnFactorToNumeric) X2[iil] <- lapply(X2[iil], tonumeric)
  #  is.factor(x) & length(levels(x)) == 2 is binary. If TurnFactorToNumeric == T, factor => numeric.
  iif <- sapply(X2, is.factor)
  if (any(iif)) {
    iibinary <- sapply(X2[iif], function(x) length(levels(x)) == 2)
    if (any(iibinary) & TurnFactorToNumeric) 
    X2[iibinary] <- lapply(X2[iibinary], tonumeric)
  }
  #  Regressand => "LHS"
  colnames(X2)[grep(paste0("^", regressand, "$"), colnames(X2))] <- "LHS"
  #  Take a first difference to eliminate fixed effects
  require("data.table")
  dX0 <- data.table(X2)
  groupstring <- grepout(group, colnames(X2))
  setkeyv(dX0, groupstring)
  # omit ID and level columns from first differencing
    #  from data.table FAQ file 2.1
    # dX0[, lapply(.SD, diff), by = groupstring, .SDcols = c("schoolp", "agHH.yr2", "yr2")]
    # dX0 <- dX0[, lapply(.SD, diff), by = groupstring]
    # But by specifying the ID columns, column position changes.
    # Need to use 1:length(IDretaincols) to specify the rerodered columns.
    # IDretaincols is a numeric vector of column positions of IDs and retained variables
  clst <- paste0("^", clusterstring, "$")
  IDcols <- grep(paste0(c(group, clst), collapse = "|"), colnames(dX0))
  if (any(!is.null(c(NotToBeDifferenced, LevelCovariates)))) 
    retaincols <- grep(paste0(c(NotToBeDifferenced, LevelCovariates), collapse = "|"), 
                      colnames(dX0)) else
    retaincols <- NULL
  IDretaincols <- c(IDcols, retaincols)
  # Take a FD of all variables but IDretaincols
  dX <- dX0[, lapply(.SD, diff), by = eval(colnames(dX0)[IDretaincols]), 
    .SDcols = colnames(dX0)[-IDretaincols]]
  if ((opposite.time.order)) { # (t-1) - t differnce
    # Invert signs only of time-varying variables. 
    SignInvertedVariables <- "LHS"
    if (!is.null(TimeVariant)) 
      SignInvertedVariables <- c(SignInvertedVariables, grepout(TimeVariant, colnames(dX0)))
    SignInvertedVariables <- unique(c(SignInvertedVariables, colnames(dX0)[-IDretaincols]))
    dX[, (SignInvertedVariables) := lapply(.SD, function(x) x*(-1)), .SDcols = SignInvertedVariables]
    FD.time.order <- "(t-1) - t"
  } else FD.time.order <- "t - (t-1)"
  #  Drop unbalanced portion (obs with NAs)
  if (any(iina <- is.na(rowSums(dX[, -(1:length(IDretaincols)), with = F])))) {
    iiNAID <- dX[iina, IDcols, with = F]
    dropped.rows <- which(iina)
    dX <- dX[!iina]
  } else iiNAID <- dropped.rows <- NULL
  if (print.messages) cat("Dropped", sum(iina), "obs due to NA.\n")
  #  Check if LHS has too small variations
  if (Lmean <- dX[, mean(LHS, na.rm = T)] != 0)
    coeffvar <- dX[, var(LHS, na.rm = T)]^(.5) / Lmean else
    coeffvar <- dX[, var(LHS, na.rm = T)]^(.5)
  if (abs(coeffvar) < .00001)
    stop(paste0("LHS has little variation: coefficient of variation ", coeffvar, ".\n"))
  #  Check if 3/4 of original obs is dropped
  maxT <- max(data.table(X)[, .(max = .N), by = eval(groupstring)][, max], na.rm = T)
  if (nrow(dX)/(nrow(X)/maxT) < .25)
    stop("Deleted more than 3/4 of original data due to NAs!\n")
  if (is.null(clusterstring)) cat("No cluster variable specified. No clustering of standard errors.\n")
  #  clusterNum: cluster as a numeric vector
  qclusterstring <- quote(list(clusterstring))
  cluster <- dX[, eval(quote(clusterstring)), with = F]
  clusterNum <- as.numeric(factor(asc(cluster)))
  #  Create formula.
  if (intercept) form <- "LHS ~" else form <- "LHS ~ -1+"
  #  Add covariates other than "exclude, LH, NotToBeDifferenced, clusterstring, group".
  #  dX already dropped exclude. Drop LHS, NotToBeDifferenced, clusterstring, group.
  form <- paste(form, 
               paste(colnames(dX)[-grep(paste0(c("LH", NotToBeDifferenced, 
               paste0("^", clusterstring, "$"), group), collapse = "|"), colnames(dX))], 
               collapse = " + "))
  form <- as.formula(form)
  reg1 <- lm(form, data = dX)
  ns <- nrow(reg1$model)
  list(est = summary(reg1)$coeff, reg = reg1, level.data = dX0, diff.data = dX, 
      groupname = groupstring,omitgroupid = iiNAID, droppedRows = dropped.rows, 
      FD.time.order = FD.time.order, N = ns, nonrobust = reg1)
}
clx <-function(fm, dfcw = 1, cluster, returnV = F, dfc2 = F, DFC = F,
  Estfun = "sandwich", deviation = F)
#  1 way clustering robust covariance matrix of Liang and Zeger
#  following Arai 2010
#  fm: fitted lm model
#  cluster: 1 column matrix
#  dfcw: degree of freedom adjustment when using deviation data
#   Note: When using the FE estimator with N obs and M groups, 
#   lm uses dof as N-K, but it should be N-M-K so dfcw = (N-K-M)/(N-K).
#   In FD estimator, there is no need of dof correction.
#  dfc2: some suggests this dfc, but not sure if this is correct
#  DFC: User provided dof
#  returnV: if T, aslo returns cluster-robust covariance
#  ESTfun: use estfun provided by "sandwich" package or "seiro"
#  deviation: T if using deviations data, if F, dfcw is set to 1
{
  require(sandwich); require(lmtest)
  M <- length(unique(cluster))
  N <- length(cluster)
  dfc <- (M / (M - 1)) * ((N - 1) / (N - fm$rank))
  if (dfc2) dfc <- (N -  fm$rank) / (N - M - fm$rank)
  if (DFC) dfc <- DFC
  #  switch estfun according to specified by estfun = "sandwich" (default) or "seiro"
  esfun <- function(fm, ESTfun) switch(ESTfun, sandwich = estfun(fm), seiro = estfun2(fm))
  #u <- apply(estfun2(fm), 2, function(x) tapply(x, cluster, sum))
  # This sums estimating function [resid*x (in FOC of OLS)] by cluster.
  u <- apply(esfun(fm, Estfun), 2, function(x) tapply(x, cluster, sum))
  # There is no attribute called "df": lm$df.residual, glm$residual.
  # But leave it for backward compatibility.
  if (any(grepl("^df$", names(fm))))  
    dfcw <-  fm$df /(fm$df - (M-1)) else
  if (deviation)  
    dfcw <-  fm$df.residual /(fm$df.residual - (M-1))
  vcovCL <- dfc * sandwich(fm, meat = crossprod(u) / N) * dfcw
  if (returnV) list(est = lmtest::coeftest(fm, vcov. = vcovCL), 
      ci = lmtest::coefci(fm, vcov. = vcovCL),
      V = vcovCL, dfc = dfc, dfcw = dfcw, reg = fm, 
      clusters.M = M, cluster = cluster, N = length(fm$fit)) else
  coeftest(fm, vcovCL) 
}
a2b.data.table <- function(obj, a, b, a.is.regexp = F)
# changes "a" (scalar) in a data.table obj (or a vector) to "b" (scalar)
# a.is.regexp: if T, a is regular expression and accepts all matching a values
## This can be improved if one use set(data, i, j, value) for all substitutions. .SD is less efficient.
{
  require(data.table)
  if (dimnull <- is.null(dim(obj))) {
    dim(obj) <- c(length(obj), 1)
    obj <- data.table(obj)
  } else org.colname <- names(obj)
  if (any(grepl("-", names(obj)))) setnames(obj, org.colname, gsub("-", ".", org.colname))
  iic <- sapply(obj, is.character); iif <- sapply(obj, is.factor)
  iil <- sapply(obj, is.logical); iin <- sapply(obj, is.numeric)
  # factor => character for easy substitution
  if (any(iif)) obj[, (names(iif[iif])) := lapply(.SD, as.character), .SDcols = names(iif[iif])]
  if (!a.is.regexp & !(is.na(a) | is.nan(a))) a <- paste0("^", a, "$")
  for (m in 1:ncol(obj)) 
    if (is.na(a)) 
      set(obj, i = which(is.na(obj[[m]])), j = m, value = b) else
    if (is.nan(a)) 
      set(obj, i = which(is.nan(obj[[m]])), j = m, value = b) else
      set(obj, i = which(grepl(a, obj[[m]])), j = m, value = b)
  if (any(iif)) obj[, (names(iif[iif])) := lapply(.SD, as.factor), .SDcols = names(iif[iif])]
  if (any(iic)) obj[, (names(iic[iic])) := lapply(.SD, as.character), .SDcols = names(iic[iic])]
  if (any(iil)) obj[, (names(iil[iil])) := lapply(.SD, as.logical), .SDcols = names(iil[iil])]
  if (any(iin)) obj[, (names(iin[iin])) := lapply(.SD, as.numeric), .SDcols = names(iin[iin])]
  if (dimnull) {
    names(obj) <- NULL
    obj <- unlist(obj)
  } else setnames(obj, colnames(obj), org.colname)
  return(obj[])
}
putzeroontop <- function(z, autozero = T, totaldigits = F)
#  autozero: if numeric, add the given digit of zero on top
#  totaldigits: if numeric, total digits will be set (001 is 3) if maximum 0s to be added is smaller
#    modified to accept NA in z: July 18, 2016
{
  iic <- grep("\\D", z, perl = T)
  iina <- which(is.na(z))
  iin <- grep("\\d", z, perl = T)
  digs <- nchar(asc(z))
  adddig <- max(digs, na.rm = T) - digs
  if (is.numeric(autozero)) 
  {
    maxadd <- max(max(digs, na.rm = T) - digs, na.rm = T)
    if (autozero > maxadd) adddig <- adddig + (autozero - maxadd)
  }
  if (is.numeric(totaldigits))
    if (totaldigits > max(digs, na.rm = T)) 
    adddig <- adddig + (totaldigits - max(digs, na.rm = T))
  adddig[c(iic, iina)] <- 0
  z <- paste(seqrep("0", adddig), z, sep = "")
  z[grep("NA", z)] <- NA
  return(z)
}
makeDummyFromFactor <- function(x, reference = 1, use.numeric.labels = F,
nameprefix = "dummy") {
# Create a matrix of dummy variables from a factor. Requires data.table package.
# From SO {\footnotesize\url{https://stackoverflow.com/questions/14921805/convert-a-factor-to-indicator-variables}}
# reference: factor level assigned as a reference category 
#   (so dropped from a matrix). Default is 1st factor level.
#  Can be set to NULL not to drop reference column.
# use.numeric.labels: If T, use dummy01, dummy02, ... 
#  instead of dummy.levels[1], dummy.levels[2] ...
# nameprefix: nameprefix.levels[1], ...
  require("data.table")
  if (!is.factor(x)) stop("Need a factor variable. Thank you.")
  dums <- data.table(diag(nlevels(x)))
  dums <- sapply(dums, as.integer)
  dums <- data.table(dums[x, ])
  # NA => 0
  if (any(is.na(x))) dums <- a2b.data.table(dums, NA, 0)
  fac.levels <- levels(x)
  if (use.numeric.labels) {
    fac.levels <- 1:nlevels(x)
    if (nlevels(x) > 9) fac.levels <- putzeroontop(fac.levels, totaldigits = 2)
  }
  setnames(dums, paste0(nameprefix, 
    toupper(substr(fac.levels, 1, 1)), substr(fac.levels, 2, 30)))
  if (is.null(reference)) 
    return(dums) else
    return(dums[, -reference, with = F])
}
DID2 <- function(dX0, Group, Regressand, Cluster = NULL, TimeVar = "time",
  Exclude = NULL, intercept = T, opposite.time.order = F, TimeVariant = NULL,
  PeriodEffects = F, 
  SmallClusterCorrection = "satterthwaite", WCBType = "rademacher",
  return.V = F, return.level.data = F, return.diff.data = F, print.messages = T, ...)
# Data:
#   Assume first-differenced data produced by DID1 is used.
#   (plm does not directly accept different level of clustering than panel grouping, so not used.)
# Model:
#   y_{it}= a_{i} + b_{t} + c*x_{it} + e_{it},
#   dy_{it} = db_{t} + c*dx_{it} + de_{it},
#  where dx_{it}=x_{it}-x_{it-1}, b_{t} is a time t effect, 
#  db_{t}=b_{t}-b_{t-1} is a period t (between time t-1 and time t) effect.
# Covariance estimation:
#   A wrapper for clx of Arai 2000 after first differencing using data.table
#   ** for the time being, we assume T = 2.
#   ** can extend for T > 2 but need to examine if contiguous time periods are observed.
#  -- Arguments --
#  dX0: first-differenced data object
#  Group: regexp for the grouping variable (must be a single numeric column)
#  Regressand: regexp for LHS of regression (internally, column name is changed to "LHS")
#  Cluster: regexp for clustering variable, if NULL no clustring of SE
#  TimeVar: regexp for time index variable.
#  opposite.time.order: if T, take diff (t-1) - t, so signs in data will be reversed for 
#    LHS and time varying variables. If F, take t - (t-1) diff. diff() in R takes t - (t-1) differences. 
#    Conventionally, FD takes t - (t-1) differences. Event study design takes (t-1) - t.
#  TimeVariant: reg exp for time-varying variables (not cross terms with time dummies)
#  PeriodEffects: if T, use period [betweem t and t-1, or t - (t-1)] specific effects. 
#     If F, impose db_{t}=db_{t'} for all t, t' with db_{t}=b_{t}-b_{t-1}.
#       PeriodEffects= F & intercept = F  =>  db_{t}=db_{t'}=0 for all t, t'.
#       PeriodEffects= F & intercept = T  =>  db_{t}=db_{t'} for all t, t'.
#       PeriodEffects= T & intercept = F  =>  allow db_{t} for all t=1,...,T.
#       PeriodEffects= T & intercept = T  =>  allow db_{t} for all t=2,...,T, intercept is period 1 effect.
#  Exclude: regexp for variables to be dropped from regression 
#    other than Group, Cluster, TimeVar
#  SmallClusterCorrection: Less than 42 is the magic number (Mostly Harmless...)
#    If LiangZeger, no correction. 
#    If satterthwaite, use clubSandwich's satterthwaite correction with CR2.
#    If wildbootstrap use fwildclusterboot.
#  WCBType: "type" in boottest.lm. Default is rademacher. Use "webb" for a small number of clusters.
#    Webb (2014) is unpublished but has a nice intro to WCB. Rademacher uses 2 = {-1, 1} values for
#    for WCB weights, so the number of unique samples becomes 2^{G}, with G = # of clusters.
#    Webb uses 6 values so number of unique samples 6^{G}. Warning: No asymptotics is 
#    proved for Webb, only Monte Carlo evidence.
#  return.V: if T, return covariance matrix
#  return.level.data: if return.V==T, return undifferenced data matrix (y and X)
#  return.diff.data: if return.V==T, return differenced data matrix (y and X). 
#    May have less obs than Level.data because we drop NAs only after differencing.
#  print.messages: if T, "dropped X obs due to NA" is printed.
#  -- Outputs --
#  est: regression estimation object (lm)
#  V: covariance matrix
#  level.data: Data matrix before differencing
#  diff.data: Data matrix after differencing and NA dropping
#  reg: Estimation results
#  groupname: Group string.
#  omitgroupid: IDs dropped due to being too short or NA.
#  dropped.rows: Dropped row numbers in level.data due to NA.
#  Ttable: tabulation of T (largest time dimension for each group)
#  FD.time.order: t - (t-1) or (t-1) - t
#  N: Number of obs
#  nonrobust: Results without robustifying standard errors
{
  require("data.table")
  dX <- data.table(dX0)
  #  Regressand => "LHS"
  if (length(grep(Regressand, colnames(dX)))==1) 
    setnames(dX, grepout(Regressand, colnames(dX)), "LHS")
  #  Check if PeriodEffects = F, intercept == T
  if (!PeriodEffects & !intercept) warning("If PeriodEffects is F, setting intercept to F amounts to imposing zero period effects for all periods.")
  if (PeriodEffects & !intercept) message("Note: PeriodEffects == T & intercept == F => Allow period specific effects for t=1, ..., T.")
  if (PeriodEffects & intercept) message("Note: PeriodEffects == T & intercept == T => Intercept is period 1 effect.")
  #  Get cluster as a numeric vector
  clusterstring <- grepout(Cluster, colnames(dX))
  qclusterstring <- quote(list(clusterstring))
  clusterNum <- dX[, eval(quote(clusterstring)), with = F]
  clusterNum <- as.numeric(factor(asc(clusterNum)))
  #  Maximum length in time dimension
  groupstring <- grepout(Group, colnames(dX))
  dX[, Tee := .N, by = eval(parse(text = groupstring))]
  dxt <- unique(dX[, grep(paste(Group, "Tee", sep = "|"), colnames(dX)), with = F])
  tabT <- table(dxt[, Tee])
  timevector <- dX[, eval(parse(text = TimeVar))]
  tabtee <- table0(timevector)
  # Create PeriodEffects (period specific intercepts)
  if (PeriodEffects) {
    timevector <- as.factor(timevector)
    if (intercept)
      TE <- makeDummyFromFactor(timevector, nameprefix = "period") else
      TE <- makeDummyFromFactor(timevector, reference = NULL, nameprefix = "period")
    TENames <- colnames(TE)
    dX <- cbind(dX, TE)
  }
  SignInvertedVariables <- NULL
  if ((opposite.time.order)) {
    # If T: diff is t-1 - (t). 
    # invert signs only of time-varying variables
    SignInvertedVariables <- "LHS"
    if (!is.null(TimeVariant)) 
      SignInvertedVariables <- c(SignInvertedVariables, grepout(TimeVariant, colnames(dX)))
    if (PeriodEffects) SignInvertedVariables <- c(SignInvertedVariables, TENames)
    dX[, (SignInvertedVariables) := lapply(.SD, function(x) x*(-1)), .SDcols = SignInvertedVariables]
    FD.time.order <- "(t-1) - t"
  } else FD.time.order <- "t - (t-1)"
  #  Formula
  if (intercept) form0 <- "LHS ~" else form0 <- "LHS ~ -1 +"
  #  Covariates: variables remaining after dropping "LH, Group, Cluster, TimeVar, Exclude, "Tee"
  form0 <- paste(form0, 
       paste(colnames(dX)[-grep(
         paste0(c("LH", Group, Cluster, TimeVar, Exclude, "Tee"), collapse = "|"), 
         colnames(dX))], 
       collapse = " + "))
  # If "Exclude" drops all the covariates, an error is issued.
  if (!any(grepl("~ -1 \\+ [A-Za-z]|~ [A-Za-z]", form0))) 
    stop("\n\nSomething is wrong: No covariate specified. \nA suggestion: Check \'Exclude\'.")
  form <- as.formula(form0)
  reg1 <- lm(form, data = dX)
  # If there is NA in estimates (due to singularity), need to drop them
  CovariateNames <- names(reg1$coeff)
  if (any(is.na(reg1$coeff))) {
    NAEstimates <- CovariateNames[is.na(reg1$coeff)]
    # "\\+?" is necessary if coeff of a is NA in LHS ~ a + b + c
    # Trailing " " is necessary to drop only one term.
    form00 <- gsub(paste(paste0("\\+? ?", NAEstimates, " "), collapse = "|"), "", form0)
    form <- as.formula(form00)
    reg1 <- lm(form, data = dX)
  } else NAEstimates <- "No NAs in estimated coefficients"
  # If there is NA in data, need to adjust clusterNum.
  # (tapply in clx needs same dimension.)
  if (length(reg1$na.action) > 0) clusterNum <- clusterNum[-(reg1$na.action)]
  ci <- NULL
  # Inference using cluster robust SEs. 
  # If SmallClusterCorrection == Satterthwaite (for small number of clusters)
  # use Pustejovsky and Tipton's bias reduced linearization
  if (grepl("^[sS]atter", SmallClusterCorrection)) {
    require(clubSandwich)
    estres <- clubSandwich::coef_test(reg1, vcov = "CR2", 
      cluster = clusterNum, test = "Satterthwaite")
    ci <- clubSandwich::conf_int(reg1, vcov = "CR2", level = 0.95, 
      test = "Satterthwaite", cluster = clusterNum, coefs = "All", p_values = T)
  } else if (grepl("^[wW]ild", SmallClusterCorrection)) {
  # If SmallClusterCorrection == "wildbootstrap" (for small number of clusters)
  # use Cameron Gelbach Miller's wild cluster bootstrap.
  # If engine = "WildBootTests.jl", it takes 30-40 secs for the 1st run.
    require(fwildclusterboot)
    #require(JuliaConnectoR.utils)
    #set_julia_nthreads() # instructions to set nthreads for Julia
    #dqrng::dqset.seed(1234)
    # install_julia_packages("StableRNGs.jl") # need this to run WildBootTests.jl
    # JuliaConnectoR.utils::set_julia_nthreads() says the default setting is set to auto
    # where julia uses max number of cores (8 in this laptop).
    set.seed(1234)
    bootcoef <- names(reg1$coeff)
    estres <- vector(length = length(bootcoef), mode = "list")
    for (h in 1:length(bootcoef))
      estres[[h]] <- fwildclusterboot::boottest(reg1, clustid = clusterstring, B = 9999*1+1,
                  param = bootcoef[h], type = WCBType,
                  engine = "WildBootTests.jl")
    estres <- lapply(estres, fwildclusterboot::tidy)
    estres <- lapply(estres, data.table)
    estres <- rbindlist(estres)
    # Std.Error is technically a wrong label, but I will use this label 
    # to conform with the usual regression table format.
    estres[, Std.Error := abs(estimate/statistic)]
    RNames.estres <- gsub("^1\\*(.*) = 0$", "\\1", estres[, term])
    estres <- as.matrix(estres[, .(estimate, Std.Error, statistic, p.value, conf.low, conf.high)])
    dimnames(estres) <- list(RNames.estres, colnames(estres))
    colnames(estres)[4:6] <- c("Pr(>|t|)", "ci.lb", "ci.ub")
  } else if (grepl("^[lL]iang", SmallClusterCorrection)) {
    # Set deviation = F for FD estimator. Set to T for FE estimator.
    estres <- clx(reg1, cluster = matrix(clusterNum), 
      returnV = return.V, deviation = F)
  }
  ns <- nrow(reg1$model)
  # Tee after dropping NAs
  if (!is.null(reg1$na.action)) 
    ActualTeeTab <-   table(unique(dX[-reg1$na.action, 
      grep(paste(Group, "Tee", sep = "|"), colnames(dX)), with = F])[, Tee]) else
    ActualTeeTab <- tabT
  dX[, Tee := NULL]
  if (return.V && grepl("^[lL]iang", SmallClusterCorrection)) 
    list(est = estres$est, CI = estres$ci, V = estres$V, 
      data = dX, reg = estres$reg, groupname = groupstring, FD.time.order = FD.time.order,
      SignInverted = SignInvertedVariables,
      Ttable = tabT, TimeVarTable = tabtee, ActualTTable = ActualTeeTab,
      N = length(estres$reg$fit), nonrobust = reg1, form = form, 
      SmallClusterCorrection = SmallClusterCorrection, 
      DroppedCovariates=NAEstimates) else
    list(est = estres, CI = ci,
      data = dX, reg = reg1, groupname = groupstring, FD.time.order = FD.time.order, 
      SignInverted = SignInvertedVariables,
      Ttable = tabT, TimeVarTable = tabtee, ActualTTable = ActualTeeTab,
      N = ns, nonrobust = reg1, form = form, 
      SmallClusterCorrection = SmallClusterCorrection, 
      DroppedCovariates=NAEstimates)
}
