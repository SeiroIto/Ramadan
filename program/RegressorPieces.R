# Regressor pieces
# area2 <- "^dummy[A-Z].*yr2"
# area3 <- "^dummy[A-Z].*yr3"
# indiv2 <- "^age.?$|^sex.*2|border.*2$"
# indiv3 <- "^age.?$|^sex.*3"
# indivage22 <- "^sex.*2|age2"
# indivage32 <- "^sex.*3|age2"
# indivagesib22 <- "^sex.*2|age2|OldSib.*2$" # exclude OldSchSib[FM].
# indivagesib32 <- "^sex.*3|age2|OldSib.*3$"
# indivonlyage2 <- "age2"
# indivonlyage3 <- "age2"
# indivage2 <- "^sex.yr2" # exclude sex.agHH.yr2
# indivage3 <- "^sex.yr3"
# indivnoage2 <- "^sex.*2|dSib.*2$" # include sex.agHH.yr2
# indivnoage3 <- "^sex.*3|dSib.*3$"
# hd2 <- "hd\\.e.*2|hd.sex.*2|^prog" 
# hd3 <- "hd\\.e.*3|^prog" 
# #hd2 <- "hd\\.e.*2|^prog" 
# #sp2 <- "sp\\.edulevel\\..*2|sp.sex.*2" 
# sp2 <- "sp\\.edulevel\\..*2" 
# sp3 <- "sp\\.edulevel\\..*3|sp.sex.*3" 
# wealth2 <- "pc.*2"
# wealth3 <- "pc.*3"
# hygiene2 <- "^k.*latr.*2|ownwater.*2|^pcl.*2$|^pcn.*2$"
# hygiene3 <- "^k.*latr.*3|ownwater.*3|^pcl.*3$|^pcn.*3$"
# agewise2 <- "^DummyAgeI.*yr2"
# agewise3 <- "^DummyAgeI.*yr3"
# #ageGroupwise2 <- "^DummyAgeG.*13.*yr2|^DummyAgeG.*15.*yr2"
# #ageGroupwise3 <- "^DummyAgeG.*13.*yr3|^DummyAgeG.*15.*yr3"
# #agegroupwise2 <- "^DummyAgeg.*13.*yr2|^DummyAgeg.*15.*yr2"
# #agegroupwise3 <- "^DummyAgeg.*13.*yr3|^DummyAgeg.*15.*yr3"
# #ageGroupwise2 <- "^DummyAgeG.*1011.*yr2|^DummyAgeG.*1213.*yr2|^DummyAgeG.*1415.*yr2|^DummyAgeG.*Ab.*yr2"
# #agegroupwise2 <- "^DummyAgeg.*1012.*yr2|^DummyAgeg.*1315.*yr2|^DummyAgeg.*Ab.*yr2"
# ageGroupwise2 <- "^DummyAgeG.*1113.*yr2|^DummyAgeG.*1415.*yr2|^DummyAgeG.*1617.*yr2|^DummyAgeG.*Ab.*yr2"
# ageGroupwise3 <- gsub("yr2", "yr3", ageGroupwise2)
# agegroupwise2 <- "^DummyAgeg.*1117.*yr2|^DummyAgeg.*Ab.*yr2"
# agegroupwise3 <- gsub("yr2", "yr3", agegroupwise2)

# time varying
yield <- "^yield$"
prog <- "^prog"
StipendProg <- "dProgram"
OnlyStipendProg <- "^StipendProgram"
FFEProg <- "FFEProgram"
FFWProg <- "FFWProgram"
  # clear, temperature, maxCloudCeiling, dew have strong correlations in 1999
#weather.variables1 <- "^highM|^lowM"
weather.variables1 <- "^highM|^lowM|^rainfallM"
weather.variables2 <- "^highM|^lowM"
#yieldweather <- "^yield$|^highM|^lowM"
yieldweather <- "^yield$|rain|^highM|^lowM"
anth3 <- "dh|dw"
indivonlyage <- "age2"

# levels of time fixed variables
arealevel <- "^dummy[A-Z].*[a-z]$"
hdlevel <- "hd\\.e.*\\w$|hdsex$" 
splevel <- "sp\\.e.*\\w$" 

# year interactions
areaDD <- "^dummy[A-Z].*yrDD"
indivDD <- "^age.?$|^sex.*DD"
#indivageDD <- "^sex.yrDD" # exclude sex.agHH.yrDD
indivageDD <- "^sex.*DD|age2"
indivagesibDD <- "^sex.*DD|age2|^OldSib.*DD$" # exclude OldSchSib[FM].
indivagesibyrDD <- "^sex.yrDD|age2|^OldSib..yrDD$" # exclude OldSchSib[FM].
indivnoagesibDD <- "^sex.*DD|^OldSib.*DD$" # include sex.agHH.yrDD
indivNoAgeNoAgHHsibDD <- "^sex.yrDD$|^OldSib.yrDD$" # include sex.agHH.yrDD
indivnosexagesibDD <- "^age2|^OldSib.*DD$" # exclude sex.agHH.yrDD
indivyieldweatherDD <- "^age2$|^sex.*DD|^OldSib.*DD$|^yield$|rain|^highM|^lowM"
sibDD <- "^OldSib.*DD$"
hdDD <- "hd\\.e.*DD|hdsex.*DD|^prog" 
hdedusexDD <- "hd\\.e.*DD|hdsex.*DD" 
hdeduonlyDD <- "hd\\.e.*DD" 
hdnosexDD <- "hd\\.e.*DD|^prog" 
hdsexyronlyDD <- "hd\\.e.*DD|hdsex.yrDD|^prog" 
hdedunoagHHDD <- "hd.e.*y.yrDD"
hdyrDD <- "hd\\.e.*y.yrDD|hdsex.yrDD|^prog" 
spDD <- "sp\\.edulevel\\..*DD" 
speduonlyDD <- "sp\\.e.*DD" 
spedunoagHHDD <- "sp.e.*y.yrDD"
wealthDD <- "pc.*DD"
hygieneDD <- "^k.*latr.*DD|ownwater.*DD|^pc[ln].*DD$"
hygieneyrDD <- "^k.*trine.yrDD|ownwater.yrDD$|^pc.*[dt].yrDD$"
agewiseDD <- "^DummyAgeI.*yrDD"
ageGroupwiseDD <- "^DummyAgeG.*1113.*yrDD|^DummyAgeG.*1415.*yrDD|^DummyAgeG.*1617.*yrDD|^DummyAgeG.*Ab.*yrDD"
agegroupwiseDD <- "^DummyAgeg.*1117.*yrDD|^DummyAgeg.*Ab.*yrDD"
nonmuslimsDD <- "^nonm.*yrDD$"
floodDD <- "^flooded.yrDD$"
floodDD.int <- "flooded.[aA].*yrDD"
nonfloodDD <- "^Nonflood.yrDD$"
nonfloodDD.int <- "Nonflood.[aA].*yrDD"
for (v in c("areaDD", "indivDD", "indivageDD", "indivagesibDD", 
  "indivnoagesibDD", "indivNoAgeNoAgHHsibDD", "indivnosexagesibDD", 
  "indivyieldweatherDD", "sibDD",
  "hdDD", "hdedusexDD", "hdeduonlyDD", "hdnosexDD", "hdsexyronlyDD", 
  "hdedunoagHHDD", "spDD", "speduonlyDD", "spedunoagHHDD",
  "wealthDD", "hygieneDD", 
  "hdyrDD", "hygieneyrDD", "indivagesibyrDD",
  "agewiseDD", "ageGroupwiseDD", "agegroupwiseDD",
  "nonmuslimsDD", "floodDD", "floodDD.int",
  "nonfloodDD", "nonfloodDD.int")) {
  rp <- get(v)
  assign(gsub("DD", 2, v), gsub("DD", "2", rp))
  assign(gsub("DD", 3, v), gsub("DD", "3", rp))
}

# 2002 only covariates
eduGroupwise2 <- "^DummyEdu.*yr2"
flood2 <- "^flooded.yr2$"
flood2.int <- "flooded.[aA].*yr2"

# DID: level, yrX interactions, yrX*agHH interactions
indivageD <- "^age2$|dSib"
sexD <- "sex"
hygieneD <- "k.*latr|ownwater|pc[ln]"

# OLS covariates
indivageOLS <- "^sex$|^age" 
hdOLS <- "hd\\.ed.*y$|^prog"  
spOLS <- "sp\\.ed.*y$|sp.sex$" 
hygieneOLS <- "^k.*latrine$|ownwater$" 
