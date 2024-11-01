sbt <- c("^se\\$", "",
  "^anypr", "program membership (factor)",
  "^program$", "program membership",
  "^FFWProgram$", "program membership to FFW",
  "^NonFFWProgram$", "program membership to non-FFW",
  "^CreditProgram$", "membership to micro credit programs",
  "^StipendProgram$", "Membership to stipend program",
  "^NonStipendProgram$", "Membership to non-stipend program",
  "^yield", "Yield (thana)",
  "^age$", "Age",
  "^Sex$", "Sex (female = 1)",
  "^yr3", "year 2006",
  "^yr2$", "year 2002",
  "^yr1", "year 1999",
  "^.gHH$", "Agricultural household (combined definition)",
  "^.cagHH$", "Agricultural household (occupation definition)",
  "^.dagHH$", "Agricultural household (head definition)",
  "^.sagHH$", "Agricultural household (income definition)",
  "^.gHHold", "\\hspace{.5em}Agricultural household $\\times$  age $>$ 10",
  "^agHH.y", "\\hspace{.5em}Agricultural household",
  "^hdagHH.y", "\\hspace{.5em}ag household (any member)",

# UD variables (for destat and balance test tables)
  "^UDhdsex", "Head sex (female = 1)",
  "^UDnonmuslim", "Non-Muslim",
  "^UDflooded", "Flooded",
  "^UDOldSibF", "Number of older female siblings",
  "^UDOldSibM", "Number of older male siblings",

# class wise interaction variables
  "^DummyEdu1999.Primary0103.yr2$", "\\hspace{.5em}Primary0103",
  "^DummyEdu1999.Primary0405.yr2$", "\\hspace{.5em}Primary0405",
  "^DummyEdu1999.Secondary0608.yr2$", "\\hspace{.5em}Secondary0608",
  "^DummyEdu1999.Secondary0912.yr2$", "\\hspace{.5em}Secondary0912",
  "^DummyEdu1999.Primary0103.agHH.yr2$", "\\hspace{.5em}ag HH * Primary0103",
  "^DummyEdu1999.Primary0405.agHH.yr2$", "\\hspace{.5em}ag HH * Primary0405",
  "^DummyEdu1999.Secondary0608.agHH.yr2$", "\\hspace{.5em}ag HH * Secondary0608",
  "^DummyEdu1999.Secondary0912.agHH.yr2$", "\\hspace{.5em}ag HH * Secondary0912",
# agewise interaction variables
  "^DummyAgeGroupIn1999..elow.*ag*", "\\hspace{.5em}ag HH * age below 11 in 1999",
  "^DummyAgeGroupIn1999.11.*ag*", "\\hspace{.5em}ag HH * age 11-13 in 1999",
  "^DummyAgeGroupIn1999.14.*ag*", "\\hspace{.5em}ag HH * age 14-16 in 1999",
  "^DummyAgeGroupIn1999..bove.*ag*", "\\hspace{.5em}ag HH * age 17 above in 1999",
  "^DummyAgeGroupIn1999..elo", "\\hspace{.5em}age group below 11 in 1999",
  "^DummyAgeGroupIn1999.11", "\\hspace{.5em}age group 11-13 in 1999",
  "^DummyAgeGroupIn1999.13", "\\hspace{.5em}age group 14-16 in 1999",
  "^DummyAgeGroupIn1999..bove", "\\hspace{.5em}age group 17 above in 1999",

  "^DummyAgeGroup1999..elow.*ag*", "\\hspace{.5em}ag HH * age below 10 in 1999",
  "^DummyAgeGroup1999.10.*ag*", "\\hspace{.5em}ag HH * age 10-11 in 1999",
  "^DummyAgeGroup1999.12.*ag*", "\\hspace{.5em}ag HH * age 12-13 in 1999",
  "^DummyAgeGroup1999.14.*ag*", "\\hspace{.5em}ag HH * age 14-15 in 1999",
  "^DummyAgeGroup1999..bove.*ag*", "\\hspace{.5em}ag HH * age above 15 in 1999",
  "^DummyAgeGroup1999..elo", "\\hspace{.5em}age group below 10 in 1999",
  "^DummyAgeGroup1999.10", "\\hspace{.5em}age group 10-11 in 1999",
  "^DummyAgeGroup1999.12", "\\hspace{.5em}age group 12-13 in 1999",
  "^DummyAgeGroup1999.14", "\\hspace{.5em}age group 14-15 in 1999",
  "^DummyAgeGroup1999..bove", "\\hspace{.5em}age group above 15 in 1999",

  "^DummyAgegroup1999..elow.*ag*", "\\hspace{.5em}ag HH * age below 10 in 1999",
  "^DummyAgegroup1999.10.*ag*", "\\hspace{.5em}ag HH * age 10-12 in 1999",
  "^DummyAgegroup1999.13.*ag*", "\\hspace{.5em}ag HH * age 13-15 in 1999",
  "^DummyAgegroup1999..bove.*ag*", "\\hspace{.5em}ag HH * age above 15 in 1999",
  "^DummyAgegroup1999..elo", "\\hspace{.5em}age group below 10 in 1999",
  "^DummyAgegroup1999.10", "\\hspace{.5em}age group 10-12 in 1999",
  "^DummyAgegroup1999.13", "\\hspace{.5em}age group 13-15 in 1999",
  "^DummyAgegroup1999..bove", "\\hspace{.5em}age group above 15 in 1999",

  "^DummyAgeIn1999.10.*ag*", "\\hspace{.5em}agricultural HH * age 10 in 1999",
  "^DummyAgeIn1999.11.*ag*", "\\hspace{.5em}agricultural HH * age 11 in 1999",
  "^DummyAgeIn1999.12.*ag*", "\\hspace{.5em}agricultural HH * age 12 in 1999",
  "^DummyAgeIn1999.13.*ag*", "\\hspace{.5em}agricultural HH * age 13 in 1999",
  "^DummyAgeIn1999.14.*ag*", "\\hspace{.5em}agricultural HH * age 14 in 1999",
  "^DummyAgeIn1999.15.*ag*", "\\hspace{.5em}agricultural HH * age 15 in 1999",
  "^DummyAgeIn1999.16.*ag*", "\\hspace{.5em}agricultural HH * age 16 in 1999",
  "^DummyAgeIn1999.17.*ag*", "\\hspace{.5em}agricultural HH * age 17 in 1999",
  "^DummyAgeIn1999.18.*ag*", "\\hspace{.5em}agricultural HH * age 18$+$ in 1999",
  "^DummyAgeIn1999.10", "\\hspace{.5em}age 10 in 1999",
  "^DummyAgeIn1999.11", "\\hspace{.5em}age 11 in 1999",
  "^DummyAgeIn1999.12", "\\hspace{.5em}age 12 in 1999",
  "^DummyAgeIn1999.13", "\\hspace{.5em}age 13 in 1999",
  "^DummyAgeIn1999.14", "\\hspace{.5em}age 14 in 1999",
  "^DummyAgeIn1999.15", "\\hspace{.5em}age 15 in 1999",
  "^DummyAgeIn1999.16", "\\hspace{.5em}age 16 in 1999",
  "^DummyAgeIn1999.17", "\\hspace{.5em}age 17 in 1999",
  "^DummyAgeIn1999.18", "\\hspace{.5em}age 18$+$ in 1999",

#  "^DummyAge.10.ag.*", "\\hspace{.5em}agricultural HH * age 10 in 1999",
#  "^DummyAge.11.ag.*", "\\hspace{.5em}agricultural HH * age 11 in 1999",
#  "^DummyAge.12.ag.*", "\\hspace{.5em}agricultural HH * age 12 in 1999",
#  "^DummyAge.13.ag.*", "\\hspace{.5em}agricultural HH * age 13 in 1999",
#  "^DummyAge.14.ag.*", "\\hspace{.5em}agricultural HH * age 14 in 1999",
#  "^DummyAge.15.ag.*", "\\hspace{.5em}agricultural HH * age 15 in 1999",
#  "^DummyAge.16.ag.*", "\\hspace{.5em}agricultural HH * age 16 in 1999",
#  "^DummyAge.17.ag.*", "\\hspace{.5em}agricultural HH * age 17 in 1999",
#  "^DummyAge.18.ag.*", "\\hspace{.5em}agricultural HH * age 18$+$ in 1999",
#  "^DummyAge.10.yr3", "\\hspace{.5em}age 10 in 1999",
#  "^DummyAge.11.yr3", "\\hspace{.5em}age 11 in 1999",
#  "^DummyAge.12.yr3", "\\hspace{.5em}age 12 in 1999",
#  "^DummyAge.13.yr3", "\\hspace{.5em}age 13 in 1999",
#  "^DummyAge.14.yr3", "\\hspace{.5em}age 14 in 1999",
#  "^DummyAge.15.yr3", "\\hspace{.5em}age 15 in 1999",
#  "^DummyAge.16.yr3", "\\hspace{.5em}age 16 in 1999",
#  "^DummyAge.17.yr3", "\\hspace{.5em}age 17 in 1999",
#  "^DummyAge.18.yr3", "\\hspace{.5em}age 18$+$ in 1999",

  "^DummyAgeIn2002.10.*ag*", "\\hspace{.5em}agricultural HH * age 10 in 2002",
  "^DummyAgeIn2002.11.*ag*", "\\hspace{.5em}agricultural HH * age 11 in 2002",
  "^DummyAgeIn2002.12.*ag*", "\\hspace{.5em}agricultural HH * age 12 in 2002",
  "^DummyAgeIn2002.13.*ag*", "\\hspace{.5em}agricultural HH * age 13 in 2002",
  "^DummyAgeIn2002.14.*ag*", "\\hspace{.5em}agricultural HH * age 14 in 2002",
  "^DummyAgeIn2002.15.*ag*", "\\hspace{.5em}agricultural HH * age 15 in 2002",
  "^DummyAgeIn2002.16.*ag*", "\\hspace{.5em}agricultural HH * age 16 in 2002",
  "^DummyAgeIn2002.17.*ag*", "\\hspace{.5em}agricultural HH * age 17 in 2002",
  "^DummyAgeIn2002.18.*ag*", "\\hspace{.5em}agricultural HH * age 18$+$ in 2002",
  "^DummyAgeIn2002.10", "\\hspace{.5em}age 10 in 2002",
  "^DummyAgeIn2002.11", "\\hspace{.5em}age 11 in 2002",
  "^DummyAgeIn2002.12", "\\hspace{.5em}age 12 in 2002",
  "^DummyAgeIn2002.13", "\\hspace{.5em}age 13 in 2002",
  "^DummyAgeIn2002.14", "\\hspace{.5em}age 14 in 2002",
  "^DummyAgeIn2002.15", "\\hspace{.5em}age 15 in 2002",
  "^DummyAgeIn2002.16", "\\hspace{.5em}age 16 in 2002",
  "^DummyAgeIn2002.17", "\\hspace{.5em}age 17 in 2002",
  "^DummyAgeIn2002.18", "\\hspace{.5em}age 18$+$ in 2002",

  "^DummyAgeGroup2002..elow.*ag*", "\\hspace{.5em}ag HH * age below 10 in 2002",
  "^DummyAgeGroup2002.10.*ag*", "\\hspace{.5em}ag HH * age 10-11 in 2002",
  "^DummyAgeGroup2002.12.*ag*", "\\hspace{.5em}ag HH * age 12-13 in 2002",
  "^DummyAgeGroup2002.14.*ag*", "\\hspace{.5em}ag HH * age 14-15 in 2002",
  "^DummyAgeGroup2002..bove.*ag*", "\\hspace{.5em}ag HH * age above 15 in 2002",
  "^DummyAgeGroup2002..elo", "\\hspace{.5em}age group below 10 in 2002",
  "^DummyAgeGroup2002.10", "\\hspace{.5em}age group 10-11 in 2002",
  "^DummyAgeGroup2002.12", "\\hspace{.5em}age group 12-13 in 2002",
  "^DummyAgeGroup2002.14", "\\hspace{.5em}age group 14-15 in 2002",
  "^DummyAgeGroup2002..bove", "\\hspace{.5em}age group above 15 in 2002",
  "^DummyAgegroup2002..elow.*ag*", "\\hspace{.5em}ag HH * age below 10 in 2002",
  "^DummyAgegroup2002.10.*ag*", "\\hspace{.5em}ag HH * age 10-12 in 2002",
  "^DummyAgegroup2002.13.*ag*", "\\hspace{.5em}ag HH * age 13-15 in 2002",
  "^DummyAgegroup2002..bove.*ag*", "\\hspace{.5em}ag HH * age above 15 in 2002",
  "^DummyAgegroup2002..elo", "\\hspace{.5em}age group below 10 in 2002",
  "^DummyAgegroup2002.10", "\\hspace{.5em}age group 10-12 in 2002",
  "^DummyAgegroup2002.13", "\\hspace{.5em}age group 13-15 in 2002",
  "^DummyAgegroup2002..bove", "\\hspace{.5em}age group above 15 in 2002",

  # HH characteristics
  "^pcland.y", "\\hspace{.5em}Per member land holding",
  "^pcland.a", "\\hspace{.5em}Per member land holding $\\times$ agHH",
  #  pcland was inflated
  # in data_rnw.R: 
  #  z[, "totalland"] <- z[, "totalland"]/1000000
  #  z[, "totalvalue"] <- z[, "totalvalue"]/1000000
  "^pcland100$", "Per member land holding (acre, in 1999)",
  "^pclandDec$", "Per member land holding (decimal, in 1999)",
  "^pcnlasset.y", "\\hspace{.5em}Per member nonland asset",
  "^pcnlasset.a", "\\hspace{.5em}Per member nonland asset $\\times$ agHH",
  "^pcnlasset1000$", "Per member nonland asset (1000 Tk, in 1999)",
  "totalvalue.y", "\\hspace{.5em}Nonland asset (1 million Tk)",
  "totalvalue.a", "\\hspace{.5em}Nonland asset (1 million Tk) $\\times$ agHH",
  "Chandpur", "\\hspace{.5em}Chandpur district $\\times$ agHH",
  "CoxB", "\\hspace{.5em}Cox Bazar district $\\times$ agHH",
  "Narail", "\\hspace{.5em}Narail district $\\times$ agHH",
  "Nil", "\\hspace{.5em}Nilphamari district $\\times$ agHH",
  "Nowg", "\\hspace{.5em}Nowgaon district $\\times$ agHH",
  "Sher", "\\hspace{.5em}Sherpur district $\\times$ agHH",
  "Tanga", "\\hspace{.5em}Tangail district $\\times$ agHH",
  "^pov", "poverty (=1 if BPL)",
  "^hd.age", "\\hspace{.5em}Head age",
  "^hd.edu.*pr", "\\hspace{.5em}Head education: primary",
  "^hd.edu.*sec", "\\hspace{.5em}Head education: secondary",
  "^sp.age", "\\hspace{.5em}Spouse age",
  "^sp.edu.*pr", "\\hspace{.5em}Head's spouse education: primary",
  "^sp.edu.*sec", "\\hspace{.5em}Head's spouse education: secondary",
  "^sp.sex", "\\hspace{.5em}Spouse sex (female = 1)",
  "^hd.sex", "\\hspace{.5em}Head sex (female = 1)",
  "^Hd.edu.*pr", "Head education: primary",
  "^Hd.edu.*sec", "Head education: secondary",
  "^Sp.age", "Spouse age",
  "^Sp.edu.*pr", "Head's spouse education: primary",
  "^Sp.edu.*sec", "Head's spouse education: secondary",
  "^OldSibF.y.*", "Number of older female siblings",
  "^OldSibM.y.*", "Number of older male siblings",
  "^OldSibF.a.*", "\\hspace{.5em}Number of older female siblings",
  "^OldSibM.a.*", "\\hspace{.5em}Number of older male siblings",
  "^dwei", "\\hspace{.5em}weight deviation",
  "^dhei", "\\hspace{.5em}height deviation",
  "^sex", "\\hspace{.5em}sex (female = 1)",
  "^older", "\\hspace{.5em}11 or older",
  "^gps", "\\hspace{.5em}GPS distance",
  "^nonm.*[aA]g.*", "\\hspace{.5em}Non-Muslim $\\times$ ag HH",
  "^nonmuslim.yr.$", "\\hspace{.5em}Non-Muslim",
  "latrine.yr.$", "\\hspace{.5em}Structured toilet",
  "latrine.*H.yr.$", "\\hspace{.5em}Structured toilet $\\times$ ag HH",
  "^kutchalatrine$", "Structured toilet",
  "^ownwater", "\\hspace{.5em}Own piped water",

# weather variables
  "^flooded.yr2$", "\\hspace{.5em}flood",
  "^flooded.[aA]g.*", "\\hspace{.5em}flood $\\times$ ag HH",
  "^cle.*M", "mean clear sky",
  "^rai.*M", "mean rainfall",
  "^clo.*M", "mean cloud cover",
  "^tempM", "mean temperature",
  "^dewM", "mean dew point",
  "^vis.*M", "mean visibility",
  "^maxC.*M", "mean max cloud ceiling",
  "^sea.*M", "mean sea level pressure",
  "^dayd.*x$", "mean temperature diff",
  "^clearS", "std clear sky",
  "^cloudS", "std cloud cover",
  "^tempS", "std temperature",
  "^dew.*S", "std dew point",
  "^vis.*S", "std visibility",
  "^maxC.*S", "std max cloud ceiling",
  "^sea.*S", "std sea level pressure",
  # below is for met association data
  "^highM", "mean high temperature",
  "^lowM", "mean low temperature",
  "^rainf.*M", "mean rainfall",
  "^rai.*HH$", "mean rainfall $\\times$ ag HH",
  "^clo.*HH$", "mean cloud cover $\\times$ ag HH",
  "^tempM.*HH$", "mean temperature $\\times$ ag HH",
  "^dayd.*HH$", "mean temperature diff $\\times$ ag HH",
  "^rai.*HH.yr2$", "\\hspace{.5em}mean rainfall $\\times$ ag HH",
  "^clo.*HH.yr2$", "\\hspace{.5em}mean cloud cover $\\times$ ag HH",
  "^tempM.*HH.yr2$", "\\hspace{.5em}mean temperature $\\times$ ag HH",
  "^dayd.*HH.yr2$", "\\hspace{.5em}mean temperature diff $\\times$ ag HH")
sbt <- matrix(sbt, byrow = T, ncol = 2)
colnames(sbt) <- c("org", "changedto")
