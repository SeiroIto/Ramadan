      for (m in 1:4) {
        # tabulate by specification
        main <- rbind(
          mr[grepl(aghh.defs[m], agdef) & grepl("^ag", Coef) & grepl("^B", inference), Estimate]
          ,
          unlist(mr[grepl(aghh.defs[m], agdef) & grepl("^ag", Coef) & grepl("Z", inference), ci])
          ,
          unlist(mr[grepl(aghh.defs[m], agdef) & grepl("^ag", Coef) & grepl("B", inference), ci])
        )
        sib <-  rbind(
           mr[grepl(aghh.defs[m], agdef)  & grepl("SibF.*H.*yr", Coef) & grepl("BR", inference), Estimate]
           , 
           mr[grepl(aghh.defs[m], agdef) & grepl("SibF.*H.*yr", Coef) & grepl("Z", inference), ci]
           ,
           mr[grepl(aghh.defs[m], agdef) & grepl("SibF.*H.*yr", Coef) & grepl("B", inference), ci]
           ,
           mr[grepl(aghh.defs[m], agdef) & grepl("SibM.*H.*yr", Coef) & grepl("BR", inference), Estimate]
           , 
           mr[grepl(aghh.defs[m], agdef) & grepl("SibM.*H.*yr", Coef) & grepl("Z", inference), ci]
           ,
           mr[grepl(aghh.defs[m], agdef) & grepl("SibM.*H.*yr", Coef) & grepl("B", inference), ci]
         )
         sib <- cbind("", "", sib[, 1], "", "", sib[, 2], "", "", sib[, 3])
         nr <- rbind(
            formatC(nr0[grepl(aghh.defs[m], agdef), ][order(gender, reg), R], digits = 4, format = "f")
          , formatC(nr0[grepl(aghh.defs[m], agdef), ][order(gender, reg), Yes], digits = 0, format = "f")
          , formatC(nr0[grepl(aghh.defs[m], agdef), ][order(gender, reg), n], digits = 0, format = "f")
         )
         nr <- data.table(nr)
         nr[2:3, c(1, 3, 4, 6, 7, 9) := " "]
         nr <- as.matrix(nr)
         enr <- matrix(
           enr0[grepl(aghh.defs[m], agdef), ][order(gender, tee, agHH), EnRate]
           , byrow = F, nrow = 4)
         enr <- data.table(enr[, rep(1:3, each = 3)])
         enr[, c(1, 3, 4, 6, 7, 9) := " "]
         enr <- as.matrix(enr)
         main <- rbind(main, sib, nr, enr)
         assign(paste0("main", m), main)
      }
      # ii = 1: 3 specs = spec 1 direct, spec 2 direct, ...
      mrtab <- rbind(main1, main2, main3, main4)
      mrtab <- 
        cbind(
            rep(c(
              "Agricultural households * year 2002", 
              "\\hspace{1em} CI (LZ)", "\\hspace{1em} CI (BRL)", 
              "\\underline{\\phantom{mm}} * Older sisters",
              "\\hspace{1em} CI (LZ)", "\\hspace{1em} CI (BRL)", 
              "\\underline{\\phantom{mm}} * Older brothers",
              "\\hspace{1em} CI (LZ)", "\\hspace{1em} CI (BRL)", 
              "$\\bar{R}^{2}$", "N: Agricultural HHs", "N",
              paste0("Mean of ", rep(c("control", "treated"), 2), " in ", 
                list(rep(c(1999, 2002), each = 2), rep(c(2002, 2006), each = 2))[[ii]])
               ), 4
            ), 
         mrtab)
      #### LaTeX output
      SepCols <- c(3, 6) 
      ltb <- latextab(mrtab, delimiterline = NULL, 
          hcenter = c(3, rep(1.3, ncol(mrtab)-1)),
          hleft = c("\\scriptsize", rep("\\hfil\\scriptsize$", ncol(mrtab)-1)), 
          hright = c("\\hfill", rep("$", ncol(mrtab)-1)),
          headercolor = NULL, 
          adjustlineskip = "-1ex", adjlskiprows = grep("CI", mrtab[, 1])-1,
          addseparatingcols = SepCols, separatingcolwidth = rep(.1, 2), 
          separatingcoltitle = c("\\textsf{Boys+girls}", "\\textsf{Boys}", "\\textsf{Girls}")
        ) 
      ltb <- c(
        ltb[1], "\\hline", 
        ltb[2:grep("cline", ltb), ],
        paste0(
          "&\\textsf{Spec 1} & \\textsf{Spec 2} & \\textsf{Spec 3}",
          paste(rep("&&\\textsf{Spec 1} & \\textsf{Spec 2} & \\textsf{Spec 3}", 2), collapse = "")
          , "\\\\"),
        "&\\multicolumn{11}{l}{}\\\\",
        paste("AgHH def:", aghh.defs[1], "&",
          gsub("3\\)", "3)&", gsub("6\\)", "6)&", paste(paste0("(", 1:9, ")", collapse = "&"), "\\\\")))
          ),
        ltb[(grep("cline", ltb)+2):(grep("M.*ed in 2", ltb)[1]), ], 
        "&\\multicolumn{11}{l}{}\\\\",
        paste("AgHH def:", aghh.defs[2], "&",
          gsub("2\\)", "2)&", gsub("5\\)", "5)&", paste(paste0("(", 1:9+9, ")", collapse = "&"), "\\\\")))
          ),
        ltb[(grep("^Ag", ltb)[2]):(grep("M.*ed in 2", ltb)[2]), ],
        "&\\multicolumn{11}{l}{}\\\\",
        paste("AgHH def:", aghh.defs[3], "&",
          gsub("1\\)", "1)&", gsub("4\\)", "4)&", paste(paste0("(", 1:9+18, ")", collapse = "&"), "\\\\")))
          ),
        ltb[(grep("^Ag", ltb)[3]):(grep("M.*ed in 2", ltb)[3]), ], 
        "&\\multicolumn{11}{l}{}\\\\",
        paste("AgHH def:", aghh.defs[4], "&",
          gsub("0\\)", "0)&", gsub("3\\)", "3)&", paste(paste0("(", 1:9+27, ")", collapse = "&"), "\\\\")))
          ),
        ltb[(grep("^Ag", ltb)[4]):(grep("M.*ed in 2", ltb)[4]), ], "\\hline",
          ltb[nrow(ltb), ]
        ) 
      ltb <- gsub("CI \\(.*?\\)", "", ltb)
      ltb <- gsub("(^\\\\cline.*$)", "\\1[-1ex]", ltb)
      ltb <- gsub("households", "HHs", ltb)
      ltb <- gsub("Number of ol", "Ol", ltb)
      ltb <- ltb[!grepl("Raw", ltb)]
      write.tablev(ltb
        ,  paste0("../save/", c("Main", "Placebo")[ii], c(1999, 2002)[jj], "Older", (10:12)[s], 
             "ByGenderByAgHHdefResults.tex")
        ,  colnamestrue = F)
      #### HTML output
      mrtab <- rbind(mrtab, 
        "Thana fixed trends" = c("Thana fixed trends", rep(c("", "Y", "Y"), 3)),
        "HH fixed trends" = c("HH fixed trends", rep(c("", "", "Y"), 3)))
      Tab <- data.table(mrtab)
      setnames(Tab, paste0("v", 1:ncol(Tab)))
      aghhrows <- grep("^Ag", Tab[, v1])
      nt <- ncol(Tab)-1
      Tab <- rbind(t(c("", 1:nt)), Tab, use.names = F)
      for (iii in 2:length(aghhrows)) {
        setnames(Tab, paste0("v", 1:ncol(Tab)))
        aghhrows <- grep("^Ag", Tab[, v1])
        Tab <- rbind(
          Tab[1:(aghhrows[iii]-1), ],
          t(c("", nt*(iii-1)+1:nt)), 
          Tab[aghhrows[iii]:nrow(Tab), ], use.names = F) 
      }
      setnames(Tab, c("variables", paste0("v", 1:nt)))
      Tab <- lapply(Tab, function(x) gsub("\\\\mbox\\{\\\\tiny(.*)\\}", "\\1", x))
      Tab <- lapply(Tab, function(x) gsub("\\^.*", "", x))
      Tab <- lapply(Tab, function(x) gsub("\\\\phantom\\{-\\}", " ", x))
      Tab <- lapply(Tab, function(x) gsub("\\\\hspace\\{.*\\}", " ", x))
      Tab <- lapply(Tab, function(x) gsub(" *CI *\\(.*", " ", x))
      Tab <- lapply(Tab, function(x) gsub("\\\\underline\\{.*\\}", "____", x))
      Tab <- lapply(Tab, function(x) gsub("^.*bar.R.*", "R2", x))
      Tab <- do.call(cbind, Tab)
      colnames(Tab) <- c("variables", rep(paste("Spec", 1:3), 3))
      bgcolor <- "#ffcc99"
      kt <- kbl(Tab, align = paste0("r", paste0(rep("c", ncol(Tab)-1), collapse = "")), 
        row.names = F, caption = 
        "Main results: Enrollment impacts for year 1999-2002, 10-18 years old, direct offspring, Satterthwaite correction")
      kt <- kable_styling(kt, full_width = F, position = "left", fixed_thead = T, 
        bootstrap_options = c("condensed"))
      kt <- add_header_above(kt, c(" " = 1, "Boys+girls" = 3, "Boys" = 3, "Girls" = 3),
        background = bgcolor)
      kt <- row_spec(kt, 0, align = paste0(rep("c", ncol(Tab)-1), collapse = ""), 
        background = bgcolor)
      aghhrows <- grep("^Ag", Tab[, "variables"])
      kt <- row_spec(kt, rep(aghhrows, each = 3)+0:2, bold = F, 
        color = "black", background = "#87cefa")
      for (iii in 1:length(aghhrows)) 
        kt <- pack_rows(kt, group_label=aghh.defs[iii], aghhrows[iii]-1, aghhrows[iii]+2)
      kt <- pack_rows(kt, group_label="Common specifications", grep("^Tha", Tab[, 1]), grep("^HH", Tab[, 1]))
      kt <- scroll_box(kt, height = "800px")
      kt <- footnote(kt, 
                 general = "Sample of direct offspring of household heads. ",
                 number = c(SpecMemo1, SEMemoForSelectedResults)
                 )
      assign(paste0(c("M", "P")[ii], c(9, 2)[jj], (10:12)[s], "L"), ltb)
      assign(paste0(c("M", "P")[ii], c(9, 2)[jj], (10:12)[s]), mrtab)
      assign(paste0(c("M", "P")[ii], c(9, 2)[jj], (10:12)[s], "H"), kt)
      qsave(kt, paste0("../save/", c("M", "P")[ii], c(9, 2)[jj], (10:12)[s], "H.qs"))
