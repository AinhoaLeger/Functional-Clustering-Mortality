
#-------------------------------------------------------------------
# Select years
# This code allows to extract the years of each country which 
# curves were classified in cluster k
#-------------------------------------------------------------------

 yearsAUS <- str_extract(names(GF[GF==k]),"[A][U][S][0-9]+")
 yearsAUS <- yearsAUS[!is.na(yearsAUS)]
 yearsAUT <- str_extract(names(GF[GF==k]),"[A][U][T][0-9]+")
 yearsAUT <- yearsAUT[!is.na(yearsAUT)]
 yearsBEL <- str_extract(names(GF[GF==k]),"[B][E][L][0-9]+")
 yearsBEL <- yearsBEL[!is.na(yearsBEL)]
 yearsBGR <- str_extract(names(GF[GF==k]),"[B][G][R][0-9]+")
 yearsBGR <- yearsBGR[!is.na(yearsBGR)]
 yearsBLR <- str_extract(names(GF[GF==k]),"[B][L][R][0-9]+")
 yearsBLR <- yearsBLR[!is.na(yearsBLR)]
 yearsCAN <- str_extract(names(GF[GF==k]),"[C][A][N][0-9]+")
 yearsCAN <- yearsCAN[!is.na(yearsCAN)]
 yearsCHE <- str_extract(names(GF[GF==k]),"[C][H][E][0-9]+")
 yearsCHE <- yearsCHE[!is.na(yearsCHE)]
 yearsCZE <- str_extract(names(GF[GF==k]),"[C][Z][E][0-9]+")
 yearsCZE <- yearsCZE[!is.na(yearsCZE)]
 yearsDEUTE <- str_extract(names(GF[GF==k]),"[D][E][U][T][E][0-9]+")
 yearsDEUTE <- yearsDEUTE[!is.na(yearsDEUTE)]
 yearsDEUTW <- str_extract(names(GF[GF==k]),"[D][E][U][T][W][0-9]+")
 yearsDEUTW <- yearsDEUTW[!is.na(yearsDEUTW)]
 yearsDNK <- str_extract(names(GF[GF==k]),"[D][N][K][0-9]+")
 yearsDNK <- yearsDNK[!is.na(yearsDNK)]
 yearsESP <- str_extract(names(GF[GF==k]),"[E][S][P][0-9]+")
 yearsESP <- yearsESP[!is.na(yearsESP)]
 yearsEST <- str_extract(names(GF[GF==k]),"[E][S][T][0-9]+")
 yearsEST <- yearsEST[!is.na(yearsEST)]
 yearsFIN <- str_extract(names(GF[GF==k]),"[F][I][N][0-9]+")
 yearsFIN <- yearsFIN[!is.na(yearsFIN)]
 yearsFRA <- str_extract(names(GF[GF==k]),"[F][R][A][0-9]+")
 yearsFRA <- yearsFRA[!is.na(yearsFRA)]
 yearsGBR <- str_extract(names(GF[GF==k]),"[G][B][R][0-9]+")
 yearsGBR <- yearsGBR[!is.na(yearsGBR)]
 yearsHUN <- str_extract(names(GF[GF==k]),"[H][U][N][0-9]+")
 yearsHUN <- yearsHUN[!is.na(yearsHUN)]
 yearsIRL <- str_extract(names(GF[GF==k]),"[I][R][L][0-9]+")
 yearsIRL <- yearsIRL[!is.na(yearsIRL)]
 yearsITA <- str_extract(names(GF[GF==k]),"[I][T][A][0-9]+")
 yearsITA <- yearsITA[!is.na(yearsITA)]
 yearsJPN <- str_extract(names(GF[GF==k]),"[J][P][N][0-9]+")
 yearsJPN <- yearsJPN[!is.na(yearsJPN)]
 yearsLTU <- str_extract(names(GF[GF==k]),"[L][T][U][0-9]+")
 yearsLTU <- yearsLTU[!is.na(yearsLTU)]
 yearsLVA <- str_extract(names(GF[GF==k]),"[L][V][A][0-9]+")
 yearsLVA <- yearsLVA[!is.na(yearsLVA)]
 yearsNLD <- str_extract(names(GF[GF==k]),"[N][L][D][0-9]+")
 yearsNLD <- yearsNLD[!is.na(yearsNLD)]
 yearsNOR <- str_extract(names(GF[GF==k]),"[N][O][R][0-9]+")
 yearsNOR <- yearsNOR[!is.na(yearsNOR)]
 yearsNZL <- str_extract(names(GF[GF==k]),"[N][Z][L][0-9]+")
 yearsNZL <- yearsNZL[!is.na(yearsNZL)]
 yearsPOL <- str_extract(names(GF[GF==k]),"[P][O][L][0-9]+")
 yearsPOL <- yearsPOL[!is.na(yearsPOL)]
 yearsPRT <- str_extract(names(GF[GF==k]),"[P][R][T][0-9]+")
 yearsPRT <- yearsPRT[!is.na(yearsPRT)]
 yearsRUS <- str_extract(names(GF[GF==k]),"[R][U][S][0-9]+")
 yearsRUS <- yearsRUS[!is.na(yearsRUS)]
 yearsSVK <- str_extract(names(GF[GF==k]),"[S][V][K][0-9]+")
 yearsSVK <- yearsSVK[!is.na(yearsSVK)]
 yearsSWE <- str_extract(names(GF[GF==k]),"[S][W][E][0-9]+")
 yearsSWE <- yearsSWE[!is.na(yearsSWE)]
 yearsUKR <- str_extract(names(GF[GF==k]),"[U][K][R][0-9]+")
 yearsUKR <- yearsUKR[!is.na(yearsUKR)]
 yearsUSA <- str_extract(names(GF[GF==k]),"[U][S][A][0-9]+")
 yearsUSA <- yearsUSA[!is.na(yearsUSA)]

 years_selected <- c(yearsAUS,yearsAUT,yearsBEL,yearsBGR,yearsBLR,
	yearsCAN,yearsCHE,yearsCZE,yearsDEUTE,yearsDEUTW,yearsDNK,
	yearsESP,yearsEST,yearsFIN,yearsFRA,yearsGBR,yearsHUN,yearsIRL,
	yearsITA,yearsJPN,yearsLTU,yearsLVA,yearsNLD,yearsNOR,yearsNZL,
	yearsPOL,yearsPRT,yearsRUS,yearsSVK,yearsSWE,yearsUKR,yearsUSA)



