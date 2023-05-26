# This script processes the data obtained from the FRED-MD database. It includes our data transformations which differ from the "default" choices, and our classification of "fast" and "slow" variables
rm(list=ls())
library(HDLPrepro) #1.0.0
# in case the following package is not installed, run:
#install.packages("xtable")
library(xtable) #1.8-4

# Read in data ------------------------------------------------------------
raw_data<-read.csv(file=system.file("extdata", "current.csv", package="HDLPrepro", mustWork = TRUE),encoding = "UTF-8") #the file "current.csv" was obtained from https://research.stlouisfed.org/econ/mccracken/fred-databases/ on 9/9/2021
colnames(raw_data)[colnames(raw_data)=="S.P.500"]<-"S&P 500" #renamed S.P.500 -> S&P 500, read.csv() changed it
colnames(raw_data)[colnames(raw_data)=="S.P..indust"]<-"S&P: indust" #renamed S.P..indust -> S&P: indust, read.csv() changed it
colnames(raw_data)[colnames(raw_data)=="S.P.div.yield"]<-"S&P div yield" #renamed S.P.div.yield ->S&P div yield, read.csv() changed it
colnames(raw_data)[colnames(raw_data)=="S.P.PE.ratio"]<-"S&P PE ratio" #renamed S.P.PE.ratio -> S&P PE ratio, read.csv() changed it

# Transformation codes of Bernanke ----------------------------------------

# some of these variables are no longer in the FRED-MD database
variables<-c("IPFPNSS","IPFINAL","IPCONGD","IPDCONGD","IPNCONGD",
             "IPBUSEQ","IPMAT", "IPDMAT","IPNMAT","IPMANSICS",
             "IPB51222S","INDPRO","CUMFNS","NAPM","NAPMPI",
             "RPI","W875RX1","HWI","HWIURATIO","CLF16OV",
             "CE16OV","UNRATE","UEMPMEAN","UEMPLT5","UEMP5TO14",
             "UEMP15OV","UEMP15T26","PAYEMS", "CEU0500000001","USGOOD",
             "CES1021000001","USCONS","MANEMP","DMANEMP","NDMANEMP",
             "SRVPRD","USTPU","USWTRADE","USFIRE","USSERV",
             "USGOVT","AWHMAN","AWOTMAN","NAPMEI","DPCERA3M086SBEA",
             "DDURRG3M086SBEA","DNDGRG3M086SBEA","DSERRG3M086SBEA","HOUST","HOUSTNE",
             "HOUSTMW","HOUSTS","HOUSTW","NAPMII","NAPMNOI",
             "NAPMSDI","A0M008","A0M027","S&P 500","S&P: indust",
             "S&P div yield","S&P PE ratio","EXSZUSx","EXJPUSx","EXUSUKx",
             "EXCAUSx","FEDFUNDS","TB3MS","TB6MS","GS1",
             "GS5","GS10","AAA","BAA","TB3SMFFM",
             "TB6SMFFM","T1YFFM","T5YFFM","T10YFFM","AAAFFM",
             "BAAFFM","M1SL","M2SL","M3SL","M2REAL",
             "BOGMBASE","TOTRESNS","NONBORRES","BUSLOANS","NONREVSL",
             "NAPMPRI","WPSFD49207","WPSFD49502","WPSID61","WPSID62",
             "A0M099","CPIAUCSL","CPIAPPSL","CPITRNSL","CPIMEDSL",
             "CUSR0000SAC","CUSR0000SAD","CUSR0000SAS","CPIULFSL","CUSR0000SA0L2",
             "CUSR0000SA0L5","CES2000000008","CES3000000008","UMCSENTx"
)
DRI_mcgraw_names<-c("IPP", "IPF", "IPC", "IPCD", "IPCN",
                    "IPE", "IPM", "IPMD", "IPMND", "IPMFG",
                    "IPUT", "IP", "IPXMCA", "PMI", "PMP",
                    "GMPYQ ", "GMYXPQ","LHEL", "LHELX", "LHEM",
                    "LHNAG", "LHUR", "LHU680", "LHU5", "LHU14",
                    "LHU15", "LHU26", "LPNAG", "LP", "LPGD",
                    "LPMI", "LPCC", "LPEM", "LPED", "LPEN",
                    "LPSP", "LPTU", "LPT", "LPFR", "LPS",
                    "LPGOV", "LPHRM", "LPMOSA", "PMEMP", "GMCQ",
                    "GMCDQ", "GMCNQ", "GMCSQ", "HSFR", "HSNE",
                    "HSMW", "HSSOU", "HSWST", "PMNV", "PMNO",
                    "PMDEL", "MOCMQ", "MSONDQ", "FSPCOM", "FSPIN",
                    "FSDXP", "FSPXE", "EXRSW", "EXRJAN", "EXRUK",
                    "EXRCAN", "FYFF", "FYGM3", "FYGM6", "FYGT1",
                    "FYGT5", "FYGT10", "FYAAAC", "FYBAAC", "SFYGM3",
                    "SFYGM6", "SFYGT1", "SFYGT5", "SFYGT10", "SFYAAAC",
                    "SFYBAAC", "FM1", "FM2", "FM3", "FM2DQ",
                    "FMFBA", "FMRRA", "FMRNBA", "FCLNQ", "CCINRV",
                    "PMCP", "PWFSA", "PWFCSA", "PWIMSA", "PWCMSA",
                    "PSM99Q", "PUNEW", "PU83", "PU84", "PU85",
                    "PUC", "PUCD", "PUS", "PUXF", "PUXHS",
                    "PUXM", "LEHCC", "LEHM", "HHSNTN")
codes<-c(5,5,5,5,5,
         5,5,5,5,5,
         5,5,1,1,1,
         5,5,5,4,5,
         5,1,1,1,1,
         1,1,5,5,5,
         5,5,5,5,5,
         5,5,5,5,5,
         5,1,1,1,5,
         5,5,5,4,4,
         4,4,4,1,1,
         1,5,5,5,5,
         1,1,5,5,5,
         5,1,1,1,1,
         1,1,1,1,1,
         1,1,1,1,1,
         1,5,5,5,5,
         5,5,5,5,5,
         1,5,5,5,5,
         5,5,5,5,5,
         5,5,5,5,5,
         5,5,5,1
)
bernanke_codes<-data.frame(cbind(variables,codes, DRI_mcgraw_names))

# Define the slow and fast variables --------------------------------------

# slow & fast variables matched to the original Bernanke paper
slow_variables<-c("IPFPNSS", "IPFINAL", "IPCONGD", "IPDCONGD", "IPNCONGD", "IPBUSEQ",
                  "IPMAT", "IPDMAT", "IPNMAT", "IPMANSICS", "IPB51222S",
                  #"IPMINE" does't exist
                  "INDPRO", "CUMFNS",
                  #"NAPM", removed from FRED-MD
                  #"NAPMPI", removed from FRED-MD
                  "RPI", "W875RX1", "HWI",
                  "HWIURATIO", "CLF16OV", "CE16OV", "UNRATE", "UEMPMEAN",
                  "UEMPLT5", "UEMP5TO14", "UEMP15OV", "UEMP15T26", "PAYEMS",
                  #"CEU0500000001", doesn't exist
                  "USGOOD", "CES1021000001", "USCONS", "MANEMP", "DMANEMP",
                  "NDMANEMP", "SRVPRD", "USTPU", "USWTRADE", "USFIRE",
                  #"USSERV", doesn't exist
                  "USGOVT", "AWHMAN", "AWOTMAN",
                  #"NAPMEI", removed from FRED-MD
                  "DPCERA3M086SBEA",
                  "DDURRG3M086SBEA", #"DDURRA3M086SBEA", wrong name
                  "DNDGRG3M086SBEA",  #"DNDGRA3M086SBEA", wrong name
                  "DSERRG3M086SBEA", #"DSERRA3M086SBEA", wrong name
                  "WPSFD49207", "WPSFD49502",
                  "WPSID61", "WPSID62",
                  #"A0M099", doesn't exist
                  "CPIAUCSL", "CPIAPPSL", "CPITRNSL",
                  "CPIMEDSL", "CUSR0000SAC", "CUSR0000SAD", "CUSR0000SAS", "CPIULFSL",
                  "CUSR0000SA0L2", "CUSR0000SA0L5", "CES2000000008", "CES3000000008"
)
fast_variables<-c("HOUST", "HOUSTNE", "HOUSTMW", "HOUSTS", "HOUSTW",
                  #"NAPMII", removed from FRED-MD
                  #"NAPMNOI", removed from FRED-Md
                  #"NAPMSDI", removed from FRED-MD
                  #"A0M008", doesn't exist
                  #"A0M027", doesn't exist
                  "S&P 500", "S&P: indust", "S&P div yield",
                  "S&P PE ratio", "EXSZUSx", "EXJPUSx", "EXUSUKx", "EXCAUSx",
                  #"FEDFUNDS" this is the shock variable, kept separately
                  "TB3MS", "TB6MS", "GS1", "GS5", "GS10", "AAA", "BAA", "TB3SMFFM",
                  "TB6SMFFM",
                  "T1YFFM", "T5YFFM", "T10YFFM", "AAAFFM", "BAAFFM",
                  "M1SL", "M2SL",
                  #"M3SL", discontinued
                  "M2REAL",
                  "BOGMBASE", #previously called "AMBSL"
                  "TOTRESNS", "NONBORRES",
                  "BUSLOANS", "NONREVSL"
                  #"NAPMPRI" removed from FRED-MD
                  #"UMCSENTx" #too many missing(230)
)
# new variables from FRED-MD which we decided should be slow, based on which type of variable they are
# General rules following Bernanke:
# Prices: slow (except the NAPM COMMODITY PRICES INDEX by bernanke, and we also chose to exclude OILPRICEx, and have that in fast instead)
# Output & Income: slow
# Labor Market: slow
# Consumption: slow

# Interest & Exchange Rates: fast
# Money & Credit: fast
# Stock Market: fast
# Housing: fast
new_slow_variables<-c("IPFUELS", "UEMP27OV", "CLAIMSx", "USTRADE",
                      "CES0600000007",
                      "PPICMM",
                      "PCEPI",
                      "CES0600000008"
)
new_fast_variables<-c("CMRMTSPLx", "RETAILx",
                      "PERMIT", "PERMITNE", "PERMITMW",
                      "PERMITS",
                      "PERMITW",
                      #"ACOGNO", #too many missing(400)
                      "AMDMNOx",
                      #"ANDENOx",#too many missing(111)
                      "AMDMUOx", "BUSINVx" , "ISRATIOx",
                      "REALLN", "CONSPI",
                      #"MZMSL", discontinued
                      "CP3Mx", "COMPAPFFx",
                      #"TWEXAFEGSMTHx", #previously called "TWEXMMTH", too many missing (170)
                      "DTCOLNVHFNM", "DTCTHFNM", "INVEST",
                      "OILPRICEx"
)
federal_funds<-"FEDFUNDS"

removed_variables<-c("VXOCLSx", #too many missing (43)
                     "UMCSENTx", #too many missing(230)
                     "ACOGNO", #too many missing(400)
                     "ANDENOx", #too many missing(111)
                     "TWEXAFEGSMTHx" # too many missing (170)
)
# Clean the data ----------------------------------------------------------

# filling in the bernanke transformation codes wherever I can match their data to FRED-MD
fred_tcode<-as.numeric(c(1,raw_data[1,-1]))
fred_names<-colnames(raw_data)
b_tcode<-fred_tcode
for(i in 2:length(fred_names)){
  b_id<-which(group_which(fred_names[i],bernanke_codes$variables)==T)
  if(length(b_id)==1){
    if(!(b_tcode[i]==7 && as.numeric(bernanke_codes$codes[b_id])==5)){
      b_tcode[i]<-as.numeric(bernanke_codes$codes[b_id])
    }
  }
}
# to see the differences between the "regular" FRED-MD transformations and what we do 
data.frame(names=fred_names,fred=fred_tcode,bernanke=b_tcode)

# this should give a warning message about deleting 12 rows of missing data - intended behavior
CDbmedium<-clean_data(raw_data=raw_data,
                      slow_names = c(slow_variables, new_slow_variables),
                      FFR_name = "FEDFUNDS",
                      fast_names = c(fast_variables, new_fast_variables),
                      start_date = "1/1/1959",
                      end_date = "10/1/2008",
                      transform_codes = b_tcode
)

# Make tables for appendix ------------------------------------------------
# We first copy the relevant parts of tables in the Appendix of McCracken & Ng (2015),
# then match them to our specification

# Output & Income
OI<-data.frame(matrix(c(5, "RPI", "Real Personal Income",
                        5, "W875RX1", "Real personal income ex transfer receipts",
                        5, "INDPRO", "IP Index",
                        5, "IPFPNSS", "IP: Final Products and Nonindustrial Supplies",
                        5, "IPFINAL", "IP: Final Products (Market Group)",
                        5, "IPCONGD", "IP: Consumer Goods",
                        5, "IPDCONGD", "IP: Durable Consumer Goods",
                        5, "IPNCONGD", "IP: Nondurable Consumer Goods",
                        5, "IPBUSEQ", "IP: Business Equipment",
                        5, "IPMAT", "IP: Materials",
                        5, "IPDMAT", "IP: Durable Materials",
                        5, "IPNMAT", "IP: Nondurable Materials",
                        5, "IPMANSICS", "IP: Manufacturing (SIC)",
                        5, "IPB51222S", "IP: Residential Utilities",
                        5, "IPFUELS", "IP: Fuels",
                        1, "NAPMPI", "ISM Manufacturing: Production Index",
                        2, "CUMFNS", "Capacity Utilization: Manufacturing"), ncol=3, byrow =TRUE))

OI<-cbind(OI, rep(0, nrow(OI)), rep("-", nrow(OI)),rep(0, nrow(OI)),rep(0, nrow(OI)))
names(OI)=c("codes", "variables", "description", "matched_to_B", "DRI_McGraw_name","in_my_data", "fast_slow")
for(i in 1:nrow(OI)){
  for(j in 1:nrow(bernanke_codes)){
    if(OI$variables[i]==bernanke_codes$variables[j]){
      OI$matched_to_B[i]=1
      OI$DRI_McGraw_name[i]=bernanke_codes$DRI_mcgraw_names[j]
      if(OI$codes[i]!=bernanke_codes$codes[j]){
        OI$codes[i]=paste0(bernanke_codes$codes[j],"*")
      }
    }
  }
}
slow_names<-names(CDbmedium$slow_data)
fast_names<-names(CDbmedium$fast_data)
for(i in 1:nrow(OI)){
  for(j in 1:length(slow_names)){
    if(OI$variables[i]==slow_names[j]){
      OI$in_my_data[i]=1
      OI$fast_slow[i]="S"
    }
  }

  for(j in 1:length(fast_names)){
    if(OI$variables[i]==fast_names[j]){
      OI$in_my_data[i]=1
      OI$fast_slow[i]="F"
    }
  }

}
clean_OI<-data.frame(FRED=OI$variables, Description=OI$description, DRI_McGraw=OI$DRI_McGraw_name, Transformation=OI$codes, Fast_slow=OI$fast_slow)
clean_OI<-clean_OI[clean_OI$Fast_slow!=0,]

# Labor Market
LM<-data.frame(matrix(c(2, "HWI", "Help-Wanted Index for United States" ,
                        2, "HWIURATIO", "Ratio of Help Wanted/No. Unemployed",
                        5, "CLF16OV", "Civilian Labor Force",
                        5, "CE16OV", "Civilian Employment" ,
                        2, "UNRATE", "Civilian Unemployment Rate" ,
                        2, "UEMPMEAN", "Average Duration of Unemployment (Weeks)" ,
                        5, "UEMPLT5", "Civilians Unemployed - Less Than 5 Weeks" ,
                        5, "UEMP5TO14", "Civilians Unemployed for 5-14 Weeks" ,
                        5, "UEMP15OV", "Civilians Unemployed - 15 Weeks & Over" ,
                        5, "UEMP15T26", "Civilians Unemployed for 15-26 Weeks" ,
                        5, "UEMP27OV", "Civilians Unemployed for 27 Weeks and Over" ,
                        5, "CLAIMSx", "Initial Claims" ,
                        5, "PAYEMS", "All Employees: Total nonfarm" ,
                        5, "USGOOD", "All Employees: Goods-Producing Industries" ,
                        5, "CES1021000001", "All Employees: Mining and Logging: Mining" ,
                        5, "USCONS", "All Employees: Construction" ,
                        5, "MANEMP", "All Employees: Manufacturing" ,
                        5, "DMANEMP", "All Employees: Durable goods" ,
                        5, "NDMANEMP", "All Employees: Nondurable goods" ,
                        5, "SRVPRD", "All Employees: Service-Providing Industries" ,
                        5, "USTPU", "All Employees: Trade, Transportation & Utilities" ,
                        5, "USWTRADE", "All Employees: Wholesale Trade" ,
                        5, "USTRADE", "All Employees: Retail Trade" ,
                        5, "USFIRE", "All Employees: Financial Activities" ,
                        5, "USGOVT", "All Employees: Government",
                        1, "CES0600000007", "Avg Weekly Hours : Goods-Producing" ,
                        2, "AWOTMAN", "Avg Weekly Overtime Hours : Manufacturing" ,
                        1, "AWHMAN", "Avg Weekly Hours : Manufacturing" ,
                        1, "NAPMEI", "ISM Manufacturing: Employment Index" ,
                        6, "CES0600000008", "Avg Hourly Earnings : Goods-Producing" ,
                        6, "CES2000000008", "Avg Hourly Earnings : Construction" ,
                        6, "CES3000000008", "Avg Hourly Earnings : Manufacturing" ), ncol=3, byrow =TRUE))

LM<-cbind(LM, rep(0, nrow(LM)), rep("-", nrow(LM)),rep(0, nrow(LM)),rep(0, nrow(LM)))
names(LM)=c("codes", "variables", "description", "matched_to_B", "DRI_McGraw_name","in_my_data", "fast_slow")
for(i in 1:nrow(LM)){
  for(j in 1:nrow(bernanke_codes)){
    if(LM$variables[i]==bernanke_codes$variables[j]){
      LM$matched_to_B[i]=1
      LM$DRI_McGraw_name[i]=bernanke_codes$DRI_mcgraw_names[j]
      if(LM$codes[i]!=bernanke_codes$codes[j]){
        LM$codes[i]=paste0(bernanke_codes$codes[j],"*")
      }
    }
  }
}
slow_names<-names(CDbmedium$slow_data)
fast_names<-names(CDbmedium$fast_data)
for(i in 1:nrow(LM)){
  for(j in 1:length(slow_names)){
    if(LM$variables[i]==slow_names[j]){
      LM$in_my_data[i]=1
      LM$fast_slow[i]="S"
    }
  }

  for(j in 1:length(fast_names)){
    if(LM$variables[i]==fast_names[j]){
      LM$in_my_data[i]=1
      LM$fast_slow[i]="F"
    }
  }

}
clean_LM<-data.frame(FRED=LM$variables, Description=LM$description, DRI_McGraw=LM$DRI_McGraw_name, Transformation=LM$codes, Fast_slow=LM$fast_slow)
clean_LM<-clean_LM[clean_LM$Fast_slow!=0,]

# Consumption & Orders
CO<-data.frame(matrix(c(4, "HOUST", "Housing Starts: Total New Privately Owned" ,
                        4, "HOUSTNE", "Housing Starts, Northeast",
                        4, "HOUSTMW", "Housing Starts, Midwest",
                        4, "HOUSTS", "Housing Starts, South" ,
                        4, "HOUSTW", "Housing Starts, West",
                        4, "PERMIT", "New Private Housing Permits (SAAR)",
                        4, "PERMITNE", "New Private Housing Permits, Northeast (SAAR)",
                        4, "PERMITMW", "New Private Housing Permits, Midwest (SAAR)" ,
                        4, "PERMITS", "New Private Housing Permits, South (SAAR) ",
                        4, "PERMITW", "New Private Housing Permits, West (SAAR)"), ncol=3, byrow =TRUE))

CO<-cbind(CO, rep(0, nrow(CO)), rep("-", nrow(CO)),rep(0, nrow(CO)),rep(0, nrow(CO)))
names(CO)=c("codes", "variables", "description", "matched_to_B", "DRI_McGraw_name","in_my_data", "fast_slow")
for(i in 1:nrow(CO)){
  for(j in 1:nrow(bernanke_codes)){
    if(CO$variables[i]==bernanke_codes$variables[j]){
      CO$matched_to_B[i]=1
      CO$DRI_McGraw_name[i]=bernanke_codes$DRI_mcgraw_names[j]
      if(CO$codes[i]!=bernanke_codes$codes[j]){
        CO$codes[i]=paste0(bernanke_codes$codes[j],"*")
      }
    }
  }
}
slow_names<-names(CDbmedium$slow_data)
fast_names<-names(CDbmedium$fast_data)
for(i in 1:nrow(CO)){
  for(j in 1:length(slow_names)){
    if(CO$variables[i]==slow_names[j]){
      CO$in_my_data[i]=1
      CO$fast_slow[i]="S"
    }
  }

  for(j in 1:length(fast_names)){
    if(CO$variables[i]==fast_names[j]){
      CO$in_my_data[i]=1
      CO$fast_slow[i]="F"
    }
  }

}
clean_CO<-data.frame(FRED=CO$variables, Description=CO$description, DRI_McGraw=CO$DRI_McGraw_name, Transformation=CO$codes, Fast_slow=CO$fast_slow)
clean_CO<-clean_CO[clean_CO$Fast_slow!=0,]

# Orders & Inventories
OrIn<-data.frame(matrix(c(5, "DPCERA3M086SBEA", "Real personal consumption expenditures",
                          5, "CMRMTSPLx", "Real Manu. and Trade Industries Sales",
                          5, "RETAILx", "Retail and Food Services Sales" ,
                          1, "NAPM", "ISM : PMI Composite Index" ,
                          1, "NAPMNOI", "ISM : New Orders Index" ,
                          1, "NAPMSDI", "ISM : Supplier Deliveries Index",
                          1, "NAPMII", "ISM : Inventories Index" ,
                          5, "ACOGNO", "New Orders for Consumer Goods",
                          5, "AMDMNOx", "New Orders for Durable Goods",
                          5, "ANDENOx", "New Orders for Nondefense Capital Goods" ,
                          5, "AMDMUOx", "Unfilled Orders for Durable Goods",
                          5, "BUSINVx", "Total Business Inventories" ,
                          2, "ISRATIOx", "Total Business: Inventories to Sales Ratio",
                          2, "UMCSENTx", "Consumer Sentiment Index"), ncol=3, byrow =TRUE))
OrIn<-cbind(OrIn, rep(0, nrow(OrIn)), rep("-", nrow(OrIn)),rep(0, nrow(OrIn)),rep(0, nrow(OrIn)))
names(OrIn)=c("codes", "variables", "description", "matched_to_B", "DRI_McGraw_name","in_my_data", "fast_slow")
for(i in 1:nrow(OrIn)){
  for(j in 1:nrow(bernanke_codes)){
    if(OrIn$variables[i]==bernanke_codes$variables[j]){
      OrIn$matched_to_B[i]=1
      OrIn$DRI_McGraw_name[i]=bernanke_codes$DRI_mcgraw_names[j]
      if(OrIn$codes[i]!=bernanke_codes$codes[j]){
        OrIn$codes[i]=paste0(bernanke_codes$codes[j],"*")
      }
    }
  }
}
slow_names<-names(CDbmedium$slow_data)
fast_names<-names(CDbmedium$fast_data)
for(i in 1:nrow(OrIn)){
  for(j in 1:length(slow_names)){
    if(OrIn$variables[i]==slow_names[j]){
      OrIn$in_my_data[i]=1
      OrIn$fast_slow[i]="S"
    }
  }

  for(j in 1:length(fast_names)){
    if(OrIn$variables[i]==fast_names[j]){
      OrIn$in_my_data[i]=1
      OrIn$fast_slow[i]="F"
    }
  }

}
clean_OrIn<-data.frame(FRED=OrIn$variables, Description=OrIn$description, DRI_McGraw=OrIn$DRI_McGraw_name, Transformation=OrIn$codes, Fast_slow=OrIn$fast_slow)
clean_OrIn<-clean_OrIn[clean_OrIn$Fast_slow!=0,]

# Money & Credit
MC<-data.frame(matrix(c(6, "M1SL", "M1 Money Stock",
                        6, "M2SL", "M2 Money Stock",
                        5, "M2REAL", "Real M2 Money Stock",
                        6, "BOGMBASE", "St. Louis Adjusted Monetary Base",
                        6, "TOTRESNS", "Total Reserves of Depository Institutions",
                        7, "NONBORRES", "Reserves Of Depository Institutions",
                        6, "BUSLOANS", "Commercial and Industrial Loans",
                        6, "REALLN", "Real Estate Loans at All Commercial Banks",
                        6, "NONREVSL", "Total Nonrevolving Credit",
                        2, "CONSPI", "Nonrevolving consumer credit to Personal Income",
                        6, "MZMSL", "MZM Money Stock",
                        6, "DTCOLNVHFNM", "Consumer Motor Vehicle Loans Outstanding",
                        6, "DTCTHFNM", "Total Consumer Loans and Leases Outstanding",
                        6, "INVEST", "Securities in Bank Credit at All Commercial Banks"), ncol=3, byrow =TRUE))
MC<-cbind(MC, rep(0, nrow(MC)), rep("-", nrow(MC)),rep(0, nrow(MC)),rep(0, nrow(MC)))
names(MC)=c("codes", "variables", "description", "matched_to_B", "DRI_McGraw_name","in_my_data", "fast_slow")
for(i in 1:nrow(MC)){
  for(j in 1:nrow(bernanke_codes)){
    if(MC$variables[i]==bernanke_codes$variables[j]){
      MC$matched_to_B[i]=1
      MC$DRI_McGraw_name[i]=bernanke_codes$DRI_mcgraw_names[j]
      if(MC$codes[i]!=bernanke_codes$codes[j]){
        MC$codes[i]=paste0(bernanke_codes$codes[j],"*")
      }
    }
  }
}
slow_names<-names(CDbmedium$slow_data)
fast_names<-names(CDbmedium$fast_data)
for(i in 1:nrow(MC)){
  for(j in 1:length(slow_names)){
    if(MC$variables[i]==slow_names[j]){
      MC$in_my_data[i]=1
      MC$fast_slow[i]="S"
    }
  }

  for(j in 1:length(fast_names)){
    if(MC$variables[i]==fast_names[j]){
      MC$in_my_data[i]=1
      MC$fast_slow[i]="F"
    }
  }

}
clean_MC<-data.frame(FRED=MC$variables, Description=MC$description, DRI_McGraw=MC$DRI_McGraw_name, Transformation=MC$codes, Fast_slow=MC$fast_slow)
clean_MC<-clean_MC[clean_MC$Fast_slow!=0,]

# Interest rate & Exchange rates
IE<-data.frame(matrix(c(2, "FEDFUNDS", "Effective Federal Funds Rate" ,
                        2, "CP3Mx", "3-Month AA Financial Commercial Paper Rate" ,
                        2, "TB3MS", "3-Month Treasury Bill",
                        2, "TB6MS", "6-Month Treasury Bill",
                        2, "GS1", "1-Year Treasury Rate" ,
                        2, "GS5", "5-Year Treasury Rate" ,
                        2, "GS10", "10-Year Treasury Rate" ,
                        2, "AAA", "Moody's Seasoned Aaa Corporate Bond Yield",
                        2, "BAA", "Moody's Seasoned Baa Corporate Bond Yield" ,
                        1, "COMPAPFFx", "3-Month Commercial Paper Minus FEDFUNDS" ,
                        1, "TB3SMFFM", "3-Month Treasury C Minus FEDFUNDS" ,
                        1, "TB6SMFFM", "6-Month Treasury C Minus FEDFUNDS" ,
                        1, "T1YFFM", "1-Year Treasury C Minus FEDFUNDS" ,
                        1, "T5YFFM", "5-Year Treasury C Minus FEDFUNDS",
                        1, "T10YFFM", "10-Year Treasury C Minus FEDFUNDS" ,
                        1, "AAAFFM", "Moody's Aaa Corporate Bond Minus FEDFUNDS" ,
                        1, "BAAFFM", "Moody's Baa Corporate Bond Minus FEDFUNDS" ,
                        5, "TWEXMMTH", "Trade Weighted U.S. Dollar Index: Major Currencies" ,
                        5, "EXSZUSx", "Switzerland / U.S. Foreign Exchange Rate" ,
                        5, "EXJPUSx", "Japan / U.S. Foreign Exchange Rate" ,
                        5, "EXUSUKx", "U.S. / U.K. Foreign Exchange Rate" ,
                        5, "EXCAUSx", "Canada / U.S. Foreign Exchange Rate" ), ncol=3, byrow =TRUE))
IE<-cbind(IE, rep(0, nrow(IE)), rep("-", nrow(IE)),rep(0, nrow(IE)),rep(0, nrow(IE)))
names(IE)=c("codes", "variables", "description", "matched_to_B", "DRI_McGraw_name","in_my_data", "fast_slow")
for(i in 1:nrow(IE)){
  for(j in 1:nrow(bernanke_codes)){
    if(IE$variables[i]==bernanke_codes$variables[j]){
      IE$matched_to_B[i]=1
      IE$DRI_McGraw_name[i]=bernanke_codes$DRI_mcgraw_names[j]
      if(IE$codes[i]!=bernanke_codes$codes[j]){
        IE$codes[i]=paste0(bernanke_codes$codes[j],"*")
      }
    }
  }
}
slow_names<-names(CDbmedium$slow_data)
fast_names<-c(names(CDbmedium$fast_data),"FEDFUNDS")
for(i in 1:nrow(IE)){
  for(j in 1:length(slow_names)){
    if(IE$variables[i]==slow_names[j]){
      IE$in_my_data[i]=1
      IE$fast_slow[i]="S"
    }
  }

  for(j in 1:length(fast_names)){
    if(IE$variables[i]==fast_names[j]){
      IE$in_my_data[i]=1
      IE$fast_slow[i]="F"
    }
  }

}
clean_IE<-data.frame(FRED=IE$variables, Description=IE$description, DRI_McGraw=IE$DRI_McGraw_name, Transformation=IE$codes, Fast_slow=IE$fast_slow)
clean_IE<-clean_IE[clean_IE$Fast_slow!=0,]

# Prices
Pr<-data.frame(matrix(c(6, "WPSFD49207", "PPI: Finished Goods" ,
                        6, "WPSFD49502", "PPI: Finished Consumer Goods" ,
                        6, "WPSID61", "PPI: Intermediate Materials" ,
                        6, "WPSID62", "PPI: Crude Materials" ,
                        6, "OILPRICEx", "Crude Oil, spliced WTI and Cushing" ,
                        6, "PPICMM", "PPI: Metals and metal products",
                        1, "NAPMPRI", "ISM Manufacturing: Prices Index" ,
                        6, "CPIAUCSL", "CPI : All Items" ,
                        6, "CPIAPPSL", "CPI : Apparel" ,
                        6, "CPITRNSL", "CPI : Transportation" ,
                        6, "CPIMEDSL", "CPI : Medical Care",
                        6, "CUSR0000SAC", "CPI : Commodities" ,
                        6, "CUSR0000SAD", "CPI : Durables" ,
                        6, "CUSR0000SAS", "CPI : Services",
                        6, "CPIULFSL", "CPI : All Items Less Food" ,
                        6, "CUSR0000SA0L2", "CPI : All items less shelter",
                        6, "CUSR0000SA0L5", "CPI : All items less medical care" ,
                        6, "PCEPI", "Personal Cons. Expend.: Chain Index",
                        6, "DDURRG3M086SBEA", "Personal Cons. Exp: Durable goods",
                        6, "DNDGRG3M086SBEA", "Personal Cons. Exp: Nondurable goods" ,
                        6, "DSERRG3M086SBEA", "Personal Cons. Exp: Services"), ncol=3, byrow =TRUE))
Pr<-cbind(Pr, rep(0, nrow(Pr)), rep("-", nrow(Pr)),rep(0, nrow(Pr)),rep(0, nrow(Pr)))
names(Pr)=c("codes", "variables", "description", "matched_to_B", "DRI_McGraw_name","in_my_data", "fast_slow")
for(i in 1:nrow(Pr)){
  for(j in 1:nrow(bernanke_codes)){
    if(Pr$variables[i]==bernanke_codes$variables[j]){
      Pr$matched_to_B[i]=1
      Pr$DRI_McGraw_name[i]=bernanke_codes$DRI_mcgraw_names[j]
      if(Pr$codes[i]!=bernanke_codes$codes[j]){
        Pr$codes[i]=paste0(bernanke_codes$codes[j],"*")
      }
    }
  }
}
slow_names<-names(CDbmedium$slow_data)
fast_names<-names(CDbmedium$fast_data)
for(i in 1:nrow(Pr)){
  for(j in 1:length(slow_names)){
    if(Pr$variables[i]==slow_names[j]){
      Pr$in_my_data[i]=1
      Pr$fast_slow[i]="S"
    }
  }

  for(j in 1:length(fast_names)){
    if(Pr$variables[i]==fast_names[j]){
      Pr$in_my_data[i]=1
      Pr$fast_slow[i]="F"
    }
  }

}
clean_Pr<-data.frame(FRED=Pr$variables, Description=Pr$description, DRI_McGraw=Pr$DRI_McGraw_name, Transformation=Pr$codes, Fast_slow=Pr$fast_slow)
clean_Pr<-clean_Pr[clean_Pr$Fast_slow!=0,]

# Stock Market
SM<-data.frame(matrix(c(5, "S&P 500", "S&P's Common Stock Price Index: Composite",
                        5, "S&P: indust", "S&P's Common Stock Price Index: Industrials",
                        2, "S&P div yield", "S&P's Composite Common Stock: Dividend Yield",
                        5, "S&P PE ratio", "S&P's Composite Common Stock: Price-Earnings Ratio"), ncol=3, byrow =TRUE))
SM<-cbind(SM, rep(0, nrow(SM)), rep("-", nrow(SM)),rep(0, nrow(SM)),rep(0, nrow(SM)))
names(SM)=c("codes", "variables", "description", "matched_to_B", "DRI_McGraw_name","in_my_data", "fast_slow")
for(i in 1:nrow(SM)){
  for(j in 1:nrow(bernanke_codes)){
    if(SM$variables[i]==bernanke_codes$variables[j]){
      SM$matched_to_B[i]=1
      SM$DRI_McGraw_name[i]=bernanke_codes$DRI_mcgraw_names[j]
      if(SM$codes[i]!=bernanke_codes$codes[j]){
        SM$codes[i]=paste0(bernanke_codes$codes[j],"*")
      }
    }
  }
}
slow_names<-names(CDbmedium$slow_data)
fast_names<-names(CDbmedium$fast_data)
fast_names[6:9]<-c("S&P 500", "S&P: indust", "S&P div yield", "S&P PE ratio")
for(i in 1:nrow(SM)){
  for(j in 1:length(slow_names)){
    if(SM$variables[i]==slow_names[j]){
      SM$in_my_data[i]=1
      SM$fast_slow[i]="S"
    }
  }

  for(j in 1:length(fast_names)){
    if(SM$variables[i]==fast_names[j]){
      SM$in_my_data[i]=1
      SM$fast_slow[i]="F"
    }
  }

}
clean_SM<-data.frame(FRED=SM$variables, Description=SM$description, DRI_McGraw=SM$DRI_McGraw_name, Transformation=SM$codes, Fast_slow=SM$fast_slow)
clean_SM<-clean_SM[clean_SM$Fast_slow!=0,]

# export to latex ---------------------------------------------------------
# checking that all variables are accounted for
nrow(clean_OI)+nrow(clean_LM)+nrow(clean_CO)+nrow(clean_OrIn)+nrow(clean_MC)+nrow(clean_IE)+nrow(clean_Pr)+nrow(clean_SM)
xtable(clean_OI)
xtable(clean_LM)
xtable(clean_CO)
xtable(clean_OrIn)
xtable(clean_MC)
xtable(clean_IE)
xtable(clean_Pr)
xtable(clean_SM)
