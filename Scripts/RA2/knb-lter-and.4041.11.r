# Package ID: knb-lter-and.4041.11 Cataloging System:https://pasta.lternet.edu.
# Data set title: LTER Intersite Fine Litter Decomposition Experiment (LIDET), 1990 to 2002.
# Data set creator:    - Andrews Forest LTER Site 
# Data set creator:  Mark Harmon -  
# Metadata Provider:  Becky Fasth -  
# Contact:    - Information Manager LTER Network Office  - tech-support@lternet.edu
# Contact:    - Information Manager   - hjaweb@fsl.orst.edu
# Contact:  Mark Harmon -    - mark.harmon@oregonstate.edu
# Metadata Link: https://portal.lternet.edu/nis/metadataviewer?packageid=knb-lter-and.4041.11
# Stylesheet v2.11 for metadata conversion into program: John H. Porter, Univ. Virginia, jporter@virginia.edu 

inUrl1  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-and/4041/11/2566660859c9ccd8c49226c6f0c1f0bd" 
infile1 <- tempfile()
try(download.file(inUrl1,infile1,method="curl"))
if (is.na(file.size(infile1))) download.file(inUrl1,infile1,method="auto")

                   
 dt1 <-read.csv(infile1,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "STCODE",     
                    "FORMAT",     
                    "SITE",     
                    "REP",     
                    "DURATION",     
                    "SPECIES",     
                    "TYPE",     
                    "NIR_NUM",     
                    "MESH",     
                    "FILL_DATE",     
                    "IADW",     
                    "IODW"    ), check.names=TRUE)
               
unlink(infile1)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt1$STCODE)!="factor") dt1$STCODE<- as.factor(dt1$STCODE)
if (class(dt1$FORMAT)=="factor") dt1$FORMAT <-as.numeric(levels(dt1$FORMAT))[as.integer(dt1$FORMAT) ]               
if (class(dt1$FORMAT)=="character") dt1$FORMAT <-as.numeric(dt1$FORMAT)
if (class(dt1$SITE)!="factor") dt1$SITE<- as.factor(dt1$SITE)
if (class(dt1$REP)!="factor") dt1$REP<- as.factor(dt1$REP)
if (class(dt1$DURATION)=="factor") dt1$DURATION <-as.numeric(levels(dt1$DURATION))[as.integer(dt1$DURATION) ]               
if (class(dt1$DURATION)=="character") dt1$DURATION <-as.numeric(dt1$DURATION)
if (class(dt1$SPECIES)!="factor") dt1$SPECIES<- as.factor(dt1$SPECIES)
if (class(dt1$TYPE)!="factor") dt1$TYPE<- as.factor(dt1$TYPE)
if (class(dt1$NIR_NUM)=="factor") dt1$NIR_NUM <-as.numeric(levels(dt1$NIR_NUM))[as.integer(dt1$NIR_NUM) ]               
if (class(dt1$NIR_NUM)=="character") dt1$NIR_NUM <-as.numeric(dt1$NIR_NUM)
if (class(dt1$MESH)=="factor") dt1$MESH <-as.numeric(levels(dt1$MESH))[as.integer(dt1$MESH) ]               
if (class(dt1$MESH)=="character") dt1$MESH <-as.numeric(dt1$MESH)                                   
# attempting to convert dt1$FILL_DATE dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp1FILL_DATE<-as.Date(dt1$FILL_DATE,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp1FILL_DATE) == length(tmp1FILL_DATE[!is.na(tmp1FILL_DATE)])){dt1$FILL_DATE <- tmp1FILL_DATE } else {print("Date conversion failed for dt1$FILL_DATE. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp1FILL_DATE) 
if (class(dt1$IADW)=="factor") dt1$IADW <-as.numeric(levels(dt1$IADW))[as.integer(dt1$IADW) ]               
if (class(dt1$IADW)=="character") dt1$IADW <-as.numeric(dt1$IADW)
if (class(dt1$IODW)=="factor") dt1$IODW <-as.numeric(levels(dt1$IODW))[as.integer(dt1$IODW) ]               
if (class(dt1$IODW)=="character") dt1$IODW <-as.numeric(dt1$IODW)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt1)                            
attach(dt1)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(STCODE)
summary(FORMAT)
summary(SITE)
summary(REP)
summary(DURATION)
summary(SPECIES)
summary(TYPE)
summary(NIR_NUM)
summary(MESH)
summary(FILL_DATE)
summary(IADW)
summary(IODW) 
                # Get more details on character variables
                 
summary(as.factor(dt1$STCODE)) 
summary(as.factor(dt1$SITE)) 
summary(as.factor(dt1$REP)) 
summary(as.factor(dt1$SPECIES)) 
summary(as.factor(dt1$TYPE))
detach(dt1)               
         

inUrl2  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-and/4041/11/22580af809b2925546e213e404c04e90" 
infile2 <- tempfile()
try(download.file(inUrl2,infile2,method="curl"))
if (is.na(file.size(infile2))) download.file(inUrl2,infile2,method="auto")

                   
 dt2 <-read.csv(infile2,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "STCODE",     
                    "FORMAT",     
                    "SITE",     
                    "REP",     
                    "DURATION",     
                    "SPECIES",     
                    "TYPE",     
                    "NIR_NUM",     
                    "DUP",     
                    "ANALY_DATE",     
                    "NIR_N",     
                    "NIR_PAFNN",     
                    "NIR_NPE",     
                    "NIR_LIGNIN",     
                    "NIR_WSCARB",     
                    "NIR_TANNIN",     
                    "LAB",     
                    "COMMENT"    ), check.names=TRUE)
               
unlink(infile2)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt2$STCODE)!="factor") dt2$STCODE<- as.factor(dt2$STCODE)
if (class(dt2$FORMAT)=="factor") dt2$FORMAT <-as.numeric(levels(dt2$FORMAT))[as.integer(dt2$FORMAT) ]               
if (class(dt2$FORMAT)=="character") dt2$FORMAT <-as.numeric(dt2$FORMAT)
if (class(dt2$SITE)!="factor") dt2$SITE<- as.factor(dt2$SITE)
if (class(dt2$REP)!="factor") dt2$REP<- as.factor(dt2$REP)
if (class(dt2$DURATION)=="factor") dt2$DURATION <-as.numeric(levels(dt2$DURATION))[as.integer(dt2$DURATION) ]               
if (class(dt2$DURATION)=="character") dt2$DURATION <-as.numeric(dt2$DURATION)
if (class(dt2$SPECIES)!="factor") dt2$SPECIES<- as.factor(dt2$SPECIES)
if (class(dt2$TYPE)!="factor") dt2$TYPE<- as.factor(dt2$TYPE)
if (class(dt2$NIR_NUM)=="factor") dt2$NIR_NUM <-as.numeric(levels(dt2$NIR_NUM))[as.integer(dt2$NIR_NUM) ]               
if (class(dt2$NIR_NUM)=="character") dt2$NIR_NUM <-as.numeric(dt2$NIR_NUM)
if (class(dt2$DUP)!="factor") dt2$DUP<- as.factor(dt2$DUP)                                   
# attempting to convert dt2$ANALY_DATE dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp2ANALY_DATE<-as.Date(dt2$ANALY_DATE,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp2ANALY_DATE) == length(tmp2ANALY_DATE[!is.na(tmp2ANALY_DATE)])){dt2$ANALY_DATE <- tmp2ANALY_DATE } else {print("Date conversion failed for dt2$ANALY_DATE. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp2ANALY_DATE) 
if (class(dt2$NIR_N)=="factor") dt2$NIR_N <-as.numeric(levels(dt2$NIR_N))[as.integer(dt2$NIR_N) ]               
if (class(dt2$NIR_N)=="character") dt2$NIR_N <-as.numeric(dt2$NIR_N)
if (class(dt2$NIR_PAFNN)=="factor") dt2$NIR_PAFNN <-as.numeric(levels(dt2$NIR_PAFNN))[as.integer(dt2$NIR_PAFNN) ]               
if (class(dt2$NIR_PAFNN)=="character") dt2$NIR_PAFNN <-as.numeric(dt2$NIR_PAFNN)
if (class(dt2$NIR_NPE)=="factor") dt2$NIR_NPE <-as.numeric(levels(dt2$NIR_NPE))[as.integer(dt2$NIR_NPE) ]               
if (class(dt2$NIR_NPE)=="character") dt2$NIR_NPE <-as.numeric(dt2$NIR_NPE)
if (class(dt2$NIR_LIGNIN)=="factor") dt2$NIR_LIGNIN <-as.numeric(levels(dt2$NIR_LIGNIN))[as.integer(dt2$NIR_LIGNIN) ]               
if (class(dt2$NIR_LIGNIN)=="character") dt2$NIR_LIGNIN <-as.numeric(dt2$NIR_LIGNIN)
if (class(dt2$NIR_WSCARB)=="factor") dt2$NIR_WSCARB <-as.numeric(levels(dt2$NIR_WSCARB))[as.integer(dt2$NIR_WSCARB) ]               
if (class(dt2$NIR_WSCARB)=="character") dt2$NIR_WSCARB <-as.numeric(dt2$NIR_WSCARB)
if (class(dt2$NIR_TANNIN)=="factor") dt2$NIR_TANNIN <-as.numeric(levels(dt2$NIR_TANNIN))[as.integer(dt2$NIR_TANNIN) ]               
if (class(dt2$NIR_TANNIN)=="character") dt2$NIR_TANNIN <-as.numeric(dt2$NIR_TANNIN)
if (class(dt2$LAB)!="factor") dt2$LAB<- as.factor(dt2$LAB)
if (class(dt2$COMMENT)!="factor") dt2$COMMENT<- as.factor(dt2$COMMENT)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt2)                            
attach(dt2)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(STCODE)
summary(FORMAT)
summary(SITE)
summary(REP)
summary(DURATION)
summary(SPECIES)
summary(TYPE)
summary(NIR_NUM)
summary(DUP)
summary(ANALY_DATE)
summary(NIR_N)
summary(NIR_PAFNN)
summary(NIR_NPE)
summary(NIR_LIGNIN)
summary(NIR_WSCARB)
summary(NIR_TANNIN)
summary(LAB)
summary(COMMENT) 
                # Get more details on character variables
                 
summary(as.factor(dt2$STCODE)) 
summary(as.factor(dt2$SITE)) 
summary(as.factor(dt2$REP)) 
summary(as.factor(dt2$SPECIES)) 
summary(as.factor(dt2$TYPE)) 
summary(as.factor(dt2$DUP)) 
summary(as.factor(dt2$LAB)) 
summary(as.factor(dt2$COMMENT))
detach(dt2)               
         

inUrl3  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-and/4041/11/e6565748d4efafac3d825b39a32a9315" 
infile3 <- tempfile()
try(download.file(inUrl3,infile3,method="curl"))
if (is.na(file.size(infile3))) download.file(inUrl3,infile3,method="auto")

                   
 dt3 <-read.csv(infile3,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "STCODE",     
                    "FORMAT",     
                    "SPECIES",     
                    "BATCH",     
                    "TYPE1",     
                    "NIR_NUM",     
                    "DUP",     
                    "SAMPLEDATE",     
                    "DURATION",     
                    "REP",     
                    "SITE",     
                    "ASH",     
                    "NPE",     
                    "WS",     
                    "ACIDSOL",     
                    "LIGNIN",     
                    "TANNIN",     
                    "WSCARB",     
                    "ASCARB",     
                    "CARBON",     
                    "NITROGEN",     
                    "LAB"    ), check.names=TRUE)
               
unlink(infile3)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt3$STCODE)!="factor") dt3$STCODE<- as.factor(dt3$STCODE)
if (class(dt3$FORMAT)=="factor") dt3$FORMAT <-as.numeric(levels(dt3$FORMAT))[as.integer(dt3$FORMAT) ]               
if (class(dt3$FORMAT)=="character") dt3$FORMAT <-as.numeric(dt3$FORMAT)
if (class(dt3$SPECIES)!="factor") dt3$SPECIES<- as.factor(dt3$SPECIES)
if (class(dt3$BATCH)!="factor") dt3$BATCH<- as.factor(dt3$BATCH)
if (class(dt3$TYPE1)!="factor") dt3$TYPE1<- as.factor(dt3$TYPE1)
if (class(dt3$NIR_NUM)=="factor") dt3$NIR_NUM <-as.numeric(levels(dt3$NIR_NUM))[as.integer(dt3$NIR_NUM) ]               
if (class(dt3$NIR_NUM)=="character") dt3$NIR_NUM <-as.numeric(dt3$NIR_NUM)
if (class(dt3$DUP)!="factor") dt3$DUP<- as.factor(dt3$DUP)                                   
# attempting to convert dt3$SAMPLEDATE dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d %H:%M:%S" 
tmp3SAMPLEDATE<-as.POSIXct(dt3$SAMPLEDATE,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp3SAMPLEDATE) == length(tmp3SAMPLEDATE[!is.na(tmp3SAMPLEDATE)])){dt3$SAMPLEDATE <- tmp3SAMPLEDATE } else {print("Date conversion failed for dt3$SAMPLEDATE. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp3SAMPLEDATE) 
if (class(dt3$DURATION)=="factor") dt3$DURATION <-as.numeric(levels(dt3$DURATION))[as.integer(dt3$DURATION) ]               
if (class(dt3$DURATION)=="character") dt3$DURATION <-as.numeric(dt3$DURATION)
if (class(dt3$REP)!="factor") dt3$REP<- as.factor(dt3$REP)
if (class(dt3$SITE)!="factor") dt3$SITE<- as.factor(dt3$SITE)
if (class(dt3$ASH)=="factor") dt3$ASH <-as.numeric(levels(dt3$ASH))[as.integer(dt3$ASH) ]               
if (class(dt3$ASH)=="character") dt3$ASH <-as.numeric(dt3$ASH)
if (class(dt3$NPE)=="factor") dt3$NPE <-as.numeric(levels(dt3$NPE))[as.integer(dt3$NPE) ]               
if (class(dt3$NPE)=="character") dt3$NPE <-as.numeric(dt3$NPE)
if (class(dt3$WS)=="factor") dt3$WS <-as.numeric(levels(dt3$WS))[as.integer(dt3$WS) ]               
if (class(dt3$WS)=="character") dt3$WS <-as.numeric(dt3$WS)
if (class(dt3$ACIDSOL)=="factor") dt3$ACIDSOL <-as.numeric(levels(dt3$ACIDSOL))[as.integer(dt3$ACIDSOL) ]               
if (class(dt3$ACIDSOL)=="character") dt3$ACIDSOL <-as.numeric(dt3$ACIDSOL)
if (class(dt3$LIGNIN)=="factor") dt3$LIGNIN <-as.numeric(levels(dt3$LIGNIN))[as.integer(dt3$LIGNIN) ]               
if (class(dt3$LIGNIN)=="character") dt3$LIGNIN <-as.numeric(dt3$LIGNIN)
if (class(dt3$TANNIN)=="factor") dt3$TANNIN <-as.numeric(levels(dt3$TANNIN))[as.integer(dt3$TANNIN) ]               
if (class(dt3$TANNIN)=="character") dt3$TANNIN <-as.numeric(dt3$TANNIN)
if (class(dt3$WSCARB)=="factor") dt3$WSCARB <-as.numeric(levels(dt3$WSCARB))[as.integer(dt3$WSCARB) ]               
if (class(dt3$WSCARB)=="character") dt3$WSCARB <-as.numeric(dt3$WSCARB)
if (class(dt3$ASCARB)=="factor") dt3$ASCARB <-as.numeric(levels(dt3$ASCARB))[as.integer(dt3$ASCARB) ]               
if (class(dt3$ASCARB)=="character") dt3$ASCARB <-as.numeric(dt3$ASCARB)
if (class(dt3$CARBON)=="factor") dt3$CARBON <-as.numeric(levels(dt3$CARBON))[as.integer(dt3$CARBON) ]               
if (class(dt3$CARBON)=="character") dt3$CARBON <-as.numeric(dt3$CARBON)
if (class(dt3$NITROGEN)=="factor") dt3$NITROGEN <-as.numeric(levels(dt3$NITROGEN))[as.integer(dt3$NITROGEN) ]               
if (class(dt3$NITROGEN)=="character") dt3$NITROGEN <-as.numeric(dt3$NITROGEN)
if (class(dt3$LAB)!="factor") dt3$LAB<- as.factor(dt3$LAB)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt3)                            
attach(dt3)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(STCODE)
summary(FORMAT)
summary(SPECIES)
summary(BATCH)
summary(TYPE1)
summary(NIR_NUM)
summary(DUP)
summary(SAMPLEDATE)
summary(DURATION)
summary(REP)
summary(SITE)
summary(ASH)
summary(NPE)
summary(WS)
summary(ACIDSOL)
summary(LIGNIN)
summary(TANNIN)
summary(WSCARB)
summary(ASCARB)
summary(CARBON)
summary(NITROGEN)
summary(LAB) 
                # Get more details on character variables
                 
summary(as.factor(dt3$STCODE)) 
summary(as.factor(dt3$SPECIES)) 
summary(as.factor(dt3$BATCH)) 
summary(as.factor(dt3$TYPE1)) 
summary(as.factor(dt3$DUP)) 
summary(as.factor(dt3$REP)) 
summary(as.factor(dt3$SITE)) 
summary(as.factor(dt3$LAB))
detach(dt3)               
         

inUrl4  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-and/4041/11/7255601b1ada3505dc88279846513e3f" 
infile4 <- tempfile()
try(download.file(inUrl4,infile4,method="curl"))
if (is.na(file.size(infile4))) download.file(inUrl4,infile4,method="auto")

                   
 dt4 <-read.csv(infile4,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "STCODE",     
                    "FORMAT",     
                    "SITE",     
                    "MEAS_MONTH",     
                    "STARTYR",     
                    "ENDYR",     
                    "MEANTEMP",     
                    "MINTEMP",     
                    "MAXTEMP",     
                    "PRECIP_TM"    ), check.names=TRUE)
               
unlink(infile4)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt4$STCODE)!="factor") dt4$STCODE<- as.factor(dt4$STCODE)
if (class(dt4$FORMAT)=="factor") dt4$FORMAT <-as.numeric(levels(dt4$FORMAT))[as.integer(dt4$FORMAT) ]               
if (class(dt4$FORMAT)=="character") dt4$FORMAT <-as.numeric(dt4$FORMAT)
if (class(dt4$SITE)!="factor") dt4$SITE<- as.factor(dt4$SITE)
if (class(dt4$MEAS_MONTH)!="factor") dt4$MEAS_MONTH<- as.factor(dt4$MEAS_MONTH)
if (class(dt4$STARTYR)=="factor") dt4$STARTYR <-as.numeric(levels(dt4$STARTYR))[as.integer(dt4$STARTYR) ]               
if (class(dt4$STARTYR)=="character") dt4$STARTYR <-as.numeric(dt4$STARTYR)
if (class(dt4$ENDYR)=="factor") dt4$ENDYR <-as.numeric(levels(dt4$ENDYR))[as.integer(dt4$ENDYR) ]               
if (class(dt4$ENDYR)=="character") dt4$ENDYR <-as.numeric(dt4$ENDYR)
if (class(dt4$MEANTEMP)=="factor") dt4$MEANTEMP <-as.numeric(levels(dt4$MEANTEMP))[as.integer(dt4$MEANTEMP) ]               
if (class(dt4$MEANTEMP)=="character") dt4$MEANTEMP <-as.numeric(dt4$MEANTEMP)
if (class(dt4$MINTEMP)=="factor") dt4$MINTEMP <-as.numeric(levels(dt4$MINTEMP))[as.integer(dt4$MINTEMP) ]               
if (class(dt4$MINTEMP)=="character") dt4$MINTEMP <-as.numeric(dt4$MINTEMP)
if (class(dt4$MAXTEMP)=="factor") dt4$MAXTEMP <-as.numeric(levels(dt4$MAXTEMP))[as.integer(dt4$MAXTEMP) ]               
if (class(dt4$MAXTEMP)=="character") dt4$MAXTEMP <-as.numeric(dt4$MAXTEMP)
if (class(dt4$PRECIP_TM)=="factor") dt4$PRECIP_TM <-as.numeric(levels(dt4$PRECIP_TM))[as.integer(dt4$PRECIP_TM) ]               
if (class(dt4$PRECIP_TM)=="character") dt4$PRECIP_TM <-as.numeric(dt4$PRECIP_TM)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt4)                            
attach(dt4)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(STCODE)
summary(FORMAT)
summary(SITE)
summary(MEAS_MONTH)
summary(STARTYR)
summary(ENDYR)
summary(MEANTEMP)
summary(MINTEMP)
summary(MAXTEMP)
summary(PRECIP_TM) 
                # Get more details on character variables
                 
summary(as.factor(dt4$STCODE)) 
summary(as.factor(dt4$SITE)) 
summary(as.factor(dt4$MEAS_MONTH))
detach(dt4)               
         

inUrl5  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-and/4041/11/d6d50a4d66b08d2604ea53a422c4002a" 
infile5 <- tempfile()
try(download.file(inUrl5,infile5,method="curl"))
if (is.na(file.size(infile5))) download.file(inUrl5,infile5,method="auto")

                   
 dt5 <-read.csv(infile5,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "STCODE",     
                    "FORMAT",     
                    "SITE",     
                    "SITENAME",     
                    "LOCATION",     
                    "LATDEG",     
                    "LATMIN",     
                    "LONGDEG",     
                    "LONGMIN",     
                    "ELEV",     
                    "TEMP",     
                    "PRECIP",     
                    "AET",     
                    "PET",     
                    "BIOME",     
                    "HLZ",     
                    "VEG"    ), check.names=TRUE)
               
unlink(infile5)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt5$STCODE)!="factor") dt5$STCODE<- as.factor(dt5$STCODE)
if (class(dt5$FORMAT)=="factor") dt5$FORMAT <-as.numeric(levels(dt5$FORMAT))[as.integer(dt5$FORMAT) ]               
if (class(dt5$FORMAT)=="character") dt5$FORMAT <-as.numeric(dt5$FORMAT)
if (class(dt5$SITE)!="factor") dt5$SITE<- as.factor(dt5$SITE)
if (class(dt5$SITENAME)!="factor") dt5$SITENAME<- as.factor(dt5$SITENAME)
if (class(dt5$LOCATION)!="factor") dt5$LOCATION<- as.factor(dt5$LOCATION)
if (class(dt5$LATDEG)=="factor") dt5$LATDEG <-as.numeric(levels(dt5$LATDEG))[as.integer(dt5$LATDEG) ]               
if (class(dt5$LATDEG)=="character") dt5$LATDEG <-as.numeric(dt5$LATDEG)
if (class(dt5$LATMIN)=="factor") dt5$LATMIN <-as.numeric(levels(dt5$LATMIN))[as.integer(dt5$LATMIN) ]               
if (class(dt5$LATMIN)=="character") dt5$LATMIN <-as.numeric(dt5$LATMIN)
if (class(dt5$LONGDEG)=="factor") dt5$LONGDEG <-as.numeric(levels(dt5$LONGDEG))[as.integer(dt5$LONGDEG) ]               
if (class(dt5$LONGDEG)=="character") dt5$LONGDEG <-as.numeric(dt5$LONGDEG)
if (class(dt5$LONGMIN)=="factor") dt5$LONGMIN <-as.numeric(levels(dt5$LONGMIN))[as.integer(dt5$LONGMIN) ]               
if (class(dt5$LONGMIN)=="character") dt5$LONGMIN <-as.numeric(dt5$LONGMIN)
if (class(dt5$ELEV)=="factor") dt5$ELEV <-as.numeric(levels(dt5$ELEV))[as.integer(dt5$ELEV) ]               
if (class(dt5$ELEV)=="character") dt5$ELEV <-as.numeric(dt5$ELEV)
if (class(dt5$TEMP)=="factor") dt5$TEMP <-as.numeric(levels(dt5$TEMP))[as.integer(dt5$TEMP) ]               
if (class(dt5$TEMP)=="character") dt5$TEMP <-as.numeric(dt5$TEMP)
if (class(dt5$PRECIP)=="factor") dt5$PRECIP <-as.numeric(levels(dt5$PRECIP))[as.integer(dt5$PRECIP) ]               
if (class(dt5$PRECIP)=="character") dt5$PRECIP <-as.numeric(dt5$PRECIP)
if (class(dt5$AET)=="factor") dt5$AET <-as.numeric(levels(dt5$AET))[as.integer(dt5$AET) ]               
if (class(dt5$AET)=="character") dt5$AET <-as.numeric(dt5$AET)
if (class(dt5$PET)=="factor") dt5$PET <-as.numeric(levels(dt5$PET))[as.integer(dt5$PET) ]               
if (class(dt5$PET)=="character") dt5$PET <-as.numeric(dt5$PET)
if (class(dt5$BIOME)!="factor") dt5$BIOME<- as.factor(dt5$BIOME)
if (class(dt5$HLZ)!="factor") dt5$HLZ<- as.factor(dt5$HLZ)
if (class(dt5$VEG)!="factor") dt5$VEG<- as.factor(dt5$VEG)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt5)                            
attach(dt5)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(STCODE)
summary(FORMAT)
summary(SITE)
summary(SITENAME)
summary(LOCATION)
summary(LATDEG)
summary(LATMIN)
summary(LONGDEG)
summary(LONGMIN)
summary(ELEV)
summary(TEMP)
summary(PRECIP)
summary(AET)
summary(PET)
summary(BIOME)
summary(HLZ)
summary(VEG) 
                # Get more details on character variables
                 
summary(as.factor(dt5$STCODE)) 
summary(as.factor(dt5$SITE)) 
summary(as.factor(dt5$SITENAME)) 
summary(as.factor(dt5$LOCATION)) 
summary(as.factor(dt5$BIOME)) 
summary(as.factor(dt5$HLZ)) 
summary(as.factor(dt5$VEG))
detach(dt5)               
         

inUrl6  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-and/4041/11/61a736e6a461b74e9becae525c827a62" 
infile6 <- tempfile()
try(download.file(inUrl6,infile6,method="curl"))
if (is.na(file.size(infile6))) download.file(inUrl6,infile6,method="auto")

                   
 dt6 <-read.csv(infile6,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "STCODE",     
                    "FORMAT",     
                    "SPECIES",     
                    "TYPE",     
                    "FILL_DATE",     
                    "MCF"    ), check.names=TRUE)
               
unlink(infile6)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt6$STCODE)!="factor") dt6$STCODE<- as.factor(dt6$STCODE)
if (class(dt6$FORMAT)=="factor") dt6$FORMAT <-as.numeric(levels(dt6$FORMAT))[as.integer(dt6$FORMAT) ]               
if (class(dt6$FORMAT)=="character") dt6$FORMAT <-as.numeric(dt6$FORMAT)
if (class(dt6$SPECIES)!="factor") dt6$SPECIES<- as.factor(dt6$SPECIES)
if (class(dt6$TYPE)!="factor") dt6$TYPE<- as.factor(dt6$TYPE)                                   
# attempting to convert dt6$FILL_DATE dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp6FILL_DATE<-as.Date(dt6$FILL_DATE,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp6FILL_DATE) == length(tmp6FILL_DATE[!is.na(tmp6FILL_DATE)])){dt6$FILL_DATE <- tmp6FILL_DATE } else {print("Date conversion failed for dt6$FILL_DATE. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp6FILL_DATE) 
if (class(dt6$MCF)=="factor") dt6$MCF <-as.numeric(levels(dt6$MCF))[as.integer(dt6$MCF) ]               
if (class(dt6$MCF)=="character") dt6$MCF <-as.numeric(dt6$MCF)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt6)                            
attach(dt6)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(STCODE)
summary(FORMAT)
summary(SPECIES)
summary(TYPE)
summary(FILL_DATE)
summary(MCF) 
                # Get more details on character variables
                 
summary(as.factor(dt6$STCODE)) 
summary(as.factor(dt6$SPECIES)) 
summary(as.factor(dt6$TYPE))
detach(dt6)               
         

inUrl7  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-and/4041/11/7e12f1a3e1fd9c8016ce55acfd6fe7f6" 
infile7 <- tempfile()
try(download.file(inUrl7,infile7,method="curl"))
if (is.na(file.size(infile7))) download.file(inUrl7,infile7,method="auto")

                   
 dt7 <-read.csv(infile7,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "STCODE",     
                    "FORMAT",     
                    "SITE",     
                    "REP",     
                    "DURATION",     
                    "SPECIES",     
                    "TYPE",     
                    "TAG_NUM",     
                    "NUMBER",     
                    "NIR_NUM",     
                    "DATEOUT",     
                    "DATEIN",     
                    "STRR",     
                    "WHERE_GO",     
                    "ID_NR",     
                    "TYPE1",     
                    "COMMENT"    ), check.names=TRUE)
               
unlink(infile7)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt7$STCODE)!="factor") dt7$STCODE<- as.factor(dt7$STCODE)
if (class(dt7$FORMAT)=="factor") dt7$FORMAT <-as.numeric(levels(dt7$FORMAT))[as.integer(dt7$FORMAT) ]               
if (class(dt7$FORMAT)=="character") dt7$FORMAT <-as.numeric(dt7$FORMAT)
if (class(dt7$SITE)!="factor") dt7$SITE<- as.factor(dt7$SITE)
if (class(dt7$REP)!="factor") dt7$REP<- as.factor(dt7$REP)
if (class(dt7$DURATION)=="factor") dt7$DURATION <-as.numeric(levels(dt7$DURATION))[as.integer(dt7$DURATION) ]               
if (class(dt7$DURATION)=="character") dt7$DURATION <-as.numeric(dt7$DURATION)
if (class(dt7$SPECIES)!="factor") dt7$SPECIES<- as.factor(dt7$SPECIES)
if (class(dt7$TYPE)!="factor") dt7$TYPE<- as.factor(dt7$TYPE)
if (class(dt7$TAG_NUM)=="factor") dt7$TAG_NUM <-as.numeric(levels(dt7$TAG_NUM))[as.integer(dt7$TAG_NUM) ]               
if (class(dt7$TAG_NUM)=="character") dt7$TAG_NUM <-as.numeric(dt7$TAG_NUM)
if (class(dt7$NUMBER)=="factor") dt7$NUMBER <-as.numeric(levels(dt7$NUMBER))[as.integer(dt7$NUMBER) ]               
if (class(dt7$NUMBER)=="character") dt7$NUMBER <-as.numeric(dt7$NUMBER)
if (class(dt7$NIR_NUM)=="factor") dt7$NIR_NUM <-as.numeric(levels(dt7$NIR_NUM))[as.integer(dt7$NIR_NUM) ]               
if (class(dt7$NIR_NUM)=="character") dt7$NIR_NUM <-as.numeric(dt7$NIR_NUM)                                   
# attempting to convert dt7$DATEOUT dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp7DATEOUT<-as.Date(dt7$DATEOUT,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp7DATEOUT) == length(tmp7DATEOUT[!is.na(tmp7DATEOUT)])){dt7$DATEOUT <- tmp7DATEOUT } else {print("Date conversion failed for dt7$DATEOUT. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp7DATEOUT)                                    
# attempting to convert dt7$DATEIN dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp7DATEIN<-as.Date(dt7$DATEIN,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp7DATEIN) == length(tmp7DATEIN[!is.na(tmp7DATEIN)])){dt7$DATEIN <- tmp7DATEIN } else {print("Date conversion failed for dt7$DATEIN. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp7DATEIN) 
if (class(dt7$STRR)!="factor") dt7$STRR<- as.factor(dt7$STRR)
if (class(dt7$WHERE_GO)!="factor") dt7$WHERE_GO<- as.factor(dt7$WHERE_GO)
if (class(dt7$ID_NR)=="factor") dt7$ID_NR <-as.numeric(levels(dt7$ID_NR))[as.integer(dt7$ID_NR) ]               
if (class(dt7$ID_NR)=="character") dt7$ID_NR <-as.numeric(dt7$ID_NR)
if (class(dt7$TYPE1)!="factor") dt7$TYPE1<- as.factor(dt7$TYPE1)
if (class(dt7$COMMENT)!="factor") dt7$COMMENT<- as.factor(dt7$COMMENT)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt7)                            
attach(dt7)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(STCODE)
summary(FORMAT)
summary(SITE)
summary(REP)
summary(DURATION)
summary(SPECIES)
summary(TYPE)
summary(TAG_NUM)
summary(NUMBER)
summary(NIR_NUM)
summary(DATEOUT)
summary(DATEIN)
summary(STRR)
summary(WHERE_GO)
summary(ID_NR)
summary(TYPE1)
summary(COMMENT) 
                # Get more details on character variables
                 
summary(as.factor(dt7$STCODE)) 
summary(as.factor(dt7$SITE)) 
summary(as.factor(dt7$REP)) 
summary(as.factor(dt7$SPECIES)) 
summary(as.factor(dt7$TYPE)) 
summary(as.factor(dt7$STRR)) 
summary(as.factor(dt7$WHERE_GO)) 
summary(as.factor(dt7$TYPE1)) 
summary(as.factor(dt7$COMMENT))
detach(dt7)               
         

inUrl8  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-and/4041/11/e761953b9d45c2f51d6e6b0c0aff9a9b" 
infile8 <- tempfile()
try(download.file(inUrl8,infile8,method="curl"))
if (is.na(file.size(infile8))) download.file(inUrl8,infile8,method="auto")

                   
 dt8 <-read.csv(infile8,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "STCODE",     
                    "FORMAT",     
                    "SITE",     
                    "REP",     
                    "DURATION",     
                    "SPECIES",     
                    "TYPE",     
                    "NIR_NUM",     
                    "MESH",     
                    "DATEOUT",     
                    "IADW",     
                    "IODW",     
                    "DATEIN",     
                    "FWW",     
                    "FOW",     
                    "LENGTH",     
                    "IADW1",     
                    "IODW1",     
                    "IASH",     
                    "FASH",     
                    "IAFW",     
                    "FAFW",     
                    "PRM",     
                    "PAFRM",     
                    "KDW",     
                    "KAFW",     
                    "TYPE1",     
                    "TIMEOUT",     
                    "COMMENT",     
                    "FLAG"    ), check.names=TRUE)
               
unlink(infile8)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt8$STCODE)!="factor") dt8$STCODE<- as.factor(dt8$STCODE)
if (class(dt8$FORMAT)=="factor") dt8$FORMAT <-as.numeric(levels(dt8$FORMAT))[as.integer(dt8$FORMAT) ]               
if (class(dt8$FORMAT)=="character") dt8$FORMAT <-as.numeric(dt8$FORMAT)
if (class(dt8$SITE)!="factor") dt8$SITE<- as.factor(dt8$SITE)
if (class(dt8$REP)!="factor") dt8$REP<- as.factor(dt8$REP)
if (class(dt8$DURATION)=="factor") dt8$DURATION <-as.numeric(levels(dt8$DURATION))[as.integer(dt8$DURATION) ]               
if (class(dt8$DURATION)=="character") dt8$DURATION <-as.numeric(dt8$DURATION)
if (class(dt8$SPECIES)!="factor") dt8$SPECIES<- as.factor(dt8$SPECIES)
if (class(dt8$TYPE)!="factor") dt8$TYPE<- as.factor(dt8$TYPE)
if (class(dt8$NIR_NUM)=="factor") dt8$NIR_NUM <-as.numeric(levels(dt8$NIR_NUM))[as.integer(dt8$NIR_NUM) ]               
if (class(dt8$NIR_NUM)=="character") dt8$NIR_NUM <-as.numeric(dt8$NIR_NUM)
if (class(dt8$MESH)=="factor") dt8$MESH <-as.numeric(levels(dt8$MESH))[as.integer(dt8$MESH) ]               
if (class(dt8$MESH)=="character") dt8$MESH <-as.numeric(dt8$MESH)                                   
# attempting to convert dt8$DATEOUT dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp8DATEOUT<-as.Date(dt8$DATEOUT,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp8DATEOUT) == length(tmp8DATEOUT[!is.na(tmp8DATEOUT)])){dt8$DATEOUT <- tmp8DATEOUT } else {print("Date conversion failed for dt8$DATEOUT. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp8DATEOUT) 
if (class(dt8$IADW)=="factor") dt8$IADW <-as.numeric(levels(dt8$IADW))[as.integer(dt8$IADW) ]               
if (class(dt8$IADW)=="character") dt8$IADW <-as.numeric(dt8$IADW)
if (class(dt8$IODW)=="factor") dt8$IODW <-as.numeric(levels(dt8$IODW))[as.integer(dt8$IODW) ]               
if (class(dt8$IODW)=="character") dt8$IODW <-as.numeric(dt8$IODW)                                   
# attempting to convert dt8$DATEIN dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp8DATEIN<-as.Date(dt8$DATEIN,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp8DATEIN) == length(tmp8DATEIN[!is.na(tmp8DATEIN)])){dt8$DATEIN <- tmp8DATEIN } else {print("Date conversion failed for dt8$DATEIN. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp8DATEIN) 
if (class(dt8$FWW)=="factor") dt8$FWW <-as.numeric(levels(dt8$FWW))[as.integer(dt8$FWW) ]               
if (class(dt8$FWW)=="character") dt8$FWW <-as.numeric(dt8$FWW)
if (class(dt8$FOW)=="factor") dt8$FOW <-as.numeric(levels(dt8$FOW))[as.integer(dt8$FOW) ]               
if (class(dt8$FOW)=="character") dt8$FOW <-as.numeric(dt8$FOW)
if (class(dt8$LENGTH)=="factor") dt8$LENGTH <-as.numeric(levels(dt8$LENGTH))[as.integer(dt8$LENGTH) ]               
if (class(dt8$LENGTH)=="character") dt8$LENGTH <-as.numeric(dt8$LENGTH)
if (class(dt8$IADW1)=="factor") dt8$IADW1 <-as.numeric(levels(dt8$IADW1))[as.integer(dt8$IADW1) ]               
if (class(dt8$IADW1)=="character") dt8$IADW1 <-as.numeric(dt8$IADW1)
if (class(dt8$IODW1)=="factor") dt8$IODW1 <-as.numeric(levels(dt8$IODW1))[as.integer(dt8$IODW1) ]               
if (class(dt8$IODW1)=="character") dt8$IODW1 <-as.numeric(dt8$IODW1)
if (class(dt8$IASH)=="factor") dt8$IASH <-as.numeric(levels(dt8$IASH))[as.integer(dt8$IASH) ]               
if (class(dt8$IASH)=="character") dt8$IASH <-as.numeric(dt8$IASH)
if (class(dt8$FASH)=="factor") dt8$FASH <-as.numeric(levels(dt8$FASH))[as.integer(dt8$FASH) ]               
if (class(dt8$FASH)=="character") dt8$FASH <-as.numeric(dt8$FASH)
if (class(dt8$IAFW)=="factor") dt8$IAFW <-as.numeric(levels(dt8$IAFW))[as.integer(dt8$IAFW) ]               
if (class(dt8$IAFW)=="character") dt8$IAFW <-as.numeric(dt8$IAFW)
if (class(dt8$FAFW)=="factor") dt8$FAFW <-as.numeric(levels(dt8$FAFW))[as.integer(dt8$FAFW) ]               
if (class(dt8$FAFW)=="character") dt8$FAFW <-as.numeric(dt8$FAFW)
if (class(dt8$PRM)=="factor") dt8$PRM <-as.numeric(levels(dt8$PRM))[as.integer(dt8$PRM) ]               
if (class(dt8$PRM)=="character") dt8$PRM <-as.numeric(dt8$PRM)
if (class(dt8$PAFRM)=="factor") dt8$PAFRM <-as.numeric(levels(dt8$PAFRM))[as.integer(dt8$PAFRM) ]               
if (class(dt8$PAFRM)=="character") dt8$PAFRM <-as.numeric(dt8$PAFRM)
if (class(dt8$KDW)=="factor") dt8$KDW <-as.numeric(levels(dt8$KDW))[as.integer(dt8$KDW) ]               
if (class(dt8$KDW)=="character") dt8$KDW <-as.numeric(dt8$KDW)
if (class(dt8$KAFW)=="factor") dt8$KAFW <-as.numeric(levels(dt8$KAFW))[as.integer(dt8$KAFW) ]               
if (class(dt8$KAFW)=="character") dt8$KAFW <-as.numeric(dt8$KAFW)
if (class(dt8$TYPE1)!="factor") dt8$TYPE1<- as.factor(dt8$TYPE1)
if (class(dt8$TIMEOUT)=="factor") dt8$TIMEOUT <-as.numeric(levels(dt8$TIMEOUT))[as.integer(dt8$TIMEOUT) ]               
if (class(dt8$TIMEOUT)=="character") dt8$TIMEOUT <-as.numeric(dt8$TIMEOUT)
if (class(dt8$COMMENT)!="factor") dt8$COMMENT<- as.factor(dt8$COMMENT)
if (class(dt8$FLAG)!="factor") dt8$FLAG<- as.factor(dt8$FLAG)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt8)                            
attach(dt8)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(STCODE)
summary(FORMAT)
summary(SITE)
summary(REP)
summary(DURATION)
summary(SPECIES)
summary(TYPE)
summary(NIR_NUM)
summary(MESH)
summary(DATEOUT)
summary(IADW)
summary(IODW)
summary(DATEIN)
summary(FWW)
summary(FOW)
summary(LENGTH)
summary(IADW1)
summary(IODW1)
summary(IASH)
summary(FASH)
summary(IAFW)
summary(FAFW)
summary(PRM)
summary(PAFRM)
summary(KDW)
summary(KAFW)
summary(TYPE1)
summary(TIMEOUT)
summary(COMMENT)
summary(FLAG) 
                # Get more details on character variables
                 
summary(as.factor(dt8$STCODE)) 
summary(as.factor(dt8$SITE)) 
summary(as.factor(dt8$REP)) 
summary(as.factor(dt8$SPECIES)) 
summary(as.factor(dt8$TYPE)) 
summary(as.factor(dt8$TYPE1)) 
summary(as.factor(dt8$COMMENT)) 
summary(as.factor(dt8$FLAG))
detach(dt8)               
         

inUrl9  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-and/4041/11/148002412c47219a2be6606e39857927" 
infile9 <- tempfile()
try(download.file(inUrl9,infile9,method="curl"))
if (is.na(file.size(infile9))) download.file(inUrl9,infile9,method="auto")

                   
 dt9 <-read.csv(infile9,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "STCODE",     
                    "FORMAT",     
                    "SITE",     
                    "REP",     
                    "DURATION",     
                    "SPECIES",     
                    "TYPE",     
                    "NIR_NUM",     
                    "CRWT",     
                    "CRSWT",     
                    "CRASH",     
                    "ASH",     
                    "ASHFREE",     
                    "EST",     
                    "ASH_LAB",     
                    "NIR_ASH",     
                    "NIR_ASHFRE",     
                    "NIR_EST",     
                    "NIR_LAB",     
                    "COMMENT"    ), check.names=TRUE)
               
unlink(infile9)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt9$STCODE)!="factor") dt9$STCODE<- as.factor(dt9$STCODE)
if (class(dt9$FORMAT)=="factor") dt9$FORMAT <-as.numeric(levels(dt9$FORMAT))[as.integer(dt9$FORMAT) ]               
if (class(dt9$FORMAT)=="character") dt9$FORMAT <-as.numeric(dt9$FORMAT)
if (class(dt9$SITE)!="factor") dt9$SITE<- as.factor(dt9$SITE)
if (class(dt9$REP)!="factor") dt9$REP<- as.factor(dt9$REP)
if (class(dt9$DURATION)=="factor") dt9$DURATION <-as.numeric(levels(dt9$DURATION))[as.integer(dt9$DURATION) ]               
if (class(dt9$DURATION)=="character") dt9$DURATION <-as.numeric(dt9$DURATION)
if (class(dt9$SPECIES)!="factor") dt9$SPECIES<- as.factor(dt9$SPECIES)
if (class(dt9$TYPE)!="factor") dt9$TYPE<- as.factor(dt9$TYPE)
if (class(dt9$NIR_NUM)=="factor") dt9$NIR_NUM <-as.numeric(levels(dt9$NIR_NUM))[as.integer(dt9$NIR_NUM) ]               
if (class(dt9$NIR_NUM)=="character") dt9$NIR_NUM <-as.numeric(dt9$NIR_NUM)
if (class(dt9$CRWT)=="factor") dt9$CRWT <-as.numeric(levels(dt9$CRWT))[as.integer(dt9$CRWT) ]               
if (class(dt9$CRWT)=="character") dt9$CRWT <-as.numeric(dt9$CRWT)
if (class(dt9$CRSWT)=="factor") dt9$CRSWT <-as.numeric(levels(dt9$CRSWT))[as.integer(dt9$CRSWT) ]               
if (class(dt9$CRSWT)=="character") dt9$CRSWT <-as.numeric(dt9$CRSWT)
if (class(dt9$CRASH)=="factor") dt9$CRASH <-as.numeric(levels(dt9$CRASH))[as.integer(dt9$CRASH) ]               
if (class(dt9$CRASH)=="character") dt9$CRASH <-as.numeric(dt9$CRASH)
if (class(dt9$ASH)=="factor") dt9$ASH <-as.numeric(levels(dt9$ASH))[as.integer(dt9$ASH) ]               
if (class(dt9$ASH)=="character") dt9$ASH <-as.numeric(dt9$ASH)
if (class(dt9$ASHFREE)=="factor") dt9$ASHFREE <-as.numeric(levels(dt9$ASHFREE))[as.integer(dt9$ASHFREE) ]               
if (class(dt9$ASHFREE)=="character") dt9$ASHFREE <-as.numeric(dt9$ASHFREE)
if (class(dt9$EST)!="factor") dt9$EST<- as.factor(dt9$EST)
if (class(dt9$ASH_LAB)!="factor") dt9$ASH_LAB<- as.factor(dt9$ASH_LAB)
if (class(dt9$NIR_ASH)=="factor") dt9$NIR_ASH <-as.numeric(levels(dt9$NIR_ASH))[as.integer(dt9$NIR_ASH) ]               
if (class(dt9$NIR_ASH)=="character") dt9$NIR_ASH <-as.numeric(dt9$NIR_ASH)
if (class(dt9$NIR_ASHFRE)=="factor") dt9$NIR_ASHFRE <-as.numeric(levels(dt9$NIR_ASHFRE))[as.integer(dt9$NIR_ASHFRE) ]               
if (class(dt9$NIR_ASHFRE)=="character") dt9$NIR_ASHFRE <-as.numeric(dt9$NIR_ASHFRE)
if (class(dt9$NIR_EST)!="factor") dt9$NIR_EST<- as.factor(dt9$NIR_EST)
if (class(dt9$NIR_LAB)!="factor") dt9$NIR_LAB<- as.factor(dt9$NIR_LAB)
if (class(dt9$COMMENT)!="factor") dt9$COMMENT<- as.factor(dt9$COMMENT)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt9)                            
attach(dt9)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(STCODE)
summary(FORMAT)
summary(SITE)
summary(REP)
summary(DURATION)
summary(SPECIES)
summary(TYPE)
summary(NIR_NUM)
summary(CRWT)
summary(CRSWT)
summary(CRASH)
summary(ASH)
summary(ASHFREE)
summary(EST)
summary(ASH_LAB)
summary(NIR_ASH)
summary(NIR_ASHFRE)
summary(NIR_EST)
summary(NIR_LAB)
summary(COMMENT) 
                # Get more details on character variables
                 
summary(as.factor(dt9$STCODE)) 
summary(as.factor(dt9$SITE)) 
summary(as.factor(dt9$REP)) 
summary(as.factor(dt9$SPECIES)) 
summary(as.factor(dt9$TYPE)) 
summary(as.factor(dt9$EST)) 
summary(as.factor(dt9$ASH_LAB)) 
summary(as.factor(dt9$NIR_EST)) 
summary(as.factor(dt9$NIR_LAB)) 
summary(as.factor(dt9$COMMENT))
detach(dt9)               
         

inUrl10  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-and/4041/11/8b0b3858c9edf0172fc3b0cfd58732ad" 
infile10 <- tempfile()
try(download.file(inUrl10,infile10,method="curl"))
if (is.na(file.size(infile10))) download.file(inUrl10,infile10,method="auto")

                   
 dt10 <-read.csv(infile10,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "STCODE",     
                    "FORMAT",     
                    "SITE",     
                    "REP",     
                    "DURATION",     
                    "SPECIES",     
                    "TYPE",     
                    "NIR_NUM",     
                    "STARTDATE",     
                    "N",     
                    "AL",     
                    "B",     
                    "CA",     
                    "CU",     
                    "FE",     
                    "K",     
                    "MG",     
                    "MN",     
                    "P",     
                    "S",     
                    "ZN",     
                    "NA",     
                    "LAB"    ), check.names=TRUE)
               
unlink(infile10)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt10$STCODE)!="factor") dt10$STCODE<- as.factor(dt10$STCODE)
if (class(dt10$FORMAT)=="factor") dt10$FORMAT <-as.numeric(levels(dt10$FORMAT))[as.integer(dt10$FORMAT) ]               
if (class(dt10$FORMAT)=="character") dt10$FORMAT <-as.numeric(dt10$FORMAT)
if (class(dt10$SITE)!="factor") dt10$SITE<- as.factor(dt10$SITE)
if (class(dt10$REP)!="factor") dt10$REP<- as.factor(dt10$REP)
if (class(dt10$DURATION)=="factor") dt10$DURATION <-as.numeric(levels(dt10$DURATION))[as.integer(dt10$DURATION) ]               
if (class(dt10$DURATION)=="character") dt10$DURATION <-as.numeric(dt10$DURATION)
if (class(dt10$SPECIES)!="factor") dt10$SPECIES<- as.factor(dt10$SPECIES)
if (class(dt10$TYPE)!="factor") dt10$TYPE<- as.factor(dt10$TYPE)
if (class(dt10$NIR_NUM)=="factor") dt10$NIR_NUM <-as.numeric(levels(dt10$NIR_NUM))[as.integer(dt10$NIR_NUM) ]               
if (class(dt10$NIR_NUM)=="character") dt10$NIR_NUM <-as.numeric(dt10$NIR_NUM)                                   
# attempting to convert dt10$STARTDATE dateTime string to R date structure (date or POSIXct)                                
tmpDateFormat<-"%Y-%m-%d"
tmp10STARTDATE<-as.Date(dt10$STARTDATE,format=tmpDateFormat)
# Keep the new dates only if they all converted correctly
if(length(tmp10STARTDATE) == length(tmp10STARTDATE[!is.na(tmp10STARTDATE)])){dt10$STARTDATE <- tmp10STARTDATE } else {print("Date conversion failed for dt10$STARTDATE. Please inspect the data and do the date conversion yourself.")}                                                                    
rm(tmpDateFormat,tmp10STARTDATE) 
if (class(dt10$N)=="factor") dt10$N <-as.numeric(levels(dt10$N))[as.integer(dt10$N) ]               
if (class(dt10$N)=="character") dt10$N <-as.numeric(dt10$N)
if (class(dt10$AL)=="factor") dt10$AL <-as.numeric(levels(dt10$AL))[as.integer(dt10$AL) ]               
if (class(dt10$AL)=="character") dt10$AL <-as.numeric(dt10$AL)
if (class(dt10$B)=="factor") dt10$B <-as.numeric(levels(dt10$B))[as.integer(dt10$B) ]               
if (class(dt10$B)=="character") dt10$B <-as.numeric(dt10$B)
if (class(dt10$CA)=="factor") dt10$CA <-as.numeric(levels(dt10$CA))[as.integer(dt10$CA) ]               
if (class(dt10$CA)=="character") dt10$CA <-as.numeric(dt10$CA)
if (class(dt10$CU)=="factor") dt10$CU <-as.numeric(levels(dt10$CU))[as.integer(dt10$CU) ]               
if (class(dt10$CU)=="character") dt10$CU <-as.numeric(dt10$CU)
if (class(dt10$FE)=="factor") dt10$FE <-as.numeric(levels(dt10$FE))[as.integer(dt10$FE) ]               
if (class(dt10$FE)=="character") dt10$FE <-as.numeric(dt10$FE)
if (class(dt10$K)=="factor") dt10$K <-as.numeric(levels(dt10$K))[as.integer(dt10$K) ]               
if (class(dt10$K)=="character") dt10$K <-as.numeric(dt10$K)
if (class(dt10$MG)=="factor") dt10$MG <-as.numeric(levels(dt10$MG))[as.integer(dt10$MG) ]               
if (class(dt10$MG)=="character") dt10$MG <-as.numeric(dt10$MG)
if (class(dt10$MN)=="factor") dt10$MN <-as.numeric(levels(dt10$MN))[as.integer(dt10$MN) ]               
if (class(dt10$MN)=="character") dt10$MN <-as.numeric(dt10$MN)
if (class(dt10$P)=="factor") dt10$P <-as.numeric(levels(dt10$P))[as.integer(dt10$P) ]               
if (class(dt10$P)=="character") dt10$P <-as.numeric(dt10$P)
if (class(dt10$S)=="factor") dt10$S <-as.numeric(levels(dt10$S))[as.integer(dt10$S) ]               
if (class(dt10$S)=="character") dt10$S <-as.numeric(dt10$S)
if (class(dt10$ZN)=="factor") dt10$ZN <-as.numeric(levels(dt10$ZN))[as.integer(dt10$ZN) ]               
if (class(dt10$ZN)=="character") dt10$ZN <-as.numeric(dt10$ZN)
if (class(dt10$`NA`)=="factor") dt10$`NA` <-as.numeric(levels(dt10$`NA`))[as.integer(dt10$`NA`) ]               
if (class(dt10$`NA`)=="character") dt10$`NA` <-as.numeric(dt10$`NA`)
if (class(dt10$LAB)!="factor") dt10$LAB<- as.factor(dt10$LAB)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt10)                            
attach(dt10)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(STCODE)
summary(FORMAT)
summary(SITE)
summary(REP)
summary(DURATION)
summary(SPECIES)
summary(TYPE)
summary(NIR_NUM)
summary(STARTDATE)
summary(N)
summary(AL)
summary(B)
summary(CA)
summary(CU)
summary(FE)
summary(K)
summary(MG)
summary(MN)
summary(P)
summary(S)
summary(ZN)
summary(NA)
summary(LAB) 
                # Get more details on character variables
                 
summary(as.factor(dt10$STCODE)) 
summary(as.factor(dt10$SITE)) 
summary(as.factor(dt10$REP)) 
summary(as.factor(dt10$SPECIES)) 
summary(as.factor(dt10$TYPE)) 
summary(as.factor(dt10$LAB))
detach(dt10)               
         

inUrl11  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-and/4041/11/0789c88327b9fb7ab789bbd9a28b9965" 
infile11 <- tempfile()
try(download.file(inUrl11,infile11,method="curl"))
if (is.na(file.size(infile11))) download.file(inUrl11,infile11,method="auto")

                   
 dt11 <-read.csv(infile11,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "STCODE",     
                    "FORMAT",     
                    "SITE",     
                    "REP",     
                    "DURATION",     
                    "SPECIES",     
                    "TYPE",     
                    "NIR_NUM",     
                    "IODW",     
                    "FOW",     
                    "IASH",     
                    "FASH",     
                    "I_ASH_MASS",     
                    "I_RT_MASS",     
                    "NEW_IASH"    ), check.names=TRUE)
               
unlink(infile11)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt11$STCODE)!="factor") dt11$STCODE<- as.factor(dt11$STCODE)
if (class(dt11$FORMAT)=="factor") dt11$FORMAT <-as.numeric(levels(dt11$FORMAT))[as.integer(dt11$FORMAT) ]               
if (class(dt11$FORMAT)=="character") dt11$FORMAT <-as.numeric(dt11$FORMAT)
if (class(dt11$SITE)!="factor") dt11$SITE<- as.factor(dt11$SITE)
if (class(dt11$REP)!="factor") dt11$REP<- as.factor(dt11$REP)
if (class(dt11$DURATION)=="factor") dt11$DURATION <-as.numeric(levels(dt11$DURATION))[as.integer(dt11$DURATION) ]               
if (class(dt11$DURATION)=="character") dt11$DURATION <-as.numeric(dt11$DURATION)
if (class(dt11$SPECIES)!="factor") dt11$SPECIES<- as.factor(dt11$SPECIES)
if (class(dt11$TYPE)!="factor") dt11$TYPE<- as.factor(dt11$TYPE)
if (class(dt11$NIR_NUM)=="factor") dt11$NIR_NUM <-as.numeric(levels(dt11$NIR_NUM))[as.integer(dt11$NIR_NUM) ]               
if (class(dt11$NIR_NUM)=="character") dt11$NIR_NUM <-as.numeric(dt11$NIR_NUM)
if (class(dt11$IODW)=="factor") dt11$IODW <-as.numeric(levels(dt11$IODW))[as.integer(dt11$IODW) ]               
if (class(dt11$IODW)=="character") dt11$IODW <-as.numeric(dt11$IODW)
if (class(dt11$FOW)=="factor") dt11$FOW <-as.numeric(levels(dt11$FOW))[as.integer(dt11$FOW) ]               
if (class(dt11$FOW)=="character") dt11$FOW <-as.numeric(dt11$FOW)
if (class(dt11$IASH)=="factor") dt11$IASH <-as.numeric(levels(dt11$IASH))[as.integer(dt11$IASH) ]               
if (class(dt11$IASH)=="character") dt11$IASH <-as.numeric(dt11$IASH)
if (class(dt11$FASH)=="factor") dt11$FASH <-as.numeric(levels(dt11$FASH))[as.integer(dt11$FASH) ]               
if (class(dt11$FASH)=="character") dt11$FASH <-as.numeric(dt11$FASH)
if (class(dt11$I_ASH_MASS)=="factor") dt11$I_ASH_MASS <-as.numeric(levels(dt11$I_ASH_MASS))[as.integer(dt11$I_ASH_MASS) ]               
if (class(dt11$I_ASH_MASS)=="character") dt11$I_ASH_MASS <-as.numeric(dt11$I_ASH_MASS)
if (class(dt11$I_RT_MASS)=="factor") dt11$I_RT_MASS <-as.numeric(levels(dt11$I_RT_MASS))[as.integer(dt11$I_RT_MASS) ]               
if (class(dt11$I_RT_MASS)=="character") dt11$I_RT_MASS <-as.numeric(dt11$I_RT_MASS)
if (class(dt11$NEW_IASH)=="factor") dt11$NEW_IASH <-as.numeric(levels(dt11$NEW_IASH))[as.integer(dt11$NEW_IASH) ]               
if (class(dt11$NEW_IASH)=="character") dt11$NEW_IASH <-as.numeric(dt11$NEW_IASH)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt11)                            
attach(dt11)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(STCODE)
summary(FORMAT)
summary(SITE)
summary(REP)
summary(DURATION)
summary(SPECIES)
summary(TYPE)
summary(NIR_NUM)
summary(IODW)
summary(FOW)
summary(IASH)
summary(FASH)
summary(I_ASH_MASS)
summary(I_RT_MASS)
summary(NEW_IASH) 
                # Get more details on character variables
                 
summary(as.factor(dt11$STCODE)) 
summary(as.factor(dt11$SITE)) 
summary(as.factor(dt11$REP)) 
summary(as.factor(dt11$SPECIES)) 
summary(as.factor(dt11$TYPE))
detach(dt11)               
         

inUrl12  <- "https://pasta.lternet.edu/package/data/eml/knb-lter-and/4041/11/af9e4f07116821dd7e7192a28f3f33c7" 
infile12 <- tempfile()
try(download.file(inUrl12,infile12,method="curl"))
if (is.na(file.size(infile12))) download.file(inUrl12,infile12,method="auto")

                   
 dt12 <-read.csv(infile12,header=F 
          ,skip=1
            ,sep=","  
                ,quot='"' 
        , col.names=c(
                    "STCODE",     
                    "FORMAT",     
                    "SITE",     
                    "REP",     
                    "DURATION",     
                    "SPECIES",     
                    "TYPE",     
                    "NIR_NUM",     
                    "I_NITRO",     
                    "AF_I_NITRO",     
                    "F_NITRO",     
                    "AF_F_NITRO",     
                    "IAFW",     
                    "FAFW",     
                    "PAFRM",     
                    "KAFW",     
                    "I_N_CONT",     
                    "F_N_CONT",     
                    "N_CONC",     
                    "TIMEOUT",     
                    "COMMENT"    ), check.names=TRUE)
               
unlink(infile12)
		    
# Fix any interval or ratio columns mistakenly read in as nominal and nominal columns read as numeric or dates read as strings
                
if (class(dt12$STCODE)!="factor") dt12$STCODE<- as.factor(dt12$STCODE)
if (class(dt12$FORMAT)=="factor") dt12$FORMAT <-as.numeric(levels(dt12$FORMAT))[as.integer(dt12$FORMAT) ]               
if (class(dt12$FORMAT)=="character") dt12$FORMAT <-as.numeric(dt12$FORMAT)
if (class(dt12$SITE)!="factor") dt12$SITE<- as.factor(dt12$SITE)
if (class(dt12$REP)!="factor") dt12$REP<- as.factor(dt12$REP)
if (class(dt12$DURATION)=="factor") dt12$DURATION <-as.numeric(levels(dt12$DURATION))[as.integer(dt12$DURATION) ]               
if (class(dt12$DURATION)=="character") dt12$DURATION <-as.numeric(dt12$DURATION)
if (class(dt12$SPECIES)!="factor") dt12$SPECIES<- as.factor(dt12$SPECIES)
if (class(dt12$TYPE)!="factor") dt12$TYPE<- as.factor(dt12$TYPE)
if (class(dt12$NIR_NUM)=="factor") dt12$NIR_NUM <-as.numeric(levels(dt12$NIR_NUM))[as.integer(dt12$NIR_NUM) ]               
if (class(dt12$NIR_NUM)=="character") dt12$NIR_NUM <-as.numeric(dt12$NIR_NUM)
if (class(dt12$I_NITRO)=="factor") dt12$I_NITRO <-as.numeric(levels(dt12$I_NITRO))[as.integer(dt12$I_NITRO) ]               
if (class(dt12$I_NITRO)=="character") dt12$I_NITRO <-as.numeric(dt12$I_NITRO)
if (class(dt12$AF_I_NITRO)=="factor") dt12$AF_I_NITRO <-as.numeric(levels(dt12$AF_I_NITRO))[as.integer(dt12$AF_I_NITRO) ]               
if (class(dt12$AF_I_NITRO)=="character") dt12$AF_I_NITRO <-as.numeric(dt12$AF_I_NITRO)
if (class(dt12$F_NITRO)=="factor") dt12$F_NITRO <-as.numeric(levels(dt12$F_NITRO))[as.integer(dt12$F_NITRO) ]               
if (class(dt12$F_NITRO)=="character") dt12$F_NITRO <-as.numeric(dt12$F_NITRO)
if (class(dt12$AF_F_NITRO)=="factor") dt12$AF_F_NITRO <-as.numeric(levels(dt12$AF_F_NITRO))[as.integer(dt12$AF_F_NITRO) ]               
if (class(dt12$AF_F_NITRO)=="character") dt12$AF_F_NITRO <-as.numeric(dt12$AF_F_NITRO)
if (class(dt12$IAFW)=="factor") dt12$IAFW <-as.numeric(levels(dt12$IAFW))[as.integer(dt12$IAFW) ]               
if (class(dt12$IAFW)=="character") dt12$IAFW <-as.numeric(dt12$IAFW)
if (class(dt12$FAFW)=="factor") dt12$FAFW <-as.numeric(levels(dt12$FAFW))[as.integer(dt12$FAFW) ]               
if (class(dt12$FAFW)=="character") dt12$FAFW <-as.numeric(dt12$FAFW)
if (class(dt12$PAFRM)=="factor") dt12$PAFRM <-as.numeric(levels(dt12$PAFRM))[as.integer(dt12$PAFRM) ]               
if (class(dt12$PAFRM)=="character") dt12$PAFRM <-as.numeric(dt12$PAFRM)
if (class(dt12$KAFW)=="factor") dt12$KAFW <-as.numeric(levels(dt12$KAFW))[as.integer(dt12$KAFW) ]               
if (class(dt12$KAFW)=="character") dt12$KAFW <-as.numeric(dt12$KAFW)
if (class(dt12$I_N_CONT)=="factor") dt12$I_N_CONT <-as.numeric(levels(dt12$I_N_CONT))[as.integer(dt12$I_N_CONT) ]               
if (class(dt12$I_N_CONT)=="character") dt12$I_N_CONT <-as.numeric(dt12$I_N_CONT)
if (class(dt12$F_N_CONT)=="factor") dt12$F_N_CONT <-as.numeric(levels(dt12$F_N_CONT))[as.integer(dt12$F_N_CONT) ]               
if (class(dt12$F_N_CONT)=="character") dt12$F_N_CONT <-as.numeric(dt12$F_N_CONT)
if (class(dt12$N_CONC)=="factor") dt12$N_CONC <-as.numeric(levels(dt12$N_CONC))[as.integer(dt12$N_CONC) ]               
if (class(dt12$N_CONC)=="character") dt12$N_CONC <-as.numeric(dt12$N_CONC)
if (class(dt12$TIMEOUT)=="factor") dt12$TIMEOUT <-as.numeric(levels(dt12$TIMEOUT))[as.integer(dt12$TIMEOUT) ]               
if (class(dt12$TIMEOUT)=="character") dt12$TIMEOUT <-as.numeric(dt12$TIMEOUT)
if (class(dt12$COMMENT)!="factor") dt12$COMMENT<- as.factor(dt12$COMMENT)
                
# Convert Missing Values to NA for non-dates
                


# Here is the structure of the input data frame:
str(dt12)                            
attach(dt12)                            
# The analyses below are basic descriptions of the variables. After testing, they should be replaced.                 

summary(STCODE)
summary(FORMAT)
summary(SITE)
summary(REP)
summary(DURATION)
summary(SPECIES)
summary(TYPE)
summary(NIR_NUM)
summary(I_NITRO)
summary(AF_I_NITRO)
summary(F_NITRO)
summary(AF_F_NITRO)
summary(IAFW)
summary(FAFW)
summary(PAFRM)
summary(KAFW)
summary(I_N_CONT)
summary(F_N_CONT)
summary(N_CONC)
summary(TIMEOUT)
summary(COMMENT) 
                # Get more details on character variables
                 
summary(as.factor(dt12$STCODE)) 
summary(as.factor(dt12$SITE)) 
summary(as.factor(dt12$REP)) 
summary(as.factor(dt12$SPECIES)) 
summary(as.factor(dt12$TYPE)) 
summary(as.factor(dt12$COMMENT))
detach(dt12)               
        




