rm(list=ls(all=TRUE))
library(xlsReadWrite)
library(zoo)
library(xts)
###############################
# Temperature Descriptor Function #
###############################
tempmetrics <- function(temperaturefiles,outtable) {
  setwd(temperaturefiles)
  filelist <- list.files()
  ###################
  # Daily functions #
  ###################
  daily_mean <- function(x) { answer <- as.data.frame(aggregate(x,as.Date,mean))
  colnames(answer) <- "MeanDaily"
  return(answer)}
  daily_max <- function(x) { answer <- as.data.frame(aggregate(x,as.Date,max))
  colnames(answer) <- "MaxDaily"
  return(answer)}
  daily_min <- function(x) { answer <- as.data.frame(aggregate(x,as.Date,min))
  colnames(answer) <- "MinDaily"
  return(answer)}
  ################
  # Create Table #
  ################
  if (!file.exists(outtable)) {
    headers <- matrix(c("LoggerID","XYEAR","MDAT","MDMT","MWMT","MWAT","AWAT","GSDD","OVER_MIN",
                        "MAT","WMT5","WMT25","WMT50","WMT75","WMT95","MOV","MMAX","MMIN","MMOV","DELTA_MAX",
                        "SIGMA_MN","SIGMA_MIN","SIGMA_MAX","CV_MN","CV_MIN","CV_MAX","RNG","WEEK_14",
                        "WEEK_18","WEEK22","SUM_14","SUM_18","SUM_22","DMOV10","DMOV15","DMOV20","DMAX10",
                        "DMAX15","DMAX20","PG5","PG10","PG15","PG20","MDMT_DATE","MDMT_DATE_jd",
                        "MDMT_DATE_ROLL","MDMT_DATE_ROLL_jd","MIN_DATE","MIN_DATE_jd","MIN_DATE_ROLL",
                        "MIN_DATE_ROLL_jd"),ncol=51)
    write.table(headers,outtable,sep=",",row.names=F,col.names=F,quote=F,append=F)
    start <- 1
  } else { # if file does exist, don't write headers, and start at next record.
    oldtable <- read.csv(outtable)
    start <- nrow(oldtable) + 1
  }
  newtableflag <- 1
  for (j in 1:length(filelist)) {
    # read in file
    intemps <- as.xts(read.zoo(read.xls(filelist[j],colNames = FALSE, sheet = 1,dateTime = "character"),header=FALSE))
    siteid <- substr(filelist[j],1,nchar(filelist[j])-4)
    #####################
    # Calculate Metrics #
    #####################
    intemps <- na.omit(intemps)
    startdate <- as.character(min(index(intemps)))
    enddate <- as.character(max(index(intemps)))
    xyear <- .indexyear(last(intemps))+1900
    st <- paste(xyear,"-07-1",sep = "")
    ed <- paste(xyear,"-08-30",sep = "")
    sumsub <- window(intemps, start = as.Date(st), end = as.Date(ed))
    if (startdate > st | enddate < ed){
      newrow <- cbind(siteid,xyear,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,
                      NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
    }else{
      #Magnitude
      MDAT <- max(daily_mean(sumsub)) #Daily average on the hottest day
      MDMT <- max(sumsub) #Overall maximum recorded temperature
      MWMT <- max((apply.weekly(daily_mean(sumsub), FUN = mean)))#Maximum weekly mean temperature
      MWAT <- max((apply.weekly(daily_max(sumsub), FUN = mean)))#Maximum weekly maximum temperature
      AWAT <- mean(sumsub) #Overall summer average = Mean across all data 7-15 to 9-15
      GSDD <- sum(daily_mean(sumsub)) #Cumulative degree-days
      OVER_MIN <- min(sumsub) #Overall min during summer
      MAT <- mean(intemps) #Mean over all available data
      WMT5 <- as.numeric(quantile(apply.weekly(sumsub, FUN = mean),0.05)) #5th pctile mean weekly temps
      WMT25 <- as.numeric(quantile(apply.weekly(sumsub, FUN = mean),0.25)) #25th pctile mean weekly temps
      WMT50 <- as.numeric(quantile(apply.weekly(sumsub, FUN = mean),0.50)) #50th pctile mean weekly temps
      WMT75 <- as.numeric(quantile(apply.weekly(sumsub, FUN = mean),0.75)) #75th pctile mean weekly temps
      WMT95 <- as.numeric(quantile(apply.weekly(sumsub, FUN = mean),0.95)) #95th pctile mean weekly temps
      MOV <- max(rollmean(daily_max(sumsub),3)) #max of seven day rolling daily maxes
      MMAX <- as.numeric(colMeans(daily_max(sumsub))) #mean of all daily maxes
      MMIN <- as.numeric(colMeans(daily_min(sumsub))) #mean of all daily mins
      MMOV <- mean(rollmean(daily_mean(sumsub),3)) # mean of all seven day daily means
      #Variability
      DELTA_MAX <- max(daily_max(sumsub) - daily_min(sumsub)) #Maximum daily temperature range
      SIGMA_MN <- as.numeric(var(daily_mean(sumsub), y=NULL)) #variance of summer daily means
      SIGMA_MIN <- as.numeric(var(daily_min(sumsub), y=NULL)) #variance of summer daily mins
      SIGMA_MAX <- as.numeric(var(daily_max(sumsub), y=NULL)) #variance of summer daily maxes
      CV_MN <- as.numeric(sd(daily_mean(sumsub))/mean(daily_mean(sumsub))) #CV of summer daily means
      CV_MIN <- as.numeric(sd(daily_min(sumsub))/mean(daily_min(sumsub))) #CV of summer daily mins
      CV_MAX <- as.numeric(sd(daily_max(sumsub))/mean(daily_max(sumsub))) #CV of summer daily maxes
      RNG <- MDMT - OVER_MIN #Overall max minus min
      #Frequency
      w_dm <- as.xts(daily_max(sumsub))
      week_max <- split.xts(w_dm, f="weeks")
      HOTWEEK <- as.numeric(which.max(apply.weekly(w_dm, FUN = mean)))
      WEEK_14 <- as.numeric(sum(week_max[[HOTWEEK]]>14)) #Cumulative days >14C in hottest week
      WEEK_18 <- as.numeric(sum(week_max[[HOTWEEK]]>18)) #Cumulative days >18C in hottest week
      WEEK_22 <- as.numeric(sum(week_max[[HOTWEEK]]>22)) #Cumulative days >22C in hottest week
      SUM_14 <- as.numeric(sum(w_dm > 14)) #Cumulative days >14C in summer
      SUM_18 <- as.numeric(sum(w_dm > 18)) #Cumulative days >18C in summer
      SUM_22 <- as.numeric(sum(w_dm > 22)) #Cumulative days >22C in summer
      #Duration
      DMOV10 <- as.numeric(sum(rollmean(daily_max(sumsub), 3) > 10)) #No. days seven day rolling mean of daily maxes > 10C
      DMOV15 <- as.numeric(sum(rollmean(daily_max(sumsub), 3) > 15)) #No. days seven day rolling mean of daily maxes > 15C
      DMOV20 <- as.numeric(sum(rollmean(daily_max(sumsub), 3) > 20)) #No. days seven day rolling mean of daily maxes > 20C
      DMAX10 <- as.numeric(sum(daily_max(sumsub) > 10)) #No. days daily max exceeded 10 C
      DMAX15 <- as.numeric(sum(daily_max(sumsub) > 15)) #No. days daily max exceeded 15 C
      DMAX20 <- as.numeric(sum(daily_max(sumsub) > 20)) #No. days daily max exceeded 20 C
      PG5 <- (sum(sumsub > 5) / length (sumsub))*100 #% of temps > 5C
      PG10 <- (sum(sumsub > 10) / length (sumsub))*100 #% of temps > 10C
      PG15 <- (sum(sumsub > 15) / length (sumsub))*100 #% of temps > 15C
      PG20 <- (sum(sumsub > 20) / length (sumsub))*100 #% of temps > 20C
      #Timing
      XYEAR <- xyear #Year in which data were collected
      maxdate <- which.max(sumsub)
      MDMT_DATE <- as.character(index(sumsub[maxdate])) #Date of instantaneous maxiumum temperature
      dmax <- as.xts(daily_max(sumsub))
      maxdate2 <- which.max(dmax)
      MDMT_DATE_ROLL <- as.character(index(dmax[maxdate2])) #Date of highest 7 day running mean of daily max
      mindate <- which.min(sumsub)
      MIN_DATE <- as.character(index(sumsub[mindate])) #Date of instantaneous minimum temperature
      dmin <- as.xts(daily_min(sumsub))
      mindate3 <- which.min(dmin)
      MIN_DATE_ROLL <- as.character(index(dmin[mindate3])) #Date of highest 7 day running mean of daily max
      HOTWEEK <- as.numeric(which.max(apply.weekly(w_dm, FUN = mean)))
      MDMT_DATE_jd <- as.numeric(julian(index(sumsub[maxdate]))) #MDMT_DATE julian date
      MDMT_DATE_ROLL_jd <- round(as.numeric(julian(index(dmax[maxdate2]))),0)
      MIN_DATE_jd <- as.numeric(julian(index(sumsub[mindate])))
      MIN_DATE_ROLL_jd <- as.numeric(julian(index(sumsub[mindate])))
      #########
      # Table #
      #########
      newrow <- cbind(siteid,XYEAR,round(MDAT,3),round(MDMT,3),round(MWMT,3),round(MWAT,3),round(AWAT,3),round(GSDD,0),
                      round(OVER_MIN,3),round(MAT,3),round(WMT5,3),round(WMT25,3),round(WMT50,3),round(WMT75,3),
                      round(WMT95,3),round(MOV,3),round(MMAX,3),round(MMIN,3),round(MMOV,3),round(DELTA_MAX,3),
                      round(SIGMA_MN,3),round(SIGMA_MIN,3),round(SIGMA_MAX,3),round(CV_MN,3),
                      round(CV_MIN,3),round(CV_MAX,3),round(RNG,3),WEEK_14,WEEK_18,WEEK_22,SUM_14,SUM_18,SUM_22,
                      DMOV10,DMOV15,DMOV20,DMAX10,DMAX15,DMAX20,round(PG5,2),round(PG10,2),round(PG15,2),round(PG20,2),
                      MDMT_DATE,MDMT_DATE_jd,MDMT_DATE_ROLL,MDMT_DATE_ROLL_jd,MIN_DATE,MIN_DATE_jd,MIN_DATE_ROLL,
                      MIN_DATE_ROLL_jd)
    }
    if(newtableflag==1) {
      temptable <- newrow
      newtableflag <- 0} else {
        temptable <- rbind(temptable,newrow)
      }
    if(j/100-round(j/100)==0 | j==length(filelist)) { # save every 100 records
      write.table(temptable,outtable,sep=",",row.names=F,col.names=F,quote=F,append=T)
      newtableflag <- 1
    }
    # end of loop
  }
  # end of function
}
#Call tempmetrics function. Supply path to datafiles and name of output file. Then bring output file back into R.
tempmetrics("C:/Temp/waterdata/","temp_metrics_all_sites.csv")
metrics.output<-data.frame(read.csv("temp_metrics_all_sites.csv"))