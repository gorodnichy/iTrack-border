# readBorder.R

#. functions ----
if (T) {
  library(data.table, quietly=T)
  options(datatable.print.class=TRUE)
  #library(tidyverse) # includes: 
  library(readxl)
  library(magrittr)
  library(lubridate,  quietly=T)
  options(lubridate.week.start =  1)
  library(stringr)
  library(timeDate)
  #library(tibble)
  library(ggplot2)
  library(png)
  
  library(plotly)
  library(maps)
  
  # library(highcharter)
  library(treemap)
  
  library(leaflet)
  
  
  cleanEntries <- function(x){
    x %>% 
      gsub("No delay",  "0", .) %>% 
      gsub("No Delay",  "0", .) %>% 
      gsub(" minutes",  "", .) %>% 
      gsub(" minute",  "", .) %>% 
      gsub("Closed",  Inf, .) %>% 
      gsub("Temporarily closed",  Inf, .) %>% 
      gsub("--",  Inf, .)  %>% 
      gsub("Missed entry",  NA, .) %>% 
      gsub("Not applicable",  NA, .)  %>% 
      gsub("Not Applicable",  NA, .) 
  }
  
  
  roundByInt <- function(x, roundBy = 5)  {
    k = as.integer(round(10 / roundBy))
    return ( as.integer(round( round(x*k,-1)/k)) )
  }
  
  roundByMins <- function (.date, byMins=60) {
    if (byMins == 60)
      return (as.POSIXct(round.POSIXt(.date, "hour")))
    scalefactor <- 60*byMins
    as.POSIXct(round(as.numeric(.date)/scalefactor) * scalefactor, origin="1970-01-01", tz="") #Sys.timezone(location = TRUE))
    # Also possible:
    #  require(lubridate)
    #  round_date(.date, paste0(byMins," mins")) #  '2 hours')
  }
  
  
  #NB: modifies by reference
  dt.cleanBWT.byreference <- function(dt, numcols =  c("commercial",  "traveller") ) {
    dt[, (numcols) := lapply(.SD, cleanEntries), .SDcols = numcols] 
    dt[, (numcols) := lapply(.SD,function(x)(as.numeric(as.character(x)))), .SDcols = numcols]
    dt[, (numcols) := lapply(.SD, roundByInt), .SDcols = numcols]
    
    dt[, TIME := as.POSIXct(TIME, format = "%Y-%m-%d %H:%M ") ]
    dt[, TIME := roundByMins(TIME)]
    
    
    # not needed, once dtPort is created. - also in future for reading US BWT 
    if(F) {
      # dt[, COUNTRY:="CA"]
      dt$CA_US <- dt$LOCATION %>% str_split("/") 
      dt$CA  <- lapply( dt$CA_US, function(x) {x[1]} )
      
      dt$REGION <- dt$CA %>% str_extract(", [:alpha:][:alpha:]")  %>% str_replace(", ", "")
      dt$CA <- dt$CA %>% str_replace("\\. ", "\\.") %>%  str_replace(", ", " ")
      
      #      dt$US  <- lapply( dt$CA_US, function(x) {x[2]} )
      # dt$REGIONUS <- dt$US %>% str_extract(", [:alpha:][:alpha:]")  %>% str_replace(", ", "")
      # dt$US <- dt$US %>% str_replace("\\. ", "\\.") %>%  str_replace(", ", " ")
      
      #cUScities <- dt$US %>% unique
      
      dt$name <-  dt$CA
      
      dt$LOCATION <-  dt$CA_US <- dt$CA <- NULL
      
      dt$REGION <- dt$LOCATION %>% str_extract(", [:alpha:][:alpha:]")  %>% str_replace(", ", "")
      dt$name <- dt$LOCATION %>% str_replace("\\. ", "\\.") %>%  str_replace(", ", " ")
      
    }
  }
  
  #  date0 =as.POSIXct("2013-02-28 23:50:00 EDT")
  # So that "2014-04-05 00:59:00 EDT" -> "2014-04-05 01:00:00 EDT"
  #
  # #moved to D7.R
  dd.expandDate <- function(my.date="2017/10/29") { # my.date can be also a list or array
    
    my.date <- lubridate::ymd(my.date)
    .dt <- data.table(
      yy=lubridate::year(my.date) %>% as.integer(),
      mm=lubridate::month(my.date, label = TRUE),
      dd=lubridate::day(my.date) %>% as.integer(),
      mw=lubridate::week(my.date),  # month week
      wd=lubridate::wday(my.date, label = TRUE, week_start = getOption("lubridate.week.start", 1)),
      bHoliday=F
    )
    .dt[, mw := as.integer( 1 + mw - lubridate::week(    lubridate::ymd(paste0(yy,"-",mm,"-01"))    ) )]
    
    .dt[wd %in% c("Sat", "Sun"), bHoliday := T ]
    
    .dt[mm=="Jul" & dd==1, bHoliday := T ] # Canada Day
    .dt[mm=="Aug" & mw==2 & wd=="Mon",  bHoliday := T] # Civic Day  in Ontario
    .dt[mm=="Sep" & mw==2 & wd=="Mon",  bHoliday := T] # Labour Day
    .dt[mm=="Nov" & dd==11,  bHoliday := T] # Remembrance Day
    
    return(.dt)
  }
  
  dt.expandDateColumn.byreference <- function(.dt, columnDate="date") {
    col <- names(dd.expandDate("1971-10-29")) # just to get names
    .dt[, (col) := dd.expandDate (get(columnDate))]
  }
  
  # 
  # 
  # # expand TIME type column into several numeric columns,
  # # USED TO BE change in dt by reference!, Now somehow it stopped doing it, so you need to to write dt <- ...(dt)
  # dt.expandTimeColumn.byreference <- function(dt, columnDATE="date") {
  # 
  #   DATE = as.name(columnDATE)
  #   dt[, ':=' (wd=lubridate::wday(DATE, label = TRUE, week_start = getOption("lubridate.week.start", 1)),
  #              hh=lubridate::hour(DATE), mm=lubridate::month(DATE, label = TRUE),
  #              yy=lubridate::year(DATE),
  #              bHolidayCA=timeDate(DATE,  FinCenter = "Toronto") %>% isHoliday(), # dd.isHoliday(DATE, "Toronto"),
  #              bHolidayUS=timeDate(DATE,  FinCenter = "New_York") %>% isHoliday() # dd.isHoliday(DATE, "New_York")
  #              )
  #      ]
  #   
  #   # Other factors:
  #   #  long-weekend or not
  #   #  events: plane arrival, hockey game, blues-fest etc.
  #   #  weather, currency
  #   
  #   # mw: month week
  #   
  #   dt[, ww := lubridate::week(DATE)
  #      ][, mw := min(ww), by=mm
  #        ][, mw := ww - mw
  #          ]
  #   dt[, ww := NULL]
  #   #dt [, mw := ordered(mw,labels=c("1st","2nd","3rd", "4th", "5th"))]
  #   ##levels(dt$mw) <- c("1st","2nd","3rd", "4th", "5th")
  #   #To change the levels by reference with no copy of mydt :
  #   setattr(dt$mw,"levels",c("1st week","2nd week","3rd week", "4th week", "5th week"))
  #   
  #   
  #   if (F) d <- dt[mw == 5, .(yy,mm)]  %>% unique; d
  #   #  dt [yy == d$yy[1] & mm %in% d$mm[1] ] %>% head(25); #dd.d()
  #   
  #   for (i in 1:nrow(d)) {
  #     dt [yy == d[i]$yy & mm == d[i]$mm, mw := sapply(mw, function(x) { max (0, x-1)} )  ] # %>% head(25)
  #   }
  #   
  #   dt[, mw:=mw+1]
  #   
  #   # Assign holidays:
  #   dt[mm=="Oct" & mw==2 & wd=="Mon", bHolidayCA:=T] # Thanksgiving
  # }
  # #
  # if(F) {
  #   dt.expandTimeColumn.byreference(dt);
  #   #cols <- 2:7;
  #   cols <- c("yy","mw")
  #   dt[, (cols):=lapply(.SD, ordered), .SDcols=cols]
  # }
  # 
  # 
  
  
}

#. Globals ----

if (F) { # ... input ----
  input <- list()
  input$date <- format(Sys.time(), "%Y-%m-%d")
  input$region <- "BC"
  input$office <- "Abbotsford-Huntingdon"
  input$showTravellers <- "Travellers"
}

dtNow <- dtPast <- dtPredict <- dtPredictSelected <- data.table()
keyPredict <- c("OFFICE", "mm", "mw", "hh", "wd", "bHoliday")

#. dtNow: read current BWT ----

readNow <- function()  {
  strUrl <- "http://www.cbsa-asfc.gc.ca/bwt-taf/bwt-eng.csv"
  dtNow <<- fread(strUrl, stringsAsFactors = T, header=T)
  
  cols <- c("V2", "V4", "V6", "V8", "V10", "V12", "V14", "V15")
  dtNow [, (cols):=NULL]
  # dtNow %>% summary()
  # dtNow %>% names
  # dtNow[1:3]  
  names(dtNow) <- c("OFFICE",  "LOCATION",   "TIME",  "commercial",  "commercial_US", "traveller", "traveller_US") 
  
  
  #  dt.cleanBWT.byreference(dtNow)
  
  numcols =  c("commercial",  "traveller")
  dtNow[, (numcols) := lapply(.SD, cleanEntries), .SDcols = numcols] 
  dtNow[, (numcols) := lapply(.SD,function(x)(as.numeric(as.character(x)))), .SDcols = numcols]
  dtNow[, (numcols) := lapply(.SD, roundByInt), .SDcols = numcols]
  
  dtNow[, TIME := as.POSIXct(TIME, format = "%Y-%m-%d %H:%M ") ]
  # dtNow[, TIME := roundByMins(TIME)]
  
  return(dtNow)
}
if (F) 
  readNow()

#. dtPast: read past BWT ----

readPast <- function()  {
  if (T) { 
    dtPast <<-  readRDS ("dtPast-2010-2018.rds")
    
    return(dtPast)
  }else {
    
    if (F) {
      strUrl <- "http://www.cbsa-asfc.gc.ca/data/bwt-taf-2010-2014-eng.csv"
      dtPast <- fread(strUrl, stringsAsFactors = F, header=T) # 68.1 Mb
      names(dtPast) <- c("OFFICE", "LOCATION", "TIME", "commercial",  "traveller")
      
      
    }
    
    dtPast <<- NULL
    y=2018; q=1
    for (y in 2014:2018) { # Data available only for: 2014 Q2 - 2018 Q1
      #for (y in 2017:2017) {
      for (q in 1:4) {
        strUrl <- sprintf("http://cbsa-asfc.gc.ca/data/bwt-taf-%i-%02i-01--%i-%02i-31-en.csv", 
                          y, 3*(q-1)+1,y, 3*q); strUrl %>% print
        dt <- try(fread(strUrl, stringsAsFactors = T, header=T), T) # (5.1 MB)
        
        if (is.data.frame(dt) == F) {
          strUrl <- sprintf("http://cbsa-asfc.gc.ca/data/bwt-taf-%i-%02i-01--%i-%02i-30-en.csv", 
                            y, 3*(q-1)+1,y, 3*q); strUrl %>% print
          dt <- try(fread(strUrl, stringsAsFactors = T, header=T), T) # (5.1 MB)  
        }
        if (is.data.frame(dt) == F) 
          next
        
        names(dt) <- c("OFFICE",  "LOCATION",   "TIME",  "commercial",  "traveller")
        dtPast <<- dt %>% rbind(dtPast)
      }
    }
    
    
    dt.cleanBWT.byreference(dtPast)
    
    
    # not needed, once dtPort is created. We index by OFFICE
    dtPast$LOCATION <- NULL
    
    if (F) {
      dtPast %>% summary
      dtPast$traveller %>% unique()
      dtPast[1:3]
    }
    saveRDS(dtPast, "dtPast-2017.rds")
    saveRDS(dtPast, "dtPast-2014-2018.rds")
    saveRDS(dtPast, "dtPast-2010-2014.rds")
    dt <- dtPast  %>% rbind(readRDS("dtPast-2014-2018.rds"))
    setkey(dt,OFFICE,TIME)
    saveRDS(dt, "dtPast-2010-2018.rds")
    # fwrite (dtPast, "dtPast-2014-2018.csv", sep="\t")
    #fwrite (dtPast, "dtPast-2017.csv", sep="\t")
    
  }
  
}


plotPastData.forWeek <- function (office, today, bTraveller="Travellers") {
  
  dtToday <- today %>% dd.expandDate()
  dt <- dtPast[OFFICE == office] 
  
  dt <- dt[, date := date(TIME)] %>% dt.expandDateColumn.byreference(columnDate = "date") 
  dt[, hh:=hour(TIME)]
  
  if (bTraveller=="Travellers") {
    dt[, BWT:=traveller]
  } else {
    dt[, BWT:=commercial ]
  }
  
  
  maxBWT = dt[mw==dtToday$mw & mm==dtToday$mm]$BWT %>% max (na.rm=T)
  

  g <- ggplot(dt[mw==dtToday$mw & mm==dtToday$mm], size=2) + 
    geom_line(aes(hh,BWT, col=wd), size=1) +
    geom_point(aes(hh,BWT, col=wd), size=2) +
    #geom_smooth(aes(hh,BWT, col=wd)  +
    #scale_color_brewer(palette = "Blues", name="Year") +  #Spectral
     # scale_color_gradient(name="Year") +
    guides(color="none") +
    facet_grid(yy ~ wd) + 
    scale_x_continuous(breaks=6*(0:4)) +    
    scale_y_continuous(breaks=seq(0, maxBWT, 20), limit=c(0,maxBWT)) +
    #    theme_bw() +
    theme_minimal() +
    # labs(title=   sprintf( "Historical data for %s: week %i in %s (%s)",   office, dtToday$mw, dtToday$mm, bTraveller), 
    labs(title=   sprintf( "Historical Border Wait Time - %s",   bTraveller), 
         subtitle=sprintf("Port: %s. Month: %s. Week: #%i", office, dtToday$mm, dtToday$mw), 
         caption=paste0("Generated by iTrack Border (https://itrack.shinyapps.io/border)"), 
         x="Hour", y="BWT"
    ) 
  
  g
  #ggplotly(g)
}

if (F) {
  offices <- dtPorts[REGION == "BC"]$OFFICE
  office <- offices[1]
  today <- "2019-05-31"
  dates <-  ymd(today): (ymd(today)+days(7))
  dates <-  ymd(today): (ymd(today)+days(30))
  
  plotPastData.forWeek(office, today, "Travellers")
  plotPastData.forWeek(input$office, input$date, input$showTravellers)
}

#. dtPredict: build BWT predictions ----

predictBWT <- function (office, today="2019-05-31", bPlot=F) {
  
  wd <- wday(today); today; wd
  
  d <- list(); dtD <- list()
  d[[1]] <- ymd(today)  + days(2) - years(2); 
  d[[2]] <- ymd(today)  + days(3) - years(3); 
  
  d[[3]] <- ymd(today)  + days(5) - years(4); 
  d[[4]] <- ymd(today)  + days(6) - years(5); 
  d[[5]] <- ymd(today)  + days(7) - years(6); 
  d[[6]] <- ymd(today)  + days(8) - years(7); 
  
  d[[7]] <- ymd(today)  + days(10) - years(8); 
  d[[8]] <- ymd(today)  + days(11) - years(9); 
  
  dtD <- data.table()
  for (i in 1:8) {
    #print( paste(d[[i]], wday(d[[i]])) )
    dt <- dtPast[OFFICE %in% office][as_date(TIME) == as_date(d[[i]]), 2:4][,hh:=hour(TIME)]
    dt  <- dt  %>% rbind ( dt[hh==0][, hh:=24] )
    dt  <- dt  %>% rbind ( dt[hh==1][, hh:=25] )
    dt  <- dt  %>% rbind ( dt[hh==2][, hh:=26] )
    dt  <- dt  %>% rbind ( dt[hh==3][, hh:=27] )
    dt  <- dt  %>% rbind ( dt[hh==23][, hh:=-1] )
    dt  <- dt  %>% rbind ( dt[hh==22][, hh:=-2] )
    dt  <- dt  %>% rbind ( dt[hh==21][, hh:=-3] )
    
    dt[, traveller:=2.5*traveller];     dt[, commercial:=3*commercial]; 
    dtD <- dtD %>% rbind (dt)
  }
  setkey(dtD, hh) # order by hh
  
  dtD[, t0:=ifelse(traveller>20, traveller-10, traveller)]
  dtD[, c0:=ifelse(commercial>20, commercial-10, commercial)]
  
  #dtD[t0>10]
 
  return ( dtD ) 
}
if (F){

    office <- "Windsor and Detroit Tunnel" #dtPorts$OFFICE[i]
    office <- "Abbotsford-Huntingdon"
    office <- dtPorts$OFFICE[1:4]
    today="2019-05-18"

  predictBWT(office, today)

}

readPredict <- function()  {
  
  if (T) {
    dtPredict <<- readRDS("dtPredictTC.rds")
    dtPredict[t>20]
    #dtPredict <<- readRDS("dtPredict.rds")
    #dtPredict[ave>20]
  } else {
    
    
    dtPast[, date := date(TIME)] %>% dt.expandDateColumn.byreference(columnDate = "date")  
    
    dtPast[, hh:=lubridate::hour(TIME)]
    
    dtPredictT <- dtPast[, .(median=median(traveller, na.rm = T), 
                             ave=mean(traveller, na.rm = T), 
                             min=min(traveller, na.rm = T), max=max(traveller, na.rm = T), .N), by=keyPredict]
    dtPredictC <- dtPast[, .(medianC=median(commercial, na.rm = T), 
                             aveC=mean(commercial, na.rm = T), 
                             minC=min(commercial, na.rm = T), maxC=max(commercial, na.rm = T)), by=keyPredict]
    
    setkeyv(dtPredictT, keyPredict)
    setkeyv(dtPredictC, keyPredict)
    dtPredict <- dtPredictC[dtPredictT]
    
    
    
    dtPredict$t<-   dtPredict$t0 <- dtPredict$c<-   dtPredict$c0 <-  0
    
    dtPredict[, t:=roundByInt(median)][, t0:=ifelse(t>15, t-5, t)]
    dtPredict[, c:=roundByInt(medianC)][, c0:=ifelse(c>15, c-5, c)]
    dtPredict[c0>10]
    
    dtPredict[1]
    
    dtPredict[, c(1:6, 16:19)]
    
    fwrite(dtPredict[, c(1:6, 16:19)], "dtPredictTC.csv", sep="\t" )
    saveRDS(dtPredict[, c(1:6, 16:19)], "dtPredictTC.rds")
    
    fwrite(dtPredict, "dtPredict.csv", sep="\t" )
    saveRDS(dtPredict, "dtPredict.rds")
  }
}
if (F) {
  dtPredict[, .N, by=c("OFFICE", "mm", "mw",  "wd", "bHoliday")]
  dtPredict[, .N, by=c("OFFICE", "mm", "mw",  "wd")]
}



plotPredict <- function() {
  
  maxBWT <- dtPredictSelected$t %>% max * 1.5
  #maxBWT <- dt$max %>% max 
  #maxBWT <- 60
  
  g <- ggplot(dt) +
    geom_smooth(aes(hh,t), col="blue", se=F, size=2) + 
    geom_smooth(aes(hh,t0), col="green") +
    # geom_ribbon(aes(ymin=min, ymax=max, x=hh), alpha=0.5) + 
    scale_x_continuous(breaks=3*(0:8)) +    
    scale_y_continuous(breaks=seq(0, maxBWT, 10), limit=c(0,maxBWT)) +
    #    theme_bw() +
    theme_minimal() +
    labs(title=   sprintf( "Expected Delay for %s (%s)",  today, dtToday$wd), 
         subtitle=sprintf("Port: %s", dtPorts$OFFICE[i], dtPorts$name[i]),
         caption=paste0("Generated by iTrack Border (https://itrack.shinyapps.io/border)"), 
         x="Hour", y="BWT"
    ) + facet_wrap(OFFICE ~ .)
  
  print(g)
}

# . dtPorts: merge with Geo ----


readPorts <- function() {
  
  if (T) {
    dtPorts <<- fread("dtPorts.csv")
    return(dtPorts)
  } else {
    # dtMapUS <- data.table(us.cities)
    # dtMapUS$name <- dtMapUS$name %>% str_replace("\\. ", "\\.") 
    
    
    dtMap <- data.table(canada.cities)
    dtMap$name <- dtMap$name %>% str_replace("\\. ", "\\.") 
    setkey(dtMap,  name)
    setkey(dtPorts,  name)
    setkey(dtPast,  name)
    
    dt <- dtNow
    dt <- dtPast 
    dtPorts <- dt[, .(REGION, name, OFFICE)] %>% unique() 
    setkey(dtPorts,  name)
    
    dtPorts <- dtMap[dtPorts]
    dtPorts$country.etc <- dtPorts$capital <- NULL
    dtPorts
    
    if (T) {
      dtMap[str_detect(name, "^La")]
      
      dtPorts[name=="Belleville NB", ':='(lat=46.13, long=-67.77, cbsa=T)]
      dtPorts[name=="Coutts AB", ':='(lat=48.999, long=-111.96, cbsa=T)]
      dtPorts[name=="Delta BC", ':='(lat=49.011, long=-123.066, cbsa=T)]
      dtPorts[name=="Emerson MB", ':='(lat=49.00, long=-97.23, cbsa=T)]
      dtPorts[name=="Huntingdon BC", ':='(lat=49.00, long=-122.26, cbsa=T)]
      dtPorts[name=="Lansdowne ON", ':='(lat=44.348, long=-75.984, cbsa=T)]
      dtPorts[name=="Niagara Falls ON", ':='(lat=43.09, long=-79.0699, cbsa=T)]
      dtPorts[name=="North Portal SK", ':='(lat=48.999, long=-102.55, cbsa=T)]
      dtPorts[name=="Queenston ON", ':='(lat=43.156, long=-79.049, cbsa=T)]
      dtPorts[name=="Sault Ste.Marie ON", ':='(lat=46.519, long=-84.34, cbsa=T)]
      dtPorts[name=="St.Armand QC", ':='(lat=45.01837, long=-73.084, cbsa=T)]
      dtPorts[name=="St.Stephen NB", ':='(lat=45.1933, long=-67.283, cbsa=T)]
      dtPorts[name=="Stanstead QC", ':='(lat=45.007, long=-72.087, cbsa=T)]
      dtPorts[name=="Surrey BC", ':='(lat=49.012, long=-122.733, cbsa=T)]
      
      
    }
    
    if (F) { #This is only for Historical data. - some Offices are new there
      setkey(dtPorts,  OFFICE)
      setkey(dtPast,  OFFICE)
      dtPast [, .N, keyby=OFFICE ]
      dtPorts[dtPast [, .N, keyby=OFFICE ]] 
      
      
      
      dt <- data.table(name="Clair NB", pop=NA, lat=47.25, long=-68.605,  capital = 0, REGION="NB", OFFICE="Clair", cbsa=T)
      dtPorts <- dtPorts %>% rbind(dt)
      dt <- data.table(name="Grand Falls NB", pop=NA, lat=46.872, long=-67.768,  capital = 0, REGION="NB", OFFICE="Grand Falls", cbsa=T)
      dtPorts <- dtPorts %>% rbind(dt)
      dt <- data.table(name="ST-L\xe9onard NB", pop=NA, lat=47.18, long=-67.93,  capital = 0, REGION="NB", OFFICE="ST-L\xe9onard", cbsa=T)
      dtPorts <- dtPorts %>% rbind(dt)
      
      
    }
    
    fwrite (dtPorts, "dtPorts.csv", sep="\t")
    #dtPorts <- fread ("dtPorts.csv")
    
    return(dtPorts)
    #dtPorts[complete.cases(dtPorts[,c("pop", "lat")]),]
  }
  
}


# plot Canadian cities ----------------------------------------------------

plotMapNow <- function (strOffices ) {
  
  setkey(dtNow,OFFICE)
  setkey(dtPorts,OFFICE)
  dt <- dtPorts[dtNow]
  
  # dtD <-  predictBWT(strOffices, input$date);
  
  #  pal <- colorNumeric(c("green", "yellow", "red"), 0:30)
  
  dt[, ratingcol:= ifelse(is.na(traveller), "black",
                          ifelse(traveller <10, "green",
                                 ifelse(traveller <= 20, "yellow", "red")))]
  
  dt[, strMessage:= paste(
    sprintf("<b>%s <br/>    %s</b><br/><br/>", OFFICE, LOCATION),
    sprintf("BWT is %i mins ", traveller), 
    sprintf("(Reported at %s)",TIME) 
    # sprintf("Predicted: %i mins", )
  )
  ]
  # sprintf("BWT at %02i:00  -", hour(TIME)), "<br/>",
  # "  Reported on  ", traveller, "(mins)<br/>",
  # "  Predicted: ", traveller, "/", traveller, "(mins)")]
  
  
  anOffices = 1:3
  
  leaflet(data = dt) %>% 
    addTiles() %>%
    addCircleMarkers(~long, ~lat, 
                     #color = ~pal(traveller), 
                     color = ~ratingcol, 
                     popup = ~as.character(strMessage),
                     label = ~as.character(OFFICE)) %>% 
    #  addMarkers(clusterOptions = markerClusterOptions())
    #  addMarkers(~long, ~lat,   popup = ~as.character(traveller), label = paste("BWT: ", ~traveller)             )  %>%
    addPopups(dt[OFFICE %in% strOffices]$long, 
              dt[OFFICE %in% strOffices]$lat, 
              popup = dt[OFFICE %in% strOffices]$strMessage, 
              options = popupOptions(closeButton = FALSE) ) %>% 
    addLegend("bottomleft", 
              colors = c("green", "yellow", "red", "black"),
              labels = c("No delay",
                         "Slight delay",
                         "Long delay",
                         "N/A"), 
              opacity = 0.7)
  
}


# >>. Other ----



# . Using Leaflet with Shiny ----------------------------------------------------

if (F) {
  r_colors <- rgb(t(col2rgb(colors()) / 255))
  names(r_colors) <- colors()
  
  ui <- fluidPage(
    leafletOutput("mymap"),
    p(),
    actionButton("recalc", "New points")
  )
  
  
  points <- eventReactive(input$recalc, {
    cbind(rnorm(40) * 2 + 13, rnorm(40) + 48)
  }, ignoreNULL = FALSE)
  
  
  renderLeaflet({
    leaflet() %>%
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)
      ) %>%
      addMarkers(data = points())
  })
  
  
}
if (F) { # . plotly ----
  
  data(canada.cities, package = "maps")
  viz <- ggplot(canada.cities, aes(long, lat)) +
    borders(regions = "canada") +
    coord_equal() +
    geom_point(aes(text = name, size = pop), colour = "red", alpha = 1/2)
  ggplotly(viz, tooltip = c("text", "size"))
  
  
  d <- highlight_key(mtcars)
  qplot(data = d, x = mpg, y = wt) %>%
    subplot(qplot(data = d, x = mpg, y = vs)) %>% 
    # layout(title = "Click and drag to select points") %>%
    highlight("plotly_selected")
  
  
}


if (F) { # . treemap ----
  
  data(GNI2014)
  treemap(GNI2014,
          index=c("continent", "iso3"),
          vSize="population",
          vColor="GNI",
          type="value",
          format.legend = list(scientific = FALSE, big.mark = " "))
  
  
  tm <- treemap(GNI2014, index = c("continent", "iso3"),
                vSize = "population", vColor = "GNI",
                #type = "comp",
                type="value",
                #palette = rev(viridis(6)),
                format.legend = list(scientific = FALSE, big.mark = " "),
                draw = T)
  
  hctreemap(tm, allowDrillToNode = TRUE, layoutAlgorithm = "squarified") %>% 
    hc_title(text = "Gross National Income World Data") %>% 
    hc_tooltip(pointFormat = "<b>{point.name}</b>:<br>
                             Pop: {point.value:,.0f}<br>
                             GNI: {point.valuecolor:,.0f}")
  
  
  
  data("Groceries", package = "arules")
  dfitems <- tbl_df(Groceries@itemInfo)
  
  set.seed(10)
  
  dfitemsg <- dfitems %>%
    mutate(category = gsub(" ", "-", level1),
           subcategory = gsub(" ", "-", level2)) %>%
    group_by(category, subcategory) %>% 
    summarise(sales = n() ^ 3 ) %>% 
    ungroup() %>% 
    sample_n(31)
  
  tm <- treemap(dfitemsg, index = c("category", "subcategory"),
                vSize = "sales", vColor = "sales",
                type = "value", palette = rev(viridis(6)))
  
  highchart() %>% 
    hc_add_series_treemap(tm, allowDrillToNode = TRUE,
                          layoutAlgorithm = "squarified") %>% 
    hc_add_theme(thm)
  
}
