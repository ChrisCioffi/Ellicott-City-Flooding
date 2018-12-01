
#installs the dataRetrieval package from the USGS: https://owi.usgs.gov/R/training-curriculum/usgs-packages/dataRetrieval-discovery/
#the goal of this code will be to use the USGS data to evaluate stream gauges' peak flow data to see if the frequency of flooding around Ellicott City has increased. 
#install.packages('dataRetrieval')
#I'm also going to use GGMAP to visualize where these sites are, and if they're in-line with where I was told they are.
#install.packages("ggmap")
#install.packages("ggrepel")
library(ggmap)
library(dataRetrieval)
library(tidyverse)
library(devtools)
library(ggplot2)
library(ggrepel)

#for safe keeping, here's the The dataRetrieval R package manual: https://pubs.usgs.gov/tm/04/a10/pdf/tm4A10_appendix_1.pdf
#here's the training curriculum: https://owi.usgs.gov/R/training-curriculum/usgs-packages/dataRetrieval-discovery/
#here's a slideshow: https://acwi.gov/monitoring/conference/2016/0_monday_may2/A1B1/dataRetrieval%20Tutorial_secure.pdf




#here's an example from Stack Exchange on how the GGMAP should work https://stackoverflow.com/questions/15069963/getting-a-map-with-points-using-ggmap-and-ggplot2
#d <- data.frame(lat=c(50.659631, 50.607213, 50.608129),
#                lon=c(3.09319, 3.011473, 3.031529))
#note, you must reverse the lat/long from how it is on google maps to get a good image.
#Lille <- (get_map(location=c(3.056081,50.626773), zoom=13, scale=2)) # results in below map (wohoo!)
#p <- ggmap(Lille)
#p <-  p + geom_point(data=d, aes(x=lon, y=lat),size=5)
#p





#To discover what data is available for a particular USGS site, including measured parameters, period of record, and number of samples (count), use the whatNWISdata function.

#From USGS Manual https://pubs.usgs.gov/tm/04/a10/pdf/tm4A10_appendix_1.pdf, page 7, chapt '2.1.2 whatNWISdata'

# So now we want to find what the database contains for several  This pulls out just the daily, mean data:


#So let's try and find some specific data that is available for one of the sites provided by Jon Dillow. The three sites are above. Dillow told me they were close to Ellicott and would give an idea of what happened historically near Ellicott City.
#(1)  01589000 Patapsco River at Hollofield (next upstream from the short-record gage at Catonsville, and above Ellicott city),
#the patapsco at Hollowfield's flow is regulated by Liberty Reservoir, 11 mi upstream, beginning July 22, 1954....something to ask about. Flood stage is at 11 feet, action is 9. According to nws. https://water.weather.gov/ahps2/hydrograph.php?gage=prhm2&wfo=lwx

#(2)  01589330 Dead Run at Franklintown 

#from USGS: occasional regulation at low flow from unknown source upstream from station. Flood stage is 10 feet. Action is 8 feet. https://water.weather.gov/ahps2/hydrograph.php?gage=drfm2&wfo=lwx

#(3)  01589100 East Branch Herbert Run at Arbutus.
#(4) 01593500 LITTLE PATUXENT RIVER AT GUILFORD, MD - added because of its long record and general proximity to Ellicott City
#Slight regulation at low flow from unknown source upstream from station. But no dam, it appears. Flood stage is 7 feet. Action is 5.
#(5) 01591000 PATUXENT RIVER NEAR UNITY, MD - Lat 39°14'17.7", long 77°03'20.6", Montgomery County, Hydrologic Unit 02060006, on right bank at downstream side of bridge on State Highway 97, 0.6 mi upstream from Cattail Creek, 0.8 mi upstream from Triadelphia Reservoir, 1.1 mi northeast of Unity, and 97 mi upstream from mouth.


#so first we're going to use and modify code provided in the USGS manuals to look at these sites.
#make a variable called site numbers, so I can easily tell R which sites I want.
siteNumbers <- c("01589000","01589330","01589100", "01593500", "01591000")
#e function to obtain all of the information available for a particular USGS site (or sites) such as full station name, drainage area, latitude, and longitude.
siteINFO <- readNWISsite(siteNumbers)
#the readNWISSITE data also has some embedded metadata. That can be accessed with the comment function
comment(siteINFO)


#Now we can see where these stations are, in relation to Ellicott City. 
#note, you must reverse the lat/long from how it is on google maps to get a good image.
#on zoom function. 13 is about the closest zoom you can get. The lower the number the farther the zoom. 
#had real issues getting ggmap to work. But now it is working....This will be needed later. It's just on top because that's what I did. 
#after some fighting with the internet I was able to get ggmap to work by adding an api. 
register_google(key = "API KEY ")
#tests the map.
ggmap(get_googlemap())

ellicott_city <- (get_map(location=c(-76.762132, 39.275110), zoom=10, scale=2)) # results in below map (wohoo!)
e <- ggmap(ellicott_city) +
  #sets the point and displays as a size 5 with alpha/opacity of .6
     geom_point(data=siteINFO, aes(x=dec_long_va, y=dec_lat_va),color="red", size=5, alpha=0.6) +
  #sets the positions of the label, what rows the label should come from, and size. nuuuuudge_ is great.
      geom_label_repel(data = siteINFO, aes(x=dec_long_va, y=dec_lat_va, label = station_nm), size = 3) 
e


#OK, so now that I've spent lots of time making a map I'll probably never use, let's get to looking at what this database has. I am asking for daily values (dv) and max values (statCd)
#first, let's remember something. We're going to want to know these Stat Codes.
#Table 3. Commonly used USGS Stat Codes
#StatCode shortName
#00001 Maximum
#00002 Minimum
#00003 Mean
#00008 Median
dailyDataAvailable <- whatNWISdata(siteNumber = siteNumbers, service="dv", statCd="00003")
comment(dailyDataAvailable)

attr(dailyDataAvailable, "url")

#pulls out the peak dates from each site....this data is sort of wonky, according to Robert Mason...the peak events are somewhat sporratic. pre 1990, scientists used to pull a few peaks from each year. Post, it was one event.  
hollofield_peak <- readNWISpeak("01589000")
#data dictionary available with this command:
comment(hollofield_peak)
deadrun_peak <- readNWISpeak("01589330")
eastbranch_peak <- readNWISpeak("01589100")
patuxent_peak <- readNWISpeak("01593500")

#now let's take a quick look at our data to see if there's something that we can easily point to....I see that there's a lot more points over 10 between 2000 and 2020 than earlier. hmmmm
ggplot(data = hollofield_peak, aes(x = peak_dt, y = gage_ht))+
  geom_point(color = "#00AFBB", size = 2)
#same for dead run...I see that 7 may be an interesting cut point. 
ggplot(data = deadrun_peak, aes(x = peak_dt, y = gage_ht))+
  geom_point(color = "#00AFBB", size = 2)



#provides all usgs sites 
#parameterCd = "00060" is discharge -- gauge height is 00065
md_sites <- whatNWISdata(stateCd = "MD", parameterCd = "00060")

#filters it into just daily value discharges -- could got 
md_sites <- md_sites %>%
  mutate(years = as.numeric(difftime(end_date, begin_date, units = "weeks"))/52.25) %>%
  arrange(desc(years)) %>%
  filter(data_type_cd == "dv" &
           stat_cd == "00003")

#asks R to query the database to pull the daily value readNWISdv for all sites in Maryland

md_peaks <- readNWISdv(unique(md_sites$site_no), parameterCd = "00060")

#Now let's build some quick graphic around this flooding data that I got.
#read it in
flood_gauge <- read_csv("Patapsco_catonsville.csv")
#and we want to get out these pesky ps and turn it numeric...I think we'll have to get the date formatted too
flood_gauge$`Gageheight,  feet,` <- as.numeric(gsub("P","",flood_gauge$`Gageheight,  feet,`))
flood_gauge$`Date / Time` <- as.POSIXct(flood_gauge$`Date / Time`,format="%m/%d/%Y %H:%M",tz=Sys.timezone())

#and I've decided I don't want data from after the 29th. So I'll filter that out now
flood_gauge_short <- flood_gauge  %>%
  filter(`Date / Time` < "2018-05-28 12:00:00" & `Date / Time` > "2018-05-27 12:00:00") %>%
  arrange(`Date / Time`)

?order_by

#so now we can graph this sucker
ggplot(data=flood_gauge_short,aes(x=`Date / Time`, y=`Gageheight,  feet,`)) + 
  geom_path(colour="red") + 
  ylab("Gauge height in feet") + 
  xlab("Time") +
  scale_x_datetime(date_breaks = "8 hours", date_labels = ("%m/%d %H:%M")) 


#alright, looks cool. So let's build a list so I can export this and put it in a custom C3 graphic.

paste0(flood_gauge_short$`Gageheight,  feet,`[1:96], collapse=",")
paste0(flood_gauge_short$`Date / Time`[1:96], collapse="', '")
