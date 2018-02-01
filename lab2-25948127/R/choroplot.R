choroplot <- function (allans.QNo = all.ans$'95', QNo = "Q095", question = '95', rn = "ans.num") {
  Q <- allans.QNo
  Q$ans.num <- seq(1, nrow(Q))
  QData <- lingData %>%
    select(ID, CITY, STATE, ZIP, QNo, lat, long)

colnames(QData) <- c("ID", "CITY", "STATE", "ZIP", "ans.num", "lat", "long")

QData <- QData %>%
          full_join(Q)

data(zipcode)
zipcode$zip <- as.numeric(zipcode$zip)

Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

QAgg <- QData %>%
          rename(zip = ZIP) %>%
          full_join(zipcode) %>%
          group_by(zip) %>%
          mutate(common_ans = Mode(ans)) %>%
          drop_na(common_ans)

zipq <- QAgg %>%
          count(zip)

qKeyData <- zipq %>%
              left_join(QAgg) %>%
              select(zip, latitude, longitude, common_ans, n) %>%
              distinct() %>%
              filter(!is.na(longitude), !is.na(latitude))

mapcounties <- map_data("county")
mapstates <- map_data("state")

counties <- map('county', fill=TRUE, col="transparent", plot=FALSE)
IDs <- sapply(strsplit(counties$names, ":"), function(x) x[1])
counties_sp <- map2SpatialPolygons(counties, IDs=IDs,
                                   proj4string=CRS("+proj=longlat +datum=WGS84"))

qKeyData_sp <- qKeyData
coordinates(qKeyData_sp) <- ~longitude + latitude
proj4string(qKeyData_sp) <- CRS("+proj=longlat +datum=WGS84")
indices <- over(qKeyData_sp, counties_sp)

countyNames <- sapply(counties_sp@polygons, function(x) x@ID)
qKeyData$county <- countyNames[indices]

mapcounties$county <- with(mapcounties , paste(region, subregion, sep = ","))

qKeyData_sum <- qKeyData %>% mutate(counts = sum(n))
qKeyData_un <- qKeyData %>% distinct()
qKeyData_final <- full_join(qKeyData_sum, qKeyData_un)
mergedq <- full_join(mapcounties, qKeyData_final)

title <- quest.mat %>% filter(qnum == question) %>% select(quest)
title <- as.character(title)

map <- ggplot(data=mergedq, aes(long, lat, group=group)) + geom_polygon(aes(fill=common_ans)) + 
  geom_path(data=mapstates, colour="black", size=.3) + geom_path(data=mapcounties, colour="white", size=.5, alpha=.1) + 
  ggtitle(title) + xlab("Longitude") + ylab("Latitude") + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), legend.position = "bottom") + guides(fill=guide_legend(title="Response"))
return(map)
}
