setwd("~/StormData")
if (!"StormData.csv.bz2" %in% dir("./")) {
        download.file(
                "http://d396qusza40orc.cloudfront.net/repdata%2Fdata%2FStormData.csv.bz2",
                destfile = "StormData.csv.bz2"
        )
}
if (!"stormdata" %in% ls()) {
        stormdata <-
                read.csv(
                        bzfile("StormData.csv.bz2"),
                        sep = ",",
                        header = TRUE,
                        stringsAsFactors = FALSE
                )
}
dim(stormdata)
Stormevents <-
        c(
                "Astronomical Low Tide",
                "Avalanche",
                "Blizzard",
                "Coastal Flood",
                "Cold/Wind Chill",
                "Debris Flow",
                "Dense Fog",
                "Dense Smoke",
                "Drought",
                "Dust Devil",
                "Dust Storm",
                "Excessive Heat",
                "Extreme cold/Wind Chill",
                "Flash Flood",
                "Flood",
                "Freezing",
                "Frost/Freeze",
                "Funnel Cloud",
                "Hail",
                "Heat",
                "Heavy Rain",
                "Heavy Snow",
                "High Surf",
                "High Wind",
                "Hurricane/Typhoon",
                "Ice Storm",
                "Lakeshore Flood",
                "Lake-Effect Snow",
                "Lightning",
                "Marine Hail",
                "Marine High Wind",
                "Marine Strong Wind",
                "Marine Thunderstorm Wind",
                "Rip Current",
                "Seiche",
                "Sleet",
                "Storm Tide",
                "Strong Wind",
                "Thunderstorm Wind",
                "Tornado",
                "Tropical Depression",
                "Tropical Storm",
                "Tsunami",
                "Volcanic Ash",
                "Waterspout",
                "Wildfire",
                "Winter Storm",
                "Winter Weather"
        )
Stormevents_regex <-
        c(
                "Astronomical Low Tide|Low Tide",
                "Avalanche",
                "Blizzard",
                "Coastal Flood",
                "Cold/Wind Chill",
                "Debris Flow",
                "Dense Fog",
                "Dense Smoke",
                "Drought",
                "Dust Devil",
                "Dust Storm",
                "Excessive Heat",
                "Extreme cold/Wind Chill|Extreme Cold|Wind Chill",
                "Flash Flood",
                "Flood",
                "Freezing",
                "Frost/Freeze|Frost|Freeze",
                "Funnel Cloud",
                "Hail",
                "Heat",
                "Heavy Rain",
                "Heavy Snow",
                "High Surf",
                "High Wind",
                "Hurricane/Typhoon|Hurricane|Typhoon",
                "Ice Storm",
                "Lakeshore Flood",
                "Lake-Effect Snow",
                "Lightning",
                "Marine Hail",
                "Marine High Wind",
                "Marine Strong Wind",
                "Marine Thunderstorm Wind|Marine tstm Wind",
                "Rip Current",
                "Seiche",
                "Sleet",
                "Storm Tide",
                "Strong Wind",
                "Thunderstorm Wind|tstm wind",
                "Tornado",
                "Tropical Depression",
                "Tropical Storm",
                "Tsunami",
                "Volcanic Ash",
                "Waterspout",
                "Wildfire",
                "Winter Storm",
                "Winter Weather"
        )

options(scipen = 999)
cleandata <- data.frame(EVTYPE = character(0), FATALITIES = numeric(0), INJURIES = numeric(0), PROPDMG = numeric(0), PROPDMGEXP = character(0), CROPDMG = numeric(0), CROPDMGEXP = character(0))
for (i in 1:length(Stormevents)) {
        rows <-
                stormdata[grep(Stormevents_regex[i], ignore.case = TRUE, stormdata$EVTYPE),]
        rows <-
                rows[, c(
                        "EVTYPE",
                        "FATALITIES",
                        "INJURIES",
                        "PROPDMG",
                        "PROPDMGEXP",
                        "CROPDMG",
                        "CROPDMGEXP"
                )]
        CLEANNAME <- c(rep(Stormevents[i], nrow(rows)))
        rows <- cbind(rows, CLEANNAME)
        cleandata <- rbind(cleandata, rows)
}

cleandata[(cleandata$PROPDMGEXP == "K" |
                   cleandata$PROPDMGEXP == "k"), ]$PROPDMGEXP <- 3
cleandata[(cleandata$PROPDMGEXP == "M" |
                   cleandata$PROPDMGEXP == "m"), ]$PROPDMGEXP <- 6
cleandata[(cleandata$PROPDMGEXP == "B" |
                   cleandata$PROPDMGEXP == "b"), ]$PROPDMGEXP <- 9
cleandata[(cleandata$CROPDMGEXP == "K" |
                   cleandata$CROPDMGEXP == "k"), ]$CROPDMGEXP <- 3
cleandata[(cleandata$CROPDMGEXP == "M" |
                   cleandata$CROPDMGEXP == "m"), ]$CROPDMGEXP <- 6
cleandata[(cleandata$CROPDMGEXP == "B" |
                   cleandata$CROPDMGEXP == "b"), ]$CROPDMGEXP <- 9
cleandata$PROPDMG <-
        cleandata$PROPDMG * 10 ^ as.numeric(cleandata$PROPDMGEXP)
cleandata$CROPDMG <-
        cleandata$CROPDMG * 10 ^ as.numeric(cleandata$CROPDMGEXP)
TOTECODMG <- cleandata$PROPDMG + cleandata$CROPDMG
cleandata <- cbind(cleandata, TOTECODMG)
fatalities <-
        aggregate(FATALITIES ~ CLEANNAME, data = cleandata, FUN = sum)
fatalities <-
        fatalities[order(fatalities$FATALITIES, decreasing = TRUE), ]
MaxFatalities <- fatalities[1:10, ]
print(MaxFatalities)
injuries <-
        aggregate(INJURIES ~ CLEANNAME, data = cleandata, FUN = sum)
injuries <- injuries[order(injuries$INJURIES, decreasing = TRUE), ]
MaxInjuries <- injuries[1:10, ]
print(MaxInjuries)
par(
        mfrow = c(1, 2),
        mar = c(15, 4, 3, 2),
        mgp = c(3, 1, 0),
        cex = 0.8
)
barplot(
        MaxFatalities$FATALITIES,
        las = 3,
        names.arg = MaxFatalities$CLEANNAME,
        main = "Top 10 Highest Fatalities",
        ylab = "Fatalities(Nos.)",
        col = "RED"
)
barplot(
        MaxInjuries$INJURIES,
        las = 3,
        names.arg = MaxInjuries$CLEANNAME,
        main = "Top 10 Highest Injuries",
        ylab = "Injuries(Nos.)",
        col = "RED"
)
propdmg <-
        aggregate(PROPDMG ~ CLEANNAME, data = cleandata, FUN = sum)
propdmg <- propdmg[order(propdmg$PROPDMG, decreasing = TRUE), ]
propdmgMax <- propdmg[1:10, ]
print(propdmgMax)
cropdmg <-
        aggregate(CROPDMG ~ CLEANNAME, data = cleandata, FUN = sum)
cropdmg <- cropdmg[order(cropdmg$CROPDMG, decreasing = TRUE), ]
cropdmgMax <- cropdmg[1:10, ]
print(cropdmgMax)
ecodmg <-
        aggregate(TOTECODMG ~ CLEANNAME, data = cleandata, FUN = sum)
ecodmg <- ecodmg[order(ecodmg$TOTECODMG, decreasing = TRUE), ]
ecodmgMax <- ecodmg[1:10, ]
print(ecodmgMax)
par(
        mfrow = c(1, 3),
        mar = c(15, 4, 3, 2),
        mgp = c(3, 1, 0),
        cex = 0.8
)
barplot(
        propdmgMax$PROPDMG / (10 ^ 9),
        las = 3,
        names.arg = propdmgMax$CLEANNAME,
        main = "Top 10 Property Damages",
        ylab = "damages ($ billions)",
        col = "RED"
)
barplot(
        cropdmgMax$CROPDMG / (10 ^ 9),
        las = 3,
        names.arg = cropdmgMax$CLEANNAME,
        main = "Top Crop Damages",
        ylab = "damages ($ billions)",
        col = "RED"
)
barplot(
        ecodmgMax$TOTECODMG / (10 ^ 9),
        las = 3,
        names.arg = ecodmgMax$CLEANNAME,
        main = "Top 10 Economic Damages",
        ylab = "damages ($ billions)",
        col = "RED"
)
