**Table T00 - The 17 variables of hour.csv and their role in this study**

| variable | description | type | role |
| --- | --- | --- | --- |
| datetime | date + local clock hour, 2011-01-01 00:00 .. 2012-12-31 23:00 | hourly index | time index |
| cnt | total rentals in that hour (TARGET), 1 .. 977 | count | response |
| casual | rentals by occasional users in that hour | count | descriptive only (part of the target) |
| registered | rentals by registered users in that hour | count | descriptive only (part of the target) |
| season | astronomical quarter: 1 = 21 Dec-20 Mar (winter), 2 = 21 Mar-20 Jun (spring), 3 = 21 Jun-20 Sep (summer), 4 = 21 Sep-20 Dec (autumn) -- see the note in Section 1.2 | categorical | descriptive only |
| yr | 0 = 2011, 1 = 2012 | binary | descriptive |
| mnth | month 1..12 | categorical | descriptive |
| hr | hour of the day 0..23 | cyclical | daily seasonality |
| holiday | federal holiday (0/1) | binary | candidate covariate |
| weekday | day of week 0 = Sunday .. 6 = Saturday | categorical | weekly seasonality |
| workingday | 1 if neither weekend nor holiday | binary | profile split (exploratory) |
| weathersit | 1 clear .. 4 heavy rain/snow/fog (level 4 never occurs) | ordinal | descriptive |
| temp | normalised temperature (degC / 41) | continuous | descriptive |
| atemp | normalised feeling temperature (degC / 50) | continuous | descriptive |
| hum | normalised humidity (/100) | continuous | descriptive |
| windspeed | normalised wind speed (/67) | continuous | descriptive |
| filled | 1 if the hour had to be interpolated (165 hours), 0 if observed | binary | data-quality flag |
