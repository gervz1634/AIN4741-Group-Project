**Table T01 - Variables in day.csv and their role in the analysis**

| variable | description | type | role |
| --- | --- | --- | --- |
| instant | Record index (1..731) | ID | Dropped - identifies the row only |
| dteday | Date (2011-01-01 .. 2012-12-31) | date | Used for the time-series analysis only |
| season | Season: 1 spring, 2 summer, 3 fall, 4 winter | categorical | Predictor |
| yr | Year: 0 = 2011, 1 = 2012 | categorical | Predictor (trend / level shift) |
| mnth | Month 1..12 | categorical | Predictor (collinear with season) |
| holiday | Day is a US federal holiday (1) or not (0) | binary | Predictor |
| weekday | Day of week, 0 = Sunday .. 6 = Saturday | categorical | Predictor |
| workingday | 1 if neither weekend nor holiday, else 0 | binary | Predictor (exact function of weekday x holiday) |
| weathersit | 1 clear/partly cloudy, 2 mist/cloudy, 3 light rain or snow, 4 heavy rain/snow/fog | ordinal | Predictor |
| temp | Normalised temperature, Celsius divided by 41 | continuous | Predictor |
| atemp | Normalised feeling temperature, Celsius divided by 50 | continuous | Predictor (collinear with temp, r > 0.99) |
| hum | Normalised humidity, divided by 100 | continuous | Predictor |
| windspeed | Normalised wind speed, divided by 67 | continuous | Predictor |
| casual | Count of casual users | count | Dropped - part of the target (leakage) |
| registered | Count of registered users | count | Dropped - part of the target (leakage) |
| cnt | Total rentals = casual + registered  (TARGET) | count | Response variable |
