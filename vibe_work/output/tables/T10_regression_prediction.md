**Table T10 - Regression forecasts of the daily total for Wednesday 2 January 2013. CI = 95% confidence interval for the expected value, PI = 95% prediction interval for an individual day.**

| model | scenario | temp_C | hum_pct | wind_kmh | weathersit | fit | CI_low | CI_high | PI_low | PI_high |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| M8 final (with season:temp) | primary: weathersit = Clear, daily mean weather | 2.0 | 47.9 | 14.6 | Clear | 2358.2 | 2036.7 | 2679.7 | 914.3 | 3802.1 |
| M8 final (with season:temp) | sensitivity A: weathersit = Mist (cloudy) | 2.0 | 47.9 | 14.6 | Mist | 1874.8 | 1545.7 | 2203.8 | 429.1 | 3320.4 |
| M8 final (with season:temp) | sensitivity B: 17:00 weather snapshot, weathersit = Clear | 1.7 | 41.5 | 11.1 | Clear | 2567.5 | 2238.7 | 2896.2 | 1121.9 | 4013.0 |
| M2 additive (no interaction) | primary: weathersit = Clear, daily mean weather | 2.0 | 47.9 | 14.6 | Clear | 2758.1 | 2500.3 | 3015.9 | 1139.6 | 4376.6 |
| M2 additive (no interaction) | sensitivity A: weathersit = Mist (cloudy) | 2.0 | 47.9 | 14.6 | Mist | 2307.2 | 2033.7 | 2580.7 | 686.1 | 3928.3 |
| M2 additive (no interaction) | sensitivity B: 17:00 weather snapshot, weathersit = Clear | 1.7 | 41.5 | 11.1 | Clear | 2933.8 | 2666.9 | 3200.8 | 1313.9 | 4553.8 |
