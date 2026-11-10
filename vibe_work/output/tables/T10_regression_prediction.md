**Table T10 - Regression forecasts of the daily total for Wednesday 2 January 2013. CI = 95% confidence interval for the expected value, PI = 95% prediction interval for an individual day.**

| model | scenario | temp_C | hum_pct | wind_kmh | weathersit | fit | CI_low | CI_high | PI_low | PI_high |
| --- | --- | --- | --- | --- | --- | --- | --- | --- | --- | --- |
| M8 final (with season:temp) | primary: weathersit = Clear, daily mean weather | 2.0 | 47.9 | 14.6 | Clear | 3354.0 | 2943.8 | 3764.2 | 1887.8 | 4820.2 |
| M8 final (with season:temp) | sensitivity A: weathersit = Mist (cloudy) | 2.0 | 47.9 | 14.6 | Mist | 2870.6 | 2440.0 | 3301.1 | 1398.5 | 4342.6 |
| M8 final (with season:temp) | sensitivity B: 17:00 weather snapshot, weathersit = Clear | 1.7 | 41.5 | 11.1 | Clear | 3554.5 | 3134.5 | 3974.6 | 2085.5 | 5023.5 |
| M2 additive (no interaction) | primary: weathersit = Clear, daily mean weather | 2.0 | 47.9 | 14.6 | Clear | 4302.0 | 3991.2 | 4612.7 | 2674.2 | 5929.7 |
| M2 additive (no interaction) | sensitivity A: weathersit = Mist (cloudy) | 2.0 | 47.9 | 14.6 | Mist | 3851.1 | 3523.8 | 4178.3 | 2220.1 | 5482.1 |
| M2 additive (no interaction) | sensitivity B: 17:00 weather snapshot, weathersit = Clear | 1.7 | 41.5 | 11.1 | Clear | 4477.7 | 4159.3 | 4796.1 | 2848.5 | 6106.9 |
