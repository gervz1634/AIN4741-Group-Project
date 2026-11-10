**Table T05b - Variance inflation factors of the final model. The large values on the season and temp x season terms are mechanical: interaction terms are by construction correlated with their own components. Centring reduces the max VIF from 43027 to 49; the fit is unchanged either way.**

| Term | VIF | VIF_CI_low | VIF_CI_high | Tolerance |
| --- | --- | --- | --- | --- |
| season | 41.446 | 35.202 | 48.831 | 0.024 |
| yr | 1.365 | 1.246 | 1.540 | 0.733 |
| holiday | 1.120 | 1.051 | 1.282 | 0.893 |
| weekday | 1.183 | 1.098 | 1.339 | 0.846 |
| weathersit | 1.882 | 1.681 | 2.144 | 0.531 |
| temp_c | 9.486 | 8.120 | 11.113 | 0.105 |
| hum | 2.040 | 1.814 | 2.329 | 0.490 |
| windspeed | 1.222 | 1.129 | 1.380 | 0.819 |
| season:temp_c | 37.236 | 31.634 | 43.862 | 0.027 |
