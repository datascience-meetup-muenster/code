## DataDive 2016

### Solution by team "Panic Room Waterbears"

#### Members (in no particular order)
https://inclass.kaggle.com/thie1e

https://inclass.kaggle.com/olafmen

https://inclass.kaggle.com/lihaorocky

#### Files
`1_exploration_and_dataprep.R`, as the name suggests, should be run fist. It includes some code we used for exploring the data and our feature engineering. In addition to the supplied data we used some additional weather data (in MUNOSNA_DWD.csv) - which didn't seem to make much of a difference.

#### Model
Our final model was the xgboost model in `2_xgb2.R` which scored 273 in local CV, 260 on the public leaderboard, and 276.6 on the private leaderboard.

