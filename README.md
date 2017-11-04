# KershawPitchPrediction


Deep Learning project for predicting pitches from Clayton Kershaw. This is in progress and in the data cleaning process. Will post codes for correlation visualizations prior to deep learning implementation in keras for R. I am pulling data directly from the Stattleship API and baseball savant, but will not be posted here.

Some of the factors I'm looking at as possibly siginificant for predicting pitch type and location: current batter stats, next two batters' stats, type/location of last pitch, number of times through the order, batter hot zones, men on base, score (Dodgers ahead/behind/tied), pitch count, batter handedness. In the first two months of the season, I use player stats from the previous year (both batter and next 2 batters), otherwise I use the current year data. If no data are available from previous season, I use only current year data.
