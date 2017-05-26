SELECT Variety, AVG(Yield) as Yield, Temperature, Precipitation, `Solar Radiation`, CEC, `Organic matter`, pH, Clay, Silt, Sand FROM `Experiment` GROUP BY Variety, Temperature, Precipitation, `Solar Radiation`, CEC, `Organic matter`, pH, Clay, Silt, Sand ORDER by Variety ;

SELECT distinct Variety from `Experiment` ORDER by Variety;