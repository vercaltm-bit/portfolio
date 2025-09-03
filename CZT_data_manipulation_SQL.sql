/*
Czech District heating system Data Exploration 

Skills used: Joins, Aggregate Functions, Creating Views, Converting Data Types, Case function, Temp table

*/

#Deleting rows with multiple strings in the Kraj column
SELECT * 
FROM czt.czt_fuel_upraveno1
WHERE LOCATE(' ', `Kraj`) = 0
order by 1 desc, 4 desc
;


#The use of biomass and other renewables as a fuel in district heating systems in the Ústí nad Labem region in Czech republic
SELECT Kraj, Biomasa_OZE 
FROM czt.czt_fuel_upraveno1
WHERE Kraj = "U" and Biomasa_OZE > 0
;

#Average percentage share of renewable sources in heating plants in the Ústí nad Labem Region
SELECT Kraj, AVG(Biomasa_OZE) AS Average_renewables
FROM czt.czt_fuel_upraveno1
WHERE Kraj = "U" 
;

#Number of heating plants in the Ústí nad Labem Region
SELECT COUNT(*) 
FROM czt.czt_fuel_upraveno1
WHERE Kraj = "U"
;

#Maximum share of renewable sources in heating plants with a capacity exceeding 100 MW, broken down by region
SELECT Kraj, MAX(Biomasa_OZE) AS max_renewables
FROM czt.czt_fuel_upraveno1
WHERE Instalovany_vykon_MW > 100
GROUP BY Kraj
;


#Joining tables
CREATE TABLE fuel AS
SELECT *
FROM czt.czt_fuel_upraveno1
JOIN czt.kraje_info_csv
	ON Kraj = Kraj_ID
WHERE Kraj is not null
ORDER BY Kraj_ID
;

#Calculation of total power in MW in individual regions  
SELECT
  Kraj_nazvy,
  Pocet_obyvatel,
  SUM(`Instalovany_vykon_MW`) AS Celkovy_vykon_MW
FROM
  fuel
GROUP BY
  Kraj_nazvy, Pocet_obyvatel
ORDER BY
  Kraj_nazvy;

#Calculation of power in MW per capita in individual regions 
#Creating View to store data for later visualizations

Create View `Power_per_capita` as 
WITH AgregovanyVykon AS (
  SELECT
    Kraj_nazvy,
    Pocet_obyvatel,
    SUM(`Instalovany_vykon_MW`) AS Celkovy_vykon_MW
  FROM
    fuel
  GROUP BY
    Kraj_nazvy,
    Pocet_obyvatel
)
SELECT
  Kraj_nazvy,
  Pocet_obyvatel,
  ROUND(Celkovy_vykon_MW, 2) AS Celkovy_vykon_MW,
  ROUND((Celkovy_vykon_MW / Pocet_obyvatel), 2) AS Vykon_MW_na_obyvatele
FROM
  AgregovanyVykon
ORDER BY
  Kraj_nazvy;
  
SELECT * FROM `Power_per_capita`;

#Using Case statement to distinguish high, medium and low performing systems

SELECT Kraj_nazvy, Celkovy_vykon_MW,
CASE
	WHEN Celkovy_vykon_MW < 1000000 THEN "Low"
    WHEN Celkovy_vykon_MW BETWEEN 1000000 and 1500000 THEN "Medium"
    WHEN Celkovy_vykon_MW > 1500000 THEN "High"
END AS Vykon_hodnoceni
FROM Power_per_capita;

#Using Temp Table to select regions with population ower 1 M

CREATE TEMPORARY TABLE Pocet_obyvatel_over1M
SELECT *
FROM Power_per_capita
WHERE Pocet_obyvatel > 1000000
;

SELECT *
FROM Pocet_obyvatel_over1M
;



