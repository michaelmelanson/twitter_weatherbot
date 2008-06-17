%%%-------------------------------------------------------------------
%%% File    : defines.hrl
%%% Author  : Michael Melanson
%%% Description : Standard definitions
%%%
%%% Created : 2008-06-16 by Michael Melanson
%%%-------------------------------------------------------------------

-record(site, {city, province, language, code}).

-record(sitedata, {license,
                   creationUTC,
                   creationLocal,
                   location,
                   events,
                   currentConditions,
                   forecastGroup,
                   yesterdayConditions,
                   riseSet,
                   almanac}).

-record(datetime, {year, month, day, hour, minute, timeStamp, textSummary}).
-record(location, {continent, country, province, name, region}).
-record(warnings, {}).

-record(events, {watches=[], warnings=[], ended=[]}).
-record(event, {type,
                priority,
                description,
                issueUTC, issueLocal}).

-record(conditions, {station,
                     observationUTC,
                     observationLocal,
                     summary,
                     temperature,
                     dewpoint,
                     humidex,
                     pressure,
                     visibility,
                     relativeHumidity,
                     wind}).
-record(station, {code, lat, lon, name}).
-record(wind, {speed, gust, direction, bearing}).

-record(forecastGroup, {issueUTC,
                        issueLocal,
                        regionalNormals,
                        forecasts=[]}).
                   
-record(temperatures, {summary, high, low, precip}).
                    
-record(forecast, {period,
                   summary,
                   cloudPrecip,
                   abbreviatedForecast,
                   temperatures,
                   winds,
                   precipitation,
                   frost,
                   visibility=[],
                   uv,
                   relativeHumidity,
                   comfort}).
                   
-record(period, {name, day}).
-record(winds, {summary, readings=[]}).
-record(abbreviatedForecast, {summary, pop}).
-record(precipitation, {summary, types=[], accumulation}).
-record(frost, {summary}).
-record(accumulation, {name, amount}).
-record(precipType, {startHour, endHour, type}).
-record(uv, {category, index, summary}).

-record(riseSet, {disclaimer,
                  sunriseUTC,
                  sunriseLocal,
                  sunsetUTC,
                  sunsetLocal}).
                  
-record(almanac, {extremeMax, extremeMin,
                  normalMax, normalMin, normalMean,
                  extremeRainfall, extremeSnowfall, extremePrecipitation,
                  extremeSnowOnGround,
                  pop}).