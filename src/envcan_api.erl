%%%-------------------------------------------------------------------
%%% File    : envcan_api.erl
%%% Author  : Michael Melanson
%%% Description : API for the Environment Canada weather database
%%%
%%% Created : 2008-06-16 by Michael Melanson
%%%-------------------------------------------------------------------
-module(envcan_api).

%% API
-export([get/2, list/0]).

-include("envcan_api.hrl").
-include("xmerl.hrl").

-define(BASE_URL, "http://dd.weatheroffice.ec.gc.ca/EC_sites/xml").
-define(HEADERS, [{"User-Agent", "Weatherbot"},
                  {"X-Contact-Info", "Michael Melanson michael@codeshack.ca"}]).

%%====================================================================
%% API
%%====================================================================
%%--------------------------------------------------------------------
%% Function: get(Site, ETag) -> {ok, SiteData, ETag} | unmodified
%% Description: Gets the weather data for the specified site
%%--------------------------------------------------------------------
get(Site, ETag) ->
    Request = case ETag of
        undefined ->  {make_data_uri(Site), ?HEADERS};
        _ -> {make_data_uri(Site), ?HEADERS ++ [{"If-None-Match", ETag}]}
    end,
    
    {ok, Result} = httpc:request(get, Request),
    
    case Result of
        {{_, 200, _}, Headers, Body} ->
            {Parsed, []} = xmerl_scan:string(Body),

            Content = Parsed#xmlElement.content,
            SiteData = parse_sitedata(Content),
            {ok, SiteData, get_etag(Headers)};
        
        {{_, 304, _}, _Headers, _Body} ->
            unmodified;
            
        Other ->
            io:format("Unknown HTTP return ~p calling ~p~n", [Other, Site]),
            {error, Other}
    end.
    
list() ->
    Request = {?BASE_URL ++ "/siteList.xml", []},
    {ok, Result} = httpc:request(get, Request),
    case Result of
        {{_, 200, _}, _Headers, Body} ->
            {Parsed, []} = xmerl_scan:string(Body),
            Content = Parsed#xmlElement.content,
            {ok, parse_sitelist(Content)}
    end.

%%====================================================================
%% Internal functions
%%====================================================================
    
parse_sitelist(Xml) ->
    F = fun
        (#xmlElement{name=site,
                     attributes=[#xmlAttribute{name=code, value=Code}],
                     content=SiteXml}, Acc) ->
            {EnSite, FrSite} = parse_site(SiteXml),
            Acc ++ [EnSite#site{code=Code}, FrSite#site{code=Code}];
        (#xmlText{}, Acc) -> Acc
    end,
    
    lists:foldl(F, [], Xml).
            
parse_site(Xml) ->
    F = fun
        (#xmlElement{name=nameEn, content=[#xmlText{value=Content}]}, {EnAcc, FrAcc}) ->
            {EnAcc#site{city=Content}, FrAcc};
        
        (#xmlElement{name=nameFr, content=[#xmlText{value=Content}]}, {EnAcc, FrAcc}) ->
            {EnAcc, FrAcc#site{city=Content}};
        
        (#xmlElement{name=provinceCode, content=[#xmlText{value=Content}]}, {EnAcc, FrAcc}) ->
            {EnAcc#site{province=Content},
             FrAcc#site{province=Content}};
             
        (#xmlText{}, Acc) -> Acc
    end,
    
    lists:foldl(F, {#site{language=english}, #site{language=french}}, Xml).
        
    
parse_sitedata(Xml) ->
    F = fun
        (#xmlElement{name=license,
                     content=[#xmlText{value=License}]}, Acc) ->
            Acc#sitedata{license=License};
            
        (#xmlElement{name=dateTime,
                     attributes=[_,#xmlAttribute{name=zone, value="UTC"},_],
                     content=Date}, Acc) ->
            Acc#sitedata{creationUTC=parse_datetime(Date)};

        (#xmlElement{name=dateTime,
                     attributes=[_,#xmlAttribute{name=zone, value=Zone},_],
                     content=Date}, Acc) when Zone =/= "UTC" ->
            Acc#sitedata{creationLocal=parse_datetime(Date)};
             
        (#xmlElement{name=location,
                     content=Location}, Acc) ->
            Acc#sitedata{location=parse_location(Location)};
            
        (#xmlElement{name=warnings,
                     content=Warnings}, Acc) ->
            Acc#sitedata{events=parse_events(Warnings)};
            
        (#xmlElement{name=currentConditions,
                     content=Conditions}, Acc) ->
            Acc#sitedata{currentConditions=parse_conditions(Conditions)};
            
        (#xmlElement{name=forecastGroup, content=ForecastGroup}, Acc) ->
            Acc#sitedata{forecastGroup=parse_forecastGroup(ForecastGroup)};
                    
        (#xmlElement{name=yesterdayConditions, content=Content}, Acc) ->
            Acc#sitedata{yesterdayConditions=parse_temperatures(Content)};
            
        (#xmlElement{name=riseSet, content=Content}, Acc) ->
            Acc#sitedata{riseSet=parse_riseSet(Content)};    
        
        (#xmlElement{name=almanac, content=Content}, Acc) ->
            Acc#sitedata{almanac=parse_almanac(Content)};
         
         
        (#xmlElement{name=Name}, Acc) ->
            io:format("Ignoring ~p element~n", [Name]),
            Acc;
            
        (#xmlText{}, Acc) -> Acc
        end,
            
    lists:foldl(F, #sitedata{}, Xml).
    
    
parse_datetime(Xml) ->
    F = fun
        (#xmlElement{name=year, content=[#xmlText{value=Year}]}, Acc) ->
            Acc#datetime{year=Year};
        
        (#xmlElement{name=month, content=[#xmlText{value=Month}]}, Acc) ->
            Acc#datetime{month=Month};
    
        (#xmlElement{name=day, content=[#xmlText{value=Day}]}, Acc) ->
            Acc#datetime{day=Day};
            
        (#xmlElement{name=hour, content=[#xmlText{value=Hour}]}, Acc) ->
            Acc#datetime{hour=Hour};

        (#xmlElement{name=minute, content=[#xmlText{value=Minute}]}, Acc) ->
            Acc#datetime{minute=Minute};

        (#xmlElement{name=timeStamp,
                     content=[#xmlText{value=TimeStamp}]}, Acc) ->
            Acc#datetime{timeStamp=TimeStamp};

        (#xmlElement{name=textSummary,
                     content=[#xmlText{value=TextSummary}]}, Acc) ->
            Acc#datetime{textSummary=TextSummary};
            
        (#xmlText{}, Acc) -> Acc
        end,
    lists:foldl(F, #datetime{}, Xml).
    
parse_location(Xml) ->
    F = fun
        (#xmlElement{name=continent, content=[#xmlText{value=Value}]}, Acc) ->
            Acc#location{continent=Value};

        (#xmlElement{name=country, content=[#xmlText{value=Value}]}, Acc) ->
            Acc#location{country=Value};
                
        (#xmlElement{name=province, content=Content}, Acc) ->
            case Content of
                [] -> Acc;
                [#xmlText{value=Value}] -> Acc#location{province=Value}
            end;

        (#xmlElement{name=name, content=Content}, Acc) ->
            case Content of
                [] -> Acc;
                [#xmlText{value=Value}] -> Acc#location{name=Value}
            end;

        (#xmlElement{name=region, content=Content}, Acc) ->
            case Content of
                [] -> Acc;
                [#xmlText{value=Value}] -> Acc#location{region=Value}
            end;
            
        (#xmlText{}, Acc) -> Acc
    end,
    
    lists:foldl(F, #location{}, Xml).
    
parse_events(Xml) ->
    F = fun
        (E, Acc) when E#xmlElement.name =:= event ->
            case parse_event(E) of
                {"watch", Value} ->
                    Acc#events{watches=Acc#events.watches ++ [Value]};
                {"warning", Value} ->
                    Acc#events{warnings=Acc#events.warnings ++ [Value]};
                {"ended", Value} ->
                    Acc#events{ended=Acc#events.ended ++ [Value]}
            end;

        (#xmlText{}, Acc) -> Acc
    end,
    
    lists:foldl(F, #events{}, Xml).
    
parse_event(#xmlElement{name=event, attributes=Attributes, content=Content}) ->
    AttParser = fun
                (#xmlAttribute{name=type, value=Value}, Acc) ->
                    Acc#event{type=Value};
                (#xmlAttribute{name=priority, value=Value}, Acc) ->
                    Acc#event{priority=Value};
                (#xmlAttribute{name=description, value=Value}, Acc) ->
                    Acc#event{description=Value}
    end,
    
    ChildParser = fun
                  (#xmlElement{name=dateTime,
                               attributes=[
                                   #xmlAttribute{name=name, value="eventIssue"},
                                   #xmlAttribute{name=zone, value="UTC"}, _
                               ],
                               content=Date}, Acc) ->
                      Acc#event{issueUTC=parse_datetime(Date)};
                      
                  (#xmlElement{name=dateTime,
                                 attributes=[
                                     #xmlAttribute{name=name, value="eventIssue"},
                                     #xmlAttribute{name=zone}, _
                                 ],
                                 content=Date}, Acc) ->
                      Acc#event{issueLocal=parse_datetime(Date)};
            
                  (#xmlText{}, Acc) -> Acc
    end,
    
    Event1 = lists:foldl(AttParser, #event{}, Attributes),
    Event = lists:foldl(ChildParser, Event1, Content),
    
    {Event#event.type, Event}.
              
    
parse_conditions(Xml) ->
    F = fun
        (#xmlElement{name=station, attributes=Attrs, content=Content}, Acc) ->
            Acc#conditions{station=parse_station(Attrs, Content)};
        
        (#xmlElement{name=dateTime,
                     attributes=[#xmlAttribute{name=name, value="observation"},
                                 #xmlAttribute{name=zone, value="UTC"},
                                 _],
                     content=Content}, Acc) ->
            Acc#conditions{observationUTC=parse_datetime(Content)};
        
        (#xmlElement{name=dateTime,
                     attributes=[#xmlAttribute{name=name, value="observation"},
                                 #xmlAttribute{name=zone},
                                 _],
                     content=Content}, Acc) ->
            Acc#conditions{observationLocal=parse_datetime(Content)};
        
        (#xmlElement{name=condition, content=Content}, Acc) ->
            case Content of
                [] -> Acc;
                [#xmlText{value=Value}] -> Acc#conditions{summary=Value}
            end;
        
        (#xmlElement{name=temperature, content=[#xmlText{value=Value}]}, Acc) ->
            Acc#conditions{temperature=Value};
            
        (#xmlElement{name=dewpoint, content=[#xmlText{value=Value}]}, Acc) ->
            Acc#conditions{dewpoint=Value};

        (#xmlElement{name=humidex, content=[#xmlText{value=Value}]}, Acc) ->
            Acc#conditions{humidex=Value};

        (#xmlElement{name=pressure, content=Content}, Acc) ->
            case Content of
                [] -> Acc;
                [#xmlText{value=Value}] -> Acc#conditions{pressure=Value}
            end;

        (#xmlElement{name=visibility, content=Content}, Acc) ->
            case Content of
                [] -> Acc;
                [#xmlText{value=Value}] -> Acc#conditions{visibility=Value}
            end;

        (#xmlElement{name=relativeHumidity, content=Content}, Acc) ->
            case Content of
                [] -> Acc;
                [#xmlText{value=Value}] -> Acc#conditions{relativeHumidity=Value}
            end;

        (#xmlElement{name=wind, content=Value}, Acc) ->
            Acc#conditions{wind=parse_wind(Value)};
            
        (#xmlText{}, Acc) -> Acc
    end,
    
    lists:foldl(F, #conditions{}, Xml).

parse_station([#xmlAttribute{name=code, value=Code},
               #xmlAttribute{name=lat, value=Lat},
               #xmlAttribute{name=lon, value=Lon}],
              Contents) ->
    Name = case Contents of
               [] -> "";
               [#xmlText{value=Value}] -> Value
           end,
           
    #station{code=Code,
             lat=Lat,
             lon=Lon,
             name=Name}.
             
parse_wind(Xml) ->
    F = fun
        (#xmlElement{name=speed, content=Content}, Acc) ->
            case Content of
                [] -> Acc;
                [#xmlText{value=Value}] -> Acc#wind{speed=Value}
            end;
            
        (#xmlElement{name=gust, content=Content}, Acc) ->
            case Content of
                [] -> Acc;
                [#xmlText{value=Value}] -> Acc#wind{gust=Value}
            end;
            
        (#xmlElement{name=direction, content=Content}, Acc) ->
            case Content of
                [] -> Acc;
                [#xmlText{value=Value}] -> Acc#wind{direction=Value}
            end;
            
        (#xmlElement{name=bearing, content=Content}, Acc) ->
            case Content of
                [] -> Acc;
                [#xmlText{value=Value}] -> Acc#wind{bearing=Value}
            end;
            
        (#xmlText{}, Acc) -> Acc
    end,
    
    lists:foldl(F, #wind{}, Xml).
    
parse_forecastGroup(Xml) ->
    F = fun
        (#xmlElement{name=dateTime, attributes=[
                                        #xmlAttribute{name=name, value="forecastIssue"},
                                        #xmlAttribute{name=zone, value="UTC"}, _
                                    ],
                                    content=Content}, Acc) ->
            Acc#forecastGroup{issueUTC=parse_datetime(Content)};
        
        (#xmlElement{name=dateTime, attributes=[
                                        #xmlAttribute{name=name, value="forecastIssue"},
                                        #xmlAttribute{name=zone}, _
                                    ],
                                    content=Content}, Acc) ->
            Acc#forecastGroup{issueLocal=parse_datetime(Content)};
        
        (#xmlElement{name=regionalNormals, content=Content}, Acc) ->
            Acc#forecastGroup{regionalNormals=parse_temperatures(Content)};
            
        (#xmlElement{name=forecast, content=Content}, Acc) ->
            NewForecasts = Acc#forecastGroup.forecasts++[parse_forecast(Content)],
            Acc#forecastGroup{forecasts=NewForecasts};
            
        (#xmlText{}, Acc) -> Acc
    end,
    
    lists:foldl(F, #forecastGroup{}, Xml).
    
parse_temperatures(Xml) ->
    F = fun
        (#xmlElement{name=textSummary, content=Content}, Acc) ->
            case Content of
                [] -> Acc;
                [#xmlText{value=Value}] -> Acc#temperatures{summary=Value}
            end;
            
        (#xmlElement{name=temperature,
                     attributes=[_, _, #xmlAttribute{name=class, value=Class}],
                     content=Content}, Acc) ->
            case Content of
                [] -> Acc;
                [#xmlText{value=Value}] ->
                    case Class of
                        "high" -> Acc#temperatures{high=Value};
                        "low" -> Acc#temperatures{low=Value}
                    end
            end;
                    
        (#xmlElement{name=precip,
                     content=Content}, Acc) ->
            case Content of
                [] -> Acc;
                [#xmlText{value=Value}] -> Acc#temperatures{precip=Value}
            end;
            
        (#xmlText{}, Acc) -> Acc
    end,
    
    lists:foldl(F, #temperatures{}, Xml).
    
parse_forecast(Xml) ->
    F = fun
        (#xmlElement{name=period,
                     attributes=[#xmlAttribute{name=textForecastName,
                                               value=Name}],
                     content=[#xmlText{value=Day}]}, Acc) ->
            Acc#forecast{period=#period{name=Name, day=Day}};
            
        (#xmlElement{name=textSummary,
                     content=[#xmlText{value=Value}]}, Acc) ->
            Acc#forecast{summary=Value};
            
        (#xmlElement{name=cloudPrecip,
                     content=Content}, Acc) ->
            Acc#forecast{cloudPrecip=parse_cloudPrecip(Content)};
             
        (#xmlElement{name=abbreviatedForecast, content=Content}, Acc) ->
            Acc#forecast{abbreviatedForecast=parse_abbreviatedForecast(Content)};
            
        (#xmlElement{name=temperatures, content=Content}, Acc) ->
            Acc#forecast{temperatures=parse_temperatures(Content)};
            
        (#xmlElement{name=winds, content=Content}, Acc) ->
            Acc#forecast{winds=parse_winds(Content)};
            
        (#xmlElement{name=precipitation, content=Content}, Acc) ->
            Acc#forecast{precipitation=parse_precipitation(Content)};
            
        (#xmlElement{name=frost, content=Content}, Acc) ->
            Acc#forecast{frost=parse_frost(Content)};
            
        (#xmlElement{name=relativeHumidity, content=[#xmlText{value=Value}]}, Acc) ->
            Acc#forecast{relativeHumidity=Value};     
            
        (#xmlElement{name=uv,
                     attributes=[#xmlAttribute{name=category, value=Category}],
                     content=Content}, Acc) ->
            Parsed = parse_uv(Content),
            Uv = Parsed#uv{category=Category},
            Acc#forecast{uv=Uv};      
            
        (#xmlElement{name=visibility, content=Content}, Acc) ->
            Acc#forecast{visibility=parse_visibility(Content)};
            
        (#xmlElement{name=comfort, content=Content}, Acc) ->
            Acc#forecast{comfort=parse_comfort(Content)}; 
            
        (#xmlText{}, Acc) -> Acc
    end,
    
    lists:foldl(F, #forecast{}, Xml).
            
            
parse_cloudPrecip(Xml) ->
    F = fun
        (#xmlElement{name=textSummary, content=[#xmlText{value=Value}]}, _Acc) ->
            Value;
            
        (#xmlText{}, Acc) -> Acc
    end,
    
    lists:foldl(F, "", Xml).
    
parse_winds(Xml) ->
    F = fun
        (#xmlElement{name=textSummary, content=[#xmlText{value=Value}]}, Acc) ->
            Acc#winds{summary=Value};
            
        (#xmlElement{name=wind, content=Content}, Acc) ->
            Acc#winds{readings=Acc#winds.readings ++ [parse_wind(Content)]};
            
        (#xmlText{}, Acc) -> Acc
    end,
    
    lists:foldl(F, #winds{}, Xml).
    
parse_abbreviatedForecast(Xml) ->
    F = fun
        (#xmlElement{name=textSummary, content=[#xmlText{value=Value}]}, Acc) ->
            Acc#abbreviatedForecast{summary=Value};
            
        (#xmlElement{name=pop, content=Content}, Acc) ->
            case Content of
                [] -> Acc;
                [#xmlText{value=Value}] -> Acc#abbreviatedForecast{pop=Value}
            end;
            
        (#xmlText{}, Acc) -> Acc
    end,
    
    lists:foldl(F, #abbreviatedForecast{}, Xml).
    
parse_precipitation(Xml) ->
    F = fun
        (#xmlElement{name=textSummary, content=Content}, Acc) ->
            case Content of
                [] -> Acc#precipitation{summary=""};
                [#xmlText{value=Value}] -> Acc#precipitation{summary=Value}
            end;
            
        (#xmlElement{name=precipType,
                     attributes=[
                        #xmlAttribute{name=start, value=Start},
                        #xmlAttribute{name='end', value=End}
                     ],
                     content=Content}, Acc) ->
            case Content of
                [] -> Acc;
                [#xmlText{value=Value}] ->
                    NewTypes = Acc#precipitation.types ++ [#precipType{startHour=Start, endHour=End, type=Value}],
                    Acc#precipitation{types=NewTypes}
            end;

        (#xmlElement{name=accumulation, content=Content}, Acc) ->
            Acc#precipitation{accumulation=parse_accumulation(Content)};
            
        (#xmlText{}, Acc) -> Acc
    end,
    
    lists:foldl(F, #precipitation{}, Xml).
    

parse_frost(Xml) ->
    F = fun
        (#xmlElement{name=textSummary, content=Content}, Acc) ->
            case Content of
                [] -> Acc;
                [#xmlText{value=Value}] -> Acc#frost{summary=Value}
            end;
            
        (#xmlText{}, Acc) -> Acc
    end,
    
    lists:foldl(F, #frost{}, Xml).
    
parse_uv(Xml) ->
    F = fun
        (#xmlElement{name=index, content=[#xmlText{value=Value}]}, Acc) ->
            Acc#uv{index=Value};
            
        (#xmlElement{name=textSummary, content=[#xmlText{value=Value}]}, Acc) ->
            Acc#uv{summary=Value};
            
        (#xmlText{}, Acc) -> Acc
    end,
    
    lists:foldl(F, #uv{}, Xml).
    
parse_visibility(Xml) ->
    F = fun
        (#xmlElement{name=otherVisib, content=Content}, Acc) ->
            case Content of
                [] -> Acc;
                [#xmlText{},#xmlElement{name=textSummary, content=[#xmlText{value=Value}]}|_Tail] ->
                    Acc ++ [Value]
            end;
        (#xmlText{}, Acc) -> Acc
    end,
    
    lists:foldl(F, [], Xml).

parse_comfort(Xml) ->
    F = fun
        (#xmlElement{name=textSummary, content=[#xmlText{value=Value}]}, _Acc) ->
            Value;
            
        (#xmlText{}, Acc) -> Acc
    end,
    
    lists:foldl(F, undefined, Xml).
    
parse_accumulation(Xml) ->
    F = fun
        (#xmlElement{name=name, content=[#xmlText{value=Value}]}, Acc) ->
            Acc#accumulation{name=Value};
            
        (#xmlElement{name=amount, content=[#xmlText{value=Value}]}, Acc) ->
            Acc#accumulation{amount=Value};
            
        (#xmlText{}, Acc) -> Acc
    end,
    
    lists:foldl(F, #accumulation{}, Xml).
    
parse_riseSet(Xml) ->
    F = fun
        (#xmlElement{name=disclaimer, content=[#xmlText{value=Value}]}, Acc) ->
            Acc#riseSet{disclaimer=Value};
            
        (#xmlElement{name=dateTime,
                     attributes=[
                        #xmlAttribute{name=name, value="sunrise"},
                        #xmlAttribute{name=zone, value="UTC"}, _
                     ],
                     content=Content}, Acc) ->
            Acc#riseSet{sunriseUTC=parse_datetime(Content)};
            
        (#xmlElement{name=dateTime,
                     attributes=[
                        #xmlAttribute{name=name, value="sunrise"},
                        #xmlAttribute{name=zone}, _
                     ],
                     content=Content}, Acc) ->
            Acc#riseSet{sunriseLocal=parse_datetime(Content)};
    
        (#xmlElement{name=dateTime,
                     attributes=[
                        #xmlAttribute{name=name, value="sunset"},
                        #xmlAttribute{name=zone, value="UTC"}, _
                     ],
                     content=Content}, Acc) ->
            Acc#riseSet{sunsetUTC=parse_datetime(Content)};

        (#xmlElement{name=dateTime,
                     attributes=[
                        #xmlAttribute{name=name, value="sunset"},
                        #xmlAttribute{name=zone}, _
                     ],
                     content=Content}, Acc) ->
            Acc#riseSet{sunsetLocal=parse_datetime(Content)};
    
        (#xmlText{}, Acc) -> Acc
    end,
    
    lists:foldl(F, #riseSet{}, Xml).
    
parse_almanac(Xml) ->
    F = fun
        (#xmlElement{name=temperature,
                     attributes=[#xmlAttribute{name=class, value="extremeMax"}|_Tail],
                     content=Content}, Acc) ->
             case Content of
                 [] -> Acc;
                 [#xmlText{value=Value}] -> Acc#almanac{extremeMax=Value}
             end;
            
        (#xmlElement{name=temperature,
                     attributes=[#xmlAttribute{name=class, value="extremeMin"}|_Tail],
                     content=Content}, Acc) ->
            case Content of
                [] -> Acc;
                [#xmlText{value=Value}] -> Acc#almanac{extremeMin=Value}
            end;

        (#xmlElement{name=temperature,
                     attributes=[#xmlAttribute{name=class, value="normalMax"}|_Tail],
                     content=Content}, Acc) ->
            case Content of
                [] -> Acc;
                [#xmlText{value=Value}] -> Acc#almanac{normalMax=Value}
            end;

        (#xmlElement{name=temperature,
                     attributes=[#xmlAttribute{name=class, value="normalMin"}|_Tail],
                     content=Content}, Acc) ->
             case Content of
                 [] -> Acc;
                 [#xmlText{value=Value}] -> Acc#almanac{normalMin=Value}
             end;
             
        (#xmlElement{name=temperature,
                     attributes=[#xmlAttribute{name=class, value="normalMean"}|_Tail],
                     content=Content}, Acc) ->
             case Content of
                 [] -> Acc;
                 [#xmlText{value=Value}] -> Acc#almanac{normalMean=Value}
             end;

        (#xmlElement{name=precipitation,
                     attributes=[#xmlAttribute{name=class, value="extremeRainfall"}|_Tail],
                     content=Content}, Acc) ->
             case Content of
                 [] -> Acc;
                 [#xmlText{value=Value}] -> Acc#almanac{extremeRainfall=Value}
             end;

        (#xmlElement{name=precipitation,
                     attributes=[#xmlAttribute{name=class, value="extremeSnowfall"}|_Tail],
                     content=Content}, Acc) ->
             case Content of
                 [] -> Acc;
                 [#xmlText{value=Value}] -> Acc#almanac{extremeSnowfall=Value}
             end;

        (#xmlElement{name=precipitation,
                     attributes=[#xmlAttribute{name=class, value="extremePrecipitation"}|_Tail],
                     content=Content}, Acc) ->
             case Content of
                 [] -> Acc;
                 [#xmlText{value=Value}] -> Acc#almanac{extremePrecipitation=Value}
             end;

        (#xmlElement{name=precipitation,
                     attributes=[#xmlAttribute{name=class, value="extremeSnowOnGround"}|_Tail],
                     content=Content}, Acc) ->
             case Content of
                 [] -> Acc;
                 [#xmlText{value=Value}] -> Acc#almanac{extremeSnowOnGround=Value}
             end;

         (#xmlElement{name=pop, content=Content}, Acc) ->
              case Content of
                  [] -> Acc;
                  [#xmlText{value=Value}] -> Acc#almanac{pop=Value}
              end;


        (#xmlText{}, Acc) -> Acc
    end,
    
    lists:foldl(F, #almanac{}, Xml).


    

make_data_uri(Site) ->
    Prov = Site#site.province,
    Code = Site#site.code,
    
    Lang = case Site#site.language of
               english -> "e";
               french  -> "f"
           end,
           
    Path = "/" ++ Prov ++ "/" ++ Code ++ "_" ++ Lang ++ ".xml",
    ?BASE_URL ++ Path.
    
get_etag([{"etag", ETag}|_Tail]) -> ETag;
get_etag([_|Tail]) -> get_etag(Tail);
get_etag([]) -> undefined.
