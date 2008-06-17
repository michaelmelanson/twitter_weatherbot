{application, weather,
 [
  {description, "A weather notification bot for Twitter"},
  {vsn, "1.0"},
  {modules, [
             envcan_api,
             httpc,
             weather_sup,
             wx,
             wx_sup,
             erlang_twitter
            ]},

  {registered, [
                httpc,
                wx_sup,
                weather_sup
               ]},

  {application, [
                 kernel,
                 stdlib,
                 inets
                ]},

  {included_applications, []},
  {env, []},
  {mod, {weather_app, []}}
 ]}.
