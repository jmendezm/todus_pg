%%%-------------------------------------------------------------------
%%% @author jesu
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. abr. 2023 22:07
%%%-------------------------------------------------------------------
-module(todus_pg_logger).
-author("jesu").

%% API
-export([start/0]).

start() ->
  LogLevel = application:get_env(todus_pg, log_level, warning),
  logger:set_primary_config(level, LogLevel),

  FormatterConfig = #{legacy_header => false,
    time_designator => $ , max_size => 100*1024, single_line => false,
    template => file_template()
  },

  Config = #{ file => "./log/todus_pg.log",
              formatter => {logger_formatter, FormatterConfig},
              level => LogLevel
  },

  logger:update_formatter_config(default, FormatterConfig),
  logger:add_handler(todus_pg_log, logger_std_h,
    #{level => all, config => Config}).

console_template() ->
  [time, " [", level, "] " | msg()].

file_template() ->
  [time, " [", level, "] ", pid,
    {mfa, ["@", mfa, {line, [":", line], []}], []}, " " | msg()].

msg() ->
  [{logger_formatter, [[logger_formatter, title], ":", io_lib:nl()], []},
    msg, io_lib:nl()].