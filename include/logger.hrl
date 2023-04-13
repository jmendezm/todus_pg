%%%-------------------------------------------------------------------
%%% @author jesu
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. mar. 2023 11:40
%%%-------------------------------------------------------------------
-author("jesu").

-define(PRINT(Format, Args), io:format(Format, Args)).

-include_lib("kernel/include/logger.hrl").

-define(DEBUG(Format, Args),
  begin ?LOG_DEBUG(Format, Args), ok end).

-define(INFO_MSG(Format, Args),
  begin ?LOG_INFO(Format, Args), ok end).

-define(WARNING_MSG(Format, Args),
  begin ?LOG_WARNING(Format, Args), ok end).

-define(ERROR_MSG(Format, Args),
  begin ?LOG_ERROR(Format, Args), ok end).

-define(CRITICAL_MSG(Format, Args),
  begin ?LOG_CRITICAL(Format, Args), ok end).
