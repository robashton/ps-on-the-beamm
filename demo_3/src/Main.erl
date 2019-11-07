-module(main@foreign).

-export([
         base64/1
        ]).

base64(String) ->
  base64:encode(String).

