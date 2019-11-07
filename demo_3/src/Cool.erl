-module(cool@foreign).

-export([
         createPony/1,
         areAnyPoniesNotAwesome/1,
         writePoniesToFile/2
        ]).

createPony(Name) ->
  cool:create_pony(Name).

areAnyPoniesNotAwesome(Ponies) ->
  cool:are_any_ponies_not_awesome(Ponies).

writePoniesToFile(Filename, Ponies) ->
  fun() ->
      case cool:write_ponies_to_file(Filename, Ponies) of
        ok -> true;
        _ -> false
      end
  end.


