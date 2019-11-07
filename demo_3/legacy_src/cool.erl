-module(cool).

-export([ create_pony/1
        , are_any_ponies_not_awesome/1
        , write_ponies_to_file/2
        ]).

create_pony(Name) ->
  #{
     name => Name
  ,  is_awesome => true
  }.

are_any_ponies_not_awesome(Ponies) ->
  lists:any(fun(#{ is_awesome := Awesome }) -> not Awesome end, Ponies).

write_ponies_to_file(Filename, Ponies) ->
  file:write_file(Filename, term_to_binary(Ponies)).
