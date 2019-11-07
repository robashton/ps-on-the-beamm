-module(book_sup).

-export([ start_link/0, init/1 ]).

start_link() ->
  supervisor:start_link({local, book_sup}, book_sup, []).

init([]) ->
   ConnectionString = book_config:connection_string(),
   WebPort = book_config:web_port(),
   {ok, {{one_for_one, 10, 10},
         [
          {book_web, {book_web, start_link, [WebPort]}
                   , permanent, 5000, worker, [book_web]},

          {book_library, {book_library, start_link, [ConnectionString]}
                   , permanent, 5000, worker, [book_library]},
         ]}}.

