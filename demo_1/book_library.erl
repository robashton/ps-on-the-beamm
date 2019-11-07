-module(book_library).

-behaviour(gen_server).

-export([ start_link/1,
          init/1
        ]).

-record(state, {
          connection :: redis:connection()
         }).

-define(SERVER, book_library).


start_link(ConnectionString) ->
  gen_server:start_link(?SERVER, [ConnectionString], []).

init([ConnectionString]) ->
  { ok, Connection } = redis:open(ConnectionString),
  {ok, #state {
          connection = Connection
         }}.

create(Book) ->
  gen_server:call(?SERVER, { create, Book }).


handle_call({create, Book = #book { isbn = Isbn }}, _Sender, State = #state { connection = Connection }) ->
  Reply = case redis:get(Isbn) of
            undefined ->
              redis:put(Isbn, Book, Connection),
              ok;
            { ok, Existing } ->
              { error, already_exists }
          end,
  {reply, Reply, State }.
