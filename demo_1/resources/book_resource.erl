-module(book_resource).


-export([init/2,
         allowed_methods/2,
         get_json/2,
         put_json/2,
         delete_resource/2,
         resource_exists/2,
         content_types_provided/2,
         content_types_accepted/2
        ]).

-record(book_create, {
          id :: binary_string()
         }).

-record(state, {
          id,
          book
         }).

init(Req, _Opts) ->
  Id = cowboy_req:binding(id, Req),
  Book = case books:find_by_id(Id) of
             { ok, Result } -> Result;
             _ -> undefined
           end,

  { cowboy_rest, Req, #state {
                         id = Id,
                         book = Book
                        }}.

resource_exists(Req, State = #state { book = undefined }) ->
  { false, Req, State };
resource_exists(Req, State) ->
  { true, Req, State }.

content_types_provided(Req, State = #state{}) ->
  {[{ <<"application/json">>, get_json}], Req, State}.

content_types_accepted(Req, State = #state{}) ->
  {[{ <<"application/json">>, put_json}], Req, State}.

delete_resource(Req, State = #state { id = Id
                                    }) ->
  ok = book:delete(Id),
  { true, Req, State }.

get_json(Req, State = #state { book = Book = #book {} }) ->
  Json = book_mapper:record_to_json(Book),
  { jsx:encode(Json), Req, State }.

put_json(Req, State = #state { id = _Id }) ->
  {ok, Body, Req2} = cowboy_req:read_body(Req),
  BodyJson = jsx:decode(Body),
  #book_create { id = Id } = book_mapper:json_to_record(BodyJson, book_create),
  { true, Req2, State }.

allowed_methods(Req, State) ->
  { [ <<"DELETE">>, <<"GET">>, <<"PUT">>, <<"HEAD">>, <<"OPTIONS">>], Req, State}.
