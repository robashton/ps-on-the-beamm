-module(book_web).

-behaviour(gen_server).

-export([start_link/0
        ]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

init([WebPort]) ->
  Dispatch = cowboy_router:compile([{'_', [
                                   {<<"/api/books">>, books_resource, []},
                                   {<<"/api/books/:isbn">>, book_resource, []},
                                   {<<"/assets/[...]">>, cowboy_static, {priv_dir, book_web, <<"www/assets/">>}},
                                   {<<"/[..]">>, cowboy_static, {priv_file, book_web, <<"www/index.html">>}}
                                          ] }]),
  cowboy:start_clear(prf_control_primary_http_listener,
                     [{port, WebPort}],
                     #{env => #{
                         dispatch => Dispatch
                        },
                       middlewares => [cowboy_router, cowboy_handler]
                      }
                    ),
    {ok, #state{}}.

handle_call(_Request, _From, State) ->
  {reply, ok, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

resources(SecurityConfig) ->
  SinglePagePaths = [
                     %% old style
                     <<"/">>,
                     <<"/admin">>,

                     %% Floating style
                     <<"/ctl/">>,
                     <<"/ctl/admin">>,

                     <<"/firmware">>,
                     <<"/firmware/[...]">>
                   ],

  SinglePageRules = [ {Path, cowboy_static, {priv_file, prf_control, <<"www/index.html">>}} || Path <- SinglePagePaths ],

  id3as_auction:cowboy_endpoints(<<"/auction">>) ++
  id3as_substrate:cowboy_endpoints() ++
  [
   {<<"/ctl/environment">>, prf_control_environment_resource, []},
   {<<"/ctl/api/commands">>, prf_control_commands_resource, []},
   {<<"/ctl/channel/mta_preview/:channel_id/:index">>, prf_channel_mta_preview_resource, []},
   {<<"/ctl/api/data/firmwares">>, prf_firmwares_resource, []},
   {<<"/ctl/api/data/firmwares/encoder">>, prf_firmwares_resource, [ encoder ]},
   {<<"/ctl/api/data/firmwares/commentary">>, prf_firmwares_resource, [ commentary ]},
   {<<"/ctl/api/data/firmwares/packager">>, prf_firmwares_resource, [ packager ]},
   {<<"/ctl/api/data/firmwares/node">>, prf_firmwares_resource, [ node ]},
   {<<"/ctl/api/data/firmwares/encoder/settings">>, prf_firmware_settings_resource, [ encoder]},
   {<<"/ctl/api/data/firmwares/commentary/settings">>, prf_firmware_settings_resource, [ commentary ]},
   {<<"/ctl/api/data/firmwares/packager/settings">>, prf_firmware_settings_resource, [ packager ]},
   {<<"/ctl/api/data/firmwares/node/settings">>, prf_firmware_settings_resource, [ node ]},
   {<<"/ctl/api/data/firmwares/upload">>, prf_firmware_upload_resource , []},
   {<<"/ctl/api/data/firmware/:version">>, prf_firmware_resource, []},
   {<<"/ctl/api/data/nodes/info">>, prf_node_infos_resource, []},
   {<<"/ctl/api/data/nodes">>, prf_nodes_resource, []},
   {<<"/ctl/api/data/running_events">>, prf_running_events_resource, []},
   {<<"/ctl/api/data/running_event/:event_id">>, prf_running_event_resource, []},
   {<<"/ctl/api/data/scheduled_event/:event_id">>, prf_scheduled_event_resource, []},
   {<<"/ctl/api/data/scheduled_events">>, prf_scheduled_events_resource, []},
   {<<"/ctl/api/data/node/:id">>, prf_node_resource, []},
   {<<"/ctl/api/data/node/:id/allocations">>, prf_node_allocations_resource, []},
   {<<"/ctl/api/data/firmware_jobs">>, prf_firmware_jobs_resource, []},
   {<<"/ctl/api/data/encoders">>, prf_encoders_resource, []},
   {<<"/ctl/api/data/encoder/:id">>, prf_encoder_resource, []},
   {<<"/ctl/api/data/encoder/:id/channels">>, prf_encoder_channels_resource, []},
   {<<"/ctl/api/data/channels">>, prf_channels_resource, []},
   {<<"/ctl/api/data/channel/:id">>, prf_channel_resource, []},
   {<<"/ctl/api/data/channel/:id/outputs">>, prf_channel_outputs_resource, []},
   {<<"/ctl/api/data/channel/:id/config">>, prf_channel_config_resource, []},
   {<<"/ctl/api/data/channel/:id/status">>, prf_channel_status_resource, []},
   {<<"/ctl/api/data/channel/:id/commentary">>, prf_channel_commentary_resource, []},
   {<<"/ctl/api/data/channel/:id/events">>, prf_channel_events_resource, []},
   {<<"/ctl/api/data/channel/:id/commands">>, prf_channel_commands_resource, []},
   {<<"/ctl/api/data/channel/:id/packager">>, prf_channel_packager_resource, []},
   {<<"/ctl/api/data/channel/:channel_id/mta">>, prf_channel_mta_resource, []},
   {<<"/ctl/api/data/channel/:channel_id/mta/clean">>, prf_channel_mta_clean_resource, []},
   {<<"/ctl/api/data/channel/:channel_id/mta/:index">>, prf_channel_mta_commentary_resource, []},
   {<<"/ctl/api/data/channel/:channel_id/mta_events/:index">>, prf_channel_mta_events_resource, []},
   {<<"/ctl/api/data/system_events/:year/:month/:day">>, prf_system_events_resource, []},
   {<<"/ctl/api/data/system_events/:year/:month/:day/:event_type">>, prf_system_events_resource, []},
   {<<"/ctl/api/data/system_events/:year/:month/:day/:event_type/:event_id">>, prf_system_events_resource, []},
   {<<"/ctl/api/data/flags">>, prf_flags_resource, []},
   {<<"/ctl/api/data/flags/channels">>, prf_flags_channels_resource, []},
   {<<"/ctl/api/data/flags/events">>, prf_flags_events_resource, []},
   {<<"/ctl/api/data/flags/sessions">>, prf_flags_sessions_resource, []},
   {<<"/ctl/api/data/commentary_sessions">>, prf_commentary_sessions_resource, []},
   {<<"/ctl/api/data/commentary_session/:id">>, prf_commentary_session_resource, []},
   {<<"/ctl/api/data/commentary/:event/:customer/:language/:security_cookie">>, prf_commentary_healthcheck_resource, []},
   {<<"/ctl/api/data/commentary/:event/:customer/:language">>, prf_commentary_resource, []},
   {<<"/ctl/api/data/users">>,  prf_users_resource, []},
   {<<"/ctl/api/data/user/:username">>,  prf_user_resource, []},
   {<<"/ctl/api/data/mta_sources">>,  prf_mta_sources_resource, []},

   {<<"/ctl/internal/flags">>, flags_resource, []},
   {<<"/ctl/internal/sync/channels/:channel_id">>, prf_channel_sync_resource, []},
   {<<"/ctl/internal/clean_etcd">>, prf_etcd_resource, []},

   {<<"/ctl/api/packaging/events">>, scheduled_events_resource, []},
   {<<"/ctl/api/packaging/event/:event_id">>, scheduled_event_resource, []},

   {<<"/ctl/logout">>, prf_logout_resource, SecurityConfig},
   {<<"/ctl/logon">>, prf_logon_resource, SecurityConfig}
  ] ++
  SinglePageRules ++
  [
   {<< (prf_control_config:firmware_base_url())/binary,   "[...]">>, cowboy_static, {dir, id3as_control_config:base_firmware_dir(), []}},
   {<<"/ctl/[...]">>, cowboy_static, {priv_dir, prf_control, <<"www">>}}
  ].
