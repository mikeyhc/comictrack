-module(comicvine_api).
-behaviour(gen_server).

%% API exports
-export([volumes/1, issues/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

-define(API_KEY_ENV_VAR, "COMICVINE_API_KEY").
-define(API_HOSTNAME, "comicvine.gamespot.com").
-define(API_PREFIX, "/api/").
-define(USER_AGENT, "curl/8.7.1").

-record(state, { api_key  :: string(),
                 conn_pid :: undefined | pid()
               }).

%%====================================================================
%% API functions
%%====================================================================

volumes(Filters) ->
    start_if_needed(),
    gen_server:call(?MODULE, {get, "volumes", Filters}).

issues(Filters) ->
    start_if_needed(),
    gen_server:call(?MODULE, {get, "issues", Filters}).

%%====================================================================
%% gen_server callbacks
%%====================================================================

init([ApiKey]) ->
    gen_server:cast(self(), connect),
    {ok, #state{api_key=ApiKey}}.

handle_call({get, Location, Filters}, _From, State) ->
    Response = cv_get(State, Location, Filters),
    {reply, Response, State}.

handle_cast(connect, State) ->
    application:ensure_all_started(gun),
    {ok, ConnPid} = gun:open(?API_HOSTNAME, 443),
    link(ConnPid),
    {ok, _Protocol} = gun:await_up(ConnPid), {noreply, State#state{conn_pid=ConnPid}}.

%%====================================================================
%% Internal functions
%%====================================================================

start_link() ->
    ApiKey = case os:getenv(?API_KEY_ENV_VAR) of
                 false -> throw(no_api_key);
                 V -> V
             end,
    gen_server:start_link({local, ?MODULE}, ?MODULE,
                          [ApiKey],
                          []).

url_encode_values(Map) ->
    maps:map(fun(_Key, Value) -> uri_string:quote(Value) end, Map).

start_if_needed() ->
    case whereis(?MODULE) of
        undefined ->
            start_link();
        Pid ->
            case is_process_alive(Pid) of
                true -> ok;
                false ->
                    unregister(?MODULE),
                    start_link()
            end
    end.

cv_get(State, URI, Filters) ->
    Parameters = #{"filter" => filter_join(url_encode_values(Filters)),
                   "format"  => "json",
                   "api_key" => State#state.api_key},
    URL = ?API_PREFIX ++ URI ++ "/?" ++ map_to_strlist(Parameters, "=", "&"),
    Headers = [{<<"user-agent">>, ?USER_AGENT}],
    %io:format("URL: ~p~n", [URL]),
    ConnPid = State#state.conn_pid,
    StreamRef = gun:get(ConnPid, URL, Headers),
    case gun:await(ConnPid, StreamRef) of
        {response, nofin, 200, _Headers} ->
            {ok, Body} = gun:await_body(ConnPid, StreamRef),
            Json = jsone:decode(Body),
            #{<<"error">> := <<"OK">>,
              <<"results">> := Data} = Json,
            Data
    end.

filter_join(Map) ->
    map_to_strlist(Map, ":", ",").

map_to_strlist(Map, InnerSep, OuterSep) ->
    lists:flatten(lists:join(OuterSep,
                             lists:map(fun({K, V}) -> K ++ InnerSep ++ V end,
                                       maps:to_list(Map)))).
