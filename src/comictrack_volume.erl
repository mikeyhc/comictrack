-module(comictrack_volume).

-export([remove/1, sync/1, sync_all/0, get/1, get/2, set_owned/2, all/0,
         all/1, add/1, has_unowned/1, year_sort/2, name_sort/2]).

-define(TABLE_NAME, volumes).

-record(volume, {id                      :: integer(),
                 name                    :: binary(),
                 response                :: any(),
                 issues                  :: sets:set(integer()),
                 collect_after=undefined :: binary() | undefined
                }).

remove(VolumeId) ->
    case comictrack_db:get(?TABLE_NAME, VolumeId) of
        {ok, Volume} ->
            lists:foreach(fun comictrack_issue:remove/1,
                          sets:to_list(Volume#volume.issues)),
            comictrack_db:remove(?TABLE_NAME, VolumeId),
            ok;
        Err -> Err
    end.

sync(Volume) ->
    Issues = comicvine_api:issues(#{
        "volume" => integer_to_list(maps:get(<<"id">>, Volume))
    }),
    lists:map(fun(Issue) -> add_issue(Issue) end, Issues).

sync_all() ->
    Volumes = all(),
    lists:map(fun(Volume=#{<<"name">> := Name}) -> {Name, sync(Volume)} end,
              lists:map(fun(V) -> maps:get(response, V) end, Volumes)).

get(VolumeId) -> comictrack_volume:get(VolumeId, []).
get(VolumeId, Options) ->
    case comictrack_db:get(?TABLE_NAME, VolumeId) of
        {ok, V} ->
            case lists:member(expand_issues, Options) of
                true -> {ok, expand_issues(to_map(V))};
                false -> {ok, to_map(V)}
            end;
        Err -> Err
    end.

add_issue(Issue) ->
    DisplayName = comictrack_issue:display_name(Issue),
    #{<<"id">> := IssueId,
      <<"volume">> := #{<<"id">> := VolumeId}} = Issue,
    case comictrack_db:add_issue(Issue) of
        ok ->
            ok = comictrack_db:add_issue_to_volume(VolumeId, IssueId),
            {added, DisplayName};
        {error, already_exists} ->
            ok = comictrack_db:add_issue_to_volume(VolumeId, IssueId),
            {{error, already_exists}, DisplayName};
        Err={error, _Error} ->
            {Err, DisplayName}
    end.

to_map(#volume{name=Name, id=Id, response=Response, issues=Issues}) ->
    #{name => Name,
      id => Id,
      response => Response,
      issues => sets:to_list(Issues)}.

set_owned(Id, Owned) ->
    case ?MODULE:get(Id) of
        {ok, #{issues := Issues}} ->
            lists:map(fun(I) -> {I, comictrack_issue:set_owned(I, Owned)} end,
                      Issues);
        Err -> Err
    end.

all() -> all([]).

all(Options) ->
    {ok, Volumes0} = comictrack_db:get(?TABLE_NAME),
    Volumes1 = lists:map(fun to_map/1, Volumes0),
    case lists:member(expand_issues, Options) of
        true -> lists:map(fun expand_issues/1, Volumes1);
        false -> Volumes1
    end.

expand_issues(Volume=#{expanded_issues := true}) -> Volume;
expand_issues(Volume=#{issues := IssueIds}) ->
    {Status, Issues} = lists:unzip(lists:map(fun comictrack_issue:get/1,
                                             IssueIds)),
    false = lists:any(fun(V) -> V =/= ok end, Status),
    Volume#{issues => Issues, expanded_issues => true}.

add(Response=#{<<"id">> := Id, <<"name">> := Name}) ->
    Volume = #volume{name=Name, id=Id, response=Response, issues=sets:new()},
    case comictrack_db:add(?TABLE_NAME, Volume) of
        ok -> {ok, sync(Response)};
        E={error, already_exists} -> {E, sync(Response)};
        Err -> {Err, []}
    end.

has_unowned(#{expanded_issues := true, issues := Issues}) ->
    lists:any(fun(#{owned := Owned}) -> not Owned end, Issues);
has_unowned(Volume) ->
    has_unowned(expand_issues(Volume)).

year_sort(#{<<"start_year">> := A}, #{<<"start_year">> := B}) -> A > B.

name_sort(#{name := A}, #{name := B}) -> A < B.
