-module(comictrack).
-import(comictrack_tui, [eformat/1, eformat/2]).

%% API exports
-export([main/1]).

%%====================================================================
%% API functions
%%====================================================================

%% escript Entry point
main([]) ->
    usage(),
    erlang:halt(1);
main(["init"|_]) ->
    comictrack_db:initialize(),
    io:format("successfully initialized~n");
main(["volume",Cmd|Args]) ->
    handle_volume_cmds(Cmd, Args);
main(["volumes",Cmd|Args]) ->
    handle_volume_cmds(Cmd, Args);
main(["issue"]) ->
    issue_usage(),
    erlang:halt(1);
main(["issue",Cmd|Args]) ->
    handle_issue_cmds(Cmd, Args);
main(["issues"]) ->
    issue_usage(),
    erlang:halt(1);
main(["issues",Cmd|Args]) ->
    handle_issue_cmds(Cmd, Args);
main(["help"|_]) ->
    usage();
main([Cmd|_]) ->
    eformat("unknown command: ~s~n~n", [Cmd]),
    usage(),
    erlang:halt(1).


%%====================================================================
%% Internal functions
%%====================================================================

handle_volume_cmds("add", [VolumeName]) ->
    add_volume(VolumeName);
handle_volume_cmds("list", _Args) ->
    list_volumes();
handle_volume_cmds("show", [Id]) ->
    show_volume(list_to_integer(Id));
handle_volume_cmds("own", [Id]) ->
    own_volume(list_to_integer(Id));
handle_volume_cmds("sync", []) ->
    sync_all_volumes();
handle_volume_cmds("rm", [Id]) ->
    remove_volume(Id);
handle_volume_cmds(_Cmd, _Args) ->
    throw(not_implemented).

handle_issue_cmds("own", [Id]) ->
    set_issue_owned(list_to_integer(Id), true);
handle_issue_cmds("unown", [Id]) ->
    set_issue_owned(list_to_integer(Id), false);
handle_issue_cmds("unowned", []) ->
    unowned_issues();
handle_issue_cmds("help", _Args) ->
    issue_usage();
handle_issue_cmds(Cmd, _Args) ->
    eformat("unknown command: ~s~n~n", [Cmd]),
    issue_usage(),
    erlang:halt(1).

usage() ->
    eformat("usage: ~s {command|group}~n~n", [escript:script_name()]),
    eformat("commands:~n"),
    eformat("  init     initialize the database~n"),
    eformat("  help     help dialog for groups and commands~n"),
    eformat("~n"),
    eformat("groups:~n"),
    eformat("  volume   commands related to volumes~n"),
    eformat("  issues   commands related to issues~n"),
    eformat("~n").

issue_usage() ->
    eformat("usage: ~s issue {command} [args]~n~n", [escript:script_name()]),
    eformat("commands:~n"),
    eformat("  own      mark an issue as owned~n"),
    eformat("  unown    mark an issue as unowned~n"),
    eformat("  unowned  list all unowned issues~n"),
    eformat("  help     show this dialog~n"),
    eformat("~n").

add_volume(SearchTerm) ->
    Volumes = try
                  _Id = list_to_integer(SearchTerm),
                  comicvine_api:volumes(#{"id" => SearchTerm})
              catch
                  error:badarg ->
                      comicvine_api:volumes(#{"name" => SearchTerm})
              end,
    if length(Volumes) =/= 1 ->
           eformat("found ~w potential matches:~n", [length(Volumes)]),
           lists:foreach(fun(#{<<"name">> := N, <<"id">> := Id,
                               <<"start_year">> := Year}) ->
                                 eformat("  - ~7w ~s (~s)~n", [Id, N, Year])
                         end,
                        lists:sort(fun comictrack_volume:year_sort/2, Volumes)),
           erlang:halt(1);
       true ->
           [Volume] = Volumes,
           #{<<"name">> := VolumeName} = Volume,
           case comictrack_volume:add(Volume) of
               ok -> io:format("added volume: ~s~n", [VolumeName]);
               {error, already_exists} ->
                   io:format("already exists: ~s~n", [VolumeName]);
               {error, Error} ->
                   eformat("error adding volume ~s: ~s~n", [VolumeName, Error]),
                   erlang:halt(1)
           end
    end.

list_volumes() ->
    Volumes = comictrack_volume:all([expand_issues]),
    lists:foreach(fun(Volume=#{name := Name, id := Id}) ->
                          Tags = comictrack_tui:build_tags(
                                   [{fun comictrack_volume:has_unowned/1,
                                     {bright_yellow, "U"},
                                     {none, "-"}}],
                                   Volume),
                          io:format(Tags),
                          io:format("~7w - ~s~n", [Id, Name]) end,
                  lists:sort(fun comictrack_volume:name_sort/2, Volumes)).

show_volume(Id) ->
    case comictrack_volume:get(Id, [expand_issues]) of
        {ok, Volume} ->
            comictrack_tui:detailed(
              [id, name, {issues, fun issue_singleline/1}],
              Volume);
        {error, not_found} -> eformat("volume ~p not found~n", [Id]);
        {error, Error} -> eformat("error reading ~p: ~p~n", [Id, Error])
    end.

sync_pp(Type, Name, Code, Indent) ->
    case Code of
        ok ->
            io:format("~sadded ~s: ~s~n", [Indent, Type, Name]);
        {error, already_exists} ->
            io:format("~s~s already exists: ~s~n", [Indent, Type, Name]);
        {error, Error} ->
            io:format("~serror adding ~s ~s: ~s~n", [Indent, Type, Name, Error])
    end.

sync_all_volumes() ->
    IssuePrint = fun({Code, Name}) ->
                         sync_pp("issue", Name, Code, "  ")
                 end,
    VolumePrint = fun({Name, IssueResponses}) ->
                          sync_pp("volume", Name, ok, ""),
                          lists:foreach(IssuePrint, IssueResponses)
                  end,
    lists:foreach(VolumePrint, comictrack_volume:sync_all()).


issue_singleline(Issue) ->
    DisplayName = comictrack_issue:display_name(maps:get(response, Issue)),
    IsOwned = fun(#{owned := Owned}) -> Owned end,
    Tags = comictrack_tui:build_tags(
             [{IsOwned, {green, "O"}, {bright_yellow, "U"}}],
             Issue),
    [Tags, DisplayName].

issue_update_printer({Id, true}) ->
    io:format("issue ~p successfully updated~n", [Id]);
issue_update_printer({Id, false}) ->
    io:format("issue ~p not found~n", [Id]).


set_issue_owned(Id, Value) ->
    issue_update_printer({Id, comictrack_issue:set_owned(Id, Value)}).

own_volume(Id) ->
    lists:foreach(
      fun({I, true}) -> io:format("issue ~p updated~n", [I]);
         ({I, false}) -> io:format("could not updated issue ~p~n", [I])
      end,
      comictrack_volume:set_owned(Id, true)).

unowned_issues() ->
    lists:foreach(fun(I) ->
                          Name = comictrack_issue:display_name(I),
                          #{<<"store_date">> := Date} = I,
                          io:format("~s (~s)~n", [Name, Date])
                  end,
                  lists:sort(fun comictrack_issue:store_date_sort/2,
                             lists:filter(fun comictrack_issue:valid_store_date/1,
                                          lists:map(fun(#{response := R}) -> R end,
                                                    comictrack_issue:get_unowned_issues())))).

remove_volume(Id) ->
    case comictrack_volume:remove(list_to_integer(Id)) of
        ok -> io:format("removed volume ~s~n", [Id]);
        {error, not_found} -> eformat("volume ~s not found~n", [Id])
    end.
