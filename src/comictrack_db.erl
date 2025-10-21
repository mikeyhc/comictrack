-module(comictrack_db).
-export([initialize/0, add_issue/1, add_issue_to_volume/2,
         set_issue_owned/2, get_unowned_issues/0]).
-export([remove/2, get/1, get/2, add/2]).

-define(VOLUMES_DB, "volumes.db").
-define(ISSUES_DB, "issues.db").

-record(volume, {id                      :: integer(),
                 name                    :: binary(),
                 response                :: any(),
                 issues                  :: sets:set(integer()),
                 collect_after=undefined :: binary() | undefined
                }).

-record(issue, {id            :: integer(),
                owned = false :: boolean(),
                response      :: any()
               }).

initialize() ->
    Path = db_root_path(),
    io:format("using path ~s~n", [Path]),
    ok = file:make_dir(Path).

db_root_path() ->
    os:getenv("HOME") ++ "/.comictrack/".

open_volumes_db() ->
    Path = db_root_path() ++ ?VOLUMES_DB,
    dets:open_file(volume, [{file, Path}, {keypos, 2}]).

open_issues_db() ->
    Path = db_root_path() ++ ?ISSUES_DB,
    dets:open_file(issue, [{file, Path}, {keypos, 2}]).

with_connection(Name, Fun) ->
    {ok, Conn} = case Name of
                     volumes -> open_volumes_db();
                     issues -> open_issues_db()
                 end,
    Ret = Fun(Conn),
    dets:close(Conn),
    Ret.

add_issue(#{<<"id">> := Id} = Response) ->
    Issue = #issue{id=Id, response=Response},
    case get_issue(Id) of
        {error, not_found} -> write_issue(Issue);
        {error, already_exists} ->
            % TODO copy flags and update
            {error, already_exists};
        {ok, _Issue} -> {error, already_exists}
    end.

write_issue(Issue) ->
    with_connection(
      issues,
      fun(IssuesDB) -> dets:insert(IssuesDB, Issue) end).

add_issue_to_volume(VolumeId, IssueId) ->
    with_connection(
      volumes,
      fun(VolumesDB) ->
              [Volume] = dets:lookup(VolumesDB, VolumeId),
              Issues = sets:add_element(IssueId, Volume#volume.issues),
              dets:insert(VolumesDB, Volume#volume{issues=Issues})
      end).

get_issue(IssueId) ->
    with_connection(
      issues,
      fun(IssuesDB) ->
              case dets:lookup(IssuesDB, IssueId) of
                  [Issue] -> {ok, issue_to_map(Issue)};
                  [] -> {error, not_found}
              end
      end).

issue_to_map(#issue{id=Id, owned=Owned, response=Response}) ->
    #{id => Id,
      owned => Owned,
      response => Response}.

map_to_issue(#{id := Id, owned := Owned, response := Response}) ->
    #issue{id=Id, owned=Owned, response=Response}.

set_issue_owned(Id, Owned) ->
    case get_issue(Id) of
        {error, not_found} -> false;
        {ok, Issue0} ->
            Issue1 = Issue0#{owned := Owned},
            write_issue(map_to_issue(Issue1)),
            true
    end.

get_unowned_issues() ->
    with_connection(
      issues,
      fun(IssuesDB) ->
              dets:foldl(fun(I, L) ->
                                 if I#issue.owned -> L;
                                    true ->
                                        [issue_to_map(I)|L]
                                 end
                         end,
                         [],
                         IssuesDB)
      end).

remove(TableName, Key) ->
    with_connection(TableName, fun(Table) -> dets:delete(Table, Key) end).

get(TableName) ->
    with_connection(
      TableName,
      fun(Table) ->
          case dets:foldl(fun(V, L) -> [V|L] end, [], Table) of
              Err = {error, _Reason} -> Err;
              Result -> {ok, Result}
          end
      end).

get(TableName, Key) ->
    with_connection(
      TableName,
      fun(Table) ->
        case dets:lookup(Table, Key) of
            [Entry] -> {ok, Entry};
            [] -> {error, not_found}
        end
      end).

add(Table, Object) ->
    with_connection(
      Table,
      fun(Conn) ->
              % TODO: actually check this properly
              case dets:insert_new(Conn, Object) of
                  true -> ok;
                  false -> {error, already_exists}
              end
      end).
