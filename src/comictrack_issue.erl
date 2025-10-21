-module(comictrack_issue).

-export([display_name/1]).
-export([remove/1, set_owned/2, store_date_sort/2, get_unowned_issues/0,
         valid_store_date/1, get/1]).

-define(TABLE_NAME, issues).

-record(issue, {id            :: integer(),
                owned = false :: boolean(),
                response      :: any()
               }).

display_name(Issue) ->
    #{<<"name">> := Name,
      <<"issue_number">> := Number,
      <<"id">> := Id,
      <<"volume">> := #{<<"name">> := VolumeName}} = Issue,
    BinId = integer_to_binary(Id),
    PaddedId = if size(BinId) =:= 7 -> BinId;
                  true ->
                      Diff = 7 - size(BinId),
                      Spaces = list_to_binary(lists:duplicate(Diff, " ")),
                      <<Spaces/binary, BinId/binary>>
               end,
    if Name =/= null ->
           CleanName = string:trim(Name),
           <<PaddedId/binary, " ", VolumeName/binary, " - ", CleanName/binary,
             " - Issue ", Number/binary>>;
       true ->
           <<PaddedId/binary, " ", VolumeName/binary, " - Issue ",
             Number/binary>>
    end.

remove(IssueId) ->
    comictrack_db:remove(?TABLE_NAME, IssueId).

set_owned(Id, Value) -> comictrack_db:set_issue_owned(Id, Value).

store_date_sort(#{<<"store_date">> := A}, #{<<"store_date">> := B}) -> A < B.

valid_store_date(#{<<"store_date">> := StoreDate}) ->
    {{Y0, M0, D0}, _Time} = calendar:local_time(),
    [Y1, M1, D1] = lists:map(fun list_to_integer/1,
                             string:split(binary_to_list(StoreDate), "-", all)),
    if Y1 < Y0 -> true;
       Y1 > Y0 -> false;
       M1 < M0 -> true;
       M1 > M0 -> false;
       true -> D1 =< D0
    end.

get_unowned_issues() ->
    comictrack_db:get_unowned_issues().

get(IssueId) ->
    case comictrack_db:get(?TABLE_NAME, IssueId) of
        {ok, Issue} -> {ok, to_map(Issue)};
        Error -> Error
    end.

to_map(#issue{id=Id, owned=Owned, response=Response}) ->
    #{id => Id,
      owned => Owned,
      response => Response}.
