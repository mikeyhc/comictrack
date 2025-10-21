-module(comictrack_tui).

-export([table/2, detailed/2, build_tags/2, colorize/2]).
-export([eformat/1, eformat/2]).

color(reset) -> "\e[0m";
color(none) -> "";
color(green) -> "\e[32m";
color(bright_yellow) -> "\e[33;1m".

colorize(Color, Text) ->
    [color(Color), Text, color(reset)].

table(_Fields, _Entries) -> throw(not_implemented).

detailed(Fields, Entry) ->
    FieldWidth = max_field_length(Fields) + 1,
    FieldFormat = "~" ++ integer_to_list(FieldWidth) ++ ".. w: ",
    Pairs = lists:map(fun(T={F, _}) -> {T, maps:get(F, Entry)};
                         (F) -> {F, maps:get(F, Entry)}
                      end, Fields),
    Spacer = lists:duplicate(FieldWidth + 2, $ ),
    lists:foreach(fun ({{N, F}, [H|T]})  ->
                          io:format(FieldFormat, [N]),
                          io:format(F(H)),
                          io:nl(),
                          Builder = fun(V) -> [Spacer, F(V), "\n"] end,
                          lists:foreach(fun io:format/1,
                                        lists:map(Builder, T));
                      ({{N, F}, V}) ->
                          io:format(FieldFormat, [N]),
                          io:format(F(V)),
                          io:nl();
                      ({F, V}) when is_binary(V) ->
                          io:format(FieldFormat ++ "~s~n", [F, V]);
                     ({F, V}) ->
                          io:format(FieldFormat ++ "~p~n", [F, V])
                  end,
                  Pairs).

compose(F, G) -> fun(X) -> F(G(X)) end.

convert({V, _F}) -> convert(V);
convert(V) when is_atom(V) -> atom_to_list(V);
convert(V) -> V.

max_field_length(Fields) ->
    lists:foldl(fun max/2, 0,
                lists:map(compose(fun length/1, fun convert/1), Fields)).

eformat(FormatString) ->
    eformat(FormatString, []).

eformat(FormatString, Args) ->
    io:format(standard_error, FormatString, Args).

build_tags(Tags, Object) ->
    F = fun(T) -> build_tag(T, Object) end,
    Parts = lists:map(F, Tags),
    ["[", Parts, "]"].

build_tag({Test, {PassColor, PassChar}, {FailColor, FailChar}}, Object) ->
    case Test(Object) of
        true -> colorize(PassColor, PassChar);
        false -> colorize(FailColor, FailChar)
    end.
