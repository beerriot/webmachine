-module(wdc_graph).

-export([parse/0, parse/1,
         dot/0, dot/2]).
-export([main/1]).

parse() ->
    parse("src/webmachine_decision_core.erl").

%% @doc Returns a list of three tuples, each of the form
%% `{DecisionName, ResourceCalls, Outcomes}'.
parse(Filename) ->
    {ok, File} = file:read_file(Filename),
    {ok, Scan, _ScanLines} = erl_scan:string(binary_to_list(File)),
    {_, RevSplitScan} = lists:foldl(
                          fun({dot,_}=D, {P,A}) -> {[],    [[D|P]|A]};
                             (O,         {P,A}) -> {[O|P], A}
                          end,
                          {[],[]},
                          Scan),
    Forms = [element(2, {ok, _}=erl_parse:parse_form(lists:reverse(S)))
             || S <- lists:reverse(RevSplitScan)],
    Functions = [ F || F <- Forms,
                       is_tuple(F),
                       function == element(1, F)],
    {function, _Line, decision, _Arity, DecisionClauses} =
        lists:keyfind(decision, 3, Functions),
    [ grok_decision_clause(DC) || DC <- DecisionClauses ].

grok_decision_clause({clause, _ClauseLine,
                      [{atom, _NameLine, Name}],
                      []=_Guards,
                      Body}) ->
    ResourceCalls = find_resource_calls(Body),
    Outcomes = find_outcomes(Body),
    {Name, ResourceCalls, Outcomes}.

find_resource_calls([]) ->
    [];
find_resource_calls([{call, _CallLine,
                      {atom, _AtomLine, resource_call},
                      [{atom, _NameLine, Name}]}
                     |Rest]) ->
    [Name|find_resource_calls(Rest)];
find_resource_calls([List|Rest]) when is_list(List) ->
    find_resource_calls(List)++find_resource_calls(Rest);
find_resource_calls([Tuple|Rest]) when is_tuple(Tuple) ->
    find_resource_calls(tuple_to_list(Tuple))++find_resource_calls(Rest);
find_resource_calls([_|Rest]) ->
    find_resource_calls(Rest).

find_outcomes([]) ->
    [];
find_outcomes([{call, _CallLine,
                {atom, _AtomLine, d},
                [{atom, _NameLine, Name}]}
               |Rest]) ->
    [Name|find_outcomes(Rest)];
find_outcomes([{call, _CallLine,
                {atom, _AtomLine, decision_test},
                [_Test, _Expect, True, False]}
               |Rest]) ->
    [outcome(True),outcome(False)|find_outcomes(Rest)];
find_outcomes([{call, _CallLine,
                {atom, _AtomLine, decision_test_fn},
                [_Test, _ExpectFun, True, False]}
               |Rest]) ->
    [outcome(True),outcome(False)|find_outcomes(Rest)];
find_outcomes([{call, _CallLine,
                {atom, _AtomLine, respond},
                [{integer, _IntegerLine, Integer}]}
               |Rest]) ->
    [Integer|find_outcomes(Rest)];
find_outcomes([{call, _CallLine,
                {atom, _AtomLine, respond},
                [{integer, _IntegerLine, Integer},_Headers]}
               |Rest]) ->
    [Integer|find_outcomes(Rest)];
find_outcomes([List|Rest]) when is_list(List) ->
    find_outcomes(List)++find_outcomes(Rest);
find_outcomes([Tuple|Rest]) when is_tuple(Tuple) ->
    find_outcomes(tuple_to_list(Tuple))++find_outcomes(Rest);
find_outcomes([_|Rest]) ->
    find_outcomes(Rest).

outcome({atom, _AtomLine, Name})          -> Name;
outcome({integer, _IntegerLine, Integer}) -> Integer.

dot() ->
    dot(parse(), "webmachine_decision_core.dot").

%% @doc Takes the output of parse/1 and writes a GraphViz dot file
%% based on it to `Filename'.  To generate a pretty graph, run
%% `fdp -Tpng -oPngFilename Filename' at a command prompt.
dot(Parse, Filename) ->
    {Write, Close} = open_dot(Filename),
    Write("digraph wdc {\n"),

    Combined = combine_bs(Parse),

    %% decision nodes
    [ Write(io_lib:format("~p [pos=\"~b,~b!\" label=\"~s\" shape=diamond]~n",
                          [Decision,
                           2*col(Decision), 2*row(Decision),
                           label(Decision)]))
      || {Decision, _ResourceCalls, _Outcomes} <- Combined ],

    %% decision labels
    [ Write(io_lib:format("~p_label [label=<~s> shape=none]~n",
                          [Decision,
                           string:join(
                             [atom_to_list(RC) || RC <- ResourceCalls],
                             "<BR/>")]))
      || {Decision, ResourceCalls, _Outcomes} <- Combined,
         ResourceCalls /= [] ],

    %% status nodes
    Statuses = lists:usort(
                 lists:append(
                   [ [O || O <- Outcomes, is_integer(O)]
                     || {_Decision, _ResourceCalls, Outcomes} <- Combined ])),
    [ Write(io_lib:format("~p [pos=\"~b,~b!\"shape=box]~n",
                          [Status, 2*col(Status), 2*row(Status)]))
      || Status <- Statuses],

    %% paths
    [ [ Write(io_lib:format("~p -> ~p~n", [Decision, O]))
        || O <- Outcomes ]
      || {Decision, _ResourceCalls, Outcomes} <- Combined ],
    [ Write(io_lib:format("~p_label -> ~p [color=\"#cccccc\"]~n",
                          [Decision, Decision]))
      || {Decision, ResourceCalls, _Outcomes} <- Combined,
         ResourceCalls /= [] ],

    Write("}\n"),
    Close().

open_dot(console) ->
    {fun(Data) -> io:format(Data) end,
     fun() -> ok end};
open_dot(Filename) ->
    {ok, File} = file:open(Filename, [write]),
    {fun(Data) -> file:write(File, Data) end,
     fun() -> file:close(File) end}.

combine_bs(Parse) ->
    Bs = [ DRO || {D, _, _}=DRO <- Parse,
                  $b == hd(lists:reverse(atom_to_list(D))) ],
    [ lists:foldl(
        fun({BD, BR, BO}, {D, R, O}) ->
                case lists:member(BD, O) of
                    %% O--BD is not safe enough, because O might
                    %% contain multiple copies of BD
                    true -> {D, R++BR, ([Os || Os <- O, Os /= BD])++BO};
                    false -> {D, R, O}
                end
        end,
        DRO,
        Bs)
      || DRO <- Parse--Bs ].

label(Decision) ->
    [$v,$3|Name] = atom_to_list(Decision),
    Name.

col(Decision) when is_atom(Decision) ->
    [$v,$3,Col|_] = atom_to_list(Decision),
    1+Col-$a;
col(Status) when is_integer(Status) ->
    Plot = [{$a, [413, 415, 501, 403, 401, 400, 405, 414, 501, 503]},
            {$c, [406]},
            {$g, [412]},
            {$k, [301]},
            {$l, [307, 304, 500]},
            {$m, [404, 303, 202]},
            {$n, [410, 200]},
            {$o, [204]},
            {$p, [409, 300]},
            {$q, [201]}],
    [Col] = [ Col || {Col, Statuses} <- Plot,
                     lists:member(Status, Statuses) ],
    1+Col-$a.
    
row(Decision) when is_atom(Decision) ->
    [$v,$3,_Col|Row] = atom_to_list(Decision),
    22-list_to_integer(Row);
row(Status) when is_integer(Status) ->
    Plot = [{2, [409]},
            {4, [413, 301, 307, 410]},
            {5, [415]},
            {6, [501]},
            {7, [403, 406]},
            {8, [401, 404]},
            {9, [400]},
            {10,[405]},
            {11,[414, 303, 201]},
            {13,[503]},
            {18,[412, 304, 200, 300]},
            {20,[500]},
            {21,[202, 204]}],
    [Row] = [ Row || {Row, Statuses} <- Plot,
                     lists:member(Status, Statuses) ],
    22-Row.

%% Escript Entry Point
main(Args) ->
    {WDC, DOT} = case Args of
                     [] -> {"src/webmachine_decision_core.erl", console};
                     [WDCPath] -> {WDCPath, console};
                     [WDCPath, DOTPath] -> {WDCPath, DOTPath};
                     _ -> usage()
                 end,
    case filelib:is_regular(WDC) of
        true -> dot(parse(WDC), DOT);
        false ->
            io:format("Error: Could not find input file \"~s\"~n", [WDC]),
            usage()
    end.

usage() ->
    io:format("Usage: escript wdc_graph.erl [WDCPath] [DOTPath]~n"
              "  WDCPath: path to webmachine_decision_core.erl~n"
              "           (default: \"src/webmachine_decision_core.erl\")~n"
              "  DOTPath: name of the dot file to generate, leave absent"
              " for console output~n"
              "           (default: console output)~n"),
    exit(-1).
