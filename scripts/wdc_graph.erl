%% @doc Extract the call graph from webmachine_decision_core and
%% transform it to a DOT file for rendering.
%%
%% This module may be used from other Erlang code via {@link parse/1}
%% and {@link dot/2} or as an escript: `escript scripts/wdc_graph.erl'.
-module(wdc_graph).

-export([parse/0, parse/1,
         dot/0, dot/2]).
-export([main/1]).

%% @doc Debugging convenience.
%% @equiv parse("src/webmachine_decision_core.erl")
parse() ->
    parse("src/webmachine_decision_core.erl").

%% @doc Returns a list of three tuples, each of the form
%% `{DecisionName, ResourceCalls, Outcomes}'.
-spec parse(string()) -> [{atom(), [atom()], [atom()|integer()]}].
parse(Filename) ->
    Functions = function_forms(forms_from_file(Filename)),
    [ grok_decision_clause(DC, Functions) ||
        DC <- find_function_clauses(decision, Functions) ].

%% @doc Extract the parsed forms from the named file.
forms_from_file(Filename) ->
    {ok, File} = file:read_file(Filename),
    {ok, Scan, _ScanLines} = erl_scan:string(binary_to_list(File)),

    %% parse form takes one form at a time, so break up the list into
    %% forms (chunks ending with a dot)
    {_, RevSplitScan} = lists:foldl(
                          fun({dot,_}=D, {P,A}) -> {[],    [[D|P]|A]};
                             (O,         {P,A}) -> {[O|P], A}
                          end,
                          {[],[]},
                          Scan),
    [element(2, {ok, _}=erl_parse:parse_form(lists:reverse(S)))
     || S <- lists:reverse(RevSplitScan)].

%% @doc Filter out forms that are not functions.
function_forms(Forms) ->
    [ F || {function, _Line, _Name, _Arity, _Clauses}=F <- Forms].

%% @doc Find the clauses for a named function.  If the function is not
%% found, return an empty list of forms.
find_function_clauses(FunctionName, Functions) ->
    case lists:keyfind(FunctionName, 3, Functions) of
        {function, _Line, FunctionName, _Arity, DecisionClauses} ->
            DecisionClauses;
        false ->
            []
    end.

%% @doc Find all of the resource calls and outcomes in a given
%% function clause.
grok_decision_clause({clause, _ClauseLine,
                      [{atom, _NameLine, Name}],
                      []=_Guards,
                      Body},
                     OtherFunctions) ->
    %% these could probably be combined to make just one pass through
    %% the tree, but it's simpler to keep the extractions separate,
    %% and doesn't take that long this way
    ResourceCalls = find_resource_calls(Body, OtherFunctions),
    Outcomes = find_outcomes(Body),
    {Name, ResourceCalls, Outcomes}.

%% @doc Find all calls to the webmachine resource in a function clause
%% body.  These calls are detected by matching on calls to
%% `resource_call/1'.
%%
%% Two types of calls are skipped: those in the function `respond/1',
%% and those to the anonymous functions a resource may return for
%% producing and encoding response bodies.
%%
%% Calls to `respond/1' are replaced with the atom `RESPOND'.  This is
%% because respond makes several calls to the resource, and is itself
%% called from many points in webmachine decision core.  The
%% additional calls tend to look more like noise than useful
%% information, so they are omitted.
find_resource_calls([], _OtherFunctions) ->
    [];
find_resource_calls([{call, _CallLine,
                      {atom, _AtomLine, resource_call},
                      [{atom, _NameLine, Name}]}
                     |Rest],
                    OtherFunctions) ->
    [Name|find_resource_calls(Rest, OtherFunctions)];
find_resource_calls([{call, _CallLine,
                      {atom, _AtomLine, respond},
                      Args}
                     |Rest],
                    OtherFunctions) ->
    ['RESPOND'|(find_resource_calls(Args, OtherFunctions)
                ++find_resource_calls(Rest, OtherFunctions))];
find_resource_calls([{call, _CallLine,
                      {atom, _AtomLine, FunctionName},
                      Args}
                     |Rest],
                    OtherFunctions)
  when FunctionName /= d,
       FunctionName /= decision_test,
       FunctionName /= decision_test_fn,
       FunctionName /= respond, %% makes lots of resource calls, actually
       FunctionName /= make_encoder_stream, %% harmless but recursive
       %% known harmless (builtins & such)
       FunctionName /= get,
       FunctionName /= put,
       FunctionName /= wrcall ->
    Body = find_function_clauses(FunctionName, OtherFunctions),
    find_resource_calls(Body, OtherFunctions)
        ++find_resource_calls(Args, OtherFunctions)
        ++find_resource_calls(Rest, OtherFunctions);
find_resource_calls([List|Rest], OF) when is_list(List) ->
    find_resource_calls(List, OF)++find_resource_calls(Rest, OF);
find_resource_calls([Tuple|Rest], OF) when is_tuple(Tuple) ->
    find_resource_calls(tuple_to_list(Tuple), OF)
        ++find_resource_calls(Rest, OF);
find_resource_calls([_|Rest], OF) ->
    find_resource_calls(Rest, OF).

%% @doc Find all of the next "outcomes" of a function clause body.  An
%% outcome may be the next decision point or a response code.
%%
%% Outcomes are detected by matching calls to the functions `d/1',
%% `decision_test/4', `decision_test_fn/4', `respond/1', and
%% `respond/2'.
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

%% @doc Debugging convenience.
%% @equiv dot(wdc_graph:parse(), "webmachine_decision_core.dot").
dot() ->
    dot(parse(), "webmachine_decision_core.dot").

%% @doc Takes the output of {@link parse/1} and writes a GraphViz dot
%% file based on it to `Filename'.  To generate a pretty graph, run
%% `fdp -Tpng -oPngFilename Filename' at a command prompt.
dot(Parse, Filename) ->
    {Write, Close} = open_dot(Filename),
    Write("digraph wdc {\n"),

    %% v3___b nodes confuse the graph
    Combined = combine_bs(Parse),

    %% decision nodes
    [ Write(io_lib:format("~p [pos=\"~b,~b!\" label=\"~s\" shape=diamond]~n",
                          [Decision,
                           2*col(Decision), 2*row(Decision),
                           label(Decision)]))
      || {Decision, _ResourceCalls, _Outcomes} <- Combined ],

    %% decision resource calls
    [ Write(io_lib:format("~p_label [label=<~s> shape=none]~n",
                          [Decision,
                           string:join(
                             [atom_to_list(RC) || RC <- ResourceCalls,
                                                  RC /= 'RESPOND'],
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

    %% resource call label pointers
    [ Write(io_lib:format("~p_label -> ~p [color=\"#cccccc\" dir=none]~n",
                          [Decision, Decision]))
      || {Decision, ResourceCalls, _Outcomes} <- Combined,
         ResourceCalls /= [] ],

    Write("}\n"),
    Close().

%% @doc Provide a simple common interface for writing to a file or to
%% the console.  
-spec open_dot(console|string()) ->
    {WriteFunction::fun((iolist()) -> ok),
     CloseFunction::fun(() -> ok)}.
open_dot(console) ->
    {fun(Data) -> io:format(Data) end,
     fun() -> ok end};
open_dot(Filename) ->
    {ok, File} = file:open(Filename, [write]),
    {fun(Data) -> file:write(File, Data) end,
     fun() -> file:close(File) end}.

%% @doc Append the resource calls and outcomes of v3___b nodes to
%% their system v3___ nodes, and remove the v3___b outcome in the
%% process.
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

%% @doc Label to put in the decision's diamond.  Currently using the
%% map coordinates, as in the original image.
label(Decision) ->
    [$v,$3|Name] = atom_to_list(Decision),
    Name.

%% @doc Get the column of the decision or response code.  Response
%% code columns are hand-coded to be similar to the origin image,
%% differing where it helps to keep lines from crossing awkwardly.
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

%% @doc Get the row of the decision or response code.
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
