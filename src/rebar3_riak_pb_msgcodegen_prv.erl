%% -------------------------------------------------------------------
%%
%% compile_riak_pb_msgcodegen: rebar3 riak_pb_msgcodegen compile
%%                             operation
%%
%% Copyright (c) 2015 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------
-module(rebar3_riak_pb_msgcodegen_prv).
-behavior(provider).

-export([init/1, 
         do/1, 
         format_error/1]).

-define(PROVIDER, rebar3_riak_pb_msgcodegen).
-define(DEPS, [{default, compile}]).

-include("riak_pb_msgcodegen.hrl").

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},
            {module, ?MODULE},
            {bare, true},
            {deps, ?DEPS},
            {example, "rebar3 rebar3_riak_pb_msgcodegen"},
            {opts, []},
            {short_desc, "Rebar 3 plugin for generating Riak protobufs as part of the riak_pb build process."},
            {desc, "Rebar 3 plugin for generating Riak protobufs as part of the riak_pb build process."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("Compile riak_pb_msgcodegen files...", []),
    {ok, Dir} = file:get_cwd(),
    io:format("Current Directory: ~p~n", [Dir]),
    case rebar_utils:find_files("src", ".csv") of
        []->
            io:format("No files found!~n~n"),
            ok;
        FoundFiles ->
            Targets = [{CSV, fq_erl_file(CSV)} || CSV <- FoundFiles ],
            io:format("Targets: ~p~n", [Targets]),
            generate_each(Targets),
            io:format("*** Done! ~n~n")
    end,
    {ok, State}.

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%%% ===================================================================
%%% Internal functions
%%% ===================================================================

generate_each([]) ->
    ok;

generate_each([{CSV, Erl}|Rest]) ->
    case is_modified(CSV, Erl) of
        false ->
            io:format("Nothing to do!~n"),
            ok;
        true ->
            io:format("Generating Files: ~p~n", [CSV]),
            Tuples = load_csv(CSV),
            Module = generate_module(mod_name(CSV), Tuples),
            Formatted = erl_prettypr:format(Module),
            ok = file:write(Erl, [?MODULE_COMMENTS(CSV), Formatted]),
            rebar_api:console("Generated ~s~n", [Erl])
    end,
    generate_each(Rest).

is_modified(CSV, Erl) ->
    io:format("Checking for modifications~n"),
    io:format("CSV: ~p  Erl: ~p ~n~n", [CSV, Erl]),
    not filelib:is_regular(Erl) orelse
        filelib:last_modified(CSV) > filelib:last_modified(Erl).
    %%MAKE IT WORK
    
    %%Result = filelib:is_regular(Erl),
    %%io:format("Result: ~p~n", [Result]),
    %%Result.

    %% Result = not filelib:is_regular(Erl) orelse
    %%     filelib:last_modified(CSV) > filelib:last_modified(Erl),
    %% io:format("Generate files: ~p~n", [Result]),
    %% Result.

mod_name(SourceFile) ->
    io:format("mod_name Source File: ~p~n", [SourceFile]),
    filename:basename(SourceFile, ".csv").

fq_erl_file(SourceFile) ->
    io:format("fq_erl_file Source File: ~p~n", [SourceFile]),
    filename:join(["src", erl_file(SourceFile)]).

erl_file(SourceFile) ->
    io:format("erl_file Source File: ~p~n", [SourceFile]),
    mod_name(SourceFile) ++ ".erl".

load_csv(SourceFile) ->
    io:format("load_csv Source File: ~p~n", [SourceFile]),
    {ok, Bin} = file:read_file(SourceFile),
    csv_to_tuples(unicode:characters_to_list(Bin, latin1)).

csv_to_tuples(String) ->
    io:format("csv_to_tuples~n"),
    Lines = string:tokens(String, [$\r, $\n]),
    [ begin
          [Code, Message, Proto] = string:tokens(Line, ","),
          {list_to_integer(Code), string:to_lower(Message), Proto ++ "_pb"}
      end
      || Line <- Lines].

generate_module(Name, Tuples) ->
    io:format("Generate Module: Name: ~p~n", [Name]),
    Mod = erl_syntax:attribute(erl_syntax:atom(module),
                               [erl_syntax:atom(Name)]),
    ExportsList = [
                   erl_syntax:arity_qualifier(erl_syntax:atom(Fun),
                                              erl_syntax:integer(1))
                   || Fun <- [msg_type, msg_code, decoder_for] ],
    Exports = erl_syntax:attribute(erl_syntax:atom(export),
                                   [erl_syntax:list(ExportsList)]),
    
    Clauses = generate_msg_type(Tuples) ++
              generate_msg_code(Tuples) ++
              generate_decoder_for(Tuples),
    
    erl_syntax:form_list([Mod, Exports|Clauses]).

generate_decoder_for(Tuples) ->
    Spec = erl_syntax:text("-spec decoder_for(non_neg_integer()) -> module().\n"),
    Name = erl_syntax:atom(decoder_for),
    Clauses = [
               erl_syntax:clause([erl_syntax:integer(Code)],
                                 none,
                                 [erl_syntax:atom(Mod)])
               || {Code, _, Mod} <- Tuples ],
    [ Spec, erl_syntax:function(Name, Clauses) ].

generate_msg_code(Tuples) ->
    Spec = erl_syntax:text("-spec msg_code(atom()) -> non_neg_integer()."),
    Name = erl_syntax:atom(msg_code),
    Clauses = [
               erl_syntax:clause([erl_syntax:atom(Msg)], none,
                                 [erl_syntax:integer(Code)])
               || {Code, Msg, _} <- Tuples ],
    [ Spec, erl_syntax:function(Name, Clauses) ].

generate_msg_type(Tuples) ->
    Spec = erl_syntax:text("-spec msg_type(non_neg_integer()) -> atom()."),
    Name = erl_syntax:atom(msg_type),
    Clauses = [
               erl_syntax:clauses([erl_syntax:integer(Code)], none,
                                  [erl_syntax:atom(Msg)])
               || {Code, Msg, _} <- Tuples ],
    CatchAll = erl_syntax:clause([erl_syntax:underscore()], none,
                                 [erl_syntax:atom(undefined)]),
    [ Spec, erl_syntax:function(Name, Clauses ++ [CatchAll]) ].
