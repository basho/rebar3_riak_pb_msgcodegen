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
-module('rebar3_riak_pb_msgcodegen_prv').

-export([init/1, 
         do/1, 
         format_error/1]).

-define(PROVIDER, 'rebar3_riak_pb_msgcodegen').
%%-define(DEPS, [app_discovery]).
-define(DEPS, [{default, compile}]).

-include("riak_pb_msgcodegen.hrl").

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},            % The 'user friendly' name of the task
            {module, ?MODULE},            % The module implementation of the task
            {bare, true},                 % The task can be run by the user, always true
            {deps, ?DEPS},                % The list of dependencies
            {example, "rebar3 rebar3_riak_pb_msgcodegen"}, % How to use the plugin
            {opts, []},                   % list of options understood by the plugin
            {short_desc, "Rebar 3 plugin for generating Riak protobufs as part of the riak_pb build process."},
            {desc, "Rebar 3 plugin for generating Riak protobufs as part of the riak_pb build process."}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.


-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    io:format("Current State: ~p~n", [State]),
    rebar_api:info("Compile riak_pb_msgcodegen files...", []),
    case rebar_utils:find_files("src", ".*\\csv") of
        []->
            ok;
        FoundFiles ->
            Targets = [{CSV, ?FQ_ERL_FILE(CSV)} || CSV <- FoundFiles ],
            generate_each(Targets)
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
            io:format("Not modified"),
            ok;
        true ->
            io:format("Modified")
            %% Tuples = load_csv(CSV),
            %% Module = generate_module(?MODE_NAME(CSV), Tuples),
            %% Formatted = erl_prettypr:format(Module),
            %% ok = file:write(Erl, [?MODULE_COMMENTS(CSV), Formatted]),
            %% rebar_api:console("Generated ~s~n", [Erl])
    end,
    generate_each(Rest).

is_modified(CSV, Erl) ->
    not filelib:is_regular(Erl) orelse
        filelib:last_modified(CSV) > filelib:last_modified(Erl).
