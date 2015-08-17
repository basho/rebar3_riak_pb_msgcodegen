%% -------------------------------------------------------------------
%%
%% rebar3_riak_pb_msgcodegen: riak_pb_msgcodegen plugin for rebar3
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

%% @doc Generates a codec-mapping module from a CSV mapping of message
%% codes to messages in .proto files.

-module(rebar3_riak_pb_msgcodegen).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    {ok, State1} = compile_riak_pb_msgcodegen:init(State),
    {ok, State2} = clean_riak_pb_msgcodegen:init(State1),
    {ok, State2}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).
