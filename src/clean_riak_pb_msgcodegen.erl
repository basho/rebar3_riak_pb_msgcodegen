%% -------------------------------------------------------------------
%%
%% clean_riak_pb_msgcodegen: rebar3 riak_pb_msgcodegen clean
%%                           operation
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

-module(clean_riak_pb_msgcodegen).

-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, clean).
-define(DEPS, [{default, app_discovery}]).

%% ===================================================================
%% Public API
%% ===================================================================

-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create(
                 [
                  {name, ?PROVIDER                                        },
                  {module, ?MODULE                                        },
                  {bare, true                                             },
                  {deps, ?DEPS                                            },
                  {example, "rebar3 riak_pb_msgcodegen clean"             },
                  {opts, []                                               },
                  {short_desc, "Clean compiled riak_pb_msgcodegen files." },
                  {desc, "Clean compiled riak_pb_msgcodegen files."       },
                  {namespace, riak_pb_msgcodegen                          }
                  ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    rebar_api:info("Cleaning compiled riak_pb_msgcodegen files...", []),
    CSVs = rebar_utils:find_files("src", ".*\\.csv"),
    ErlFiles = [fq_erl_file(CSV) || CSV <- CSVs],
    case ErlFiles of
        [] -> ok;
        _ -> delete_each(ErlFiles)
    end,
    {ok, State}.

-spec format_error(any()) -> iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

%% ===================================================================
%% Internal functions
%% ===================================================================

delete_each([]) ->
    ok;
delete_each([File | Rest]) ->
    case file:delete(File) of
        ok ->
            ok;
        {error, enoent} ->
            ok;
        {error, Reason} ->
            rebar_api:error("Failed to delete ~s: ~p\n", [File, Reason])
    end,
    delete_each(Rest).
