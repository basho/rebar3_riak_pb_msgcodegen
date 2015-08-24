%% -------------------------------------------------------------------
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
-module(riak_pb_msgcodegen_util).

-export([module_comments/1,
         mod_name/1,
         erl_file/1,
         fq_erl_file/1]).

module_comments(CSV) ->
    ["%% @doc This module contains message code mappings generated from\n%% ",
     CSV,". DO NOT EDIT OR COMMIT THIS FILE!\n"].

mod_name(SourceFile) ->
    filename:basename(SourceFile, ".csv").

erl_file(SourceFile) ->
 mod_name(SourceFile) ++ ".erl".

fq_erl_file(SourceFile) -> 
    filename:join(["src", erl_file(SourceFile)]).



