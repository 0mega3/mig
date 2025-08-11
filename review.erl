%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: Apache-2.0
%%
%% Copyright (c) 2022, Artem Fedorenko
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% %CopyrightEnd%
%%

%% This file is derived from the [WWWcool](https://github.com/WWWcool)
%% licensed under Apache 2.0.
%% %% Original source: 
%% https://github.com/WWWcool/erlang-review/blob/master/review.erl

-module(o_A).

-define(ACTUAL_FORMAT_VERSION, 5).
-type timestamp() :: o_B:timestamp_ms().
-type metadata() :: o_C:md().
-type metadata_light() :: o_C:md(light).

-type some_state() :: #{
    version := ?ACTUAL_FORMAT_VERSION,
    id := binary(),
    name := binary(),
    created_at => timestamp(),
    external_id => binary(),
    metadata => #{metadata_light() => metadata()}
}.

-export_type([some_state/0]).

-export([
    version/1,
    id/1,
    name/1
]).

-export([created_at/1]).
-export([external_id/1]).
-export([metadata/1]).

-export([try_get_data_from_metametadata/2]).
-export([some_key/2]).

-spec version(some_state()) -> integer() | undefined.
-spec id(some_state()) -> binary() | undefined.
-spec name(some_state()) -> binary() | undefined.
version(#{version := V}) ->
    V.
id(#{id := V}) ->
    V.
name(#{name := V}) ->
    V.

%% Get created_at from state
-spec created_at(some_state()) -> timestamp() | undefined.
created_at(#{created_at := V}) ->
    V.

%% Get external_id from state
-spec external_id(some_state()) -> timestamp() | undefined.
external_id(S) ->
    maps:get(external_id, S, undefined).

%%--------------------------------------------------------------------
%% @doc
%% Get metadata from state if present
%%
%% @spec metadata(some_state()) -> #{metadata() => metadata()} | undefined.
%% @end
%%--------------------------------------------------------------------
metadata(#{metadata := V}) ->
    V;
metadata(_) ->
    undefined.

try_get_data_from_metametadata(Key, State) ->
    Keys = maps:keys(metadata(State)),
    case lists:foldl(fun some_key/2, {Key, undefined}, Keys) of
     {_, Value} when Value =/= undefined ->
        #{Value := Data} = metadata(State),
        {meta, Data};
    _ ->
        not_found
    end.

-spec some_key(metadata(), {metadata(), metadata() | undefined}) -> {metadata(), metadata() | undefined}.
some_key(_, {Key, Acc}) when Acc =/= undefined ->
    {Key, Acc};
some_key(Key, {Key, _Acc}) ->
    {Key, Key};
some_key(_, {Key, Acc}) ->
    {Key, Acc}.


-module(o_B).
-type timestamp_ms() :: integer().
-export_type([timestamp_ms/0]).

%% API

-spec now() -> timestamp_ms().
now() ->
    erlang:system_time(millisecond).

-module(o_C).

-type context() :: #{namespace() => md()}.

-type namespace() :: binary().
-type md(NIL) ::
    NIL
    | boolean()
    | integer()
    | float()
    %% string
    | binary()
    %% binary
    | {binary, binary()}
    | [md()]
    | #{md() => md()}.
-type md() :: md(nil).

-export_type([context/0]).
-export_type([md/0]).
-export_type([md/1]).

-type state() :: o_A:some_state().

-export([get_id_and_meta/1]).

-spec get_id_and_meta(o_A:some_state()) -> {binary(), md() | undefined}.
get_id_and_meta(State) ->
    {o_A:id(State), o_A:metadata(State)}.

