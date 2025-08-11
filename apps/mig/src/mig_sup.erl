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
%% https://github.com/WWWcool/erlang-review/blob/master/apps/chat/src/chat_sup.erl

-module(mig_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-type ip() :: {integer(), integer(), integer(), integer()}.
-type cowboy_port() :: integer().

%% API functions
-spec start_link() ->
    {ok, pid()}.

start_link() ->
    supervisor:start_link(?MODULE, []).

%% Supervisor callbacks

-spec init([]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([]) ->
    ExtPort = 8080,
    {ok, {#{strategy => one_for_all}, [get_cowboy_child_spec({0, 0, 0, 0}, ExtPort)]}}.

-spec get_cowboy_child_spec(ip(), cowboy_port()) ->
    supervisor:child_spec().

get_cowboy_child_spec(IP, Port) ->
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/", cowboy_static, {priv_file, mig, "index.html"}},
            {"/websocket", web_handler, []},
            {"/static/[...]", cowboy_static, {priv_dir, mig, "static"}}
        ]}
    ]),
    ranch:child_spec(
        ?SERVER,
        ranch_tcp,
        [
            {ip, IP},
            {port, Port},
            {num_acceptors, 4}
        ],
        cowboy_clear,
        #{
            env => #{dispatch => Dispatch}
        }
    ).
