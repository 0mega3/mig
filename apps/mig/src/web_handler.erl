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
%% https://github.com/WWWcool/erlang-review/blob/master/apps/chat/src/web_handler.erl

-module(web_handler).

-export([init/2]).
-export([websocket_init/1]).
-export([websocket_handle/2]).
-export([websocket_info/2]).

-type state() :: #{}.

-spec init(_, _) -> {cowboy_websocket, _, _}.

init(Req, Opts) ->
    {cowboy_websocket, Req, Opts}.

-spec websocket_init(term()) -> {ok, state()}.

websocket_init(_) ->
    logger:alert("websocket_init ..."),
    {ok, #{}}.

-spec websocket_handle(_, state()) -> {reply, _, state()} | {ok, state()}.

websocket_handle({text, Text}, State) ->
    #{<<"type">> := Type, <<"data">> := Data} = jsx:decode(Text, [return_maps]),
    logger:alert("Handle message - ~p", [{Type, Data}]),
    {Reply, NewState} = case {Type, Data} of
        {<<"send_message">>, Message} ->
            {{new_message, Message}, State};
        Unknown ->
            logger:alert("get unknown message - ~p", [Unknown]),
            {disconnect, State}
    end,
    {reply, {text, encode(Reply)}, NewState};
websocket_handle(_Data, State) ->
    logger:alert("get unknown message - ~p", [_Data]),
    {ok, State}.

-spec websocket_info(_, state()) -> {reply, _, state()} | {ok, state()}.

% get message
websocket_info({'DOWN', _Ref, process, _Pid, Reason}, State) ->
    logger:alert("server down with reason - ~p", [Reason]),
    {reply, {text, encode({service_message, <<"Server down">>})}, State};
websocket_info(Info, State) ->
    logger:alert("Get unexpected info - ~p", [Info]),
    {ok, State}.

encode({Type, Data}) ->
    jsx:encode(#{type => Type, data => Data}).
