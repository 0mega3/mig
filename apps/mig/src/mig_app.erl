%%
%% %CopyrightBegin%
%%
%% SPDX-License-Identifier: BSD-3-Clause
%%
%% Copyright (c) 2025, Denis Khorkin
%%
%% Licensed under the BSD 3-Clause "New" or "Revised" License;
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%      https://opensource.org/license/bsd-3-clause
%%
%% THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
%% "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
%% LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
%% A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
%% OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
%% SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
%% LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
%% DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
%% THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
%% (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
%% OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
%%
%% %CopyrightEnd%
%%

-module(mig_app).
-moduledoc(#{since => "OTP 27.0"}).
-moduledoc "
This module implements the mig application.
".
-moduledoc #{
   author => [<<"Denis Khorkin">>],
   license => <<"BSD-3-Clause License">>,
   cross_references => [binary]
  }.

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-doc """
```erlang
-callback start(StartType :: start_type(), StartArgs :: term()) ->
    {ok, pid()} | {ok, pid(), State :: term()} | {error, Reason :: term()}.
```

This function is called whenever an application is started using
`start/1,2`, and is to start the processes of the application.
If the application is structured according to the OTP design
principles as a supervision tree, this means starting the top
supervisor of the tree.
""".
-spec start(StartType :: application:start_type(), StartArgs :: term()) ->
    {ok, pid()} | ignore | {error, _}.
start(_StartType, _StartArgs) ->
    mig_sup:start_link().

-doc """
```erlang
-callback stop(State :: term()) -> term().
```

This function is called whenever an application has stopped.
It is intended to be the opposite of `Module:start/2` and is
to do any necessary cleaning up. The return value is ignored.
""".
-spec stop(State :: term()) -> term().
stop(_State) ->
    ok.
