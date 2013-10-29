%% Copyright (C) 2013 Evax Software <contact@evax.fr>
%%
%% Permission is hereby granted, free of charge, to any person obtaining a copy
%% of this software and associated documentation files (the "Software"), to deal
%% in the Software without restriction, including without limitation the rights
%% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
%% copies of the Software, and to permit persons to whom the Software is
%% furnished to do so, subject to the following conditions:
%%
%% The above copyright notice and this permission notice shall be included in
%% all copies or substantial portions of the Software.
%%
%% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
%% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
%% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
%% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
%% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
%% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
%% THE SOFTWARE.
%%
-module(coord_mnesia_service).
-behaviour(coord_service).

-export([init/3, nodeup/2, nodedown/2]).
-export([add_node/1]).

-record(state, {
    nodes = []
}).

init(true, Nodes, _Opts) ->
    lager:info("Remember: first node"),
    case mnesia_has_schema() of
        false ->
            lager:info("Remember: creating the initial mnesia schema"),
            mnesia:create_schema([node()]);
        _ -> ok
    end,
    mnesia:start(),
    #state{nodes=Nodes};
init(false, [First|_]=Nodes, _Opts) ->
    lager:info("Remember: additional node"),
    mnesia:start(),
    case mnesia_has_schema() of
        false ->
            lager:info("Remember: not yet part of the cluster, asking to join"),
            rpc:call(First, coord_mnesia_service, add_node, [node()]);
        _ ->
            lager:info("Remember: rejoining the cluster")
    end,
    #state{nodes=Nodes}.

nodeup(Node, #state{nodes=Nodes}=State) ->
    lager:info("Remember: new node for the mnesia coord service: ~p", [Node]),
    State#state{nodes=[Node|Nodes]}.

nodedown(Node, #state{nodes=Nodes}=State) ->
    lager:info("Remember: node down: ~p", [Node]),
    State#state{nodes=lists:delete(Node, Nodes)}.

add_node(Node) ->
    lager:info("Remember: adding newcomer ~p to cluster", [Node]),
    mnesia:change_config(extra_db_nodes, [Node]),
    mnesia:change_table_copy_type(schema, Node, disc_copies),
    lists:map(fun(T) ->
        mnesia:add_table_copy(T, Node, disc_copies)
    end, mnesia:system_info(tables)).

%% private
mnesia_has_schema() ->
    filelib:is_dir(mnesia:system_info(directory)).
