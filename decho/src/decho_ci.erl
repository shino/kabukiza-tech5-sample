-module(decho_ci).

-export([dump/1]).

dump([NodeName]) ->
    dump_to_file(list_to_atom(NodeName), file_name()).

dump_to_file(Node, FileName) ->
    cluster_info:dump_nodes([Node], FileName).

file_name() ->
    lists:flatten(io_lib:format("~s_cluster-info.html", [node()])).
