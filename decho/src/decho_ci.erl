-module(decho_ci).

-export([dump/1]).

dump([]) ->
    dump(file_name());
dump(FileName) ->
    cluster_info:dump_local_node(FileName).

file_name() ->
    lists:flatten(io_lib:format("~s_cluster-info.html", [node()])).
