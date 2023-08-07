-module(connect).

-compile([export_all]).
-compile([nowarn_export_all]).


-spec my_num() -> integer().
my_num() ->
  {ok, Name} = inet:gethostname(),
  list_to_integer(string:slice(Name, string:len(Name) - 1, string:len(Name))).


-spec get_peers_len() -> integer().
get_peers_len() ->
  {ok, Res} = inet_res:nslookup("erl-cluster-svc.hypermnesia.svc.cluster.local", in, a),
  Peers = inet_dns:msg(Res, anlist),
  length(Peers).

connect_peers() ->
  N = my_num(),
  PeerNum= get_peers_len(),
  Peers = [io_lib:format("erl-cluster-~p.erl-cluster-svc.hypermnesia.svc.cluster.local", [Id])
    || Id <- lists:filter(fun (X) -> X =/= N end, lists:seq(0, PeerNum))],
  Res = net_adm:ping(hd(Peers)),
  io:format("Ping result: ~p~n", [Res]).
