-module(connect).

-compile([export_all]).
-compile([nowarn_export_all]).

k8s_svc_path() ->
  "erl-cluster-svc.hypermnesia.svc.cluster.local".

-spec my_num() -> integer().
my_num() ->
  {ok, Name} = inet:gethostname(),
  list_to_integer(string:slice(Name, string:len(Name) - 1, string:len(Name))).

-spec get_peers_len() -> integer().
get_peers_len() ->
  {ok, Res} = inet_res:nslookup(k8s_svc_path(), in, a),
  Peers = inet_dns:msg(Res, anlist),
  length(Peers).

all_nodes() ->
  PeerNum = get_peers_len(),
  Peers =
    [list_to_atom("bench@erl-cluster-" ++ integer_to_list(Id) ++ "." ++ k8s_svc_path())
     || Id <- lists:seq(0, PeerNum)],
  Peers.

all_other_nodes() ->
  [P || P <- all_nodes(), P =/= node()].

connect_peers() ->
  St = 5000,
  io:format("sleeping for ~p~n", [St]),
  timer:sleep(St),
  Peers = all_other_nodes(),
  io:format("Peers: ~s~n", [hd(Peers)]),
  Res = [net_adm:ping(P) || P <- Peers],
  io:format("Ping result: ~p~n", [Res]).
