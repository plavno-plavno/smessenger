-module(mod_myrooms).

-behaviour(gen_mod).

-export([
  start/2,
  stop/1,
  reload/3,
  depends/2,
  mod_options/1,
  mod_opt_type/1,
  mod_doc/0
]).

-export([
  get_local_commands/5,
  get_local_identity/5,
  get_local_features/5,
  myrooms_item/4,
  myrooms_command/3,
  myrooms_command/4,
  get_sm_features/5,
  process_local_iq/1,
  process_sm_iq/1
]).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("translate.hrl").

-record(state, {host :: binary()}).

-define(NS_MYROOMS, <<"myrooms">>).

start(Host, Opts) ->
  ?LOG_INFO("[~p:~p/~p, ~p] Host: ~p, Opts: ~p", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE, Host, Opts]),
  {ok, [
    {iq_handler, ejabberd_local, ?NS_COMMANDS, process_local_iq},
    {iq_handler, ejabberd_sm, ?NS_COMMANDS, process_sm_iq},

    {hook, disco_local_identity, get_local_identity, 99},
    {hook, disco_local_features, get_local_features, 99},
    {hook, disco_local_items, get_local_commands, 99},
    {hook, disco_sm_identity, get_sm_identity, 99},
    {hook, disco_sm_features, get_sm_features, 99},
    {hook, disco_sm_items, get_sm_commands, 99},

    {hook, adhoc_local_items, myrooms_item, 101},
    {hook, adhoc_local_commands, myrooms_command, 101}
  ]}.

stop(Host) ->
%%  ?LOG_INFO("[~p:~p/~p, ~p] Host: ~p", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE, Host]),
  ok.

reload(Host, NewOpts, OldOpts) ->
%%  ?LOG_INFO("[~p:~p/~p, ~p] Host: ~p, NewOpts: ~p, OldOpts: ~p", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE, Host, NewOpts, OldOpts]),
  ok.

depends(Host, Opts) ->
%%  ?LOG_INFO("[~p:~p/~p, ~p] Host: ~p, Opts: ~p", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE, Host, Opts]),
  [{mod_adhoc, hard}].

mod_options(Host) ->
%%  ?LOG_INFO("[~p:~p/~p, ~p] Host: ~p", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE, Host]),
  [
    {hostname, none}
  ].

mod_opt_type(hostname) ->
%%  ?LOG_INFO("[~p:~p/~p, ~p]", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE]),
  econf:string().

mod_doc() ->
%%  ?LOG_INFO("[~p:~p/~p, ~p]", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE]),
  #{
    desc => <<"Returns list of MUC rooms where I'm joined">>,
    note => "Note...",
    opts => [{hostname, #{value => "string()", desc => "XMPP host"}}],
    example => "muc.example.org"
  }.

-spec get_local_commands(mod_disco:items_acc(), jid(), jid(), binary(), binary()) -> mod_disco:items_acc().
get_local_commands(Acc, From, #jid{server = Server, lserver = LServer} = To, <<"">>, Lang) ->
%%  ?LOG_INFO("[~p:~p/~p, ~p] Acc: ~p, From: ~p, To: ~p, Node: ~p, Lang: ~p", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE, Acc, From, To, <<"">>, Lang]),
  Display = mod_adhoc_opt:report_commands_node(LServer),
  case Display of
    false ->
      Acc;
    _ ->
      Items =
        case Acc of
          {result, I} ->
            I;
          _ ->
            []
        end,
      Nodes = [
        #disco_item{jid = jid:make(Server),
          node = ?NS_COMMANDS,
          name = translate:translate(Lang, ?T("Commands"))}
      ],
      {result, Items ++ Nodes}
  end;
get_local_commands(Acc, From, #jid{lserver = LServer} = To, ?NS_COMMANDS, Lang) ->
%%  ?LOG_INFO("[~p:~p/~p, ~p] Acc: ~p, From: ~p, To: ~p, Node: ~p, Lang: ~p",
%%            [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE, Acc, From, To, ?NS_COMMANDS, Lang]),
  ejabberd_hooks:run_fold(adhoc_local_items, LServer, {result, []}, [From, To, Lang]);
get_local_commands(Acc, From, To, <<"myrooms">>, Lang) ->
%%  ?LOG_INFO("[~p:~p/~p, ~p] Acc: ~p, From: ~p, To: ~p, Node: ~p, Lang: ~p",
%%            [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE, Acc, From, To, <<"myrooms">>, Lang]),
  {result, []};
get_local_commands(Acc, From, To, Node, Lang) ->
%%  ?LOG_INFO("[~p:~p/~p, ~p] Acc: ~p, From: ~p, To: ~p, Node: ~p, Lang: ~p",
%%            [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE, Acc, From, To, Node, Lang]),
  Acc.

-spec get_local_features(mod_disco:features_acc(), jid(), jid(), binary(), binary()) -> mod_disco:features_acc().
get_local_features(Acc, From, To, <<"">>, Lang) ->
%%  ?LOG_INFO("[~p:~p/~p, ~p] Acc: ~p, From: ~p, To: ~p, Node: ~p, Lang: ~p",
%%            [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE, Acc, From, To, <<"">>, Lang]),
  Feats =
    case Acc of
      {result, I} ->
        I;
      _ ->
        []
    end,
  {result, Feats ++ [?NS_COMMANDS]};
get_local_features(Acc, From, To, ?NS_COMMANDS, Lang) ->
%%  ?LOG_INFO("[~p:~p/~p, ~p] Acc: ~p, From: ~p, To: ~p, Node: ~p, Lang: ~p",
%%            [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE, Acc, From, To, ?NS_COMMANDS, Lang]),
  {result, []};
get_local_features(Acc, From, To, <<"myrooms">>, Lang) ->
%%  ?LOG_INFO("[~p:~p/~p, ~p] Acc: ~p, From: ~p, To: ~p, Node: ~p, Lang: ~p",
%%            [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE, Acc, From, To, <<"myrooms">>, Lang]),
  {result, [?NS_COMMANDS]};
get_local_features(Acc, From, To, Node, Lang) ->
%%  ?LOG_INFO("[~p:~p/~p, ~p] Acc: ~p, From: ~p, To: ~p, Node: ~p, Lang: ~p",
%%            [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE, Acc, From, To, Node, Lang]),
  Acc.

-spec get_local_identity([identity()], jid(), jid(), binary(), binary()) -> [identity()].
get_local_identity(Acc, From, To, ?NS_COMMANDS, Lang) ->
%%  ?LOG_INFO("[~p:~p/~p, ~p] Acc: ~p, From: ~p, To: ~p, Node: ~p, Lang: ~p",
%%            [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE, Acc, From, To, ?NS_COMMANDS, Lang]),
  [#identity{category = <<"automation">>, type = <<"command-list">>, name = translate:translate(Lang, ?T("Commands"))} | Acc];
get_local_identity(Acc, From, To, <<"myrooms">>, Lang) ->
%%  ?LOG_INFO("[~p:~p/~p, ~p] Acc: ~p, From: ~p, To: ~p, Node: ~p, Lang: ~p",
%%            [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE, Acc, From, To, <<"myrooms">>, Lang]),
  [#identity{category = <<"automation">>, type = <<"command-node">>, name = translate:translate(Lang, ?T("MyRooms"))} | Acc];
get_local_identity(Acc, From, To, Node, Lang) ->
%%  ?LOG_INFO("[~p:~p/~p, ~p] Acc: ~p, From: ~p, To: ~p, Node: ~p, Lang: ~p",
%%    [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE, Acc, From, To, Node, Lang]),
  Acc.

-spec myrooms_item(mod_disco:items_acc(), jid(), jid(), binary()) -> {result, [disco_item()]}.
myrooms_item(Acc, From, #jid{server = Server} = To, Lang) ->
%%  ?LOG_INFO("[~p:~p/~p, ~p] Acc: ~p, From: ~p, To: ~p, Lang: ~p",
%%            [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE, Acc, From, To, Lang]),
  Items =
    case Acc of
      {result, I} ->
        I;
      _ ->
        []
    end,
  Nodes = [
    #disco_item{jid = jid:make(Server),
    node = <<"myrooms">>,
    name = translate:translate(Lang, ?T("MyRooms"))}
  ],
  {result, Items ++ Nodes}.

myrooms_command(From, To, #adhoc_command{lang = Lang, node = Node, sid = SID, xdata = XData, action = Action} = Request) ->
  if Action == cancel ->
    %% User cancels request
    #adhoc_command{status = canceled, lang = Lang, node = Node, sid = SID};
    XData == undefined andalso Action == execute ->
      %% User requests form
      Form = generate_adhoc_form(Lang, Node, To#jid.lserver),
      xmpp_util:make_adhoc_response(#adhoc_command{status = executing, lang = Lang, node = Node, sid = SID, xdata = Form});
    XData /= undefined andalso (Action == execute orelse Action == complete) ->
      case handle_adhoc_form(From, To, Request) of
        ok ->
          #adhoc_command{lang = Lang, node = Node, sid = SID, status = completed};
        {error, _} = Err ->
          Err
      end;
    true ->
      Txt = ?T("Unexpected action"),
      {error, xmpp:err_bad_request(Txt, Lang)}
  end.

-spec myrooms_command(adhoc_command(), jid(), jid(), adhoc_command()) ->
  adhoc_command()
  | {error, stanza_error()}.
myrooms_command(Acc, From, To, #adhoc_command{lang = Lang, node = <<"myrooms">>, action = Action} = Request) ->
%%  ?LOG_INFO("[~p:~p/~p, ~p] Acc: ~p, From: ~p, To: ~p, Request: ~p",
%%            [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE, Acc, From, To, Request]),
  if Action == execute ->
    Fields =
      case mod_muc_admin:get_user_rooms(From#jid.luser, From#jid.lserver) of
        [] -> [];
        L -> [#xdata_field{label = <<"room-ids">>, var = <<"xxx">>, values = L}]
      end,
    AdHocCommand =
      #adhoc_command{
        status = completed,
        xdata = #xdata{
          type = result,
          title = <<"My Rooms">>,
          fields = Fields
        }
      },
    xmpp_util:make_adhoc_response(Request, AdHocCommand);
    true ->
      Txt = ?T("Incorrect value of 'action' attribute"),
      {error, xmpp:err_bad_request(Txt, Lang)}
  end;
myrooms_command(Acc, From, To, Request) ->
%%  ?LOG_INFO("[~p:~p/~p, ~p] Acc: ~p, From: ~p, To: ~p, Request: ~p",
%%            [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE, Acc, From, To, Request]),
  Acc.

-spec get_sm_features(mod_disco:features_acc(), jid(), jid(), binary(), binary()) -> mod_disco:features_acc().
get_sm_features(Acc, From, To, <<"">>, Lang) ->
%%  ?LOG_INFO("[~p:~p/~p, ~p] Acc: ~p, From: ~p, To: ~p, Node: ~p, Lang: ~p",
%%            [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE, Acc, From, To, <<"">>, Lang]),
  Feats =
    case Acc of
      {result, I} ->
        I;
      _ ->
        []
    end,
  {result, Feats ++ [?NS_COMMANDS]};
get_sm_features(Acc, From, To, ?NS_COMMANDS, Lang) ->
%%  ?LOG_INFO("[~p:~p/~p, ~p] Acc: ~p, From: ~p, To: ~p, Node: ~p, Lang: ~p",
%%            [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE, Acc, From, To, ?NS_COMMANDS, Lang]),
  {result, []};
get_sm_features(Acc, From, To, Node, Lang) ->
%%  ?LOG_INFO("[~p:~p/~p, ~p] Acc: ~p, From: ~p, To: ~p, Node: ~p, Lang: ~p",
%%            [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE, Acc, From, To, Node, Lang]),
  Acc.

-spec process_local_iq(iq()) ->
  iq()
  | ignore.
process_local_iq(IQ) ->
%%  ?LOG_INFO("[~p:~p/~p, ~p] IQ: ~p", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE, IQ]),
  process_adhoc_request(IQ, local).

-spec process_sm_iq(iq()) ->
  iq()
  | ignore.
process_sm_iq(IQ) ->
%%  ?LOG_INFO("[~p:~p/~p, ~p] IQ: ~p", [?MODULE, ?FUNCTION_NAME, ?FUNCTION_ARITY, ?LINE, IQ]),
  process_adhoc_request(IQ, sm).

-spec process_adhoc_request(iq(), sm | local) ->
  iq()
  | ignore.
process_adhoc_request(#iq{from = From, to = To, type = set, lang = Lang, sub_els = [#adhoc_command{} = SubEl]} = IQ, Type) ->
  Host = To#jid.lserver,
  Res =
    case Type of
      local ->
        ejabberd_hooks:run_fold(adhoc_local_commands, Host, empty, [From, To, fix_lang(Lang, SubEl)]);
      sm ->
        ejabberd_hooks:run_fold(adhoc_sm_commands, Host, empty, [From, To, fix_lang(Lang, SubEl)])
    end,
  case Res of
    ignore ->
      ignore;
    empty ->
      Txt = ?T("No hook has processed this command"),
      xmpp:make_error(IQ, xmpp:err_item_not_found(Txt, Lang));
    {error, Error} ->
      xmpp:make_error(IQ, Error);
    Command ->
      xmpp:make_iq_result(IQ, Command)
  end;
process_adhoc_request(#iq{} = IQ, _Hooks) ->
  xmpp:make_error(IQ, xmpp:err_bad_request()).

handle_adhoc_form(From, #jid{lserver = LServer} = To, #adhoc_command{lang = Lang, node = Node, xdata = XData}) ->
  Confirm =
    case xmpp_util:get_xdata_values(<<"confirm">>, XData) of
      [<<"true">>] -> true;
      [<<"1">>] -> true;
      _ -> false
    end,
  Subject = join_lines(xmpp_util:get_xdata_values(<<"subject">>, XData)),
  Body = join_lines(xmpp_util:get_xdata_values(<<"body">>, XData)),
  Packet = #message{
    from = From,
    to = To,
    type = headline,
    body = xmpp:mk_text(Body),
    subject = xmpp:mk_text(Subject)
  },
  Proc = gen_mod:get_module_proc(LServer, ?MODULE),
  case {Node, Body} of
    {?NS_MYROOMS, _} ->
      if	Confirm ->
        gen_server:cast(Proc, {myrooms, Packet});
        true ->
          ok
      end;
    {_, <<>>} ->
      {error, xmpp:err_not_acceptable(?T("No body provided for ..."), Lang)};
    Junk ->
      %% This can't happen, as we haven't registered any other command nodes.
      ?ERROR_MSG("Unexpected node/body = ~p", [Junk]),
      {error, xmpp:err_internal_server_error()}
  end.

-spec fix_lang(binary(), adhoc_command()) -> adhoc_command().
fix_lang(Lang, #adhoc_command{lang = <<>>} = Cmd) ->
  Cmd#adhoc_command{lang = Lang};
fix_lang(_, Cmd) ->
  Cmd.

join_lines([]) ->
  <<>>;
join_lines(Lines) ->
  join_lines(Lines, []).
join_lines([Line|Lines], Acc) ->
  join_lines(Lines, [<<"\n">>,Line|Acc]);
join_lines([], Acc) ->
  %% Remove last newline
  iolist_to_binary(lists:reverse(tl(Acc))).

generate_adhoc_form(Lang, Node, _ServerHost) ->
%%  LNode = tokenize(Node),
%%  {OldSubject, OldBody} =
%%    if (LNode == ?NS_ADMINL("edit-motd")) or (LNode == ?NS_ADMINL("edit-motd-allhosts")) ->
%%      get_stored_motd(ServerHost);
%%      true ->
%%        {<<>>, <<>>}
%%    end,
%%  Fs =
%%    if (LNode == ?NS_ADMINL("delete-motd")) or (LNode == ?NS_ADMINL("delete-motd-allhosts")) ->
%%      [
%%        #xdata_field{
%%          type = boolean,
%%          var = <<"confirm">>,
%%          label = translate:translate(Lang, ?T("Really delete message of the day?")),
%%          values = [<<"true">>]}
%%      ];
%%      true ->
%%        [
%%          #xdata_field{type = 'text-single',
%%            var = <<"subject">>,
%%            label = translate:translate(Lang, ?T("Subject")),
%%            values = vvaluel(OldSubject)
%%          },
%%          #xdata_field{
%%            type = 'text-multi',
%%            var = <<"body">>,
%%            label = translate:translate(Lang, ?T("Message body")),
%%            values = vvaluel(OldBody)
%%          }
%%        ]
%%    end,
  Fs = [],
  #xdata{
    type = form,
    title = get_title(Lang, Node),
    fields = [#xdata_field{type = hidden, var = <<"FORM_TYPE">>, values = [?NS_ADMIN]} | Fs]
  }.

get_title(Lang, <<"myrooms">>) ->
  translate:translate(Lang, ?T("Get my rooms")).
