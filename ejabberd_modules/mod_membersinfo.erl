-module(mod_membersinfo).

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
  membersinfo_item/4,
  membersinfo_command/3,
  membersinfo_command/4,
  get_sm_features/5,
  process_local_iq/1,
  process_sm_iq/1
]).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("translate.hrl").

-record(state, {host :: binary()}).

-define(NS_MEMBERSINFO, <<"membersinfo">>).

start(Host, Opts) ->
  ?LOG_INFO("Host: ~p, Opts: ~p", [Host, Opts]),
  {ok, [
    {iq_handler, ejabberd_local, ?NS_COMMANDS, process_local_iq},
    {iq_handler, ejabberd_sm, ?NS_COMMANDS, process_sm_iq},

    {hook, disco_local_identity, get_local_identity, 99},
    {hook, disco_local_features, get_local_features, 99},
    {hook, disco_local_items, get_local_commands, 99},
    {hook, disco_sm_identity, get_sm_identity, 99},
    {hook, disco_sm_features, get_sm_features, 99},
    {hook, disco_sm_items, get_sm_commands, 99},

    {hook, adhoc_local_items, membersinfo_item, 101},
    {hook, adhoc_local_commands, membersinfo_command, 101}
  ]}.

stop(Host) ->
%%  ?LOG_INFO("Host: ~p", [Host]),
  ok.

reload(Host, NewOpts, OldOpts) ->
%%  ?LOG_INFO("Host: ~p, NewOpts: ~p, OldOpts: ~p", [Host, NewOpts, OldOpts]),
  ok.

depends(Host, Opts) ->
%%  ?LOG_INFO("Host: ~p, Opts: ~p", [Host, Opts]),
  [{mod_adhoc, hard}].

mod_options(Host) ->
%%  ?LOG_INFO("Host: ~p", [Host]),
  [
    {hostname, none}
  ].

mod_opt_type(hostname) ->
  econf:string().

mod_doc() ->
  #{
    desc => <<"Returns list of MUC members with affiliations">>,
    note => "Note...",
    opts => [{roomname, #{value => "string()", desc => "Room Name"}}],
    example => "muc.example.org"
  }.

-spec get_local_commands(mod_disco:items_acc(), jid(), jid(), binary(), binary()) -> mod_disco:items_acc().
get_local_commands(Acc, From, #jid{server = Server, lserver = LServer} = To, <<"">>, Lang) ->
%%  ?LOG_INFO("Acc: ~p, From: ~p, To: ~p, Node: ~p, Lang: ~p", [Acc, From, To, <<"">>, Lang]),
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
%%  ?LOG_INFO("Acc: ~p, From: ~p, To: ~p, Node: ~p, Lang: ~p", [Acc, From, To, ?NS_COMMANDS, Lang]),
  ejabberd_hooks:run_fold(adhoc_local_items, LServer, {result, []}, [From, To, Lang]);
get_local_commands(Acc, From, To, <<"membersinfo">>, Lang) ->
%%  ?LOG_INFO("Acc: ~p, From: ~p, To: ~p, Node: ~p, Lang: ~p", [Acc, From, To, <<"membersinfo">>, Lang]),
  {result, []};
get_local_commands(Acc, From, To, Node, Lang) ->
%%  ?LOG_INFO("Acc: ~p, From: ~p, To: ~p, Node: ~p, Lang: ~p", [Acc, From, To, Node, Lang]),
  Acc.

-spec get_local_features(mod_disco:features_acc(), jid(), jid(), binary(), binary()) -> mod_disco:features_acc().
get_local_features(Acc, From, To, <<"">>, Lang) ->
%%  ?LOG_INFO("Acc: ~p, From: ~p, To: ~p, Node: ~p, Lang: ~p", [Acc, From, To, <<"">>, Lang]),
  Feats =
    case Acc of
      {result, I} ->
        I;
      _ ->
        []
    end,
  {result, Feats ++ [?NS_COMMANDS]};
get_local_features(Acc, From, To, ?NS_COMMANDS, Lang) ->
%%  ?LOG_INFO("Acc: ~p, From: ~p, To: ~p, Node: ~p, Lang: ~p", [Acc, From, To, ?NS_COMMANDS, Lang]),
  {result, []};
get_local_features(Acc, From, To, <<"membersinfo">>, Lang) ->
%%  ?LOG_INFO("Acc: ~p, From: ~p, To: ~p, Node: ~p, Lang: ~p", [Acc, From, To, <<"membersinfo">>, Lang]),
  {result, [?NS_COMMANDS]};
get_local_features(Acc, From, To, Node, Lang) ->
%%  ?LOG_INFO("Acc: ~p, From: ~p, To: ~p, Node: ~p, Lang: ~p", [Acc, From, To, Node, Lang]),
  Acc.

-spec get_local_identity([identity()], jid(), jid(), binary(), binary()) -> [identity()].
%% On disco info request to the ad-hoc node, return automation/command-list.
get_local_identity(Acc, From, To, ?NS_COMMANDS, Lang) ->
%%  ?LOG_INFO("Acc: ~p, From: ~p, To: ~p, Node: ~p, Lang: ~p", [Acc, From, To, ?NS_COMMANDS, Lang]),
  [#identity{category = <<"automation">>, type = <<"command-list">>, name = translate:translate(Lang, ?T("Commands"))} | Acc];
get_local_identity(Acc, From, To, <<"membersinfo">>, Lang) ->
%%  ?LOG_INFO("Acc: ~p, From: ~p, To: ~p, Node: ~p, Lang: ~p", [Acc, From, To, <<"membersinfo">>, Lang]),
  [#identity{category = <<"automation">>, type = <<"command-node">>, name = translate:translate(Lang, ?T("MembersInfo"))} | Acc];
get_local_identity(Acc, From, To, Node, Lang) ->
%%  ?LOG_INFO("Acc: ~p, From: ~p, To: ~p, Node: ~p, Lang: ~p", [Acc, From, To, Node, Lang]),
  Acc.

%%commands_result(Allow, From, To, Request) ->
%%  case Allow of
%%    deny ->
%%      Lang = Request#adhoc_command.lang,
%%      {error, xmpp:err_forbidden(?T("Access denied by service policy"), Lang)};
%%    allow ->
%%      membersinfo_command(From, To, Request)
%%  end.

-spec membersinfo_item(mod_disco:items_acc(), jid(), jid(), binary()) -> {result, [disco_item()]}.
membersinfo_item(Acc, From, #jid{server = Server} = To, Lang) ->
%%  ?LOG_INFO("Acc: ~p, From: ~p, To: ~p, Lang: ~p", [Acc, From, To, Lang]),
  Items =
    case Acc of
      {result, I} ->
        I;
      _ ->
        []
    end,
  Nodes = [
    #disco_item{jid = jid:make(Server),
    node = <<"membersinfo">>,
    name = translate:translate(Lang, ?T("MembersInfo"))}
  ],
  {result, Items ++ Nodes}.

membersinfo_command(From, To, #adhoc_command{lang = Lang, node = Node, sid = SID, xdata = XData, action = Action} = Request) ->
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

-spec membersinfo_command(adhoc_command(), jid(), jid(), adhoc_command()) ->
  adhoc_command()
  | {error, stanza_error()}.
membersinfo_command(Acc, From, To, #adhoc_command{lang = Lang, node = <<"membersinfo">>, action = Action, xdata = XData} = Request) ->
%%  ?LOG_INFO("Acc: ~p, From: ~p, To: ~p, Request: ~p", [Acc, From, To, Request]),
  if Action == execute ->
    [#xdata_field{var = <<"room">>, values = [RoomJID]}] = XData#xdata.fields,
    JID = jid:decode(RoomJID),
    L1 = mod_muc_admin:get_room_occupants(JID#jid.luser, JID#jid.lserver),
    L2 = mod_muc_admin:get_room_affiliations_v3(JID#jid.luser, JID#jid.lserver),
    [{OwnerJID, _, _}] = lists:filter(fun({_OwnerJID, Role, _Reason}) -> Role =:= owner end, L2),
    Owner = (jid:decode(OwnerJID))#jid.luser,
    IsOwner =
      fun(UserJID) ->
        Owner =:= (jid:decode(UserJID))#jid.luser
      end,
    F =
      fun({UserJID_, UserNick, _Role} = Entry) ->
        case IsOwner(UserJID_) of
          true ->
            {UserJID_, UserNick, <<"owner">>};
          false ->
            Entry
        end
      end,
    Fields =
      case lists:map(F, L1) of
        [] -> [];
        [_|_] = L ->
          [#xdata_field{label = Affiliation, var = Nick} || {_Jid, Nick, Affiliation} <- L]
      end,
    AdHocCommand =
      #adhoc_command{
        status = completed,
        xdata = #xdata{
          type = result,
          title = <<"Members Info">>,
          fields = Fields
        }
      },
    xmpp_util:make_adhoc_response(Request, AdHocCommand);
    true ->
      Txt = ?T("Incorrect value of 'action' attribute"),
      {error, xmpp:err_bad_request(Txt, Lang)}
  end;
membersinfo_command(Acc, From, To, Request) ->
%%  ?LOG_INFO("Acc: ~p, From: ~p, To: ~p, Request: ~p", [Acc, From, To, Request]),
  Acc.

-spec get_sm_features(mod_disco:features_acc(), jid(), jid(), binary(), binary()) -> mod_disco:features_acc().
get_sm_features(Acc, From, To, <<"">>, Lang) ->
%%  ?LOG_INFO("Acc: ~p, From: ~p, To: ~p, Node: ~p, Lang: ~p", [Acc, From, To, <<"">>, Lang]),
  Feats =
    case Acc of
      {result, I} ->
        I;
      _ ->
        []
    end,
  {result, Feats ++ [?NS_COMMANDS]};
get_sm_features(Acc, From, To, ?NS_COMMANDS, Lang) ->
%%  ?LOG_INFO("Acc: ~p, From: ~p, To: ~p, Node: ~p, Lang: ~p", [Acc, From, To, ?NS_COMMANDS, Lang]),
  {result, []};
get_sm_features(Acc, From, To, Node, Lang) ->
%%  ?LOG_INFO("Acc: ~p, From: ~p, To: ~p, Node: ~p, Lang: ~p", [Acc, From, To, Node, Lang]),
  Acc.

-spec process_local_iq(iq()) ->
  iq()
  | ignore.
process_local_iq(IQ) ->
%%  ?LOG_INFO("IQ: ~p", [IQ]),
  process_adhoc_request(IQ, local).

-spec process_sm_iq(iq()) ->
  iq()
  | ignore.
process_sm_iq(IQ) ->
%%  ?LOG_INFO("IQ: ~p", [IQ]),
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
  Room = join_lines(xmpp_util:get_xdata_values(<<"room">>, XData)),
  Body = join_lines(xmpp_util:get_xdata_values(<<"body">>, XData)),
  Packet = #message{
    from = From,
    to = To,
    type = headline,
    body = xmpp:mk_text(Body),
    subject = xmpp:mk_text(Room)
  },
  Proc = gen_mod:get_module_proc(LServer, ?MODULE),
  case {Node, Body} of
    {?NS_MEMBERSINFO, _} ->
      if	Confirm ->
        gen_server:cast(Proc, {membersinfo, Packet});
        true ->
          ok
      end;
    {_, <<>>} ->
      {error, xmpp:err_not_acceptable(?T("No room provided"), Lang)};
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
  Fs = [
    #xdata_field{type = 'text-single',
      var = <<"room">>,
      label = translate:translate(Lang, ?T("Room")),
      values = []
    }
  ],
  #xdata{
    type = form,
    title = get_title(Lang, Node),
    fields = [#xdata_field{type = hidden, var = <<"FORM_TYPE">>, values = [?NS_ADMIN]} | Fs]
  }.

get_title(Lang, <<"membersinfo">>) ->
  translate:translate(Lang, ?T("Get members information")).
