-module(yang_codegen).

-export([module/3]).

-include_lib("parse_trans/include/codegen.hrl").

module(YangF, ErlMod, Opts) ->
    case file:read_file(YangF) of
	{ok, Bin} ->
	    YangOpts = proplists:get_value(yang, Opts, []),
	    try yang_parser:deep_parse(YangF, YangOpts) of
		{ok, YangForms} ->
		    module_(YangForms, Bin, ErlMod,
			    lists:keydelete(yang, 1, Opts));
		Error ->
		    io:fwrite("Error parsing ~s: ~p~n", [YangF, Error]),
		    Error
	    catch
		Type:Catch ->
		    io:fwrite("Caught ~p:~p~n", [Type,Catch]),
		    {error, caught}
	    end;
	Error1 ->
	    io:fwrite("Error reading file ~s: ~p~n", [YangF, Error1]),
	    Error1
    end.

module_([{module,_,M,Y}] = Yang, Src, ErlMod, Opts) ->
    Forms = [{attribute,1,module,ErlMod},
	     {attribute,2,export,[{module,0},
				  {prefix,0},
				  {revisions,0},
				  {contact,0},
				  {typedef,1},
				  {groupings, 0},
				  {grouping,1},
				  {rpcs, 0},
				  {rpc, 1},
				  {notifications, 0},
				  {notification, 1},
				  {yang, 0},
				  {src, 0}]},
	     module(M),
	     prefix(Y),
	     revisions(Y),
	     contact(Y),
	     typedefs(Y),
	     groupings(Y),
	     grouping_1(Y),
	     rpcs(Y),
	     rpc_1(Y),
	     notifications(Y),
	     notification_1(Y),
	     yang(Yang),
	     src(Src),
	     {eof,1}],
    case compile:forms(Forms, Opts) of
	{ok, ModName, Bin} ->
	    io:fwrite("loading binary (~p)~n", [ModName]),
	    code:load_binary(
	      ModName, "/tmp/" ++ atom_to_list(ModName) ++ ".beam", Bin);
	Other ->
	    Other
    end.

write_to_file(Forms, F) ->
    case file:open(F, [write]) of
	{ok, Fd} ->
	    try io:fwrite(Fd, "~p~n", [Forms])
	    after
		file:close(Fd)
	    end;
	Error ->
	    Error
    end.

module(M) ->
    codegen:gen_function(
      module,
      fun() ->
	      {'$var', M}
      end).

prefix(Y) ->
    Prefix = lists:keyfind(prefix, 1, Y),
    codegen:gen_function(
      prefix,
      fun() ->
	      {'$var', Prefix}
      end).

revisions(Y) ->
    Revs = [R || {revision,_,_,_} = R <- Y],
    codegen:gen_function(
      revisions,
      fun() ->
	      {'$var', Revs}
      end).

contact(Y) ->
    Contact = lists:keyfind(contact, 1, Y),
    codegen:gen_function(
      contact,
      fun() ->
	      {'$var', Contact}
      end).

typedefs(_Y) ->
    codegen:gen_function(
      typedef,
      fun(_) ->
	      []
      end).

groupings(Y) ->
    Names = [Name || {grouping, _, Name, _} <- Y],
    codegen:gen_function(
      groupings,
      fun() ->
	      {'$var', Names}
      end).

grouping_1(Y) ->
    codegen:gen_function_alt(
      grouping,
      [fun({'$var', Name}) ->
	       {'$var', Grp}
       end || {grouping,_,Name,_} = Grp <- Y],
     fun(_) -> error end).

rpcs(Y) ->
    Names = [Name || {rpc,_,Name,_} <- Y],
    codegen:gen_function(
      rpcs,
      fun() ->
	      {'$var', Names}
      end).

rpc_1(Y) ->
    codegen:gen_function_alt(
      rpc,
      [fun({'$var', Name}) ->
	       {'$var', RPC}
       end || {rpc,_,Name,_} = RPC <- Y],
     fun(_) -> error end).

notifications(Y) ->
    Names = [Name || {notification,_,Name,_} <- Y],
    codegen:gen_function(
      notifications,
      fun() ->
	      {'$var', Names}
      end).

notification_1(Y) ->
    codegen:gen_function_alt(
      notification,
      [fun({'$var', Name}) ->
	       {'$var', N}
       end || {notification,_,Name,_} = N <- Y],
     fun(_) -> error end).

yang(_Y) ->
    codegen:gen_function(
      yang,
      fun() ->
	      %% {'$var', Y}
	      []
      end).

src(_Bin) ->
    codegen:gen_function(
      src,
      fun() ->
	      %% {'$var', Bin}
	      []
      end).
