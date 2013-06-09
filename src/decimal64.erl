%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2012, Tony Rogvall
%%% @doc
%%%    decimal64 
%%% @end
%%% Created :  7 Dec 2012 by Tony Rogvall <tony@rogvall.se>

-module(decimal64).

-export([from_float/2,   to_float/2]).
-export([from_integer/2, to_integer/2]).
-export([from_binary/2,  to_binary/2]).
-export([from_string/2,  to_string/2]).
-export([from_string/1]).

-export([add/3]).
-export([subtract/3]).
-export([multiply/3]).
-export([divide/3]).
-export([negate/2]).

-define(TEST, true).
-export([test/0]).

-type decimal64() :: integer().
-type fraction_digits() :: 1..18.

-define(is_fraction_digits(F), ((F)>=1 andalso (F)=<18)).

-spec from_integer(A::integer(), F::fraction_digits()) ->
			  decimal64().
from_integer(A,F) when is_integer(A), ?is_fraction_digits(F) ->
    A * f10(F).

-spec to_integer(A::decimal64(), F::fraction_digits()) ->
			integer().

to_integer(A,F) when is_integer(A), ?is_fraction_digits(F) ->
    A div f10(F).

-spec from_float(A::float(), F::fraction_digits()) ->
			decimal64().

from_float(A,F) when is_float(A),A<0.0, ?is_fraction_digits(F) ->
    -from_float_(-A,F);
from_float(A,F) when is_float(A), ?is_fraction_digits(F) ->
    from_float_(A,F).

from_float_(A, F) ->
    trunc(A * f10(F)).

-spec to_float(A::decimal64(), F::fraction_digits()) ->
		      float().
to_float(A, F) when is_integer(A), A<0, ?is_fraction_digits(F) ->
    -to_float_(-A,F);
to_float(A, F) when is_integer(A), ?is_fraction_digits(F) ->
    to_float_(A,F).

to_float_(A,F) ->
    A / f10(F).

-spec from_binary(A::binary(), F::fraction_digits()) ->
			 decimal64().

from_binary(<<A:64/signed>>,F) when ?is_fraction_digits(F) ->
    A.

-spec to_binary(A::decimal64(), F::fraction_digits()) ->
		       binary().

to_binary(A, F) when ?is_fraction_digits(F) ->
    <<A:64>>.

-spec add(A::decimal64(), B::decimal64(), F::fraction_digits()) ->
		 decimal64().
add(A,B,F) when is_integer(A), is_integer(B), ?is_fraction_digits(F) ->
    A+B.

-spec subtract(A::decimal64(), B::decimal64(), F::fraction_digits()) ->
		      decimal64().

subtract(A,B,F) when is_integer(A), is_integer(B), ?is_fraction_digits(F) ->
    A-B.

-spec multiply(A::decimal64(), B::decimal64(), F::fraction_digits()) ->
		      decimal64().

multiply(A,B,F) when is_integer(A), is_integer(B), ?is_fraction_digits(F) ->
    (A*B) div f10(F).

-spec divide(A::decimal64(), B::decimal64(), F::fraction_digits()) ->
		    decimal64().

divide(A,B,F) when is_integer(A), is_integer(B), ?is_fraction_digits(F) ->
    ((A * f10(F)) div B).

-spec negate(A::decimal64(), F::fraction_digits()) ->
		    decimal64().
negate(A,F) when is_integer(A), ?is_fraction_digits(F) ->
    -A.

-spec to_string(A::decimal64(), F::fraction_digits()) ->
		       string().

to_string(A,F) when is_integer(A),A < 0, ?is_fraction_digits(F) ->
    "-"++to_string_(-A,F);
to_string(A,F) when is_integer(A), ?is_fraction_digits(F) ->
    to_string_(A, F).

%% fixme: division not needed!
to_string_(A,F) ->
    L = integer_to_list(A),
    Len = length(L),
    {H,T} = if Len =< F ->
		    lists:split(1, lists:duplicate(F-Len+1,$0)++L);
	       true -> 
		    lists:split(Len-F, L)
	    end,
    case ztrim(T) of
	[] -> H++".0";
	Z  -> H++"."++Z
    end.
    
ztrim([C|Cs]) ->
    case ztrim(Cs) of
	[] when C=:=$0 -> [];
	Cs1 -> [C|Cs1]
    end;
ztrim([]) -> [].


-spec from_string(string(), F::fraction_digits()) ->
			 {decimal64(), string()} |
			 {error, no_decimal64}.

from_string([$+|String],F) when ?is_fraction_digits(F) ->
    from_string2_(String,F);
from_string([$-|String],F) when ?is_fraction_digits(F) ->
    case from_string2_(String,F) of
	E = {error,_} -> E;
	{N,Cs} -> {-N,Cs}
    end;
from_string(String,F) when ?is_fraction_digits(F) ->
    from_string2_(String,F).

from_string2_([C|Cs],F) when C>=$0,C=<$9 ->
    from_string2__(Cs,(C-$0),F);
from_string2_(_,_F) ->
    {error, no_decimal64}.

from_string2__([C|Cs],N,F) when C>=$0,C=<$9 ->
    from_string2__(Cs,N*10+(C-$0),F);
from_string2__([$.,C|Cs],N,F) when C>=$0,C=<$9 ->
    from_string2___(Cs, N*10+(C-$0), F-1);
from_string2__([$.|_],_N,_F) ->
    {error, no_decimal64};
from_string2__(Cs,N,F) ->
    {from_integer(N,F), Cs}.

from_string2___([C|Cs],N,F) when C>=$0,C=<$9,F>0 ->
    from_string2___(Cs,N*10+(C-$0),F-1);
from_string2___([C|Cs],N,F) when C>=$0,C=<$9,F=:=0 ->  %% ignore
    from_string2___(Cs,N,F);
from_string2___(Cs,N,F) ->
    {N*f10(F),Cs}.

%% @doc
%%  Dynamic from_string find the number of fraction bits
%% @end

-spec from_string(string()) ->
			 {decimal64(), fraction_digits(), string()} |
			 {error, no_decimal64}.

from_string([$+|String]) ->
    from_string1_(String);
from_string([$-|String]) ->
    case from_string1_(String) of
	E = {error,_} -> E;
	{N,F,Cs} -> {-N,F,Cs}
    end;
from_string(String) ->
    from_string1_(String).

from_string1_([C|Cs]) when C>=$0,C=<$9 ->
    from_string1_(Cs, (C-$0));
from_string1_(_) ->
    {error, no_decimal64}.

from_string1_([C|Cs], N) when C>=$0,C=<$9 ->
    from_string1_(Cs, N*10+(C-$0));
from_string1_([$.,C|Cs], N) when C>=$0,C=<$9 ->
    from_string1__(Cs, N*10+(C-$0), 1);
from_string1_([$.|_], _N) ->
    {error, no_decimal64};    
from_string1_(Cs, N) ->
    {N,0,Cs}.

from_string1__([C|Cs],N,F) when C>=$0,C=<$9 ->
    from_string1__(Cs,N*10+(C-$0),F+1);
from_string1__(Cs,N,F) ->
    {N,F,Cs}.

%%
%% Internal
%%
f10(F) ->
    case F of
	0  -> 1;
	1  -> 10;
	2  -> 100;
	3  -> 1000;
	4  -> 10000;
	5  -> 100000;
	6  -> 1000000;
	7  -> 10000000;
	8  -> 100000000;
	9  -> 1000000000;
	10 -> 10000000000;
	11 -> 100000000000;
	12 -> 1000000000000;
	13 -> 10000000000000;
	14 -> 100000000000000;
	15 -> 1000000000000000;
	16 -> 10000000000000000;
	17 -> 100000000000000000;
	18 -> 1000000000000000000
    end.

-ifdef(TEST).

test() ->
    %% test integers
    lists:foreach(
      fun(I) ->
	      test_decimal("0", "0.0", I),
	      test_decimal("-1", "-1.0", I),
	      test_decimal("-10", "-10.0", I),
	      test_decimal("-100", "-100.0", I),
	      test_decimal("100", "100.0", I),
	      test_decimal("10", "10.0", I),
	      test_decimal("1", "1.0", I)
      end, lists:seq(1,16)),
    
    %% test max representation 
    lists:foreach(
      fun(I) ->
	      {H,T} = lists:split(19-I, "9223372036854775807"),
	      test_decimal(H++"."++T,I)
      end, lists:seq(1,18)),
    %% test min representation 
    lists:foreach(
      fun(I) ->
	      {H,T} = lists:split(20-I, "-9223372036854775808"),
	      test_decimal(H++"."++T,I)
      end, lists:seq(1,18)),

    %% some arithmetic
    {A,[]} = from_string("3.14", 2),
    {B,2,[]} = from_string("3.14"),
    C = multiply(A, B, 2),
    "9.85" = to_string(C, 2),
    ok.


test_decimal(Sin, F) ->
    test_decimal(Sin, Sin, F).

test_decimal(Sin, Sout, F) ->
    io:format("Test: in=~s, expect=~s, f=~w\n", [Sin, Sout,F]),
    {N,[]} = from_string(Sin, F),  %% convert to internal N
    B = to_binary(N, F),           %% store and 
    N = from_binary(B, F),         %% fetch preserve all bits
    case to_string(N, F) of        %% convert back to string
	Sout ->  %% the expected output
	    ok;
	Sd ->    %% nope fail
	    io:format("Sin=~s, Sd=~s\n", [Sin,Sd]),
	    erlang:error(bad_input)
    end.

-endif.
