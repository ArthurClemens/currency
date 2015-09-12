%% -*- coding: utf-8 -*-

-module('currency').

%% API exports
-export([
	parse/1
]).

%% Test exports
-export([
	amounts/1
]).

-define(EUR, <<"€"/utf8>>).
-define(EUR1, <<130,172>>).
-define(GBP, <<"£">>).
-define(USD, <<"$">>).


%%====================================================================
%% API functions
%%====================================================================

-spec parse(Text) -> [{'currency',binary()} | {'whole','undefined' | integer()} | {'cents','undefined' | integer()}] when
    Text :: binary().
parse(Text) ->
    Currency = case currency_from_symbols(symbols(Text)) of
    	undefined -> currency_from_words(words(Text));
    	C -> C
    end,
    [Whole, Cents] = case amounts(Text) of
    	undefined -> [undefined, undefined];
    	[W, Ct] -> [W, Ct];
    	_ -> [undefined, undefined]
    end,
    [
    	{currency, Currency},
    	{whole, Whole},
    	{cents, Cents}
    ].


    
%%====================================================================
%% Internal functions
%%====================================================================

-spec words(Text) -> list(binary()) when
    Text :: binary().
words(Text) -> 
    Tokens = re:split(Text, <<"\\W|\\d|\\s|\\.\\,">>, [{return, binary}]),
    lists:filter(fun(T) ->
        T =/= <<>>
    end, Tokens).


-spec symbols(Text) -> list(binary()) when
    Text :: binary().
symbols(Text) -> 
    Tokens = re:split(Text, <<"\\w|\\(|\\)|\\d|\\s|\\.|\\,">>, [{return, binary}]),
    lists:filter(fun(T) ->
        T =/= <<>>
    end, Tokens).


-spec currency_from_words(Words) -> binary() when
    Words :: list(binary()).
currency_from_words(Words) ->
    lists:foldl(fun(Word, Acc) ->
        case Word of
            <<"USD">> -> <<"USD">>;
            <<"EUR">> -> <<"EUR">>;
            <<"GBP">> -> <<"GBP">>;
            _ -> Acc
        end
    end, undefined, Words).


-spec currency_from_symbols(Symbols) -> binary() | undefined when
    Symbols :: list(binary()).
currency_from_symbols(Symbols) ->
    lists:foldl(fun(Symbol, Acc) ->
        case Symbol of
        	?GBP -> <<"GBP">>;
            ?USD -> <<"USD">>;
            ?EUR -> <<"EUR">>;
            ?EUR1 -> <<"EUR">>;
            _ -> Acc
        end
    end, undefined, Symbols).


% The amount text is first trimmed to remove non-numeric info.
% This makes the amount regex much simpler.
%
% Amount regex:
% (?<WHOLE>\d+|(\d{1,3}[,\.]\d*)+)([,\.](?<CENTS>\d{1,2}))?$
% 
% Test strings:
%
% 1
% 22
% 333
% 4444
% 55555
% 666666
% 7777777
% 88888888
% 11.222
% 1.222
% 333,333
% 444,555.99
% 111,222,333,444
% 111,222,333,444.99
% 111.222,99
% 0.99
% 111,222.9
% 1,222.99
% 1,222
% 1,222.99
% 111.99
% 
% Test page: http://www.rubular.com/r/yqdz7mAL6l

-spec amounts(Text) -> list(integer()) | undefined when
    Text :: binary().
amounts(Text) -> 
	{ok, TrimPattern} = re:compile(<<"^[^\\d]+|[^\\d]+$">>),
	{ok, AmountPattern} = re:compile("(?<WHOLE>\\d+|(\\d{1,3}[,\\.]\\d*)+)([,\\.](?<CENTS>\\d{1,2}))?$"),
	Trimmed = re:replace(Text, TrimPattern, <<"">>, [global, {return, binary}]),
	case re:run(Trimmed, AmountPattern, [global, {capture, all_names, binary}]) of
		{match, [[Cents, Whole]]} ->
			[bin_to_integer(Whole), bin_to_integer(Cents, 2)];
		_ -> undefined
	end.

bin_to_integer(Bin) ->
	bin_to_integer(Bin, 0).

-spec bin_to_integer(Bin, Pad) -> integer() | undefined when
    Bin :: binary(),
    Pad :: integer().
bin_to_integer(Bin, Pad) ->
	Bin1 = re:replace(Bin, <<"[^\\d]">>, <<"">>, [global, {return, binary}]),
	Str = case Pad of
		0 -> binary_to_list(Bin1);
		P -> string:left(binary_to_list(Bin1),P,$0)
	end,
	case Str of
		"" -> 0;
		_ -> 
			try list_to_integer(Str) of
	            N -> N
	        catch
	            error:_ -> undefined
	        end
	end.


