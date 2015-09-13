%% -*- coding: utf-8 -*-

-module(currency_symbol).

%% API exports
-export([
	symbol/1
]).

-spec symbol(Text) -> binary() when
    Text :: binary().
symbol(Text) ->
	Words = words(Text),
    case match_symbol(Words) of
    	undefined -> 
    		Symbols = symbols(Text),
    		match_symbol(Symbols);
		Word -> Word
	end.

-spec match_symbol(Symbols) -> binary() | undefined when
    Symbols :: list().
match_symbol(Symbols) ->
	lists:foldl(fun(Symbol, Acc) ->
		%io:format("Symbol=~p~n", [Symbol]),
        case code(Symbol) of
        	undefined -> Acc;
        	Code -> Code
        end
    end, undefined, Symbols).


-spec words(Text) -> list(binary()) when
    Text :: binary().
words(Text) -> 
    tokens(Text, <<"\\W|\\d|\\s|\\.\\,">>).

-spec symbols(Text) -> list(binary()) when
    Text :: binary().
symbols(Text) -> 
    tokens(Text, <<"\\w|\\(|\\)|\\d|\\s|\\.|\\,">>).

-spec tokens(Text, Re) -> list(binary()) when
	Text :: binary(),
    Re :: binary().
tokens(Text, Re) -> 
    Tokens = re:split(Text, Re, [{return, binary}]),
    lists:filter(fun(T) ->
        T =/= <<>>
    end, Tokens).



%% http://www.xe.com/symbols.php

code(<<"¬">>) -> <<"EUR">>;
code(<<130,172>>) -> <<"EUR">>;
code(<<"£"/utf8>>) -> <<"GBP">>;
code(<<"£">>) -> <<"GBP">>;
code(<<"$">>) -> <<"USD">>;
code(<<"kr">>) -> <<"DKK">>;
code(<<"₤"/utf8>>) -> <<"TRL">>;
code(<<130,164>>) -> <<"TRL">>;
code(<<"USD">>) -> <<"USD">>;
code(<<"EUR">>) -> <<"EUR">>;
code(<<"GBP">>) -> <<"GBP">>;
code(<<"CHF">>) -> <<"CHF">>;
code(<<"Lek">>) -> <<"ALL">>;
code(<<"TRY">>) -> <<"TRY">>;
code(<<"TRL">>) -> <<"TRL">>;
code(<<"DKK">>) -> <<"DKK">>;
code(_) -> undefined.
