-module('parse_price').

%% API exports
-export([
	parse/1
]).

%% Test exports
-export([
	amounts/1,
	amount_to_binary/2
]).

%%====================================================================
%% API functions
%%====================================================================

-spec parse(Text) -> [{'currency',binary() | 'undefined'} | {'whole', integer() | 'undefined'} | {'fraction', integer() | 'undefined'} | {'text', binary() | 'undefined'}] when
    Text :: binary().
parse(Text) when Text =:= <<>> ->
    empty_result();
parse(Text) when is_binary(Text) ->
    Currency = currency_symbol:symbol(Text),
    [Whole, Fraction] = case amounts(Text) of
    	undefined -> [undefined, undefined];
    	[W, F] -> [W, F];
    	_ -> [undefined, undefined]
    end,
    PriceTextWhole = amount_to_binary(Whole, 0),
    PriceTextFraction = amount_to_binary(Fraction, 2),
    PriceText = to_text(PriceTextWhole, PriceTextFraction),
    [
    	{currency, Currency},
    	{whole, Whole},
    	{fraction, Fraction},
    	{text, PriceText}
    ];
parse(_Text) ->
    empty_result().

    
%%====================================================================
%% Internal functions
%%====================================================================


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
	Trimmed = re:replace(Text, <<"^[^\\d]+|[^\\d]+$">>, <<"">>, [global, {return, binary}]),
	case re:run(Trimmed, <<"(?<WHOLE>\\d+|(\\d{1,3}[,\\.]\\d*)+)([,\\.](?<CENTS>\\d{1,2}))?$">>, [global, {capture, all_names, binary}]) of
		{match, [[Fraction, Whole]]} ->
			[bin_to_integer(Whole), bin_to_integer(Fraction, 2)];
		_ -> undefined
	end.

bin_to_integer(Bin) ->
	bin_to_integer(Bin, 0).

-spec bin_to_integer(Bin, Pad) -> integer() | undefined when
    Bin :: binary(),
    Pad :: integer().
bin_to_integer(Bin, Pad) ->
	N = re:replace(Bin, <<"[^\\d]">>, <<"">>, [global, {return, binary}]),
	case N of
	    <<>> -> 0;
	    _ ->    
	        % make sure that 1.9 is read as 90 cents instead of 9
	        case Pad of
                0 -> binary_to_integer(N);
        		P ->
        		    Padded = string:left(binary_to_list(N),P,$0),
        		    list_to_integer(Padded)
	        end
	end.

-spec amount_to_binary(N, Pad) -> binary() | 'undefined' when
    N :: integer(),
    Pad :: integer().
amount_to_binary(N, _Pad) when N =:= undefined ->
    undefined;
amount_to_binary(N, Pad) ->
    case Pad of
        0 -> integer_to_binary(N);
        P ->
            Padded = string:right(integer_to_list(N),P,$0),
            list_to_binary(Padded)
    end.

-spec to_text(Whole, Fraction) -> 'undefined' | binary() when
    Whole :: 'undefined' | integer(),
    Fraction :: 'undefined' | integer().
to_text(Whole, Fraction) when Whole =:= undefined; Fraction =:= undefined ->
    undefined;
to_text(Whole, Fraction) ->
    list_to_binary(io_lib:format("~s.~s", [Whole, Fraction])).

empty_result() ->
    [
    	{currency, undefined},
    	{whole, undefined},
    	{fraction, undefined},
    	{text, undefined}
    ].