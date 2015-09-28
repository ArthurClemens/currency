%% -*- coding: utf-8 -*-

-module(parse_price_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% API functions tests
%%====================================================================

parse1_test() -> ?assert(parse_price:parse(<<"1">>) =:= [{currency, undefined},{whole, 1},{fraction, 0},{text, <<"1.00">>}]).
parse2_test() -> ?assert(parse_price:parse(<<"$1">>) =:= [{currency, <<"USD">>},{whole, 1},{fraction, 0},{text, <<"1.00">>}]).
parse3_test() -> ?assert(parse_price:parse(<<"$0.99">>) =:= [{currency, <<"USD">>},{whole, 0},{fraction, 99},{text, <<"0.99">>}]).
parse4_test() -> ?assert(parse_price:parse(<<"$(111,222.99)">>) =:= [{currency, <<"USD">>},{whole, 111222},{fraction, 99},{text, <<"111222.99">>}]).
parse5_test() -> ?assert(parse_price:parse(<<"€(111.222,99)"/utf8>>) =:= [{currency, <<"EUR">>},{whole, 111222},{fraction, 99},{text, <<"111222.99">>}]).
parse6_test() -> ?assert(parse_price:parse(<<"(111222)">>) =:= [{currency, undefined},{whole, 111222},{fraction, 0},{text, <<"111222.00">>}]).
parse7_test() -> ?assert(parse_price:parse(<<"$(111222)">>) =:= [{currency, <<"USD">>},{whole, 111222},{fraction, 0},{text, <<"111222.00">>}]).
parse8_test() -> ?assert(parse_price:parse(<<"(111,222)">>) =:= [{currency, undefined},{whole, 111222},{fraction, 0},{text, <<"111222.00">>}]).
parse9_test() -> ?assert(parse_price:parse(<<"11,222.9">>) =:= [{currency, undefined},{whole, 11222},{fraction, 90},{text, <<"11222.90">>}]).
parse10_test() -> ?assert(parse_price:parse(<<"USD 1,222.99">>) =:= [{currency, <<"USD">>},{whole, 1222},{fraction, 99},{text, <<"1222.99">>}]).
parse11_test() -> ?assert(parse_price:parse(<<"USD 1,222">>) =:= [{currency, <<"USD">>},{whole, 1222},{fraction, 0},{text, <<"1222.00">>}]).
parse12_test() -> ?assert(parse_price:parse(<<"$ 1,222.00">>) =:= [{currency, <<"USD">>},{whole, 1222},{fraction, 0},{text, <<"1222.00">>}]).
parse13_test() -> ?assert(parse_price:parse(<<"£ 111.99"/utf8>>) =:= [{currency, <<"GBP">>},{whole, 111},{fraction, 99},{text, <<"111.99">>}]).
parse14_test() -> ?assert(parse_price:parse(<<"111.99 £"/utf8>>) =:= [{currency, <<"GBP">>},{whole, 111},{fraction, 99},{text, <<"111.99">>}]).
parse15_test() -> ?assert(parse_price:parse(<<"€ 11,99"/utf8>>) =:= [{currency, <<"EUR">>},{whole, 11},{fraction, 99},{text, <<"11.99">>}]).
parse16_test() -> ?assert(parse_price:parse(<<"€ 11"/utf8>>) =:= [{currency, <<"EUR">>},{whole, 11},{fraction, 0},{text, <<"11.00">>}]).
parse17_test() -> ?assert(parse_price:parse(<<"€ 11,-"/utf8>>) =:= [{currency, <<"EUR">>},{whole, 11},{fraction, 0},{text, <<"11.00">>}]).
parse18_test() -> ?assert(parse_price:parse(<<"€ 11,--"/utf8>>) =:= [{currency, <<"EUR">>},{whole, 11},{fraction, 0},{text, <<"11.00">>}]).
parse19_test() -> ?assert(parse_price:parse(<<"11 €"/utf8>>) =:= [{currency, <<"EUR">>},{whole, 11},{fraction, 0},{text, <<"11.00">>}]).
parse20_test() -> ?assert(parse_price:parse(<<"11,- €"/utf8>>) =:= [{currency, <<"EUR">>},{whole, 11},{fraction, 0},{text, <<"11.00">>}]).

parse_EUR_test() -> ?assert(parse_price:parse(<<"EUR 108.75">>) =:= [{currency, <<"EUR">>},{whole, 108},{fraction, 75},{text, <<"108.75">>}]).
parse_CHF_test() -> ?assert(parse_price:parse(<<"CHF 108.75">>) =:= [{currency, <<"CHF">>},{whole, 108},{fraction, 75},{text, <<"108.75">>}]).
parse_ALL_test() -> ?assert(parse_price:parse(<<"Lek 108.75">>) =:= [{currency, <<"ALL">>},{whole, 108},{fraction, 75},{text, <<"108.75">>}]).
parse_TRY_test() -> ?assert(parse_price:parse(<<"TRY 108.75">>) =:= [{currency, <<"TRY">>},{whole, 108},{fraction, 75},{text, <<"108.75">>}]).
parse_TRL_test() -> ?assert(parse_price:parse(<<"TRL 108.75">>) =:= [{currency, <<"TRL">>},{whole, 108},{fraction, 75},{text, <<"108.75">>}]).
parse_TRL2_test() -> ?assert(parse_price:parse(<<"₤ 108.75"/utf8>>) =:= [{currency, <<"TRL">>},{whole, 108},{fraction, 75},{text, <<"108.75">>}]).
parse_DKK_test() -> ?assert(parse_price:parse(<<"DKK 108.75">>) =:= [{currency, <<"DKK">>},{whole, 108},{fraction, 75},{text, <<"108.75">>}]).
parse_DKK2_test() -> ?assert(parse_price:parse(<<"kr 108.75">>) =:= [{currency, <<"DKK">>},{whole, 108},{fraction, 75},{text, <<"108.75">>}]).

parse_invalid_input1_test() -> ?assert(parse_price:parse(<<>>) =:= [{currency, undefined},{whole, undefined},{fraction, undefined},{text, undefined}]).
parse_invalid_input2_test() -> ?assert(parse_price:parse([]) =:= [{currency, undefined},{whole, undefined},{fraction, undefined},{text, undefined}]).
parse_invalid_input3_test() -> ?assert(parse_price:parse(<<1>>) =:= [{currency, undefined},{whole, undefined},{fraction, undefined},{text, undefined}]).

%%====================================================================
%% Internal functions tests
%%====================================================================

amounts2_test() -> ?assert(parse_price:amounts(<<"1">>) =:= [1, 0]).
amounts3_test() -> ?assert(parse_price:amounts(<<"22">>) =:= [22, 0]).
amounts4_test() -> ?assert(parse_price:amounts(<<"333">>) =:= [333, 0]).
amounts5_test() -> ?assert(parse_price:amounts(<<"4444">>) =:= [4444, 0]).
amounts6_test() -> ?assert(parse_price:amounts(<<"55555">>) =:= [55555, 0]).
amounts7_test() -> ?assert(parse_price:amounts(<<"666666">>) =:= [666666, 0]).
amounts8_test() -> ?assert(parse_price:amounts(<<"7777777">>) =:= [7777777, 0]).
amounts9_test() -> ?assert(parse_price:amounts(<<"88888888">>) =:= [88888888, 0]).
amounts10_test() -> ?assert(parse_price:amounts(<<"11.222">>) =:= [11222, 0]).
amounts11_test() -> ?assert(parse_price:amounts(<<"1.222">>) =:= [1222, 0]).
amounts12_test() -> ?assert(parse_price:amounts(<<"333,333">>) =:= [333333, 0]).
amounts13_test() -> ?assert(parse_price:amounts(<<"444,555.99">>) =:= [444555, 99]).
amounts14_test() -> ?assert(parse_price:amounts(<<"111,222,333,444">>) =:= [111222333444, 0]).
amounts15_test() -> ?assert(parse_price:amounts(<<"111,222,333,444.99">>) =:= [111222333444, 99]).
amounts16_test() -> ?assert(parse_price:amounts(<<"111.222,99">>) =:= [111222, 99]).
amounts17_test() -> ?assert(parse_price:amounts(<<"0.99">>) =:= [0, 99]).
amounts18_test() -> ?assert(parse_price:amounts(<<"111,222.9">>) =:= [111222, 90]).
amounts19_test() -> ?assert(parse_price:amounts(<<"1,222.99">>) =:= [1222, 99]).
amounts20_test() -> ?assert(parse_price:amounts(<<"1,222">>) =:= [1222, 0]).
amounts21_test() -> ?assert(parse_price:amounts(<<"1,222.99">>) =:= [1222, 99]).
amounts22_test() -> ?assert(parse_price:amounts(<<"111.99">>) =:= [111, 99]).
amounts23_test() -> ?assert(parse_price:amounts(<<"111.999">>) =:= [111999, 0]).
amounts24_test() -> ?assert(parse_price:amounts(<<"111.999,1">>) =:= [111999, 10]).
amounts25_test() -> ?assert(parse_price:amounts(<<"111.999,01">>) =:= [111999, 1]).
