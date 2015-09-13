%% -*- coding: utf-8 -*-

-module(currency_tests).

-include_lib("eunit/include/eunit.hrl").

%%====================================================================
%% API functions tests
%%====================================================================


parse1_test() -> ?assert(currency:parse(<<"1">>) =:= [{currency, undefined},{whole, 1},{cents, 0}]).
parse2_test() -> ?assert(currency:parse(<<"$1">>) =:= [{currency, <<"USD">>},{whole, 1},{cents, 0}]).
parse3_test() -> ?assert(currency:parse(<<"$0.99">>) =:= [{currency, <<"USD">>},{whole, 0},{cents, 99}]).
parse4_test() -> ?assert(currency:parse(<<"$(111,222.99)">>) =:= [{currency, <<"USD">>},{whole, 111222},{cents, 99}]).
parse5_test() -> ?assert(currency:parse(<<"€(111.222,99)"/utf8>>) =:= [{currency, <<"EUR">>},{whole, 111222},{cents, 99}]).
parse6_test() -> ?assert(currency:parse(<<"(111222)">>) =:= [{currency, undefined},{whole, 111222},{cents, 0}]).
parse7_test() -> ?assert(currency:parse(<<"$(111222)">>) =:= [{currency, <<"USD">>},{whole, 111222},{cents, 0}]).
parse8_test() -> ?assert(currency:parse(<<"(111,222)">>) =:= [{currency, undefined},{whole, 111222},{cents, 0}]).
parse9_test() -> ?assert(currency:parse(<<"11,222.9">>) =:= [{currency, undefined},{whole, 11222},{cents, 90}]).
parse10_test() -> ?assert(currency:parse(<<"USD 1,222.99">>) =:= [{currency, <<"USD">>},{whole, 1222},{cents, 99}]).
parse11_test() -> ?assert(currency:parse(<<"USD 1,222">>) =:= [{currency, <<"USD">>},{whole, 1222},{cents, 0}]).
parse12_test() -> ?assert(currency:parse(<<"$ 1,222.00">>) =:= [{currency, <<"USD">>},{whole, 1222},{cents, 0}]).
parse13_test() -> ?assert(currency:parse(<<"£ 111.99"/utf8>>) =:= [{currency, <<"GBP">>},{whole, 111},{cents, 99}]).
parse14_test() -> ?assert(currency:parse(<<"111.99 £"/utf8>>) =:= [{currency, <<"GBP">>},{whole, 111},{cents, 99}]).
parse15_test() -> ?assert(currency:parse(<<"€ 11,99"/utf8>>) =:= [{currency, <<"EUR">>},{whole, 11},{cents, 99}]).
parse16_test() -> ?assert(currency:parse(<<"€ 11"/utf8>>) =:= [{currency, <<"EUR">>},{whole, 11},{cents, 0}]).
parse17_test() -> ?assert(currency:parse(<<"€ 11,-"/utf8>>) =:= [{currency, <<"EUR">>},{whole, 11},{cents, 0}]).
parse18_test() -> ?assert(currency:parse(<<"€ 11,--"/utf8>>) =:= [{currency, <<"EUR">>},{whole, 11},{cents, 0}]).
parse19_test() -> ?assert(currency:parse(<<"11 €"/utf8>>) =:= [{currency, <<"EUR">>},{whole, 11},{cents, 0}]).
parse20_test() -> ?assert(currency:parse(<<"11,- €"/utf8>>) =:= [{currency, <<"EUR">>},{whole, 11},{cents, 0}]).

parse_EUR_test() -> ?assert(currency:parse(<<"EUR 108.75">>) =:= [{currency, <<"EUR">>},{whole, 108},{cents, 75}]).
parse_CHF_test() -> ?assert(currency:parse(<<"CHF 108.75">>) =:= [{currency, <<"CHF">>},{whole, 108},{cents, 75}]).
parse_ALL_test() -> ?assert(currency:parse(<<"Lek 108.75">>) =:= [{currency, <<"ALL">>},{whole, 108},{cents, 75}]).
parse_TRY_test() -> ?assert(currency:parse(<<"TRY 108.75">>) =:= [{currency, <<"TRY">>},{whole, 108},{cents, 75}]).
parse_TRL_test() -> ?assert(currency:parse(<<"TRL 108.75">>) =:= [{currency, <<"TRL">>},{whole, 108},{cents, 75}]).
parse_TRL2_test() -> ?assert(currency:parse(<<"₤ 108.75"/utf8>>) =:= [{currency, <<"TRL">>},{whole, 108},{cents, 75}]).
parse_DKK_test() -> ?assert(currency:parse(<<"DKK 108.75">>) =:= [{currency, <<"DKK">>},{whole, 108},{cents, 75}]).
parse_DKK2_test() -> ?assert(currency:parse(<<"kr 108.75">>) =:= [{currency, <<"DKK">>},{whole, 108},{cents, 75}]).


%%====================================================================
%% Internal functions tests
%%====================================================================

amounts2_test() -> ?assert(currency:amounts(<<"1">>) =:= [1, 0]).
amounts3_test() -> ?assert(currency:amounts(<<"22">>) =:= [22, 0]).
amounts4_test() -> ?assert(currency:amounts(<<"333">>) =:= [333, 0]).
amounts5_test() -> ?assert(currency:amounts(<<"4444">>) =:= [4444, 0]).
amounts6_test() -> ?assert(currency:amounts(<<"55555">>) =:= [55555, 0]).
amounts7_test() -> ?assert(currency:amounts(<<"666666">>) =:= [666666, 0]).
amounts8_test() -> ?assert(currency:amounts(<<"7777777">>) =:= [7777777, 0]).
amounts9_test() -> ?assert(currency:amounts(<<"88888888">>) =:= [88888888, 0]).
amounts10_test() -> ?assert(currency:amounts(<<"11.222">>) =:= [11222, 0]).
amounts11_test() -> ?assert(currency:amounts(<<"1.222">>) =:= [1222, 0]).
amounts12_test() -> ?assert(currency:amounts(<<"333,333">>) =:= [333333, 0]).
amounts13_test() -> ?assert(currency:amounts(<<"444,555.99">>) =:= [444555, 99]).
amounts14_test() -> ?assert(currency:amounts(<<"111,222,333,444">>) =:= [111222333444, 0]).
amounts15_test() -> ?assert(currency:amounts(<<"111,222,333,444.99">>) =:= [111222333444, 99]).
amounts16_test() -> ?assert(currency:amounts(<<"111.222,99">>) =:= [111222, 99]).
amounts17_test() -> ?assert(currency:amounts(<<"0.99">>) =:= [0, 99]).
amounts18_test() -> ?assert(currency:amounts(<<"111,222.9">>) =:= [111222, 90]).
amounts19_test() -> ?assert(currency:amounts(<<"1,222.99">>) =:= [1222, 99]).
amounts20_test() -> ?assert(currency:amounts(<<"1,222">>) =:= [1222, 0]).
amounts21_test() -> ?assert(currency:amounts(<<"1,222.99">>) =:= [1222, 99]).
amounts22_test() -> ?assert(currency:amounts(<<"111.99">>) =:= [111, 99]).
amounts23_test() -> ?assert(currency:amounts(<<"111.999">>) =:= [111999, 0]).
amounts24_test() -> ?assert(currency:amounts(<<"111.999,1">>) =:= [111999, 10]).
amounts25_test() -> ?assert(currency:amounts(<<"111.999,01">>) =:= [111999, 1]).
