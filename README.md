# parse_price

Erlang utility. Reads price text (in various notations) and returns this as a data proplist.

The proplist contains the elements keys:

* ``currency``: (binary string) Currency symbols are returned as [ISO 4217 Currency Codes](http://www.xe.com/iso4217.php). Note that coverage of currency codes is incomplete.
* ``whole``: (integer) The amount before the fraction.
* ``fraction``: (integer) The fraction.
* ``text``: (binary string) Price without currency. The "." (FULL STOP) indicates the decimal point.


## Examples

~~~erlang
parse_price:parse(<<"$ 1,222.9">>).
[{currency,<<"USD">>},{whole,1222},{fraction,90},{text, <<"1222.90">>}]
~~~

~~~erlang
parse_price:parse(<<"EUR 108.75">>).
[{currency,<<"EUR">>},{whole,108},{fraction,75},{text, <<"108.75">>}]
~~~

~~~erlang
parse_price:parse(<<"11,- €"/utf8>>).
[{currency,<<"EUR">>},{whole,11},{fraction,0},{text, <<"11.00">>}]
~~~

See the test file for full coverage.


## Requirements

* Erlang 17 or higher


## Build

    $ rebar3 compile
