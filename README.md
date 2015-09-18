# currency

Erlang utility. Reads currency text (in various notations) and returns this as a data proplist.

Currency symbols are returned as [ISO 4217 Currency Codes](http://www.xe.com/iso4217.php).
Coverage of currency codes is incomplete.


## Examples

| Command  | Result |
| ------------- | ------------- |
| `currency:parse(<<"$ 1,222.99">>)`  | `[{currency,<<"USD">>},{whole,1222},{cents,99}]`  |
| `currency:parse(<<"EUR 108.75">>)`  | `[{currency,<<"EUR">>},{whole,108},{cents,75}]`  |
| `currency:parse(<<"11,- €"/utf8>>)`  | `[{currency,<<"EUR">>},{whole,11},{cents,0}]`  |

See the test file for full coverage.


## Requirements

* Erlang 17 or higher


## Build

    $ rebar3 compile
