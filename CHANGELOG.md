# Revision history for `maestro-sdk`

## [1.7.2](https://github.com/maestro-org/haskell-sdk/compare/v1.7.1..v1.7.2) -- FIXME:

* Add `PlutusV3` to `ScriptType`.

## [1.7.0](https://github.com/maestro-org/haskell-sdk/compare/v1.6.0..v1.7.0) -- 2024-08-17

* Updated to newer `era-summaries` and `protocol-parameters` endpoint from earlier `era-history` and `protocol-params` respectively.
* `AssetInfo` type which corresponds to `/assets/:asset` is updated with more response fields.

## [1.6.0](https://github.com/maestro-org/haskell-sdk/compare/v1.5.0..v1.6.0) -- 2024-04-09

Added:

* GET `/addresses/:address/transactions`
* `asset` query parameter to GET `/addresses/cred/:credential/utxos`
* POST `/addresses/cred/utxos`
* GET `/addresses/cred/:credential/transactions`
* GET `/assets/:asset`
* `from`, `to`, `limit` query parameters to GET `/markets/dexs/ohlc/:dex/:pair`
* provision to prevent api-key from being leaked in error messages
* provision to handle Maestro error bodies which are not enclosed in double quotes. Earlier behaviour was to expect message such as `"Failed to deserialise"` and not `Failed to deserialise`.
* `FromHttpApiData`, `ToHttpApiData` instance for `SlotNo`
* `Eq`, `Ord`, `Enum`, `Bounded`, `ToJSON`, `FromHttpApiData` instance for `Order`
* `Enum`, `Bounded`, `FromHttpApiData` instance for `Dex`
* `Data`, `Typeable`, `Enum`, `Bounded`, `FromHttpApiData` instance for `Resolution` and also refactored it's `Show` instance.

## [1.5.0](https://github.com/maestro-org/haskell-sdk/compare/v1.4.0..v1.5.0) -- 2024-01-02

* Added support GeniusYield DEX to market defi endpoints in [#45](https://github.com/maestro-org/haskell-sdk/pull/45).

## [1.4.0](https://github.com/maestro-org/haskell-sdk/compare/v1.3.0..v1.4.0) -- 2023-12-12

* Added support of OHCL and pair endpoints in [#44](https://github.com/maestro-org/haskell-sdk/pull/44).

## [1.3.0](https://github.com/maestro-org/haskell-sdk/compare/v1.2.0..v1.3.0) -- 2023-11-27

* Removed deprecated `/datum` endpoint in favour of `/datums`, [#42](https://github.com/maestro-org/haskell-sdk/pull/42).
* Support for endpoint to query UTxOs at a single address, [#42](https://github.com/maestro-org/haskell-sdk/pull/42).

## [1.2.0](https://github.com/maestro-org/haskell-sdk/compare/v1.1.0..v1.2.0) -- 2023-10-18

* Incorporating updated response when submitting the transaction, [#41](https://github.com/maestro-org/haskell-sdk/pull/41).

## [1.1.0](https://github.com/maestro-org/haskell-sdk/compare/v1.0.0..v1.1.0) -- 2023-09-23

* Support of v0 family of endpoints have been dropped, [#33](https://github.com/maestro-org/haskell-sdk/pull/33).
* Support of backoff, to automatically handle rate limit errors by allowing clients to use exponential backoff with maximum delay threshold, [#40](https://github.com/maestro-org/haskell-sdk/pull/40).
* Support of Preview network is added, [#37](https://github.com/maestro-org/haskell-sdk/pull/37).
