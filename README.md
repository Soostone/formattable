# formattable

A business-quality formatting package with reified formats for multiple data types

This package provides excellent support for formatting numbers including the following configurable features:

* units (thousands, millions, percent, etc)
* prefixes ($, £, etc)
* suffixes (%, ¢, etc)
* decimal separators
* thousands separators
* styles for fixed decimal notation, scientific notation, and SI prefixes
* specify precision as number of decimal places or sig figs
* different styles of negative numbers

## Examples

* runFormat (intFmt { _nfStyle = SIStyle }) (225142.3 :: Double) == "225k"

See the [test suite](https://github.com/Soostone/formattable/blob/master/test/NumFormatSpec.hs#L67) for more examples.
