# Abstract

Common Lisp で漢数字を出すというネタパッケージ。`cl:format` との統合も可能。

This is a fun package for printing numbers as Japanese numerals. This
can be integrated with `cl:format`.

# License

The MIT License. See LICENSE file.

# Loading

## Libraries depending on

* asdf
* babel
* alexandria

## Loading

```lisp
(load "jp-numeral.asd")
(asdf:load-system :jp-numeral)
```

For running tests, do below additionally.

```lisp
(load "jp-numeral-test.asd")
(asdf:test-system :jp-numeral)
```

# Examples

## 通常の漢数字を出力する / Puts as (normal) Japanese numerals.

```
CL-USER> (format nil "~/jp-numeral:jp/" 12345687890)
"百二十三億四千五百六十八万七千八百九十"

CL-USER> (format nil "~/jp-numeral:jp/" 123/4567)
"四千五百六十七分の百二十三"

CL-USER> (format nil "~/jp-numeral:jp/" -0.0245)
"マイナス二厘四毛五糸"
```

## 割合や円として出力 / Puts as rate or yen.

割合として。

Puts as a rate (using *割*).

```
CL-USER> (format nil "~/jp-numeral:wari/" 0.123)
"一割二分三厘"
```

円として。

Puts as yen (*円*).

```
CL-USER> (format nil "~/jp-numeral:yen/" 12000.67)
"一万二千円六十七銭"
```

## 大字を使用する / Puts as formal numbers.

`:` 修飾子を使用する。

Use `:` modifier.

```
CL-USER> (format nil "~:/jp-numeral:jp/" 12345687890)
"壱百弐拾参億四千五百六拾八万七千八百九拾"
```

大字で割合として。

Puts as a formal rate.

```
CL-USER> (format nil "~:/jp-numeral:wari/" 0.123)
"壱割弐分参厘"
```

大字で円として。

Puts as a formal yen.

```
CL-USER> (format nil "~:/jp-numeral:yen/" 12000.67)
"壱万弐千円六拾七銭"
```

## 旧字体を使用する / Puts with old glyphs.

`@` 修飾子を使用する。

Use `@` modifier.

```
CL-USER> (format nil "~@/jp-numeral:jp/" 12345687890)
"壹佰貳拾參億肆仟伍佰陸拾捌萬柒仟捌佰玖拾"
```

旧字体で割合として

Puts as a rate with old glyphs.

```
CL-USER> (format nil "~@/jp-numeral:wari/" 0.123)
"壹割貳分參釐"
```

旧字体で円として

Puts as a yen with old glyphs.

```
CL-USER> (format nil "~@/jp-numeral:yen/" 12000.67)
"壹萬貳仟圓陸拾柒錢"
```

## 位取り記数法を使用する / Puts with positional notation.

`:` 修飾子と `@` 修飾子を併用する。

Use both `:` and `@` modifier.

```
CL-USER> (format nil "~@:/jp-numeral:jp/" 12345687890)
"一二三四五六八七八九〇"
```


# API

## [Function] `jp`

### 書式 / Syntax

```lisp
(jp stream object
	&optional colon-p at-sign-p	digits-after-dot scale radix-point)
```

### 説明 / Description

`stream` に `object` を漢数字として書き出す。
`cl:format` の `~/` での関数呼びだしでも使用できる。

This function writes `object` into `stream` as Japanese numerals.
This can be called from `cl:format` with `~/` directive.

### 引数 / Arguments

- `stream`

	出力先の stream
	
	Output destination stream.
	
- `object`

	出力する object
	
	The object to be output.
	
- `colon-p`, `at-sign-p`
	
	二つの組み合わせで出力形式を指定する。対応は以下の通り:

	- `(and (not colon-p) (not at-sign-p))` :: 通常の漢数字
	- `(and colon-p (not at-sign-p))` :: 大字
	- `(and (not colon-p) at-sign-p)` :: 旧字体
	- `(and colon-p at-sign-p)` :: 位取り記数法

	`colon-p` and `at-sign-p` specify the style for printing.
	The corresponding is below:
	
	- `(and (not colon-p) (not at-sign-p))` :: Use normal Japanese numerals.
	- `(and colon-p (not at-sign-p))` :: Use formal styles.
	- `(and (not colon-p) at-sign-p)` :: Use Old glyphs.
	- `(and colon-p at-sign-p)` :: Use positional Notation.

- `digits-after-dot`

	浮動小数点数を出力する時に、小数点の後に何桁目まで出力するか。
	`nil` にすると、適当に十分な数で出す。
	
	Specifies how many digits putted after the radix point when
	printing a floating-point number.
	When `nil` is specified, uses an appropriate one.
	
- `scale`

	出力の時に、 `(expt 10 scale)` を掛けた値を出力する。
	
	When printing, uses a number mutiplied with `(expt 10 scale)`.
	
- `radix-point`

	小数点に使用する、文字もしくは文字列。
	
	Specifies a character or a string used as a radix point.


## [Function] `wari`

### 書式 / Syntax

```lisp
(wari stream object
	&optional colon-p at-sign-p	digits-after-dot)
```

### 説明 / Description

`jp` と同様だが、 割合として表示する。
10倍され、小数点に *割* を使用した値が表示される。

This function works like `jp`, but puts as a rate.
The output value is multiplied with 10, and *割* is used for the radix
point.

### 引数 / Arguments

`jp` の引数と同様。

Same as the arguments of `jp`.


## [Function] `yen`

### 書式 / Syntax

```lisp
(yen stream object
	&optional colon-p at-sign-p	digits-after-dot)
```

### 説明 / Description

`jp` と同様だが、 円として表示する。
指定した桁で丸め、 `1` の桁までは *円* で、 `0.01` の桁までは *銭* で、
`0.001` の桁は *厘* で表示する。

This function works like `jp`, but puts as a yen.
The output value is rounded on specified position, and printed until
`1` with *円*, until `0.01` with *銭*, and until `0.001` with *厘*.

### 引数 / Arguments

以下の引数以外は、`jp` の引数と同様。

Same as the arguments of `jp`, except below.

- `digits-after-dot`

	小数点以下のどの桁まで表示するか指定する。デフォルトは 2。
	`0` , `2` , `3` のいずれかが使用できる。
	
	Specifies how many digits putted after a radix point. The default
	is 2.
	Only one of `0`, `2`, or `3` is available.


## [Function] `format-jp-numeral`

### 書式 / Syntax

```lisp
(format-jp-numeral
	stream object style
	&key digits-after-dot scale radix-point)
```

### 説明 / Description

`jp` と同じことを行うが、普通の関数として呼び出すのに便利なように
引数を置きかえている。
`style` で指定した形式で、 `stream` に `object` を書き出す。

This function works same as `jp`, but arranges the arguments for
convenience of calling from ordinary functions.
This writes `object` into `stream` as Japanese numerals.

### 引数 / Arguments

以下の引数以外は、`jp` の引数と同様。

Same as the arguments of `jp`, except below.

- `style`

	出力形式を指定する。以下のいずれかのシンボルを渡す。

	- `:normal` :: 一般的な漢数字を使用する。
	- `:formal` :: 大字を使用する。
	- `:old` :: 旧字体を使用する。
	- `:positional` :: 位取り記数法を使用する。
	
	Specify the style of output. Pass one of these symbols:

	- `:normal` :: Use normal Japanese numerals.
	- `:formal` :: Use formal styles.
	- `:old` :: Use old glyphs.
	- `:positional` :: Use positional notations.
