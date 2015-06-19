# Abstract

Common Lisp で漢数字を出すというネタパッケージ。`format` との統合も可能。

This is a fun package for printing numbers as Japanese numerals. This
can be integrated with `format`.

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
JP-NUMERAL> (format nil "~/jp-numeral:jp/" 12345687890)
"百二十三億四千五百六十八万七千八百九十"

JP-NUMERAL> (format nil "~/jp-numeral:jp/" 123/4567)
"四千五百六十七分の百二十三"

JP-NUMERAL> (format nil "~/jp-numeral:jp/" -0.0245)
"マイナス二厘四毛五糸"
```

## 割合や円として出力 / Puts as rate or yen.

割合として。

Puts as a rate (using *割*).

```
JP-NUMERAL> (format nil "~/jp-numeral:wari/" 0.123)
"一割二分三厘"
```

円として。

Puts as yen (*円*).

```
JP-NUMERAL> (format nil "~/jp-numeral:yen/" 12000.67)
"一万二千円六十七銭"
```

## 大字を使用する / Puts as formal numbers.

`:` 修飾子を使用する。

Use `:` modifier.

```
JP-NUMERAL> (format nil "~:/jp-numeral:jp/" 12345687890)
"壱百弐拾参億四千五百六拾八万七千八百九拾"
```

大字で割合として。

Puts as a formal rate.

```
JP-NUMERAL> (format nil "~:/jp-numeral:wari/" 0.123)
"壱割弐分参厘"
```

大字で円として。

Puts as a formal yen.

```
JP-NUMERAL> (format nil "~:/jp-numeral:yen/" 12000.67)
"壱万弐千円六拾七銭"
```

## 旧字体を使用する / Puts with old glyphs.

`@` 修飾子を使用する。

Use `@` modifier.

```
JP-NUMERAL> (format nil "~@/jp-numeral:jp/" 12345687890)
"壹佰貳拾參億肆仟伍佰陸拾捌萬柒仟捌佰玖拾"
```

旧字体で割合として

Puts as a rate with old glyphs.

```
JP-NUMERAL> (format nil "~@/jp-numeral:wari/" 0.123)
"壹割貳分參釐"
```

旧字体で円として

Puts as a yen with old glyphs.

```
JP-NUMERAL> (format nil "~@/jp-numeral:yen/" 12000.67)
"壹萬貳仟圓陸拾柒錢"
```

## 位取り記数法を使用する / Puts with positional notation.

`:` 修飾子と `@` 修飾子を併用する。

Use both `:` and `@` modifier.

```
JP-NUMERAL> (format nil "~@:/jp-numeral:jp/" 12345687890)
"一二三四五六八七八九〇"
```

# API

## Function `format-jp-numeral`

### 書式 / Syntax

```lisp
(format-jp-numeral
	style stream object
	&key digits-after-dot scale radix-point)
```

### 説明 / Description

このパッケージのエントリポイント。
`style` で指定した形式で、 `stream` に `object` を書き出す。

This function is the entry point of this package.
It prints `object` into `stream` with the style specified on `style`.

### 引数 / Arguments

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
	
- `stream`

	出力先の stream
	
	Output destination stream.
	
- `object`

	出力する object
	
	The object to be output.
	
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
	

## Function `jp`

### 書式 / Syntax

```lisp
(JP stream object
	&optional colon-p at-sign-p	digits-after-dot scale radix-point)
```

### 説明 / Description

`format-jp-numeral` と同様だが、 `cl:format` の `~/` での関数呼びだしに
合うよう引数を変更している。
フラグと `style` との対応は以下の通り:


This function does same as `format-jp-numeral` but arranges arguments
for calling from `cl:format` with `~/` directive.
The corresponding between flags and `style` is below:


- `(and (not colon-p) (not at-sign-p))` :: `:normal`
- `(and colon-p (not at-sign-p))` :: `:formal`
- `(and (not colon-p) at-sign-p)` :: `:old`
- `(and colon-p at-sign-p)` :: `:positional`

## Function `wari`

### 書式 / Syntax

```lisp
(wari stream object
	&optional colon-p at-sign-p	digits-after-dot)
```

### 説明 / Description

`jp` と同様だが、 *割* を使って割合として表示する。

This function work like `jp`, but puts as a rate using *割*.

## Function `yen`

### 書式 / Syntax

```lisp
(yen stream object
	&optional colon-p at-sign-p	digits-after-dot)
```

### 説明 / Description

`jp` と同様だが、 円として表示する。

This function work like `jp`, but puts as a yen.

### 引数 / Arguments

- `digits-after-dot`

	小数点以下のどの桁まで表示するか指定する。デフォルトは 2。
	`0` (*円* まで), `2` (*銭* まで), `3` (*厘* まで) のいずれかが使用
	できる。
	
	Specifies how many digits putted after a radix point. The default
	is 2.
	Only one of `0` (until *円*, 1), `2` (until *銭*, 0.01), or `3`
	(until *厘*, 0.001) is available.
