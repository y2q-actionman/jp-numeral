# Abstract

Common Lisp の format で漢数字を出すというネタパッケージ。

# License

the MIT License. See LICENSE file.

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

# Examples

## 通常の漢数字を出力する。

```
JP-NUMERAL> (format nil "~/jp-numeral:jp/" 12345687890)
"百二十三億四千五百六十八万七千八百九十"

JP-NUMERAL> (format nil "~/jp-numeral:jp/" 123/4567)
"四千五百六十七分の百二十三"

JP-NUMERAL> (format nil "~/jp-numeral:jp/" -0.0245)
"マイナス二厘四毛五糸"
```

## 割合や金銭として出力

```
;; 割合として
JP-NUMERAL> (format nil "~/jp-numeral:wari/" 0.123)
"一割二分三厘"

;; 金銭として
JP-NUMERAL> (format nil "~/jp-numeral:yen/" 12000.67)
"一万二千円六十七銭"
```

## 大字を使用する

`:` 修飾子を使用する。

```
;; 通常
JP-NUMERAL> (format nil "~:/jp-numeral:jp/" 12345687890)
"壱百弐拾参億四千五百六拾八万七千八百九拾"

;; 割合として
JP-NUMERAL> (format nil "~:/jp-numeral:wari/" 0.123)
"壱割弐分参厘"

;; 金銭として
JP-NUMERAL> (format nil "~:/jp-numeral:yen/" 12000.67)
"壱万弐千円六拾七銭"
```

## 旧字体を使用する

`@` 修飾子を使用する。

```
;; 通常
JP-NUMERAL> (format nil "~@/jp-numeral:jp/" 12345687890)
"壹佰貳拾參億肆仟伍佰陸拾捌萬柒仟捌佰玖拾"

;; 割合として
JP-NUMERAL> (format nil "~@/jp-numeral:wari/" 0.123)
"壹割貳分參釐"

;; 金銭として
JP-NUMERAL> (format nil "~@/jp-numeral:yen/" 12000.67)
"壹萬貳仟圓陸拾柒錢"
```

## 位取り記数法を使用する

`:` 修飾子と `@` 修飾子を併用する。

```
JP-NUMERAL> (format nil "~@:/jp-numeral:jp/" 12345687890)
"一二三四五六八七八九〇"
```

# API

## format-jp-numeral

### 書式

```lisp
(format-jp-numeral
	style stream object
	&key digits-after-dot scale radix-point)
```

### 説明

漢数字プリンタのエントリポイント。
`style` で指定した形式で、 `stream` に `object` を書き出す。


### 引数

- `style`

	出力形式を指定する。以下のいずれかのシンボルを渡す。

	- `:normal`
	
		一般的な漢数字をする。
		
	- `:formal`
	
		大字を使用する。
		
	- `:old`
	
		旧字体を使用する。
		
	- `:positional`
	
		位取り記数法を使用する。
	
- `stream`

	出力先の stream
	
- `object`

	出力する object
	
- `digits-after-dot`

	浮動小数点数を出力する時に、小数点の後に何桁目まで出力するか。
	nil にすると、適当に十分な数で出す。
	
- `scale`

	出力の時に、 `10^scale` を掛けた値を出力する。
	
- `radix-point`

	小数点に使用する文字、もしくは文字列。

## jp

### 書式

```lisp
(JP stream object
	&optional colon-p at-sign-p	digits-after-dot scale radix-point)
```

### 説明

`format-jp-numeral` を cl:format の `~/` の関数呼びだしに合う形式で呼ぶためのもの。

フラグの対応は以下の通り

- `(and (not colon-p) (not at-sign-p))`

	`:normal`

- `(and colon-p (not at-sign-p))`

	`:formal`

- `(and (not colon-p) at-sign-p)`

	`:old`

- `(and colon-p at-sign-p)`

	`:positional`

## wari

### 書式

```lisp
(wari stream object
	&optional colon-p at-sign-p	digits-after-dot)
```

### 説明

`jp` と同様だが、 *割* を使って割合として表示する。

## yen

### 書式

```lisp
(yen stream object
	&optional colon-p at-sign-p	digits-after-dot)
```

### 説明

`jp` と同様だが、 *円*, *銭*, *厘* を使って金銭として表示する。


# TODO

- ~~0 の扱い~~
- ~~負の数~~
- ~~分数~~
- ~~浮動小数点数~~
- ~~桁の上限や下限を越えた時の処置~~
- ~~NAN と 無限値 -- 規定なし~~
- ~~"銭" と "割"~~
- ~~bug at scale parameter~~
- ~~Writing tests.~~
- ~~fix 'XXXXXXXX.' at printing floats.~~
- ~~complex~~
- ~~ratio at "銭"~~
- ~~positional and radix-point~~
- ~~fix for '一千万'~~

# 仕様
- :positional 以外で、 float が float か分からなくなる。
- :positional float で、 小数点以下が0のときは、その0は省略
- 単位なしで小数が出ると ・ が出てくる。
