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

JP-NUMERAL> (format nil "~/jp-numeral:wari/" 0.123)
"一割二分三厘"

JP-NUMERAL> (format nil "~/jp-numeral:yen/" 12000.67)
"一万二千円六十七銭"
```

## 大字を使用する

`:` 修飾子を使用する。

```
JP-NUMERAL> (format nil "~:/jp-numeral:jp/" 12345687890)
"壱百弐拾参億四千五百六拾八万七千八百九拾"

JP-NUMERAL> (format nil "~:/jp-numeral:wari/" 0.123)
"壱割弐分参厘"

JP-NUMERAL> (format nil "~:/jp-numeral:yen/" 12000.67)
"壱万弐千円六拾七銭"
```

## 旧字体を使用する

`@` 修飾子を使用する。

```
JP-NUMERAL> (format nil "~@/jp-numeral:jp/" 12345687890)
"壹佰貳拾參億肆仟伍佰陸拾捌萬柒仟捌佰玖拾"

JP-NUMERAL> (format nil "~@/jp-numeral:wari/" 0.123)
"壹割貳分參釐"

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

(stub)

漢数字プリンタのエントリポイント。

## jp

(stub)

`format-jp-numeral` を cl:format の `~/` の関数呼びだしに合う形式で呼ぶ。

## j

`jp` と同義。

## wari

(stub)

*割* を使って割合を表示しようとする。

## w

`wari` と同義。

## yen

(stub)

*円*, *銭*, *厘* を使って金銭を表示しようとする。

## y

`yen` と同義。


# TODO

- ~~0 の扱い~~
- ~~負の数~~
- ~~分数~~
- ~~浮動小数点数~~
- ~~桁の上限や下限を越えた時の処置~~
- ~~NAN と 無限値 -- 規定なし~~
- ~~"銭" と "割"~~
- ~~bug at scale parameter~~
- Writing tests.
- fix 'XXXXXXXX.' at printing floats.
- complex
- ratio at "銭"
- positional and radix-point
- fix for '一千万'

# 仕様
- :positional 以外で、 float が float か分からなくなる。
- :positional float で、 小数点以下が0のときは、その0は省略
- 単位なしで小数が出ると ・ が出てくる。
