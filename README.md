# Abstract

Common Lisp で漢数字を出すというネタパッケージ。`cl:format` との統合も可能。

This is a fun package for printing numbers as Japanese numerals. This
can be integrated with `cl:format`.

# License

The MIT License. See LICENSE file.

# Loading

## Loading by quicklisp

[![Quicklisp](http://quickdocs.org/badge/jp-numeral.svg)](http://quickdocs.org/jp-numeral/)

```lisp
(ql:quickload "jp-numeral")
```

## or, Loading manually

### Libraries depending on

* asdf
* babel
* alexandria
* 1am (only by test codes.)

### Loading

```lisp
(asdf:load-asd "jp-numeral.asd")
(asdf:load-system :jp-numeral)
```

For running tests, do below additionally.

```lisp
(asdf:load-asd "jp-numeral-test.asd")
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

Please see docstrings of these functions in `jp-numeral` package:

- `format-jp-numeral`
- `jp`
- `wari`
- `yen`
