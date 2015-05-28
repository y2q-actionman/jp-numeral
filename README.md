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
CL-USER> (format nil "~/jp-numeral:pprint-jp-numeral/" 12345687890)
"百二十三億四千五百六十八万七千八百九十"
```

## 大字を使用する

`:` 修飾子を使用する。

```
CL-USER> (format nil "~:/jp-numeral:pprint-jp-numeral/" 12345687890)
"壱百弐拾参億四千五百六拾八万七千八百九拾"
```

## 旧字体を使用する

`@` 修飾子を使用する。

```
CL-USER> (format nil "~@/jp-numeral:pprint-jp-numeral/" 12345687890)
"壹佰貳拾參億肆仟伍佰陸拾捌萬柒仟捌佰玖拾"
```

## 位取り記数法を使用する

`:` 修飾子と `@` 修飾子を併用する。

```
CL-USER> (format nil "~:@/jp-numeral:pprint-jp-numeral/" 12345687890)
"一二三四五六八七八九〇"
```

# API

## pprint-jp-numeral

(stub)

## j

(stub)


# TODO

- ~~0 の扱い~~
- ~~負の数~~
- ~~分数~~
- 浮動小数点数
- 桁の上限や下限を越えた時の処置
- NAN と 無限値 -- 規定なし

- "銭" と "割"
