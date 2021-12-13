# ascii85x

## Introduction

This module is meant to display information from
TI-85 variable files in a human readable way.

There were a few reasons for making this in 2021:

1. ascii85p, an ancient program for displaying
   program text, does not come with source code
2. TokenIDE, a more modern and very featureful
   tool, is written in C#, and I didn't want to
   use mono.
3. TokenIDE also does not come with source code.
4. Just let me have some fun.

## Background

Perhaps you, like many technically minded folks who
went through school with their trusty TI calculator,
have a catalog of TI-BASIC programs that you wrote
for both fun and utility.

If I look through my backup directory and see `RND.85p`,
I might wonder what's in there and try `vim RND.85p`:

```
**TI85**^Z^L^@Group file dated Sun Sep 26 16:12:47 202^@^@^L^A^G^@^A^A^R^CRND^A^Aÿ^@D0^@^K3AoD0^@^K3BoD0^@^K3CoD0^@^K3DoD0^@^K3SoD0^@^K=^DoD100^@^K=^Eo¡D35^@^K=^FoD35^@^K=^GoD25^@^K=^BoD5^@^K=^Coìo<91>o<83>oÛ^P^PCUD22^@^Q@^P3CSD500^@^Q^QoØ^P^P¥^PApD2^@^Q^QPD1^@^QoÙo3A`D1^@^K3AoÚo3B`D1^@^K3BoÞo3C`D1^@^K3Co3S`^P3Aa3B^Q^K3So<98>^P3CqD5^@/3Aa3B^Qo<98>^P3CqD5^@/3Sq3C^QoÞoë^PD2^@/D1^@/3Aoë^PD3^@/D1^@/3Boë^PD4^@/D1^@/3Sq3Cv?
```

Not so nice. As mentioned above, there are some options for decoding these files, but they didn't work for me, so I made `ascii85x`.

```
$ ascii85x RND.85p

Program "RND":
0→A
0→B
0→C
0→D
0→S
0→xMin
100→xMax
-35→yMin
35→yMax
25→xScl
5→yScl
ClLCD
FnOff 
ClDrw
While ((getKy≠22) and (C<=500))
If ((int(rand*2))==1)
Then
A+1→A
Else
B+1→B
End
C+1→C
S+(A-B)→S
PtOn(C/5,A-B)
PtOn(C/5,S/C)
End
Outpt(2,1,A
Outpt(3,1,B
Outpt(4,1,S/C
```

Much better!

## Usage

`ascii85x` will read and display any variable or group file as output by the TI-85 link backup utility.
The variable types supported are:

| Type       | Extension |
|------------|-----------|
|RealValue   | 85n       |
|ComplexValue| 85c       |
|Vector      | 85v       |
|List        | 85l       |
|Matrix      | 85m       |
|Constant    | 85k       |
|Equation    | 85y       |
|String      | 85s       |
|Program     | 85p       |

A group file (`85g`) may contain many of these types of variables.

```
$ ascii85x --help
Usage: ascii85x [-i|--info] [-D|--debug] [-v|--verbose] VARFILE [-V|--version]
  Convert TI-85 variable files to text

Available options:
  -i,--info                Show file info only
  -D,--debug               Show extra variable details
  -v,--verbose             Show variable file summary
  VARFILE                  85x variable file
  -V,--version             Show version only
  -h,--help                Show this help text
```

