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

If I look through my backup directory and see `CONICS.85p`,
I might wonder what's in there and try `vim CONICS.85p`:

```
**TI85**^Z^L^@Single file dated Mon Sep 27 15:41:32 20^@^@,^A                  
^@^^^A^R^FCONICS^^^A^\^AìoVnboç3A/3B/3C/3D/3E/3Fo;^By1?^P¡^P3B2^Ax`3E^Q`^P ^P^P3B2^Ax`3E^QJaD4^@^P3C^Q^P3A2^AxJ`3D2^Ax`3F^Q^Q^Q^Qq^PD2^@^P3C^Q^Qo;^By2?^P¡^P3B2^Ax`3E^Qa^P ^P^P3B2^Ax`3E^QJaD4^@^P3C^Q^P3A2^AxJ`3D2^Ax`3F^Q^Q^Q^Qq^PD2^@^P3C^Q^QoØ^P^P3AP3C^Q@^P3BUD0^@^Q^QnÙoD45^@^K3»oÚoD.5^@®^P3Bq^P3Aa3C^Q^Q^K3»oÞo;^By3?^P®3»^Q2^Axo;^By4?^P®^P3»a^PBqD2^@^Q^Q^Q2^Axo<91>o<84>n<89>o<90>D1^@/D2^@/D3^@/D4^@oêÑH
```

Not so nice. As mentioned above, there are some options for decoding these files, but they didn't work for me, so I made `ascii85x`.

```
$ ascii85x CONICS.85p

Variable "CONICS":
ClLCD
Radian:Func
Prompt A,B,C,D,E,F
y1=(-(Bx+E)+(√((Bx+E)²-4(C)(Ax²+Dx+F))))/(2(C))
y2=(-(Bx+E)-(√((Bx+E)²-4(C)(Ax²+Dx+F))))/(2(C))
If ((A==C) and (B≠0)):Then
45→α
Else
.5tan (B/(A-C))→α
End
y3=(tan α)x
y4=(tan (α-(π/2)))x
FnOff 
ZStd:ZSqr
FnOn 1,2,3,4
DispG
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

