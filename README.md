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

# Background

Perhaps you, like many technically minded folks who
went through school with their trusty TI calculator,
have a catalog of TI-BASIC programs that you wrote
for both fun and utility.

If I look through my backup directory and see WYCKOV.85p,
I might try `vim WYCKOV.85p` and see:

```
**TI85**^Z^L^@Group file dated Sun Sep 26 16:12:47 202^@^@w^B
^@i^B^R^FWYCKOVi^Bg^BD0^@^K5NEGoìoàM^@oL^PD1^@/-xyz^@/5xyz/D2^@/-GPos^@/4GP/D3^@/-Pute^@/8CMPUTE/D4^@/-Neg^@/5NEG/D5^@/-Quit^@/3Q^QoàGP^@oìoë^PD1^@/D1^@/-Enter general position functions^@oè3Xoè3Yoè3Zoî^P3X/3X^Qoî^P3Y/3Y^Qoî^P3Z/3Z^Qoá^@^MM^@oàxyz^@oç2^Axoç2^Ayoç3zoá^@^MM^@oàNEG^@oD1^@^K5NEGoàCMPUTE^@o3X^K4xpo3Y^K4ypo3Z^K4zpoØ5NEGPD1^@oÙo¡4xp^K4xpo¡4yp^K4ypo¡4zp^K4zpoD0^@^K5NEGoÞoØ4xpTD1^@oÙo4xpaD1^@^K4xpoÞoØ4xpQD0^@oÙo4xp`D1^@^K4xpoÞoØ4ypTD1^@oÙo4ypaD1^@^K4ypoÞoØ4ypQD0^@oÙo4yp`D1^@^K4ypoÞoØ4zpTD1^@oÙo4zpaD1^@^K4zpoÞoØ4zpQD0^@oÙo4zp`D1^@^K4zpoÞoë^PD7^@/D1^@/-                    ^@^Qoë^PD7^@/D1^@/-(^@^Qoë^PD7^@/D2^@/4xp^Qoë^PD7^@/D8^@/-,^@^Qoë^PD7^@/D9^@/4yp^Qoë^PD7^@/D14^@/-,^@^Qoë^PD7^@/D15^@/4zp^Qoë^PD7^@/D21^@/-)^@^Qoá^@^MM^@oàQ^@^TÁ
```

Not so nice. As mentioned above, there are some options for decoding these files, but they didn't work for me, so I made `ascii85x`.

```
$ ascii85x WYCKOV.85p

Variable "WYCKOV":
0→NEG
ClLCD
Lbl M
Menu(1,"xyz",xyz,2,"GPos",GP,3,"Pute",CMPUTE,4,"Neg",NEG,5,"Quit",Q)
Lbl GP
ClLCD
Outpt(1,1,"Enter general position functions"
InpStX
InpStY
InpStZ
St▸Eq(X,X)
St▸Eq(Y,Y)
St▸Eq(Z,Z)
Goto M
Lbl xyz
Promptx
Prompty
Promptz
Goto M
Lbl NEG
1→NEG
Lbl CMPUTE
X→xp
Y→yp
Z→zp
IfNEG==1
Then
-xp→xp
-yp→yp
-zp→zp
0→NEG
End
Ifxp>=1
Then
xp-1→xp
End
Ifxp<0
Then
xp+1→xp
End
Ifyp>=1
Then
yp-1→yp
End
Ifyp<0
Then
yp+1→yp
End
Ifzp>=1
Then
zp-1→zp
End
Ifzp<0
Then
zp+1→zp
End
Outpt(7,1,"                    ")
Outpt(7,1,"(")
Outpt(7,2,xp)
Outpt(7,8,",")
Outpt(7,9,yp)
Outpt(7,14,",")
Outpt(7,15,zp)
Outpt(7,21,")")
Goto M
Lbl Q
```

Much better, even if not terribly interesting!

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

