title = "Convert KiCAD BOM to Manual Pick and Place Files"
slug = "kicad-bom-to-pick-and-place"
timestamp = "2023-04-10"
tags = ["espresso", "hardware"]
---
When I assemble a PCB by hand, I like to have a list of all footprints and their respective components on hand. My PCB usually has designators on it, R1, R2, and so on, but not the actual component values, in this case resistances, mostly for space reasons. KiCAD can generate a BOM CSV, but that is more focused on purchasing, so components are grouped together by type, and not listed by designator, which makes the lookup tedious. Here I am sharing a small awk program which converts that BOM into a more useful format for this purpose:

```awk
BEGIN {
    FS = ";"
}

NR > 1 {
    gsub(/"/, "", $2);
    gsub(/"/, "", $5);
    split($2,desig,",");
    for (i in desig)
        print desig[i] "\t" $5
}
```

This program works as of KiCAD 7, and can be used like so:[^1]

```sh
gawk -f bom.awk bom.csv | sort -V
```

A full example, on a partial BOM:

```sh
$ head bom.csv
"Id";"Designator";"Footprint";"Quantity";"Designation";"Supplier and ref";
1;"C15,C16,C17,C20,C8,C7";"C_Disc_D4.7mm_W2.5mm_P5.00mm";6;"100nF";;;
2;"C18,C19";"C_Disc_D4.7mm_W2.5mm_P5.00mm";2;"10nF";;;
3;"R15,R17,R24,R19,R27";"R_Axial_DIN0207_L6.3mm_D2.5mm_P10.16mm_Horizontal";5;"100k";;;
4;"R16,R23,R18,R14";"R_Axial_DIN0207_L6.3mm_D2.5mm_P10.16mm_Horizontal";4;"220R";;;
5;"R22,R2,R1,R20,R25,R21";"R_Axial_DIN0207_L6.3mm_D2.5mm_P10.16mm_Horizontal";6;"10k";;;
6;"SW2,SW1,SW3";"PinHeader_2x01_P2.54mm_Vertical";3;"SW_Push";;;
7;"SW4";"PinHeader_1x05_P2.54mm_Vertical";1;"RotaryEncoder_Switch";;;
8;"U1";"PinHeader_1x10_P2.54mm_Vertical";1;"NHD-C0220BIZ";;;
9;"R4";"R_Axial_DIN0207_L6.3mm_D2.5mm_P10.16mm_Horizontal";1;"47R";;;

$ head bom.csv | awk -f bom.awk | sort -V
C7	100nF
C8	100nF
C15	100nF
C16	100nF
C17	100nF
C18	10nF
C19	10nF
C20	100nF
R1	10k
R2	10k
R4	47R
R14	220R
R15	100k
R16	220R
R17	100k
R18	220R
R19	100k
R20	10k
R21	10k
R22	10k
R23	220R
R24	100k
R25	10k
R27	100k
SW1	SW_Push
SW2	SW_Push
SW3	SW_Push
SW4	RotaryEncoder_Switch
U1	NHD-C0220BIZ
```


[^1]: I'm using `gawk` because I'm on macOS, where regular `awk` is BSD awk. On Linux this would not apply.
