## Page 1

# Til: Alle interesserte
Fra: OPH  
Dato: 04.03.87

## NOEN TIPS VEDRØRENDE INSTALLASJON AV COSMOS X.21 OPTION

* X.21 Går på HDLC-Link utgangen.

* Switch 13A : Alle OFF.

* Kabelen er: 325383 X-OCE X21 N100

* I S3-config skal HDLC'en være 1.

* Bruk ikke START-LINK i (UT)XMSG-COM.

### For SW-installasjon se PD-ark.
- X21NS-IN-Dxx:PROG må kjøres fullt ut.
- X21NS-START-Dxx:MODE er den eneste filen som må redigeres etter en riktig SW-inst.

### Eks. på redigering av X21NS-START-Dxx:MODE.

```
Def-net-local-endpoint,X21NS,X21-MASKIN,N,N,N,Y,1374,1360,96,123456

Def-net-remote-endpoint,REMOTE-MASKIN,X21NS,654321,200,128,0,96
```

- Du kan gå inn i å(UT)X-C for å se hva de enkelte parametere står for, eventuelt lese manualen COSMOS X.21 Option Operators Guide.

* Sjekk at XMSG har riktig patch-level (pr. 4/3-87 er det Level 06).  
Patch-level 01 er en nødvendighet for å få X.21 til å gå.

---

