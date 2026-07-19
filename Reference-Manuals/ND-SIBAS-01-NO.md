## Page 1

# PARAMETERE I SIBAS-KALL

## MODE

dette er et enkelt heltall, som sier om run-uniten ønsker å endre databasen eller ikke.

## DATA-BASE-NAME

dette definerer en variabel eller en tabell i bruker-området, som inneholder det åtte-tegns navnet på basen. Navnet må være det samme som det som er definert i data-base-skjemaet.

## PASSWORD ELLER NEW-PASSWORD

dette viser til en variabel eller en tabell som inneholder åtte-tegns passordet.

## REALM-NAME

dette viser til en variabel eller en tabell som inneholder åtte-tegns navnet på realmen.

## NO.-OF-REALMS

dette viser til en enkel heltalls-variabel som inneholder antallet realmer som skal klargjøres med ready-realm.

## KEY-NAME

dette viser til en variabel eller en tabell som inneholder åtte-tegns navnet på et felt eller et gruppfelt som er definert i database-skjemaet som en index-nøkkel eller som en kalkulert-nøkkel for den relevante record-typen.

## KEY-VALUE

dette viser til en variabel eller en tabell som inneholder eller mottar verdien på en nøkkel.

## LOW-LIMIT, HIGH-LIMIT

disse viser til felter eller tabeller som inneholder nedre og øvre grenseverdie på en korresponderende index-nøkkel. Lengden og typen på low- og high-limit må være likt med nøkkelen.

## SET-NAME

dette viser til en variabel eller en tabell som inneholder åtte-tegns navnet på et sett definert i database-skjemaet.

---

[Scanned by Jonny Oddene for Sintran Data © 2024]

---

## Page 2

# TEMPORARY-DATA-BASE-KEY

dette er en enkel heltalls-variabel. Når man bruker "0" i denne parameter vil kallet (f.eks GET eller MODIFY) arbeide på den aktuelle recorden. (definert av CRUI, se 2.4.1.2). Dersom du ønsker å arbeide på en record som ikke er aktuell lenger, må du ha brukt REMEMBER mens recorden fremdeles var aktuell. Et nummer som identifiserer recorden ville da blitt lagret i din "temporary-data-base-key"-variabel. Ved å bruke dette nummeret istedenfor "0" vil kallet arbeide på denne recorden istedenfor den nå kurrante. Husk at denne parameteren kun er out-put ved REMEMBER, ellers alltid in-put.

# TEMPORARY-SEARCH-REGION-INDICATOR

dette er en enkel heltallsvariabel. Verdien "0" i dette parameter betyr at det aktuelle søkeområdet skal brukes, som definert i CSRI (se 2.4.1.2). Dersom du ønsker å arbeide med et søkeområde som ikke lenger er aktuelt, må du ha brukt REMEMBER for dette området da det var aktuelt. Identifikasjonsnummeret som da ble lagret i "temporary-search-region-indicator"-variablen, må da brukes istedenfor "0". Husk at denne parameter kun er out-put ved REMEMBER, ellers alltid in-put.

# NO.-OF-ITEMS, NO.-WANTED, NO.-FOUND

- **no.-of-items** er en enkel heltalls-variabel som inneholder antallet item navn som er i item-listen. Den må ha en verdi større eller lik en, og mindre eller lik totalantallet av items og gruppitems.
- **no.-wanted** er en heltalls-variabel som sier hvor mange records eller nøkler run-uniten ønsker å lese.
- **no.-found** forteller run-uniten hvor mange records eller nøkler som er mottatt.

# ITEM-LIST

dette er en variabel eller en tabell som inneholder åtte-tegns-navnene på data-itemene eller gruppe-itemene definert i databasens skjema for recordtypen.

# ITEM-VALUES

dette er en variabel eller en tabell som inneholder eller mottar verdiene fra itemene og gruppe-itemene som ble navngitt i item-listen, og i samme orden. Det må gis plass for hvert item som korresponderer med data-format definisjonen i database-skjemaet.

# OPTION-CODE, USAGE-MODE, PROTECTION-MODE

dette er enkle heltallsvariabler som brukes for å spesifisere spesielle opsjoner ved noen DML-uttrykk.

---

## Page 3

# KEY-LENGHT, VALUE-LENGHT

dette er enkle heltallsvariabler som definerer lengden på et felt som sendes til SIBAS, uttrykt i antall ord (16 bit)

# STATUS

dette er en output parameter (enkel heltall-variabel), som DBCS setter til forskjellige verdier.

```
1  vellykket
0  normal, ikke vellykket, f.eks slutt på søkeområde
-1 feil, sjekk ved å kalle SDBEC
-2 til -6 etter SOPDB
```

Andre minusverdier indikerer også feil.

---

## Page 4

# DATABASEKALL

## APN DATABASE

```
CALL SOPDB (mode,
            database name,
            password,
            status)

15473 endrer, O endrer ikke basen
```

## LUKK DATABASE

```
CALL SCLDB (data-base-name,
            status)
```

## READY REALM

```
CALL SRRLM (no.-of-realms,
            realm-names,
            usage-modes,
            protection-mode,
            status)

-1 = alle i basen
0 = find, get, remember, forget
1 = 0 + store, connect, insert
2 = alt
0 = alt bortsett fra erase
1 = alt
Dette gjelder andre run-uniter ?
```

## FINISH REALM

```
CALL SFRLM (no.-of-realms,
            realm-names,
            status)

-1 = alle i basen
```

---

## Page 5

# DIREKTE SØK

## NØKKELSØK

**FIND-USING-KEY**
```
CALL SFTCH (realm-name,
            key-name,
            key-value,
            staus,
            key-length)
```

## FINN FØRSTE NØKKEL MELLOM GRENSER

**FIND-FIRST-BETWEEN-LIMITS-USING-KEY**
```
CALL SFEBL (realm-name,
            key-name,
            low-limit,
            high-limit,
            staus,
            key-length)
```

## FINN SISTE NØKKEL MELLOM GRENSER

**FIND-LAST-BETWEEN-LIMITS-USING-KEY**
```
CALL SFLBL (realm-name,
            key-name,
            low-limit,
            high-limit,
            staus,
            key-length)
```

## FINN FØRSTE I REALM

**FIND-FIRST-IN-REALM**
```
CALL SRFIR (realm-name,
            status)
```

---

## Page 6

# RELATIVT SØK

## FINN FØRSTE I SETT
```
FIND-FIRST-IN-SET
CALL SRFSM (temporary-data-base-key,
            set-name,
            status)
```

## FINN SISTE I SETT
```
FIND-LAST-IN-SET
CALL SRLSM (temporary-data-base-key,
            set-name,
            status)
```

## FINN FORIGE I SETT
```
FIND-PRIOR-IN-SET
CALL SRPSM (temporary-data-base-key,
            set-name,
            status)
```

## FINN NESTE I SETT
```
FIND-NEXT-IN-SET
CALL SRNSM (temporary-data-base-key,
            set-name,
            status)
```

## FINN NESTE I SØKEOMRÅDET
```
FIND-NEXT-IN-SEARCH-REGION
CALL SRNIS (temporary-data-base-key,
            temporary-search-region-indicator,
            status)
```

## FINN FORIGE I SØKEOMRÅDET
```
FIND-PRIOR-IN-SEARCH-REGION
CALL SRPIS (temporary-data-base-key,
            temporary-search-region-indicator,
            status)
```

---

## Page 7

# FINN SETT EIER

```
CALL SRSOW (temporary-database-key,
            set-name,
            status)
```

# HENT

### HENT  
**GET**

```
CALL SGET (temporary-database-key,
           no.-of-items,
           item list,
           item values,
           status)
```

### HENT NESTE
**GETN**

```
CALL SGETN (temporary-database-key,
            temporary search region indicator,
            no. wanted,
            no.-of-items,
            item list,
            item values,
            no. found,
            status)
```

### HENT NESTE NØKLER
**GET-INDEXES**

```
CALL SGIXN (temporary-database-key,
            temporary search region indicator,
            no. wanted,
            item values,
            no. found,
            status)
```

# MODIFISER

```
CALL SMDFY (temporary-database-key,
            no. of items,
            item list,
            item values,
            status,
            value length)
```

# LAGR

```
CALL STORE (realm name,
            no. of items,
            item list,
            item values,
            status,
            value length)
```

---

## Page 8

# Technical Operations

## FJERN

```
CALL SRASE (temporaray-database-key,
            option code,
            status)
```

## CONNECT

## DISCONNECT

## INSERT

## REMOVE

## REMEMBER

```
CALL SREMB (temporary id,
            option code,
            status)

0 = record
1 = search-region
```

## FORGET

```
CALL SFORG (temporary id,
            option code,
            status)

0 = record
1 = search-region
2 = all records
3 = all search-regions
```

## LOCK

## UNLOCK

## CHANGE-PASSWORD

## ACCEPT

feilhåndtering

## ERASE ELEMENT

## ACCUMULATE

## FINN OG LES

### FETCH-GET

```
CALL SFTGT (realm-name,
            key-name,
            lenght of key,
            key value,
            number of items,
            item list,
            item values,
            status)
```

## GET SCHEMAS INFORMATION

SIBASPAR/BH

Scanned by Jonny Oddene for Sintran Data © 2024

---

