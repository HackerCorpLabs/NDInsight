# ND-10730 — UNIQUE II for ND-500 (English)

> Status: IN-PROGRESS — real floppy set decoded, including a complete real application example; no installer found

| Field | Value |
|-------|-------|
| ND article number | `ND-10730` |
| Product name | UNIQUE II for ND-500 (English) |
| Functional category | 4th-generation application tools (DIALOGUE/UNIQUE family) |
| CPU target | ND-500 |
| Related products | `ND-210729` UNIQUE II (SIBAS, ND-100) — see [../ND-210729/README.md](../ND-210729/README.md), whose family table lists `ND-210730` as the ND-500-side UNIQUE II/SIBAS article. `ND-10730` (this entry) is presumably an older article number for the same ND-500 product, but that relationship is **not confirmed** — no PD or PI sheet exists for either number. |

## What is known — real floppy set, decoded

Three floppies, `10730A02-EN-1S`/`-2S`/`-3`, downloaded via NDwiki (imaged by Torfinn "Tingo"
Ingolfsen). All three mount cleanly. Real file inventory:

| File | Interpretation |
|---|---|
| `UNIQ-A02:MCRO` | macro source |
| `UNIQ-S-AC-EN-A00:HELP`, `UNIQ-S-MC-EN-A00:HELP` | online help text |
| `UNIQUE-II-S-A02:MENU` | the menu-driven UNIQUICK generator front end (see [ND-210729](../ND-210729/README.md)'s PI-sourced description of UNIQUICK) |
| `UNIQUE-A:BASE` | system base file |
| `UNIQUE-II-A00-0:DEFL` | definitions file |
| `DEFINE-USER-A00:UNIQ` | a `:UNIQ` application file — user/access setup |
| `UNIQPASS-A00:SYMB` | password handling module |
| `UNIQ-S-B1-A00:SYMB`, `UNIQ-S-B2-A00:SYMB` | 1-bank/2-bank runtime split (see [TWO-BANK-PROGRAMS.md](../../../Developer/Workflow/TWO-BANK-PROGRAMS.md)) |
| `UNIQ-SP-FORT-A00:SYMB`, `UNIQ-SP-COB-A00:SYMB` | FORTRAN and COBOL language-specific support modules |
| `CUSTOMER-REG-A00:UNIQ`, `PART-REG-A00:UNIQ`, `ORDER-REG-A00:UNIQ` | **three real, complete sample `:UNIQ` application files — see below** |
| `DDBTABLES-E02:VTM` | VTM terminal-type configuration file (binary; see [VTM-TERMINAL-INTERFACES.md](../../../Developer/Workflow/VTM-TERMINAL-INTERFACES.md)) |
| `UNIQ-S-1-EN-A02:NRF`, `UNIQ-S-2-EN-A02:NRF`, `UNIQ-S-3-EN-A02:NRF` | the compiled UNIQUE II interpreter, split across all 3 disks |
| `UNIQ-DIV-A00:NRF` | a further support module |
| `UNIQ-SCR-VTM-A02:NRF` | the VTM screen-driving layer |

**No installer program and no `:MODE`/`:BATC` script were found** on any of the three floppies —
this appears to be the raw product file set (compiled interpreter + sample applications), not a
guided installer.

## A real, complete worked UNIQUE application — `CUSTOMER-REG-A00:UNIQ`, verbatim

This is the *actual shipped sample* behind the worked example already quoted (from the PI sheet)
on [ND-210729's page](../ND-210729/README.md#building-a-ui--the-real-worked-example) — same
CUSTOMER register, but here as the real file rather than a manual's typeset illustration. It
**resolves** that page's open question about the real field-placeholder character: it is `^`
followed by a repeated type letter (`^N` = numeric, `^A` = alphanumeric), not the manual's
typeset `A....` or `°` placeholders. [MODE-equivalent — decoded from the real `:UNIQ` file]

```
start-form
--------------------------------------------------------------------------------
  -UNIQUE-II-              Customer register.
--------------------------------------------------------------------------------
  Customer: ^NNNNNN

  Name:     ^AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
  Address:  ^AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
            ^AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
            ^AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA

  Telephone:^AAAAAAAAAA                      Shortname : ^AAAAAAAAAAAAAAAAAAAA
--------------------------------------------------------------------------------
  Contact person: ^AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
  Payment condition:^AAAAAAAAAAAAAAAAAAAA
--------------------------------------------------------------------------------

end-form
start-fields
data-base-name=CUSTBASE
register-name=CUSTOMER
          field-name=CUSTOMER        key
          field-name=NAME            alternative-key     update
          field-name=ADDRES1                             update
          field-name=ADDRES2                             update
          field-name=ADDRES3                             update
          field-name=TELNO                               update
          field-name=POPNAME         alternative-key     update
          field-name=CONTPERS        alternative-key     update
          field-name=PAYCOND                             update
end-fields
```

New, real detail this file adds beyond the PI-sheet example: the `update` keyword marking which
fields are editable after the record is retrieved (as opposed to `key`/`alternative-key` fields,
which are lookup keys). `PART-REG-A00:UNIQ` and `ORDER-REG-A00:UNIQ` are two further real sample
applications (parts register, order register) on the same floppy set — not transcribed here, same
shape confirmed by file listing.

## Documentation
- No PD sheet or PI sheet located for `ND-10730` or `ND-210730`.

## Provenance & open items
- Source: three real floppy images, downloaded via NDwiki and decoded in this session
  (`ndfs -t`/`ndtool -x` for listing/extraction, `byte & 0x7F` for the `:UNIQ` sample files).
- **TODO:** `PART-REG-A00:UNIQ` and `ORDER-REG-A00:UNIQ` were confirmed present but not
  individually transcribed here.
- The `^`-placeholder finding should also update
  [ND-210729's open question](../ND-210729/README.md) about the real field-placeholder character.

---
**Parent:** [../README.md](../README.md) (Software catalog)
